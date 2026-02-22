# Secrets Rotation

GatePulse v2.0 encrypts endpoint webhook secrets at rest using AES-256-GCM.
This guide covers initial setup, key rotation, and emergency procedures.

## Initial Setup

Set `GP_MASTER_KEY` to a 32-byte hex string before starting GatePulse with encryption enabled:

```bash
# Generate a new key
export GP_MASTER_KEY=$(openssl rand -hex 32)

# Persist it securely (example: write to a secrets manager)
echo "GP_MASTER_KEY=$GP_MASTER_KEY" >> /etc/gatepulse/secrets.env
chmod 600 /etc/gatepulse/secrets.env
```

> **Never commit `GP_MASTER_KEY` to version control.**

When `GP_MASTER_KEY` is set, all new endpoint secrets are encrypted before storage.
To encrypt existing plaintext secrets from a v1.x deployment, call the rotate endpoint after upgrade.

## Rotating the Master Key

Key rotation re-encrypts all endpoint secrets without downtime:

### 1. Set the new key

```bash
export GP_MASTER_KEY_NEW=$(openssl rand -hex 32)
```

### 2. Call the rotate API

```bash
curl -X POST http://localhost:8080/v1/admin/rotate-secrets \
  -H "Authorization: Bearer $GP_API_KEY"
```

This endpoint:
1. Reads all endpoints from the store
2. Decrypts each secret with the current `GP_MASTER_KEY`
3. Re-encrypts each secret with `GP_MASTER_KEY_NEW`
4. Writes the updated endpoint records back to the store

The operation is atomic per endpoint. If interrupted, re-run — it is idempotent.

### 3. Update the running configuration

```bash
export GP_MASTER_KEY=$GP_MASTER_KEY_NEW
# Restart or reload GatePulse to pick up the new key
```

## Key Storage

Recommended secret management integrations:

| Platform | Method |
|----------|--------|
| AWS | SSM Parameter Store (SecureString) or Secrets Manager |
| GCP | Secret Manager |
| HashiCorp Vault | `kv-v2` or `transit` (use transit for envelope encryption) |
| Kubernetes | Sealed Secrets or external-secrets operator |
| Self-hosted | `gpg`-encrypted file loaded at startup |

### Kubernetes example (external-secrets)

```yaml
apiVersion: external-secrets.io/v1beta1
kind: ExternalSecret
metadata:
  name: gatepulse-master-key
spec:
  refreshInterval: 1h
  secretStoreRef:
    name: vault-backend
    kind: ClusterSecretStore
  target:
    name: gatepulse-secret
    creationPolicy: Owner
  data:
    - secretKey: GP_MASTER_KEY
      remoteRef:
        key: secret/gatepulse/master-key
        property: value
```

## Verifying Encryption

After rotation, confirm no plaintext secrets are stored:

```bash
# For C store backend — scan segment files
strings /var/lib/gatepulse/seg-*.gps | grep -E '"secret"\s*:\s*"[^*]' | wc -l
# Should print 0

# For Postgres backend
SELECT id, url, LEFT(secret_enc, 8) FROM endpoints;
# All secret_enc values should start with "v1:" (versioned ciphertext prefix)
```

## Emergency: Key Loss

If `GP_MASTER_KEY` is lost:

1. **Endpoints continue to deliver** — the key is only needed to decrypt secrets for new deliveries. In-flight jobs carry the plaintext secret in memory (not persisted).
2. Affected endpoints must have their secrets reset via `PATCH /v1/endpoints/:id` with a new `secret` value.
3. Notify endpoint owners to update their signature verification secret.

> There is no recovery path if the master key is lost and secrets are needed again — this is by design (encryption at rest).

## Audit Trail

Every call to `POST /v1/admin/rotate-secrets` is recorded in the audit log:

```bash
gp admin audit --limit 10 | jq '.items[] | select(.action == "secrets.rotated")'
```
