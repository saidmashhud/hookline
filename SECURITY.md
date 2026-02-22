# Security Policy

## Supported Versions

| Version | Supported |
|---------|-----------|
| 0.1.x   | ✅        |

## Reporting a Vulnerability

Please **do not** open a public GitHub issue for security vulnerabilities.

Instead, email **security@gatepulse.dev** with:
- Description of the vulnerability
- Steps to reproduce
- Potential impact
- Suggested fix (optional)

We will respond within **48 hours** and aim to release a patch within **7 days**
for critical vulnerabilities.

## Disclosure Policy

We follow [coordinated disclosure](https://en.wikipedia.org/wiki/Coordinated_vulnerability_disclosure).
Once a fix is available, we will publish a security advisory on GitHub.

## Security Considerations

- **API keys**: Rotate `GP_API_KEY` regularly in production
- **Webhook secrets**: Use strong per-endpoint signing secrets
- **TLS**: Run behind a TLS-terminating proxy (nginx, Caddy) in production
- **Network**: Restrict `gp_store` UNIX socket permissions to the gatepulse user
