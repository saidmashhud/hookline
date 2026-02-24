#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

if [[ ! -d .git ]]; then
  echo "ERROR: not a git repository: $ROOT_DIR" >&2
  exit 1
fi

dirty_count="$(git status --porcelain | wc -l | tr -d ' ')"
if [[ "$dirty_count" != "0" ]]; then
  echo "ERROR: git worktree is dirty (${dirty_count} files). Clean/stage/commit first." >&2
  git status --short >&2
  exit 2
fi

TAG="${TAG:-hookline/v$(date -u +%Y%m%d-%H%M%S)}"
if git rev-parse -q --verify "refs/tags/${TAG}" >/dev/null; then
  echo "ERROR: tag already exists: ${TAG}" >&2
  exit 1
fi

BASELINE_DIR="$ROOT_DIR/.baseline"
mkdir -p "$BASELINE_DIR"
TS="$(date -u +%Y%m%d-%H%M%S)"
BASELINE_FILE="$BASELINE_DIR/hookline-baseline-${TS}.md"

BRANCH="$(git rev-parse --abbrev-ref HEAD)"
COMMIT="$(git rev-parse HEAD)"
COMMIT_SHORT="$(git rev-parse --short HEAD)"
COMMIT_DATE="$(git show -s --format=%cI HEAD)"

cat > "$BASELINE_FILE" <<EOF
# HookLine Release Baseline

- Generated (UTC): $(date -u +'%Y-%m-%d %H:%M:%S')
- Repo: \`$ROOT_DIR\`
- Branch: \`$BRANCH\`
- HEAD: \`$COMMIT_SHORT\` (\`$COMMIT\`)
- Commit date: \`$COMMIT_DATE\`
- Tag: \`$TAG\`

## Release Gate References

- Readiness gate doc: \`/Users/saidmashhud/Projects/personal/hookline/docs/production/readiness-gate.md\`
- Production gate script: \`/Users/saidmashhud/Projects/personal/hookline/test/production-readiness.sh\`
- Backup/restore doc: \`/Users/saidmashhud/Projects/personal/hookline/docs/production/backup.md\`
EOF

git tag -a "$TAG" -m "hookline release baseline ${TS}"

echo "BASELINE_FILE=$BASELINE_FILE"
echo "TAG_CREATED=$TAG"
