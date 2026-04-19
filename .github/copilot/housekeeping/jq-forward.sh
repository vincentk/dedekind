#!/usr/bin/env bash
set -euo pipefail

if ! command -v jq >/dev/null 2>&1; then
  echo "Error: jq is not installed or not on PATH." >&2
  exit 127
fi

if [[ $# -eq 0 ]]; then
  cat >&2 <<'EOF'
Usage: .github/copilot/housekeeping/jq-forward.sh <jq-args...>

This script is a narrow wrapper that forwards arguments directly to jq.
Examples:
  .github/copilot/housekeeping/jq-forward.sh --version
  .github/copilot/housekeeping/jq-forward.sh -r '.[] | .number' /tmp/issues.json
EOF
  exit 2
fi

exec jq "$@"
