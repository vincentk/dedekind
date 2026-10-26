#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
cd "$ROOT_DIR"

# Prefer the repo venv when available.
if [[ -f .venv/bin/activate ]]; then
  # shellcheck disable=SC1091
  source .venv/bin/activate
fi

python -m pip install --quiet pytest pytest-cov

mkdir -p build

pytest src/test/python \
  --cov=dedekind \
  --cov-report=xml:build/python-coverage.xml \
  --cov-report=term-missing \
  "$@"
