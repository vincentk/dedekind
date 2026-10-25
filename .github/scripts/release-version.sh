#!/usr/bin/env bash
# Prints the next release version for a commit, or "skip".
#
# Version: 0.<YYYYMMDD>.<n>+<short hash>, semantic versioning with the date
# (ISO 8601 basic, UTC) as the minor number, n the count of releases already
# cut on that date (0 in practice), and the commit as build metadata.
#
# Skips a commit that is already tagged, and, unless forced, any commit less
# than MIN_HOURS after the last release tag.
#
# Usage: release-version.sh <commit> <min_hours> [force: 0|1]
# NOW (seconds since the epoch) may be set to test the clock.
set -euo pipefail

commit=$1
min_hours=$2
force=${3:-0}

if git tag --points-at "$commit" | grep -q '^v0\.'; then
  echo "skip"
  echo "commit $commit is already released" >&2
  exit 0
fi

now=${NOW:-$(date -u +%s)}
last=$(git for-each-ref --sort=-taggerdate --format='%(taggerdate:unix)' 'refs/tags/v0.*' | head -1)
if [ "$force" != 1 ] && [ -n "$last" ] && [ $((now - last)) -lt $((min_hours * 3600)) ]; then
  echo "skip"
  echo "last release $(((now - last) / 3600)) h ago, under $min_hours h" >&2
  exit 0
fi

if date -u -d "@$now" +%Y%m%d >/dev/null 2>&1; then
  day=$(date -u -d "@$now" +%Y%m%d)
else
  day=$(date -u -r "$now" +%Y%m%d)
fi
n=$(git tag -l "v0.${day}.*" | wc -l | tr -d ' ')
echo "0.${day}.${n}+$(git rev-parse --short=7 "$commit")"
