#!/usr/bin/env bash
# Prints the next release version for a commit, or "skip".
#
# Version: 0.<YYYYMMDD>.<n>, semantic versioning with the date (ISO 8601
# basic, UTC) as the minor number and n one past the highest release already
# cut on that date (0 in practice): a PEP 440 public version, as PyPI requires (it
# refuses the local "+<hash>" part).  The tag names the commit.
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
# One past the highest n cut that day, not their count: a deleted earlier tag
# must not hand out a number again (nor may PyPI, which never reuses one).
last_n=$(git tag -l "v0.${day}.*" | sed -n "s/^v0\.${day}\.\([0-9][0-9]*\)$/\1/p" | sort -n | tail -1)
echo "0.${day}.$((${last_n:--1} + 1))"
