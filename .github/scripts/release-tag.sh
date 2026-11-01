#!/usr/bin/env bash
# Tags a commit with a release's version and pushes the tag: the release's
# name, from which its wheel is then built.  A tag already there (a resumed
# release) is kept.
#
# Usage: release-tag.sh <version> <commit>
set -euo pipefail

version=$1
commit=$2
if git rev-parse -q --verify "refs/tags/v$version" >/dev/null; then
  echo "v$version exists"
  exit 0
fi
git config user.name "github-actions[bot]"
git config user.email "41898282+github-actions[bot]@users.noreply.github.com"
git tag -a "v$version" -m "Release $version" "$commit"
git push origin "v$version"
