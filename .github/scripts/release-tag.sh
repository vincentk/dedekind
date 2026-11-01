#!/usr/bin/env bash
# Tags a commit with a release's name and pushes the tag, from which the
# release's wheel is then built.  A tag already there (a resumed release) is
# kept.
#
# Usage: release-tag.sh <tag, v0.YYYYMMDD.n> <commit>
set -euo pipefail

tag=$1
commit=$2
if git rev-parse -q --verify "refs/tags/$tag" >/dev/null; then
  echo "$tag exists"
  exit 0
fi
git config user.name "github-actions[bot]"
git config user.email "41898282+github-actions[bot]@users.noreply.github.com"
git tag -a "$tag" -m "Release ${tag#v}" "$commit"
git push origin "$tag"
