#!/usr/bin/env bash
# Prints, as GITHUB_OUTPUT lines, whether a commit is released: version=resume
# and tag=<tag> for a tag of the commit whose GitHub release is missing, else
# version=<next> or version=skip (.github/scripts/release-version.sh).
#
# Both release workflows ask it twice: before the macOS build, to know whether
# to build at all, and again inside the serialised release job, against the
# tags as they are then, since another release may have been cut meanwhile.
#
# Usage: release-plan.sh <commit> <min_hours> [force: 0|1]   (GH_TOKEN set)
set -euo pipefail

commit=$1
min_hours=$2
force=${3:-0}

git fetch -q --tags --force origin
tag=$(git tag --points-at "$commit" | grep '^v0\.' | head -1 || true)
if [ -n "$tag" ] && ! gh release view "$tag" >/dev/null 2>&1; then
  echo "tag=$tag"
  echo "version=resume"
else
  echo "version=$(.github/scripts/release-version.sh "$commit" "$min_hours" "$force")"
fi
