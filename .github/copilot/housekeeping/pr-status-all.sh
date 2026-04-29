#!/usr/bin/env bash
# pr-status-all.sh — sweep ALL open PRs authored by the current user.
#
# Reports per PR:
#   - mergeability state (MERGEABLE / CONFLICTING / UNKNOWN)
#   - base ref
#   - draft status
#   - status-check rollup counts (grouped by conclusion)
#   - unresolved-review-thread count (paginated; reliable beyond 100 threads)
#
# Read-only: does not push, merge, or modify any PR.
#
# Usage:
#   pr-status-all.sh
set -euo pipefail

REPO="$(gh repo view --json nameWithOwner --jq .nameWithOwner)"
OWNER="${REPO%/*}"
NAME="${REPO#*/}"

echo "Sweeping open PRs in ${REPO} ..."

# Capture once; reuse for both the summary and the per-PR thread loop.
PRS="$(gh pr list --author "@me" --state open \
  --json number,title,baseRefName,isDraft,mergeable,statusCheckRollup)"

printf '%s\n' "${PRS}" | jq -r '
  .[]
  | { n:.number,
      base:.baseRefName,
      draft:.isDraft,
      merge:.mergeable,
      title:(.title[0:70]),
      checks: ( [.statusCheckRollup[]? | select(.conclusion!=null) | .conclusion]
                | (sort | group_by(.) | map({(.[0]):length}) | add // {}) ) }
  | "#\(.n) [\(.merge)\(if .draft then " DRAFT" else "" end) base=\(.base)] checks=\(.checks // {}) | \(.title)"
'

echo ""
echo "Unresolved review threads per open PR ..."

for n in $(printf '%s\n' "${PRS}" | jq -r '.[].number'); do
  C="$(gh api graphql --paginate --slurp \
        -F owner="${OWNER}" -F name="${NAME}" -F number="${n}" \
        -f query='query($owner:String!, $name:String!, $number:Int!, $endCursor:String) {
          repository(owner: $owner, name: $name) {
            pullRequest(number: $number) {
              reviewThreads(first: 100, after: $endCursor) {
                nodes { isResolved }
                pageInfo { hasNextPage endCursor }
              }
            }
          }
        }' 2>/dev/null \
      | jq '[.[].data.repository.pullRequest.reviewThreads.nodes[]
            | select(.isResolved == false)] | length' 2>/dev/null)"
  printf "  #%-4s %s unresolved\n" "${n}" "${C:-?}"
done
