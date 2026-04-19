#!/usr/bin/env bash
# pr-init.sh — initialize an issue-scoped branch, empty checkpoint commit,
#               remote push, and draft PR with title/body derived from issues.
#
# Usage:
#   pr-init.sh <issue-numbers>  [TYPE=feat|fix|docs|chore]  [BASE=main]
#
# Environment variables (override via env or caller):
#   ISSUES   — space- or comma-separated issue numbers (required)
#   TYPE     — branch-name prefix / commit type  (default: feat)
#   BASE     — target base branch                (default: main)
set -euo pipefail

# ── argument / env resolution ────────────────────────────────────────────────
ISSUE_LIST_RAW="${ISSUES:-}"
TYPE_VAL="${TYPE:-feat}"
BASE_BRANCH="${BASE:-main}"

if [[ -z "$ISSUE_LIST_RAW" ]]; then
  echo "ERROR: ISSUES is required." >&2
  echo "Usage: ISSUES=\"234 236\" $0 [TYPE=feat] [BASE=main]" >&2
  exit 2
fi

# ── working-tree guard ────────────────────────────────────────────────────────
if [[ -n "$(git status --porcelain)" ]]; then
  echo "ERROR: working tree must be clean before pr-init." >&2
  echo "Commit, stash, or discard current changes before running this script." >&2
  exit 2
fi

# ── derive branch / commit names ─────────────────────────────────────────────
ISSUE_NUMBERS="$(printf '%s\n' "$ISSUE_LIST_RAW" | tr ' ,' '\n' | sed '/^$/d')"
ISSUE_SLUG="$(printf '%s\n' "$ISSUE_NUMBERS" | paste -sd- -)"
ISSUE_REFS="$(printf '%s\n' "$ISSUE_NUMBERS" | sed 's/^/#/' | paste -sd'/' -)"

BRANCH_NAME="${TYPE_VAL}/issues-${ISSUE_SLUG}"
COMMIT_MSG="${TYPE_VAL}: initialize ${ISSUE_REFS} scope"

# ── fetch issue metadata from GitHub ─────────────────────────────────────────
# Build a combined title and body from all referenced issues.
COMBINED_TITLES=""
COMBINED_BODIES=""
for NUM in $ISSUE_NUMBERS; do
  RAW="$(gh issue view "$NUM" --json title,body --jq '"[\(.title)]"')"
  ISSUE_TITLE="$(gh issue view "$NUM" --json title --jq '.title')"
  ISSUE_BODY="$(gh issue view "$NUM" --json body --jq '.body')"

  if [[ -n "$COMBINED_TITLES" ]]; then
    COMBINED_TITLES="${COMBINED_TITLES} / ${ISSUE_TITLE}"
  else
    COMBINED_TITLES="${ISSUE_TITLE}"
  fi

  COMBINED_BODIES="${COMBINED_BODIES}### ${NUM}: ${ISSUE_TITLE}

${ISSUE_BODY}

"
done

# PR title: use issue title directly for a single issue, or a combined label.
if [[ "$(echo "$ISSUE_NUMBERS" | wc -l | tr -d ' ')" -eq 1 ]]; then
  PR_TITLE="${COMBINED_TITLES}"
else
  PR_TITLE="${COMBINED_TITLES}"
fi

# PR body: issue summaries + standard boilerplate.
PR_BODY="$(cat <<EOF
## Issues

${COMBINED_BODIES}## Notes

This is an initialization PR intended to start CI and collect subsequent commits
for the selected issue scope (${ISSUE_REFS}).  The branch was created from
\`${BASE_BRANCH}\` with an empty checkpoint commit; further implementation commits
will follow.
EOF
)"

# ── branch switch / create ────────────────────────────────────────────────────
CURRENT_BRANCH="$(git branch --show-current)"
if [[ "$CURRENT_BRANCH" != "$BRANCH_NAME" ]]; then
  if git show-ref --verify --quiet "refs/heads/${BRANCH_NAME}"; then
    git switch "$BRANCH_NAME"
  else
    git switch -c "$BRANCH_NAME"
  fi
fi

# ── empty checkpoint commit + push ───────────────────────────────────────────
git commit --allow-empty -m "$COMMIT_MSG"
git push -u origin "$BRANCH_NAME"

# ── open draft PR (idempotent) ────────────────────────────────────────────────
OPEN_COUNT="$(gh pr list --head "$BRANCH_NAME" --state open --json number --jq 'length')"
if [[ "$OPEN_COUNT" -eq 0 ]]; then
  gh pr create --draft \
    --base "$BASE_BRANCH" \
    --head "$BRANCH_NAME" \
    --title "$PR_TITLE" \
    --body "$PR_BODY"
else
  echo "Open PR already exists for ${BRANCH_NAME}; skipping PR creation."
fi
