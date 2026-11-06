# Backlog process

How the maintainer keeps the issue tracker in order.  Contributors need none of this: see [CONTRIBUTING.md](../CONTRIBUTING.md).

## Grooming and CI health

1. Check CI health for `main`:
   - `make ci-history BRANCH=main LIMIT=3`
2. Collect backlog inputs:
   - `gh issue list --state open --limit 50 --json number,title,body,labels,comments`
   - `gh pr list --state open --json number,title,body,headRepositoryOwner --jq '.[] | select(.headRepositoryOwner.login != "vincentk")'`
3. Classify each item:
   - Actionable: clear scope and no blocking ambiguity; include in current plan.
   - Ambiguous: add a clarifying issue/PR comment and defer implementation.
   - Already resolved: close with a note referencing the resolving PR.
   - Exact duplicate: keep one canonical issue open and close duplicates with a cross-reference comment.
4. Pick a focused batch:
   - Prefer exactly two actionable items in parallel when feasible.

## Prioritization matrix (correlation x causality)

When selecting from the backlog, use the quadrant labels to separate broad thematic correlation from likely enabling/causal force:

- `priority:q1-high-corr-high-causal`:
   High cross-cluster leverage and likely enabling effect. Default first-pick lane.
- `priority:q2-high-corr-low-causal`:
   Strong bridge/correlation, but usually not a strict prerequisite.
- `priority:q3-low-corr-high-causal`:
   Narrow scope, but locally blocking/enabling for a concrete chain.
- `priority:q4-low-corr-low-causal`:
   Exploratory or lower immediate leverage.

Selection heuristic:

1. Prefer Q1 issues for primary execution.
2. Pull one Q2 issue in parallel when bridge visibility is useful.
3. Use Q3 issues to unblock local chains when blocked.
4. Batch or defer Q4 unless it unlocks a near-term objective.

Operational note:

- Treat quadrant labels as best-effort, revisable metadata rather than permanent truth.
- When new evidence appears, relabel and add a short rationale comment on the issue.
- Keep native parent-child links for causal decomposition; use cross-references/comments for correlation edges.
