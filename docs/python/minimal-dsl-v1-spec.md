# Minimal DSL v1 Spec (Agent-Oriented)

## Purpose
A small, deterministic DSL for backlog and tracker operations that agents can run safely.

Design priorities:
- Deterministic behavior
- Idempotent writes
- Auditable actions
- Explicit distinction between correlation and causality
- Typed objects and explicit lifecycle (plan -> apply -> verify)

## Core Objects
- issue: GitHub issue with metadata, labels, and comments
- pr: pull request with checks/review state
- label: tracker classification token
- edge: typed relationship between issues
- comment: rationale/audit entry
- check: CI or quality gate status

## Edge Kinds
- correlates_with: thematic/cross-cluster relationship
- likely_enables: probable enabling dependency
- blocks: hard blocker
- depends_on: explicit dependency
- causal_hypothesis: provisional causal claim
- granger_candidate: candidate ordering signal pending evidence

## Core Operations
- select: choose entities from tracker state
- filter: narrow by predicates
- derive: compute attributes or scores
- tag: add/remove labels idempotently
- link: create typed edges or cross-reference comments
- annotate: add concise rationale comments
- report: produce ordered summaries
- apply: execute write operations
- verify: assert invariants and postconditions

## Execution Lifecycle
1. plan:
   - Build an ordered action set without side effects.
   - Include rationale per action.
2. apply:
   - Execute actions with idempotent semantics.
   - Emit status per action: updated, skipped, failed.
3. verify:
   - Re-read tracker state and assert expected invariants.

## Invariants
- One-priority-quadrant label per issue (if issue participates in matrix).
- No duplicate policy comments for the same policy key.
- Parent-child links used for causal decomposition only.
- Correlation edges represented by typed edges or cross-reference comments.

## Minimal Data Shapes

Action:
- kind: tag | annotate | link | close | reopen
- target: issue number or PR number
- payload: operation-specific data
- reason: short rationale
- idempotency_key: stable key for de-duplication

Verification Rule:
- name: rule identifier
- query: tracker read expression
- assert: boolean condition
- on_fail: report | halt

## Example 1: Quadrant Label Pass
- select issues in current execution batch
- derive quadrant from correlation/causality score
- tag with exactly one quadrant label
- verify each target has exactly one quadrant label

## Example 2: Correlation Visibility Pass
- select bridge-anchor issues
- link with correlates_with edges
- annotate each anchor with one concise rationale
- verify no duplicate anchor note for same bridge key

## Example 3: Causality Hypothesis Pass
- select candidate edges from execution history
- mark as granger_candidate
- annotate evidence basis (check outcomes, unblock events, latency)
- verify candidate edges are not auto-promoted to blocks without explicit review

## Mapping: Analyst Shim -> Formal DSL
- analyst table workflow corresponds to high-level derive and report actions
- quality combinators map to verify rules and normalization derive steps
- smart_join rationale maps to correlate/link operations plus annotate actions

## Non-goals (v1)
- Full query language parser
- Hidden implicit state
- Non-auditable side effects
- Automatic causal promotion without explicit evidence
