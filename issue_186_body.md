## Problem
Current Jacobian contract covers classical differentiability with explicit Jacobian witnesses. This works well for smooth maps and piecewise maps away from kink points, but not at non-differentiable locations (e.g. Heaviside at 0, call payoff max(S-K,0) at S=K).

## Desired scope
- Introduce concepts for piecewise differentiability over regions/cells
- Add generalized derivative abstractions at kinks (subdifferential / Clarke-style set-valued derivative, or explicit one-sided derivative witnesses)
- Keep compatibility with existing `DifferentiableMap` and `differential_at` APIs
- Support compositional pipelines of piecewise/nonsmooth maps (closure under composition), so users can build smooth approximations from piecewise primitives (e.g., sigmoid-like approximations).
- Add examples/tests for Heaviside, ReLU, and option payoff families

## Notes
This is intentionally scoped as a follow-up so #181 can stay focused on the finite-dimensional smooth Jacobian contract.

## Related
- #181
- #184
- #185
