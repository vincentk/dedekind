## Problem
Current Jacobian contract covers classical differentiability with explicit Jacobian witnesses. This works well for smooth maps and piecewise maps away from kink points, but not at non-differentiable locations (e.g. Heaviside at 0, call payoff max(S-K,0) at S=K).

## Desired scope
- Introduce concepts for piecewise differentiability over regions/cells
- Add generalized derivative abstractions at kinks (subdifferential / Clarke-style set-valued derivative, or explicit one-sided derivative witnesses)
- Keep compatibility with existing `DifferentiableMap` and `differential_at` APIs
- Support compositional pipelines of piecewise/nonsmooth maps (closure under composition), so users can build smooth approximations from piecewise primitives (e.g., sigmoid-like approximations).
- Add examples/tests for Heaviside, ReLU, and option payoff families

## Acceptance criteria
- Supports nonsmooth and piecewise mappings on both discrete and continuous carriers: N -> N and R -> R.
- Supports compositional closure for these mappings (piecewise with piecewise, piecewise with smooth), including sigmoid-like approximation pipelines.
- Provides a lifting/composition rule from 1D mappings to higher-dimensional product spaces, with explicit examples/tests for N^2 and R^3.
- Preserves compatibility with existing DifferentiableMap and differential_at contracts where classical differentiability holds.
- Includes tests covering behavior at regular points and documented behavior at kink/non-differentiable points.

## Notes
This is intentionally scoped as a follow-up so #181 can stay focused on the finite-dimensional smooth Jacobian contract.

## Related
- #181
- #184
- #185
