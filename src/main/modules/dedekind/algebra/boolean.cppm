/**
 * @file boolean.cppm
 * @partition :boolean
 * @module dedekind.algebra:boolean
 * @brief Boolean Starter Package: canonical Boolean ambient species aliases.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Starter_Intent
 * This partition offers a small, explicit entry point for Boolean algebra in
 * the set-builder DSL. It exports canonical Boolean universe aliases so
 * examples remain readable and stable.
 *
 * @section Notation
 * - `𝔹`: canonical Unicode symbol for the Boolean ambient set.
 * - `B`: ASCII alias for environments where Unicode input is inconvenient.
 *
 * @section Paper_Alignment
 * In the paper's Feature Cube (bool row), logical (`||`, `&&`) and bitwise
 * (`|`, `&`) operators over bool share the same lattice behavior (join/meet,
 * identities, absorbers, and distributivity). The test suite validates this
 * alignment explicitly.
 *
 * Element scouts are intentionally local (e.g. `auto b = var<BooleanSet>;`) to
 * avoid global name shadowing in downstream code.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "The science of Logic, as I conceive it, has not previously received
 * that degree of attention to which it is entitled, as forming the foundation
 * of all other sciences."
 *       -- George Boole, *Mathematical Analysis of Logic* (1847)
 *
 *       (Note: The Castelnuovo epigraph in the Historical_Note section above
 *       captures the modern view: \"Mathematics is not a bag of tricks; it is a
 *       grammar of forms.\")
 */
module;

export module dedekind.algebra:boolean;

import dedekind.category;
import dedekind.sets;

namespace dedekind::algebra {
using namespace dedekind::sets;

export template <typename L = dedekind::category::ClassicalLogic,
                 typename C = Finite>
using BooleanSetOf = Ω<bool, L, C>;

export using BooleanSet = BooleanSetOf<>;
export using 𝔹 = BooleanSet;

export inline constexpr BooleanSet B{};

}  // namespace dedekind::algebra
