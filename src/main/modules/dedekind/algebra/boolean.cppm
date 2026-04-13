/**
 * @file boolean.cppm
 * @module dedekind.algebra:boolean
 * @brief Boolean Starter Package: canonical Boolean ambient species and scout.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Starter_Intent
 * This partition offers a small, explicit entry point for Boolean algebra in
 * the set-builder DSL. It exports a canonical Boolean universe symbol and its
 * scout variable so examples can remain readable and stable.
 *
 * @section Notation
 * - `𝔹`: canonical Unicode symbol for the Boolean ambient set.
 * - `B`: ASCII alias for environments where Unicode input is inconvenient.
 * Element scouts are intentionally local (e.g. `auto b = var<BooleanSet>;`) to
 * avoid global name shadowing in downstream code.
 *
 * @section Historical_Note
 * "La matematica non e una collezione di trucchi: e grammatica delle forme."
 * (Mathematics is not a bag of tricks; it is a grammar of forms.)
 * — Emma Castelnuovo
 */
module;

export module dedekind.algebra:boolean;

import dedekind.sets;

namespace dedekind::algebra {
using namespace dedekind::sets;

export using BooleanSet = Ω<bool>;

export inline constexpr BooleanSet 𝔹{};
export inline constexpr BooleanSet B{};

}  // namespace dedekind::algebra
