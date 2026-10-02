/**
 * @file
 * src/test/cpp/modules/dedekind/python/showcase_05_halfspace_real_ambient.cpp
 * @brief Showcase 5 — Halfspace meet on ℝ (continuous carrier).
 *
 *   { x ∈ ℝ | x > 5.0 }  ∩  { x ∈ ℝ | x < 3.0 }   ≡   ∅
 *
 * Same DSL, different carrier. Bounds are `double`-valued NTTPs; the Set's
 * carrier is `Real<double>`. Structural contradiction detection works
 * identically — but the computability classification differs from ℕ:
 * continuous carriers yield Interval (not IsExtensional) when the meet
 * is non-empty.
 *
 * Expected LLVM IR: `ret i1 false`.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */

#include <concepts>

import dedekind.category;
import dedekind.sets;
import dedekind.algebra;
import dedekind.numbers;
import dedekind.order;

using namespace dedekind::category;
using namespace dedekind::sets;
using namespace dedekind::algebra;
using namespace dedekind::numbers;
using namespace dedekind::order;

// ℝ is now the ℚ(√2) coat-hanger, so machine-real (double) halfspaces live on
// @c ℝ_d = @c 𝔸<Real<double>, Boole, ℶ_1>: @b Boole logic, @b ℶ_1 cardinality.
// A bare point-free @c Halfspace IS a set, so no Set{} wrapper.
constexpr auto gt_five = ℝ_d | (χ > bound<5.0>);
constexpr auto lt_three = ℝ_d | (χ < bound<3.0>);

// Compile-time theorem: the meet folds value-first to the empty SetVal on ℝ ---
// the crossing-bound emptiness test fires on a continuous carrier just as on ℕ.
constexpr auto empty_meet = gt_five & lt_three;
static_assert(empty_meet.kind == SetKind::Empty);
static_assert(!static_cast<bool>(empty_meet(Real<double>{4.0})));

/**
 * @brief Showcase 5: halfspace contradiction on ℝ.
 *
 * The empty meet's membership call is statically `L::False`.
 *
 * Expected IR: `ret i1 false`
 */
extern "C" __attribute__((noinline)) bool witness_real_halfspace_empty() {
  using Logic = typename decltype(empty_meet)::logic_species;
  return empty_meet(Real<double>{42.0}) == Logic::True;
}
