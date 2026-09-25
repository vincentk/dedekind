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
 * continuous carriers yield OrderInterval (not IsExtensional) when the meet
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
// The bare point-free @c Halfspace keeps @c cardinality_type=ℶ_1 but its
// @c logic_species is the raw @c Boole param.  The @c Set{} wrap is
// load-bearing here: @c Set deduction runs @c NaturalLogic<Halfspace>, which
// routes the ℶ_1 carrier to @c Kleene --- the uncountable-carrier verdict this
// showcase tests
// (@c HasDecidableMembership reads @c logic_species, so it needs the Kleene).
constexpr auto gt_five = Set{ℝ_d | (χ > bound<5.0>)};
constexpr auto lt_three = Set{ℝ_d | (χ < bound<3.0>)};

// Compile-time theorem: the meet IS the empty set on ℝ.
constexpr Ø<Real<double>> empty_meet = gt_five & lt_three;
static_assert(empty_meet == Ø<Real<double>>{});

// Computability made visible: parent Sets carry NONE of the three tiers;
// the reduced Ø carries ALL THREE — continuous carrier notwithstanding.
static_assert(!HasDecidableMembership<decltype(gt_five)>);
static_assert(!IsExtensional<decltype(gt_five)>);

static_assert(HasDecidableMembership<decltype(empty_meet)>);
static_assert(IsExtensional<decltype(empty_meet)>);

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
