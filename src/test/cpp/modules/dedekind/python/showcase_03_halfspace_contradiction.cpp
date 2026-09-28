/**
 * @file
 * src/test/cpp/modules/dedekind/python/showcase_03_halfspace_contradiction.cpp
 * @brief Showcase 3 — Compile-time proof of an empty halfspace intersection on
 * ℕ.
 *
 * Two opposing halfspaces on the naturals with pivots that cannot be bridged:
 *   { n ∈ ℕ | n > 5 }   ∩   { n ∈ ℕ | n < 3 }   ≡   ∅
 *
 * Pivots live at the TYPE level (as non-type template parameters of the
 * Halfspace predicate), so `structured_and` collapses the meet to an
 * `EmptyPredicate` at compile time and the wrapping Set compares equal to `Ø`.
 *
 * Expected LLVM IR: `ret i1 false` for `witness_empty_halfspace_meet`.
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

// Point-free halfspaces over the natural-numbers universe ℕ (= 𝔸<Cardinality>),
// with pivots (5 and 3) carried as NTTPs via fix().  A halfspace IS a set, so
// no Set{} wrapper.
constexpr auto gt_5 = ℕ | (χ > fix(5_c));
constexpr auto lt_3 = ℕ | (χ < fix(3_c));

// Compile-time theorem: the meet folds value-first to the empty SetVal on ℕ.
constexpr auto empty_meet = gt_5 & lt_3;
static_assert(empty_meet.kind == SetKind::Empty);

// gt_5 is an intensional predicate on ℕ (decidable membership, no materialised
// members); the meet folds it value-first to the empty set --- witnessed by the
// folded kind and by membership.
static_assert(HasDecidableMembership<decltype(gt_5)>);
static_assert(!static_cast<bool>(empty_meet(4u)));  // empty: no inhabitant

/**
 * @brief Showcase 3: halfspace contradiction on ℕ.
 *
 * The empty meet's membership call is statically `L::False`, so comparing
 * against `L::True` folds to constant false at compile time.
 *
 * Expected IR: `ret i1 false`
 */
extern "C" __attribute__((noinline)) bool witness_empty_halfspace_meet() {
  using Logic = typename decltype(empty_meet)::logic_species;
  return empty_meet(42u) == Logic::True;
}
