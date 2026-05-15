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

// Symbolic scout ranging over the natural-numbers universe ℕ.  Per
// #551 the scout carries its ambient at the type level (the % binding
// step disappears); per #559 the canonical spelling is element<ℕ> with
// ℕ itself the universe value (= Ω<Cardinality>).
constexpr auto n = element<ℕ>;

// Two halfspace-structured sets, with pivots (5 and 3) carried as NTTPs.
constexpr auto gt_5 = Set{n | (n > bound<5>)};
constexpr auto lt_3 = Set{n | (n < bound<3>)};

// Compile-time theorem: the meet IS the empty set on ℕ.
constexpr Ø<Cardinality> empty_meet = gt_5 & lt_3;
static_assert(empty_meet == Ø<Cardinality>{});

// Post-#622: ℕ → ClassicalLogic on the carrier axis, so @c gt_5 is
// decidable on the carrier-axis fast path — Ternary→Classical
// promotion is no longer the story here.  The axis that STILL tightens
// at the reduction boundary is @b extensionality: @c gt_5 is intensional
// (predicate-shaped, opaque λ); the empty-meet reduction is extensional
// (it IS @c Ø, materialised at the type level).
static_assert(HasDecidableMembership<decltype(gt_5)>);
static_assert(!IsExtensional<decltype(gt_5)>);

static_assert(HasDecidableMembership<decltype(empty_meet)>);
static_assert(IsExtensional<decltype(empty_meet)>);

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
