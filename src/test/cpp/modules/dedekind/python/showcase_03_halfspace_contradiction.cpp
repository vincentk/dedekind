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

// Symbolic scout ranging over the universal set Ω<ℕ>.  Per #551, the
// scout carries its ambient at the type level, so the % binding step
// disappears from the set-builder DSL.
constexpr auto n = element<Ω<ℕ>>;

// Opposing halfspaces with compile-time pivots carried in the predicate type.
constexpr auto gt_five = Set{n | (n > bound<5>)};
constexpr auto lt_three = Set{n | (n < bound<3>)};

// Compile-time theorem: the meet IS the empty set on ℕ.
constexpr Ø<ℕ> empty_meet = gt_five & lt_three;
static_assert(empty_meet == Ø{});

// Computability made a compile-time observable: the parent Sets carry NONE
// of the three tiers; the reduced Ø carries ALL THREE. Compile-time reduction
// from an intensional description over a transfinite carrier to the (trivial
// extensional) empty set restores decidable, finite, type-level semantics.
static_assert(!HasDecidableMembership<decltype(gt_five)>);
static_assert(!IsFiniteSet<decltype(gt_five)>);
static_assert(!IsCompileTimeEnumerable<decltype(gt_five)>);

static_assert(HasDecidableMembership<decltype(empty_meet)>);
static_assert(IsFiniteSet<decltype(empty_meet)>);
static_assert(IsCompileTimeEnumerable<decltype(empty_meet)>);

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
