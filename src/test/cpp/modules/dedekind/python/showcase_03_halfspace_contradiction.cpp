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
 * Expected LLVM IR: `ret i1 false` for `impress_empty_halfspace_meet`.
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

// Symbolic variable ranging over ℕ; N is the canonical ambient-set witness.
constexpr auto n = var<ℕ>;

// Opposing halfspaces with compile-time pivots carried in the predicate type.
constexpr auto gt_five = Set{n % N | (n > bound<5>)};
constexpr auto lt_three = Set{n % N | (n < bound<3>)};

// Compile-time theorem: the meet IS the empty set on ℕ.
constexpr Ø<int> empty_meet = gt_five & lt_three;
static_assert(empty_meet == Ø{});

// Logic species tightens at the reduction boundary.
//   ℕ is transfinite → NaturalLogic picks TernaryLogic for the parent Sets.
//   The meet reduces to the finite extensional empty set with decidable
//   (vacuously false) membership, so the reduced type carries ClassicalLogic.
using ParentLogic = typename decltype(gt_five)::logic_species;
using ReducedLogic = typename decltype(empty_meet)::logic_species;
static_assert(std::same_as<ParentLogic, TernaryLogic>);
static_assert(std::same_as<ReducedLogic, ClassicalLogic>);

/**
 * @brief Showcase 3: halfspace contradiction on ℕ.
 *
 * The empty meet's membership call is statically `L::False`, so comparing
 * against `L::True` folds to constant false at compile time.
 *
 * Expected IR: `ret i1 false`
 */
extern "C" __attribute__((noinline)) bool impress_empty_halfspace_meet() {
  using Logic = typename decltype(empty_meet)::logic_species;
  return empty_meet(42) == Logic::True;
}
