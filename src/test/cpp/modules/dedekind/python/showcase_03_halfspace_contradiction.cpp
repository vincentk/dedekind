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
#include <type_traits>

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
inline constexpr auto n = var<ℕ>;

// Opposing halfspaces with compile-time pivots carried in the predicate type.
inline constexpr auto gt_five = Set{n % N | (n > bound<5>)};
inline constexpr auto lt_three = Set{n % N | (n < bound<3>)};

// Set-level intersection dispatches through structured_and; the contradiction
// collapses the result type to Ø<int, TernaryLogic>.
inline constexpr auto empty_meet = gt_five & lt_three;

// Compile-time witness: the meet IS the empty set on ℕ.
static_assert(std::same_as<std::decay_t<decltype(empty_meet)>,
                           Ø<int, typename decltype(gt_five)::logic_species>>);

/**
 * @brief Showcase 3: halfspace contradiction on ℕ.
 *
 * Returns whether a sample natural sits in the (empty) meet. The meet is
 * typed as `Ø<int, L>`, so the membership call is statically `L::False` and
 * the comparison against `L::True` folds to constant false.
 *
 * Expected IR: `ret i1 false`
 */
extern "C" __attribute__((noinline)) bool impress_empty_halfspace_meet() {
  using Logic = typename decltype(empty_meet)::logic_species;
  return empty_meet(42) == Logic::True;
}
