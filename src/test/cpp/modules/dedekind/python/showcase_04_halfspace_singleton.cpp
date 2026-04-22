/**
 * @file
 * src/test/cpp/modules/dedekind/python/showcase_04_halfspace_singleton.cpp
 * @brief Showcase 4 — Cardinality-1 reduction: halfspace meet collapses to {4}.
 *
 *   { n ∈ ℕ | n > 3 }  ∩  { n ∈ ℕ | n < 5 }   ≡   {4}
 *
 * Over an integral carrier, the meet of opposing halfspaces with NTTP pivots
 * has a compile-time-decidable cardinality. When that cardinality is exactly
 * one, `structured_and` elevates the meet to a `Singleton<value>` with the
 * unique inhabitant in the type — no lambdas, no predicate erasure.
 *
 * Expected LLVM IR: `ret i1 true` for `impress_halfspace_singleton` (the
 * unique inhabitant 4 is in the set).
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

// Symbolic variable ranging over ℕ.
constexpr auto n = var<ℕ>;

// Opposing halfspaces with compile-time pivots separated by exactly two.
constexpr auto gt_three = Set{n % N | (n > bound<3>)};
constexpr auto lt_five = Set{n % N | (n < bound<5>)};

// Compile-time theorem: the meet IS the singleton {4} on ℕ.
constexpr Singleton<4> in_between = gt_three & lt_five;
static_assert(in_between == Singleton<4>{});

// Logic species tightens at the reduction boundary.
//   ℕ is transfinite → NaturalLogic picks TernaryLogic for the parent Sets.
//   The meet reduces to a finite extensional object with decidable membership,
//   so the reduced type carries ClassicalLogic — Unknown is no longer reachable.
using ParentLogic  = typename decltype(gt_three)::logic_species;
using ReducedLogic = typename decltype(in_between)::logic_species;
static_assert(std::same_as<ParentLogic, TernaryLogic>);
static_assert(std::same_as<ReducedLogic, ClassicalLogic>);

/**
 * @brief Showcase 4: cardinality-1 reduction to the singleton {4}.
 *
 * `in_between(4)` is statically `L::True`; the comparison against `L::True`
 * folds to constant true at compile time.
 *
 * Expected IR: `ret i1 true`
 */
extern "C" __attribute__((noinline)) bool impress_halfspace_singleton() {
  using Logic = typename decltype(in_between)::logic_species;
  return in_between(4) == Logic::True;
}
