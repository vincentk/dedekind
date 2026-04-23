/**
 * @file
 * src/test/cpp/modules/dedekind/python/showcase_06_halfspace_interval_42.cpp
 * @brief Showcase 6 — Non-point interval on ℤ with observable cardinality.
 *
 *   { n ∈ ℤ | -21 < n } ∩ { n ∈ ℤ | n ≤ 21 }   ≡   (-21, 21]   (|·| = 42)
 *
 * Mixed strictness (strict lower, non-strict upper). On an integral carrier
 * the cardinality is compile-time computable from the bounds and the
 * strictness pair, so the reduced OrderInterval exposes `size() == 42` and
 * satisfies `IsFiniteSet` — without elevating further (no Singleton collapse
 * because cardinality > 1).
 *
 * Expected LLVM IR: `ret i1 true` — inhabitant 0 is in the meet.
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

// Symbolic variable ranging over ℤ.
constexpr auto n = var<ℤ>;

// Halfspace pair with compile-time pivots, strict lower and non-strict upper.
constexpr auto above_minus_21 = Set{n % Z | (n > bound<-21>)};
constexpr auto at_most_21 = Set{n % Z | (n <= bound<21>)};

// The meet is structurally an OrderInterval (cardinality > 1, no Singleton
// collapse) with strict/non-strict boundaries in the type.
constexpr auto iv = above_minus_21 & at_most_21;
using Iv = std::decay_t<decltype(iv)>;

// The interval's bounds and strictness live at the type level.
static_assert(Iv::lower_pivot == -21);
static_assert(Iv::upper_pivot == 21);
static_assert(Iv::lower_strictness == Strictness::Strict);
static_assert(Iv::upper_strictness == Strictness::NonStrict);

// Cardinality is computed at compile time from the bounds + strictness.
static_assert(iv.size() == 42u);

// Computability classification: finite and decidable, but not
// compile-time-enumerable (its 42 inhabitants are not in the type).
static_assert(HasDecidableMembership<decltype(iv)>);
static_assert(IsFiniteSet<decltype(iv)>);
static_assert(!IsCompileTimeEnumerable<decltype(iv)>);

/**
 * @brief Showcase 6: observable cardinality on a 42-element ℤ-interval.
 *
 * Expected IR: `ret i1 true` — inhabitant 0 is a member.
 */
extern "C" __attribute__((noinline)) bool impress_interval_42_member() {
  using Logic = typename decltype(iv)::logic_species;
  return iv(0) == Logic::True;
}
