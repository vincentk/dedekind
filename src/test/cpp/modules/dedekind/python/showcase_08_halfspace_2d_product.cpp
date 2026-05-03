/**
 * @file
 * src/test/cpp/modules/dedekind/python/showcase_08_halfspace_2d_product.cpp
 * @brief Showcase 8 — 2D rectangle via cartesian product of 1D intervals.
 *
 *   ({n ∈ ℤ | -21 < n ≤ 21})  ×  ({n ∈ ℤ |  0 ≤ n ≤ 10})  =  42 × 11 = 462
 *
 * Two 1D halfspace meets each reduce to a structural `OrderInterval`; their
 * cartesian product reduces in turn to a structural `IntervalProduct` whose
 * cardinality is the product of the factors'. Everything is decidable at
 * compile time from the bounds and strictness pairs.
 *
 * Expected LLVM IR: `ret i1 true` — (0, 5) sits inside the 2D box.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */

#include <concepts>
#include <utility>

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

constexpr auto n = element<Ω<ℤ>>;

// Two 1D intervals on ℤ.
constexpr auto I_wide =
    Set{n | (n > bound<-21>)} & Set{n | (n <= bound<21>)};  // 42 elts
constexpr auto I_tall =
    Set{n | (n >= bound<0>)} & Set{n | (n <= bound<10>)};  // 11 elts

// Structural 2D cartesian product.
constexpr auto box = I_wide * I_tall;

// Compile-time theorem: 2D cardinality = product of 1D cardinalities.
static_assert(box.size() == 42u * 11u);
static_assert(box.size() == 462u);

// Computability classification survives the product: finite and decidable,
// but not compile-time-enumerable (we don't list all 462 pairs in the type).
static_assert(HasDecidableMembership<decltype(box)>);
static_assert(IsFiniteSet<decltype(box)>);
static_assert(!IsCompileTimeEnumerable<decltype(box)>);

/**
 * @brief Showcase 8: 2D membership in a 42×11 rectangle.
 *
 * Expected IR: `ret i1 true` — point (0, 5) lies in the box.
 */
extern "C" __attribute__((noinline)) bool witness_2d_box_member() {
  using Logic = typename decltype(box)::logic_species;
  return box(std::pair{0, 5}) == Logic::True;
}
