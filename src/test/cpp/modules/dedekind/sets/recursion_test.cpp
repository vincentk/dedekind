/** @file dedekind/sets/recursion_test.cpp
 *
 * Runtime companions for sets:recursion: an inductive set names its own
 * classifier χ, the former closes a structural recursion, and refuses the
 * general one.  The load-bearing facts are static_asserts in the partition.
 */

#include <catch2/catch_test_macros.hpp>
#include <cstddef>

import dedekind.sets;

using namespace dedekind::sets;
using N = std::size_t;

TEST_CASE("sets:recursion — an inductive set names its own classifier",
          "[sets][recursion]") {
  // The multiples of three: 0, and whatever is three above one.
  constexpr auto multiples_of_3 = 𝔸<N>{} & ((π == N{0}) || χ(π - N{3}));
  STATIC_CHECK(IsSetObject<decltype(multiples_of_3)>);
  for (N n = 0; n < 40; ++n) CHECK(multiples_of_3(n) == (n % 3 == 0));
  // ℕ inductively: 0, and the successor of a natural.  Everything is in.
  constexpr auto naturals = 𝔸<N>{} & ((π == N{0}) || χ(π - N{1}));
  CHECK(naturals(N{0}));
  CHECK(naturals(N{300}));
}

// A requires-expression refuses SFINAE-style only where it is dependent.
template <typename T>
concept FormsNonDescendingRecursion =
    requires { 𝔸<T>{} & ((π == T{}) || χ(π)); };
template <typename T>
concept FormsDescendingRecursion =
    requires { 𝔸<T>{} & ((π == T{}) || χ(π - T{1})); };

TEST_CASE("sets:recursion — only a structural recursion finds a former",
          "[sets][recursion][refusal]") {
  // χ(π) descends on nothing: general recursion, no viable former.
  STATIC_CHECK(!FormsNonDescendingRecursion<N>);
  // A descent needs a well-founded carrier: int has no bottom to stop at.
  STATIC_CHECK(FormsDescendingRecursion<N>);
  STATIC_CHECK(!FormsDescendingRecursion<int>);
  // Without a χ the former is the ordinary one: a plain comprehension.
  constexpr auto zero = 𝔸<N>{} & (π == N{0});
  CHECK(zero(N{0}));
  CHECK_FALSE(zero(N{3}));
}
