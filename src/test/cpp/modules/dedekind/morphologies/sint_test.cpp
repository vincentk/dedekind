/**
 * @file sint_test.cpp
 * @brief Dedicated test suite for the @c numbers:sint partition.
 *
 * @section Scope
 * Exercises the @c std::signed_integral family classification shipped
 * by @c numbers:sint (closes part of #418): the dual error-direction
 * to @c :uint (operator surface ✓, axiomatic ring ✗ under UB), the
 * lift @c embed_sint_ℤ into ℤ, and the @c 𝕂3 → sint → ℤ
 * embedding-chain composition (the signed counterpart to @c :uint's
 * @c 𝔹 → uint → ℕ chain).  The compile-time witnesses live inside
 * the partition itself; this file exercises the @b runtime / @b
 * composition surface.
 */
#include <catch2/catch_test_macros.hpp>
#include <cstdint>
#include <functional>  // std::plus
#include <limits>
#include <type_traits>

import dedekind.algebra;
import dedekind.category;
import dedekind.morphologies;
import dedekind.sets;

using namespace dedekind::category;
using namespace dedekind::morphologies;
using namespace dedekind::sets;
using dedekind::morphologies::embed_sint_ℤ;
using dedekind::morphologies::embed_sint_ℤ_;

// ===========================================================================
// (1) The universal lift arrow: std::signed_integral ↪ ℤ
// ===========================================================================

TEST_CASE("sint: embed_sint_ℤ across canonical widths",
          "[numbers][sint][lift][carrier-lattice]") {
  CHECK(embed_sint_ℤ(0) == Integer{});
  CHECK(embed_sint_ℤ(1) == Integer{1});
  CHECK(embed_sint_ℤ(-1) == Integer{-1});
  CHECK(embed_sint_ℤ(-42) == -Integer{Numeral{42u}});
  CHECK(embed_sint_ℤ(static_cast<long>(1000)) == Integer{1000});
  CHECK(embed_sint_ℤ(static_cast<long long>(-7)) == Integer{-7});
  CHECK(embed_sint_ℤ(static_cast<short>(-100)) == Integer{-100});
  CHECK(embed_sint_ℤ(std::numeric_limits<long long>::min()).magnitude ==
        Numeral{std::uint64_t{1} << 63});
}

TEST_CASE("sint: embed_sint_ℤ_ named-arrow form",
          "[numbers][sint][lift][monic-arrow]") {
  STATIC_CHECK(IsArrow<std::decay_t<decltype(embed_sint_ℤ_)>>);
  STATIC_CHECK(IsMonicArrow<std::decay_t<decltype(embed_sint_ℤ_)>>);
  STATIC_CHECK(std::same_as<Dom<std::decay_t<decltype(embed_sint_ℤ_)>>, int>);
  STATIC_CHECK(
      std::same_as<Cod<std::decay_t<decltype(embed_sint_ℤ_)>>, Integer>);

  CHECK(embed_sint_ℤ_(0) == Integer{});
  CHECK(embed_sint_ℤ_(42) == Integer{42});
  CHECK(embed_sint_ℤ_(-42) == Integer{-42});
}

TEST_CASE("sint: lift is injective", "[numbers][sint][lift][injective]") {
  CHECK(embed_sint_ℤ(0) != embed_sint_ℤ(1));
  CHECK(embed_sint_ℤ(0) != embed_sint_ℤ(-1));
  CHECK(embed_sint_ℤ(41) != embed_sint_ℤ(42));
  CHECK(embed_sint_ℤ(-1) != embed_sint_ℤ(1));
}

// ===========================================================================
// (2) Honest Rejection: operator surface ✓, axiomatic ring ✗
// ===========================================================================

TEST_CASE(
    "sint: int has the literal ring operator surface (positive witnesses)",
    "[numbers][sint][honest-rejection][positive]") {
  // Compile-time: the operator-shape concepts fire on int / long /
  // long long.  These are the @b honest claims the library makes
  // about std::signed_integral primitives.
  STATIC_CHECK(dedekind::algebra::HasRingOperators<int>);
  STATIC_CHECK(dedekind::algebra::HasGroupOperatorsAdd<int>);
  STATIC_CHECK(dedekind::algebra::HasSuccessorOperators<int>);
  STATIC_CHECK(dedekind::algebra::HasRingOperators<long>);
  STATIC_CHECK(dedekind::algebra::HasRingOperators<long long>);
}

TEST_CASE("sint: int does NOT satisfy axiomatic ring laws (negative witnesses)",
          "[numbers][sint][honest-rejection][negative]") {
  // Compile-time: the axiomatic-ring concepts deliberately do @b NOT
  // fire on signed-integral primitives.  Signed-overflow UB defeats
  // closure-under-arithmetic as a stable axiom — the library's
  // @b Honest @b Rejection of int as a witness for ℤ.
  STATIC_CHECK(!dedekind::algebra::IsArithmeticAdditiveGroup<int>);
}

TEST_CASE(
    "sint: the dichotomy is symmetric in form to :uint's, opposite in "
    "direction",
    "[numbers][sint][honest-rejection][symmetry]") {
  // The library's posture is honest in both directions:
  //   * unsigned int claims MORE structure than ℕ has (modular inverses);
  //     IsField<unsigned int> rejected (PR #441 / :uint).
  //   * int claims LESS structure than ℤ has (UB on overflow defeats
  //     closure); IsArithmeticAdditiveGroup<int> rejected (this partition).
  // Both rejections are symmetric in form — each carrier's claim is
  // mismatched with the textbook structure it would purport to inhabit.
  STATIC_CHECK(!dedekind::algebra::IsArithmeticAdditiveGroup<int>);
  STATIC_CHECK(!dedekind::category::IsField<  // unsigned: too MUCH structure
               unsigned int, std::plus<unsigned int>,
               std::multiplies<unsigned int>>);
  // Both partitions DO claim the operator surface honestly.
  STATIC_CHECK(dedekind::algebra::HasRingOperators<int>);
  STATIC_CHECK(dedekind::algebra::HasRingOperators<unsigned int>);
}

// ===========================================================================
// (3) The lift carries the ring int lacks
// ===========================================================================

TEST_CASE("sint: an operation that is UB on int is exact in ℤ",
          "[numbers][sint][lift][carrier-lattice]") {
  // INT_MAX + 1 is UB on int; in ℤ it is the successor, and the lift
  // commutes with negation.
  const auto top = embed_sint_ℤ(std::numeric_limits<int>::max());
  CHECK(top + embed_sint_ℤ(1) > top);
  CHECK(-embed_sint_ℤ(1) == embed_sint_ℤ(-1));
}

TEST_CASE("sint: intensional first, ℤ as the working carrier",
          "[numbers][sint][ergonomics][intensional-first]") {
  // Values enter through the explicit constructor; the arithmetic is ℤ's,
  // with no machine-int arithmetic in between.
  const Integer a{7};
  const Integer b{-3};
  CHECK(a + b == Integer{4});
  CHECK(-a == Integer{-7});
  CHECK(a * b == Integer{-21});
  CHECK(a - b == Integer{10});
}

TEST_CASE("sint: negafibonacci on ℤ, the canonical intensional ℤ-series",
          "[numbers][sint][ergonomics][intensional-first][negafibonacci]") {
  // F_{-n} = (-1)^{n+1} F_n: the recurrence F_{n-1} = F_{n+1} - F_n run
  // backwards from F_1 = 1, F_0 = 0 gives 1, -1, 2, -3, 5, each step the
  // subtraction that distinguishes ℤ from ℕ.  The forward sibling on ℕ is in
  // uint_test.cpp.
  const Integer f_1{1};
  const Integer f_0{};
  const Integer f_neg1 = f_1 - f_0;
  const Integer f_neg2 = f_0 - f_neg1;
  const Integer f_neg3 = f_neg1 - f_neg2;
  const Integer f_neg4 = f_neg2 - f_neg3;
  const Integer f_neg5 = f_neg3 - f_neg4;
  CHECK(f_neg1 == Integer{1});
  CHECK(f_neg2 == Integer{-1});
  CHECK(f_neg3 == Integer{2});
  CHECK(f_neg4 == Integer{-3});
  CHECK(f_neg5 == Integer{5});
}

TEST_CASE("sint: no Integer + int overload, the rejection kept",
          "[numbers][sint][ergonomics][no-smuggling]") {
  // There is no Integer + int: an int operand would bring its overflow back
  // through ℤ's surface.  A constant enters ℤ by name, Integer{3}.
  CHECK(Integer{5} + Integer{3} == Integer{8});
}
