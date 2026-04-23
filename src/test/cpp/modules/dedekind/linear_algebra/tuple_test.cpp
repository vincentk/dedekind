/** @file dedekind/linear_algebra/tuple_test.cpp
 *
 * Unit coverage for the `:tuple` partition:
 *   - NTTP `Vec2<T, x, y>` — type-level 2-tuple with structural equality.
 *   - Value-level `Vec2V<T>` / `Covec2V<T>` — column and row carriers as
 *     2×1 and 1×2 matrices.
 *   - Transpose dual pair `Vec2V ↔ Covec2V` with the involution law
 *     `(aᵀ)ᵀ == a` on each side.
 */

#include <catch2/catch_test_macros.hpp>

import dedekind.numbers;
import dedekind.linear_algebra;

using namespace dedekind::numbers;
using namespace dedekind::linear_algebra;

namespace {
using Rat = Rational<long>;
}  // namespace

TEST_CASE("linear_algebra:tuple — NTTP Vec2 carries entries at the type level",
          "[linear_algebra][tuple][nttp]") {
  constexpr Vec2<int, 3, 7> v{};
  STATIC_CHECK(v.first == 3);
  STATIC_CHECK(v.second == 7);
  // Distinct NTTP values → distinct types → false under ==.
  STATIC_CHECK(!(v == Vec2<int, 3, 8>{}));
}

TEST_CASE(
    "linear_algebra:tuple — value-level Vec2V / Covec2V as 2×1 / 1×2 matrices",
    "[linear_algebra][tuple][value][shape]") {
  // Matrix shape lives on the tuple carriers themselves.
  STATIC_CHECK(Vec2V<Rat>::row_count == 2u);
  STATIC_CHECK(Vec2V<Rat>::column_count == 1u);
  STATIC_CHECK(Covec2V<Rat>::row_count == 1u);
  STATIC_CHECK(Covec2V<Rat>::column_count == 2u);
  STATIC_CHECK(Vec2V<Rat>::dimension == 2u);
  STATIC_CHECK(Covec2V<Rat>::dimension == 2u);
}

TEST_CASE("linear_algebra:tuple — Vec2V / Covec2V are a transpose dual pair",
          "[linear_algebra][tuple][transpose][duality]") {
  // Type-level duality: transpose sends column → row and row → column.
  STATIC_CHECK(IsTransposeDualPair<Vec2V<Rat>, Covec2V<Rat>>);

  // Type-level involution: double-transpose returns the original carrier.
  STATIC_CHECK(HasInvolutiveTranspose<Vec2V<Rat>>);
  STATIC_CHECK(HasInvolutiveTranspose<Covec2V<Rat>>);

  // Value-level: (aᵀ)ᵀ = a on each carrier — the ℤ/2 action of transpose
  // restricts to identity on both.
  constexpr Vec2V<Rat> v{Rat{7L}, Rat{-3L}};
  constexpr Covec2V<Rat> c{Rat{2L}, Rat{5L}};
  STATIC_CHECK(v.transpose().transpose() == v);
  STATIC_CHECK(c.transpose().transpose() == c);

  // Entries carry across the column/row exchange unchanged.
  STATIC_CHECK(v.transpose() == Covec2V<Rat>{Rat{7L}, Rat{-3L}});
  STATIC_CHECK(c.transpose() == Vec2V<Rat>{Rat{2L}, Rat{5L}});
}

TEST_CASE("linear_algebra:tuple — Vec2V and Covec2V are column / row vectors",
          "[linear_algebra][tuple][orientation]") {
  STATIC_CHECK(IsColumnVector<Vec2V<Rat>>);
  STATIC_CHECK(IsCovector<Covec2V<Rat>>);
  STATIC_CHECK(!IsColumnVector<Covec2V<Rat>>);
  STATIC_CHECK(!IsCovector<Vec2V<Rat>>);
}

// Runtime coverage: `constexpr` operators only reached via `static_assert`
// / `STATIC_CHECK` are elided from the instrumented binary. A small set of
// runtime `CHECK`s forces the coverage tool to see these paths.
TEST_CASE("linear_algebra:tuple — runtime-exercised Vec2V / Covec2V operators",
          "[linear_algebra][tuple][runtime_coverage]") {
  Vec2V<int> v{1, 2};
  CHECK(v == Vec2V<int>{1, 2});        // default operator==
  CHECK_FALSE(v == Vec2V<int>{3, 4});  // negative case
  CHECK(-v == Vec2V<int>{-1, -2});     // unary operator-

  Covec2V<int> c{3, 5};
  CHECK(c == Covec2V<int>{3, 5});        // default operator== on Covec2V
  CHECK_FALSE(c == Covec2V<int>{0, 0});  // negative case
}
