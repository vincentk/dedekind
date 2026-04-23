/** @file dedekind/linear_algebra/invertible2x2_test.cpp
 *
 * Unit coverage for `Invertible2x2<T, a, b, c, d>` (Tier 0 of issue #366).
 * Exercises: full-rank enforcement, closed-form Cramer's inverse, matrix-
 * matrix composition, matrix-vector action on `Vec2<T, x, y>`, and the
 * existential proof `M * M.inverse() == Identity2x2<T>` over ℚ.
 */

#include <catch2/catch_test_macros.hpp>

import dedekind.category;
import dedekind.numbers;
import dedekind.linear_algebra;

using namespace dedekind::numbers;
using namespace dedekind::linear_algebra;

namespace {
// Canonical paper-facing carrier: ℚ as a proxy for ℝ (exact arithmetic).
using Rat = Rational<long>;
}  // namespace

TEST_CASE("linear_algebra:invertible2x2 — identity at the type level",
          "[linear_algebra][invertible2x2][identity]") {
  constexpr Identity2x2<Rat> I{};
  STATIC_CHECK(I.m11 == Rat{1L});
  STATIC_CHECK(I.m12 == Rat{0L});
  STATIC_CHECK(I.m21 == Rat{0L});
  STATIC_CHECK(I.m22 == Rat{1L});
  STATIC_CHECK(I.det == Rat{1L});
}

TEST_CASE("linear_algebra:invertible2x2 — Cramer's inverse on ℚ",
          "[linear_algebra][invertible2x2][inverse]") {
  // M = [[1, 2], [3, 4]],  det = -2,
  // M^{-1} = (-1/2) * [[4, -2], [-3, 1]] = [[-2, 1], [3/2, -1/2]]
  constexpr Invertible2x2<Rat, Rat{1L}, Rat{2L}, Rat{3L}, Rat{4L}> M{};
  STATIC_CHECK(M.det == Rat{-2L});

  // Existential proof: the closed-form inverse is the honest two-sided
  // inverse, not just a left- or right-inverse by luck.
  STATIC_CHECK(M * M.inverse() == Identity2x2<Rat>{});
  STATIC_CHECK(M.inverse() * M == Identity2x2<Rat>{});
}

TEST_CASE("linear_algebra:invertible2x2 — shear matrix on ℤ (det = 1)",
          "[linear_algebra][invertible2x2][integer]") {
  // M = [[1, 1], [0, 1]],  det = 1;  M^{-1} = [[1, -1], [0, 1]] over ℤ.
  constexpr Invertible2x2<int, 1, 1, 0, 1> shear{};
  STATIC_CHECK(shear.det == 1);
  STATIC_CHECK(shear * shear.inverse() == Identity2x2<int>{});
}

TEST_CASE("linear_algebra:invertible2x2 — matrix-vector action",
          "[linear_algebra][invertible2x2][action]") {
  // Identity on ℤ² acts trivially.
  constexpr Identity2x2<int> I{};
  constexpr Vec2<int, 3, 7> v{};
  STATIC_CHECK(I * v == Vec2<int, 3, 7>{});

  // Shear M = [[1, 1], [0, 1]] sends (x, y) to (x + y, y).
  constexpr Invertible2x2<int, 1, 1, 0, 1> shear{};
  STATIC_CHECK(shear * Vec2<int, 3, 7>{} == Vec2<int, 10, 7>{});

  // M * M^{-1} acts as identity on any vector.
  STATIC_CHECK((shear * shear.inverse()) * Vec2<int, 3, 7>{} ==
               Vec2<int, 3, 7>{});
}

TEST_CASE("linear_algebra:invertible2x2 — composition is associative",
          "[linear_algebra][invertible2x2][composition]") {
  // Basic group-axiom sanity: (A·B)·C == A·(B·C) over ℤ with det = 1 factors.
  constexpr Invertible2x2<int, 1, 1, 0, 1> shear{};
  constexpr Invertible2x2<int, 1, 0, 1, 1> transpose_shear{};
  // 90° rotation (det = 1); avoid naming it `C` — collides with ℂ / `C`.
  constexpr Invertible2x2<int, 0, -1, 1, 0> rot{};

  STATIC_CHECK((shear * transpose_shear) * rot ==
               shear * (transpose_shear * rot));
}

TEST_CASE(
    "linear_algebra:invertible2x2 — Tier 1: DirectSum preserves invertibility",
    "[linear_algebra][invertible2x2][direct_sum]") {
  // Two independently-invertible 2×2 blocks over ℚ, direct-summed into a
  // block-diagonal 4×4 operator. Invertibility is preserved structurally:
  // (A ⊕ B)^{-1} = A^{-1} ⊕ B^{-1}, and (A ⊕ B) · (A ⊕ B)^{-1} is the
  // identity direct sum.
  constexpr Invertible2x2<Rat, Rat{1L}, Rat{2L}, Rat{3L}, Rat{4L}> block_M{};
  // 90° rotation over ℚ. Naming avoids collisions with ℝ / `R`.
  constexpr Invertible2x2<Rat, Rat{0L}, Rat{-1L}, Rat{1L}, Rat{0L}> block_rot{};

  constexpr DirectSum<decltype(block_M), decltype(block_rot)> ds{};
  constexpr auto ds_inv = ds.inverse();

  using IdentityDirectSum = DirectSum<Identity2x2<Rat>, Identity2x2<Rat>>;
  STATIC_CHECK(ds * ds_inv == IdentityDirectSum{});
  STATIC_CHECK(ds_inv * ds == IdentityDirectSum{});
}

TEST_CASE("linear_algebra:invertible2x2 — Tier 1: DirectSum composes blockwise",
          "[linear_algebra][invertible2x2][direct_sum]") {
  // `(A ⊕ B) · (A' ⊕ B') = (A·A') ⊕ (B·B')`.
  constexpr Invertible2x2<int, 1, 1, 0, 1> A{};
  constexpr Invertible2x2<int, 1, 0, 1, 1> B{};
  constexpr Invertible2x2<int, 1, 2, 0, 1> A2{};
  constexpr Invertible2x2<int, 1, 0, 2, 1> B2{};

  constexpr DirectSum<decltype(A), decltype(B)> AB{};
  constexpr DirectSum<decltype(A2), decltype(B2)> AB2{};

  constexpr auto lhs = AB * AB2;
  constexpr DirectSum<decltype(A * A2), decltype(B * B2)> rhs{};
  STATIC_CHECK(lhs == rhs);
}

TEST_CASE("linear_algebra:invertible2x2 — Tier 2: BlockUpperTriangular inverse",
          "[linear_algebra][invertible2x2][block_upper_triangular]") {
  // Build a block-upper-triangular 4×4 over ℚ with A, D invertible and B
  // non-singular. The closed-form inverse requires no Schur complement
  // (since C = 0) but IS the non-trivial recurrence beyond DirectSum:
  // A^{-1} appears in the cross-block term.
  using A_t = Invertible2x2<Rat, Rat{1L}, Rat{2L}, Rat{3L}, Rat{4L}>;  // det -2
  using D_t = Invertible2x2<Rat, Rat{2L}, Rat{1L}, Rat{1L}, Rat{1L}>;  // det  1
  using B_t = Matrix2x2<Rat, Rat{1L}, Rat{0L}, Rat{0L}, Rat{1L}>;  // identity-
                                                                   // shaped but
                                                                   // lives in B
  using M_t = BlockUpperTriangular<A_t, B_t, D_t>;

  constexpr M_t M{};
  constexpr auto M_inv = M.inverse();

  using IdentityBlock =
      BlockUpperTriangular<Identity2x2<Rat>, Zero2x2<Rat>, Identity2x2<Rat>>;
  STATIC_CHECK(M * M_inv == IdentityBlock{});
  STATIC_CHECK(M_inv * M == IdentityBlock{});
}
