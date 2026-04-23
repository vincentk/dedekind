/** @file dedekind/linear_algebra/embeddings_test.cpp
 *
 * Unit coverage for the canonical regular representations ℂ ↪ M₂(ℚ) and
 * 𝔻 ↪ M₂(ℚ) in `:embeddings`. The `static_assert`s in the module itself
 * already pin the ring-homomorphism witnesses at build time; this file
 * mirrors them into Catch2 `STATIC_CHECK`s so they show up in the test
 * report alongside the rest of the linear-algebra suite.
 */

#include <catch2/catch_test_macros.hpp>

import dedekind.algebra;
import dedekind.linear_algebra;
import dedekind.numbers;

using namespace dedekind::algebra;
using namespace dedekind::linear_algebra;
using namespace dedekind::numbers;

namespace {
using Rat = Rational<long>;
}  // namespace

TEST_CASE(
    "algebra:modules — a bona fide field element is a 1D vector space over "
    "itself",
    "[algebra][modules][scalar][field]") {
  // Operational witnesses (IsFieldLikeScalar-based) — the concepts the
  // machine-backed carriers actually satisfy under the active numeric policy.
  STATIC_CHECK(IsScalarLike<int>);
  STATIC_CHECK(IsScalarLike<unsigned int>);
  STATIC_CHECK(IsFieldElementLike<Rat>);
}

TEST_CASE("linear_algebra:embeddings — ℂ ↪ M₂(ℚ) is a ring homomorphism",
          "[linear_algebra][embeddings][complex]") {
  // a + b·i ↦ [[a, -b], [b, a]]
  constexpr Complex<Rat> z{Rat{1L}, Rat{2L}};
  constexpr Complex<Rat> w{Rat{3L}, Rat{-5L}};

  constexpr auto Mz = as_matrix2x2(z);
  STATIC_CHECK(Mz.m11 == Rat{1L});
  STATIC_CHECK(Mz.m12 == Rat{-2L});
  STATIC_CHECK(Mz.m21 == Rat{2L});
  STATIC_CHECK(Mz.m22 == Rat{1L});

  // Additive and multiplicative homomorphism on a non-symmetric pair.
  STATIC_CHECK(as_matrix2x2(z + w) == as_matrix2x2(z) + as_matrix2x2(w));
  STATIC_CHECK(as_matrix2x2(z * w) == as_matrix2x2(z) * as_matrix2x2(w));

  // Units map to units.
  STATIC_CHECK(as_matrix2x2(Complex<Rat>{Rat{0L}, Rat{0L}}) ==
               zero_matrix2x2_v<Rat>);
  STATIC_CHECK(as_matrix2x2(Complex<Rat>{Rat{1L}, Rat{0L}}) ==
               identity_matrix2x2_v<Rat>);

  // Injectivity: the left inverse round-trips on the image.
  STATIC_CHECK(complex_from_matrix2x2(as_matrix2x2(z)) == z);
}

TEST_CASE("linear_algebra:embeddings — 𝔻 ↪ M₂(ℚ) is a ring homomorphism",
          "[linear_algebra][embeddings][dual]") {
  // a + b·ε ↦ [[a, b], [0, a]]
  constexpr Dual<Rat> d{Rat{2L}, Rat{3L}};
  constexpr Dual<Rat> e{Rat{-1L}, Rat{5L}};

  constexpr auto Md = as_matrix2x2(d);
  STATIC_CHECK(Md.m11 == Rat{2L});
  STATIC_CHECK(Md.m12 == Rat{3L});
  STATIC_CHECK(Md.m21 == Rat{0L});
  STATIC_CHECK(Md.m22 == Rat{2L});

  STATIC_CHECK(as_matrix2x2(d + e) == as_matrix2x2(d) + as_matrix2x2(e));
  STATIC_CHECK(as_matrix2x2(d * e) == as_matrix2x2(d) * as_matrix2x2(e));

  STATIC_CHECK(as_matrix2x2(Dual<Rat>{Rat{0L}, Rat{0L}}) ==
               zero_matrix2x2_v<Rat>);
  STATIC_CHECK(as_matrix2x2(Dual<Rat>{Rat{1L}, Rat{0L}}) ==
               identity_matrix2x2_v<Rat>);

  STATIC_CHECK(dual_from_matrix2x2(as_matrix2x2(d)) == d);
}

TEST_CASE("linear_algebra:embeddings — nilpotent ε² = 0 lifts to M₂(ℚ)",
          "[linear_algebra][embeddings][dual][nilpotent]") {
  // The nilpotent generator ε = 0 + 1·ε has matrix image [[0, 1], [0, 0]]
  // whose square is the zero matrix — the homomorphism carries the defining
  // relation of 𝔻 exactly.
  constexpr Dual<Rat> eps_q{Rat{0L}, Rat{1L}};
  STATIC_CHECK(as_matrix2x2(eps_q * eps_q) == zero_matrix2x2_v<Rat>);
  STATIC_CHECK(as_matrix2x2(eps_q) * as_matrix2x2(eps_q) ==
               zero_matrix2x2_v<Rat>);
}

TEST_CASE(
    "linear_algebra:contracts — the nine matrix slogans witnessed on "
    "Matrix2x2V<ℚ>",
    "[linear_algebra][contracts][concepts]") {
  // (1) A matrix column is a vector.
  STATIC_CHECK(IsColumnVector<Vec2V<Rat>>);

  // (2) A matrix row is a row vector.
  STATIC_CHECK(IsCovector<Covec2V<Rat>>);

  // (3) Vectors and covectors carry a dimension.
  STATIC_CHECK(HasDimensionCount<Vec2V<Rat>>);
  STATIC_CHECK(HasDimensionCount<Covec2V<Rat>>);

  // (4) Matrices decompose both horizontally (columns) and vertically (rows).
  STATIC_CHECK(HasColumnDecomposition<Matrix2x2V<Rat>>);
  STATIC_CHECK(HasRowDecomposition<Matrix2x2V<Rat>>);

  // (5) Matrices carry two dimensionalities: row count and column count.
  STATIC_CHECK(HasMatrixShape<Matrix2x2V<Rat>>);
  STATIC_CHECK(Matrix2x2V<Rat>::row_count == 2u);
  STATIC_CHECK(Matrix2x2V<Rat>::column_count == 2u);

  // (6) Matrix over a ring is at least a submodule-like carrier.
  STATIC_CHECK(IsMatrixSubmoduleLike<Matrix2x2V<Rat>, Rat>);

  // (7) Matrix over a field is at least a ring.
  STATIC_CHECK(IsMatrixOverFieldRingLike<Matrix2x2V<Rat>, Rat>);

  // (8) Matrix multiplication is closed (and non-commutative in general).
  STATIC_CHECK(HasMatrixMultiplication<Matrix2x2V<Rat>>);

  // (9) Ring operations plus transpose — umbrella matrix-algebra concept.
  STATIC_CHECK(HasTranspose<Matrix2x2V<Rat>>);
  STATIC_CHECK(IsMatrixAlgebra<Matrix2x2V<Rat>, Rat>);
}

TEST_CASE(
    "linear_algebra:contracts — decomposition and transpose agree on a probe",
    "[linear_algebra][contracts][decomposition][transpose]") {
  constexpr Matrix2x2V<Rat> M{Rat{1L}, Rat{2L}, Rat{3L}, Rat{4L}};

  // Horizontal concatenation view: columns are (1, 3)^T and (2, 4)^T.
  STATIC_CHECK(M.column(0) == Vec2V<Rat>{Rat{1L}, Rat{3L}});
  STATIC_CHECK(M.column(1) == Vec2V<Rat>{Rat{2L}, Rat{4L}});

  // Vertical concatenation view: rows are (1, 2) and (3, 4).
  STATIC_CHECK(M.row(0) == Covec2V<Rat>{Rat{1L}, Rat{2L}});
  STATIC_CHECK(M.row(1) == Covec2V<Rat>{Rat{3L}, Rat{4L}});

  // Transpose is an involution with off-diagonal swap.
  STATIC_CHECK(M.transpose() ==
               Matrix2x2V<Rat>{Rat{1L}, Rat{3L}, Rat{2L}, Rat{4L}});
  STATIC_CHECK(M.transpose().transpose() == M);
}

TEST_CASE("linear_algebra:contracts — matrix multiplication is non-commutative",
          "[linear_algebra][contracts][non_commutative]") {
  // Two nilpotent matrices over ℚ for which lhs·rhs ≠ rhs·lhs. Pins the
  // property that the concept documentation claims without requiring.
  // Names avoid the ambient `B` (BooleanSet) in dedekind::algebra.
  constexpr Matrix2x2V<Rat> nilpotent_lhs{Rat{0L}, Rat{1L}, Rat{0L}, Rat{0L}};
  constexpr Matrix2x2V<Rat> nilpotent_rhs{Rat{0L}, Rat{0L}, Rat{1L}, Rat{0L}};
  STATIC_CHECK(nilpotent_lhs * nilpotent_rhs != nilpotent_rhs * nilpotent_lhs);
}
