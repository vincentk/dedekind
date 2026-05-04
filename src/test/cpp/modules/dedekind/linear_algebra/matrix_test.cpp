/** @file dedekind/linear_algebra/matrix_test.cpp
 *
 * Unit coverage for the `:matrix` partition:
 *   - NTTP family: `Vec2`, `Invertible2x2`, `Matrix2x2`, `DirectSum`,
 *     `BlockUpperTriangular` (Tiers 0–2 of issue #366).
 *   - Value-level family: `Matrix2x2V<T>` with full ring-plus-transpose
 *     surface, shape-conforming actions, and the orthogonal group
 *     O(2, ℚ).
 *   - Concept conformance against `:contracts` for the value-level triple
 *     `(Matrix2x2V, Vec2V, Covec2V)`.
 */

#include <catch2/catch_test_macros.hpp>
#include <functional>  // std::plus / std::multiplies / std::bit_xor / std::bit_and (Algebraic Lattice corners)

import dedekind.algebra; // IsAlgebra (universal-algebra closure-tier predicate; Algebraic Lattice)
import dedekind.analysis; // Dual<F> (relocated from :numbers at #513)
import dedekind.category;
import dedekind.numbers;
import dedekind.linear_algebra;
import dedekind.order; // dedekind::order::HasLatticeOperators on bool (bool lattice corner)
import dedekind.sets; // SignedExtensionalCardinal (the canonical Rational<default_integer> carrier for the lattice corners)

using namespace dedekind::analysis;
using namespace dedekind::numbers;
using namespace dedekind::linear_algebra;

namespace {
// Canonical paper-facing carrier: ℚ as a proxy for ℝ (exact arithmetic).
using Rat = Rational<long>;
}  // namespace

TEST_CASE("linear_algebra:matrix — identity at the type level",
          "[linear_algebra][matrix][identity]") {
  constexpr Identity2x2<Rat> I{};
  STATIC_CHECK(I.m11 == Rat{1L});
  STATIC_CHECK(I.m12 == Rat{0L});
  STATIC_CHECK(I.m21 == Rat{0L});
  STATIC_CHECK(I.m22 == Rat{1L});
  STATIC_CHECK(I.det == Rat{1L});
}

TEST_CASE("linear_algebra:matrix — Cramer's inverse on ℚ",
          "[linear_algebra][matrix][inverse]") {
  // M = [[1, 2], [3, 4]],  det = -2,
  // M^{-1} = (-1/2) * [[4, -2], [-3, 1]] = [[-2, 1], [3/2, -1/2]]
  constexpr Invertible2x2<Rat, Rat{1L}, Rat{2L}, Rat{3L}, Rat{4L}> M{};
  STATIC_CHECK(M.det == Rat{-2L});

  // Existential proof: the closed-form inverse is the honest two-sided
  // inverse, not just a left- or right-inverse by luck.
  STATIC_CHECK(M * M.inverse() == Identity2x2<Rat>{});
  STATIC_CHECK(M.inverse() * M == Identity2x2<Rat>{});
}

TEST_CASE("linear_algebra:matrix — shear matrix on ℤ (det = 1)",
          "[linear_algebra][matrix][integer]") {
  // M = [[1, 1], [0, 1]],  det = 1;  M^{-1} = [[1, -1], [0, 1]] over ℤ.
  constexpr Invertible2x2<int, 1, 1, 0, 1> shear{};
  STATIC_CHECK(shear.det == 1);
  STATIC_CHECK(shear * shear.inverse() == Identity2x2<int>{});
}

TEST_CASE("linear_algebra:matrix — matrix-vector action",
          "[linear_algebra][matrix][action]") {
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

TEST_CASE("linear_algebra:matrix — composition is associative",
          "[linear_algebra][matrix][composition]") {
  // Basic group-axiom sanity: (A·B)·C == A·(B·C) over ℤ with det = 1 factors.
  constexpr Invertible2x2<int, 1, 1, 0, 1> shear{};
  constexpr Invertible2x2<int, 1, 0, 1, 1> transpose_shear{};
  // 90° rotation (det = 1); avoid naming it `C` — collides with ℂ / `C`.
  constexpr Invertible2x2<int, 0, -1, 1, 0> rot{};

  STATIC_CHECK((shear * transpose_shear) * rot ==
               shear * (transpose_shear * rot));
}

TEST_CASE("linear_algebra:matrix — Tier 1: DirectSum preserves invertibility",
          "[linear_algebra][matrix][direct_sum]") {
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

TEST_CASE("linear_algebra:matrix — Tier 1: DirectSum composes blockwise",
          "[linear_algebra][matrix][direct_sum]") {
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

TEST_CASE("linear_algebra:matrix — Tier 2: BlockUpperTriangular inverse",
          "[linear_algebra][matrix][block_upper_triangular]") {
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

/** @section Value_level_Matrix2x2V_and_Concept_Witnesses
 *
 *  The nine matrix slogans, shape conformance, and O(2, ℚ) witnessed on the
 *  concrete value-level triple `(Matrix2x2V<Rat>, Vec2V<Rat>, Covec2V<Rat>)`.
 */

TEST_CASE(
    "linear_algebra:matrix — the nine matrix slogans witnessed on "
    "Matrix2x2V<ℚ>",
    "[linear_algebra][matrix][value][concepts]") {
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

TEST_CASE("linear_algebra:matrix — concatenation builds matrices from tuples",
          "[linear_algebra][matrix][value][concatenation]") {
  constexpr Matrix2x2V<Rat> M{Rat{1L}, Rat{2L}, Rat{3L}, Rat{4L}};

  // Horizontal: two columns concatenate to the matrix. Inverse of
  // column decomposition.
  STATIC_CHECK((M.column(0) | M.column(1)) == M);

  // Vertical: two rows concatenate to the matrix. Inverse of row
  // decomposition.
  STATIC_CHECK((M.row(0) / M.row(1)) == M);

  // Scalar → tuple: aggregate init IS concatenation; the matrix can be
  // built bottom-up from explicit scalar pairs via tuples.
  constexpr Vec2V<Rat> c1{Rat{1L}, Rat{3L}};
  constexpr Vec2V<Rat> c2{Rat{2L}, Rat{4L}};
  STATIC_CHECK((c1 | c2) == M);

  constexpr Covec2V<Rat> r1{Rat{1L}, Rat{2L}};
  constexpr Covec2V<Rat> r2{Rat{3L}, Rat{4L}};
  STATIC_CHECK((r1 / r2) == M);

  // The two builders agree on the same matrix, as they should: two
  // distinct routes (columns via |, rows via /) to the same underlying
  // value.
  STATIC_CHECK((c1 | c2) == (r1 / r2));
}

TEST_CASE(
    "linear_algebra:matrix — decomposition and transpose agree on a probe",
    "[linear_algebra][matrix][value][decomposition][transpose]") {
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

TEST_CASE("linear_algebra:matrix — matrix multiplication is non-commutative",
          "[linear_algebra][matrix][value][non_commutative]") {
  // Two nilpotent matrices over ℚ for which lhs·rhs ≠ rhs·lhs. Pins the
  // property that the concept documentation claims without requiring.
  // Names avoid the ambient `B` (BooleanSet) in dedekind::algebra.
  constexpr Matrix2x2V<Rat> nilpotent_lhs{Rat{0L}, Rat{1L}, Rat{0L}, Rat{0L}};
  constexpr Matrix2x2V<Rat> nilpotent_rhs{Rat{0L}, Rat{0L}, Rat{1L}, Rat{0L}};
  STATIC_CHECK(nilpotent_lhs * nilpotent_rhs != nilpotent_rhs * nilpotent_lhs);
}

TEST_CASE(
    "linear_algebra:matrix — shape-conformant addition and multiplication",
    "[linear_algebra][matrix][value][shape]") {
  // Addition only between matching shape + scalar; mismatches are ill-typed.
  STATIC_CHECK(MatchesAdditiveShape<Matrix2x2V<Rat>, Matrix2x2V<Rat>>);
  STATIC_CHECK_FALSE(MatchesAdditiveShape<Matrix2x2V<Rat>, Vec2V<Rat>>);
  STATIC_CHECK_FALSE(MatchesAdditiveShape<Vec2V<Rat>, Covec2V<Rat>>);

  // Multiplication requires inner dimensions to agree.
  STATIC_CHECK(MatchesMultiplicativeShape<Matrix2x2V<Rat>, Matrix2x2V<Rat>>);
  STATIC_CHECK(MatchesMultiplicativeShape<Matrix2x2V<Rat>, Vec2V<Rat>>);
  STATIC_CHECK(MatchesMultiplicativeShape<Covec2V<Rat>, Matrix2x2V<Rat>>);
  STATIC_CHECK_FALSE(
      MatchesMultiplicativeShape<Vec2V<Rat>, Matrix2x2V<Rat>>);  // 1 ≠ 2
  STATIC_CHECK_FALSE(
      MatchesMultiplicativeShape<Matrix2x2V<Rat>, Covec2V<Rat>>);  // 2 ≠ 1

  STATIC_CHECK(HasConformingMatrixAddition<Matrix2x2V<Rat>, Matrix2x2V<Rat>>);
  STATIC_CHECK(
      HasConformingMatrixMultiplication<Matrix2x2V<Rat>, Matrix2x2V<Rat>>);
  STATIC_CHECK(HasConformingMatrixMultiplication<Matrix2x2V<Rat>, Vec2V<Rat>>);
  STATIC_CHECK(
      HasConformingMatrixMultiplication<Covec2V<Rat>, Matrix2x2V<Rat>>);
}

TEST_CASE(
    "linear_algebra:matrix — O(2, ℚ) as a multiplicative group of "
    "orthogonal matrices",
    "[linear_algebra][matrix][value][orthogonal][group]") {
  STATIC_CHECK(IsOrthogonalMatrixCarrier<Matrix2x2V<Rat>>);

  constexpr auto I = identity_matrix2x2_v<Rat>;
  constexpr Matrix2x2V<Rat> R90{Rat{0L}, Rat{-1L}, Rat{1L}, Rat{0L}};
  constexpr Matrix2x2V<Rat> Rx{Rat{1L}, Rat{0L}, Rat{0L}, Rat{-1L}};

  // Orthogonality law Mᵀ·M = M·Mᵀ = I on each concrete orthogonal.
  STATIC_CHECK(I.transpose() * I == I);
  STATIC_CHECK(R90.transpose() * R90 == I);
  STATIC_CHECK(R90 * R90.transpose() == I);
  STATIC_CHECK(Rx.transpose() * Rx == I);
  STATIC_CHECK(Rx * Rx.transpose() == I);

  // Group closure and inverse law.
  constexpr auto R180 = R90 * R90;
  STATIC_CHECK(R180.transpose() * R180 == I);
  STATIC_CHECK(R180 == Matrix2x2V<Rat>{Rat{-1L}, Rat{0L}, Rat{0L}, Rat{-1L}});

  // Linear action on ℚ² via the shape-conforming matrix-vector product.
  constexpr Vec2V<Rat> e1{Rat{1L}, Rat{0L}};
  constexpr Vec2V<Rat> e2{Rat{0L}, Rat{1L}};
  STATIC_CHECK(R90 * e1 == e2);
  STATIC_CHECK(R90 * e2 == -e1);
}

// Runtime coverage: `constexpr` matrix operators only reached via
// `STATIC_CHECK` are elided from the instrumented binary. A small set of
// runtime `CHECK`s forces the coverage tool to see these paths.
TEST_CASE("linear_algebra:matrix — runtime-exercised Matrix2x2V operators",
          "[linear_algebra][matrix][runtime_coverage]") {
  Matrix2x2V<int> A{1, 2, 3, 4};
  Matrix2x2V<int> B{5, 6, 7, 8};

  CHECK(A == Matrix2x2V<int>{1, 2, 3, 4});        // default operator==
  CHECK_FALSE(A == B);                            // negative case
  CHECK(A + B == Matrix2x2V<int>{6, 8, 10, 12});  // operator+ body
}

// ===========================================================================
// Algebraic-Lattice corners on the canonical numeric tower.
//
// This test is the read-side anchor for Figure 1 (`fig:carrier-lattice`)
// of `paper.tex`.  Each STATIC_CHECK below pins one corner of the
// 3D oblique lattice as satisfying the universal-algebra (A, F)
// pattern at the closure tier (cf. `dedekind.algebra:universal`).
//
// Cells that compile today are pinned with positive STATIC_CHECKs.
// The matrix-tier corners (M_2(R), M_2(X(ℂ))) closed at PR #500;
// the original FIXME breadcrumbs at those cells turned out to be
// over-cautious — Matrix2x2V<T>'s operator+/operator* surface is
// already enough for the closure-tier IsAlgebra check to fire.
//
// Originally lived in linear_algebra/embeddings_test.cpp (where the
// matrix-tier work surfaced); moved here at PR #500 to a dedicated
// matrix-tier home, since the lattice-corner story is broader than
// the ℂ ↪ M₂ / 𝔻 ↪ M₂ embeddings that file is otherwise about.
// ===========================================================================

namespace {
// Arbitrary-precision signed rational — the canonical ℚ carrier for the
// lattice corners (matches embeddings_test.cpp's choice of carrier and
// keeps the closure-tier identities exact at the ring layer).
using LatticeRat = Rational<dedekind::sets::SignedExtensionalCardinal<>>;
}  // namespace

TEST_CASE("Algebraic Lattice (Figure 1): cube corners on the numeric tower",
          "[linear_algebra][algebraic_lattice][universal]") {
  using dedekind::algebra::IsAlgebra;
  using R = LatticeRat;

  // ---- Front face (no quotient applied) ----
  //   Bottom row: scalars Q, R, C.

  // ℚ as carrier — the canonical exact rational.
  STATIC_CHECK(IsAlgebra<R, std::plus<R>, std::multiplies<R>>);

  // ℂ as carrier — Complex<R> closes ring-shape under (+, *).
  STATIC_CHECK(IsAlgebra<Complex<R>, std::plus<Complex<R>>,
                         std::multiplies<Complex<R>>>);

  // 𝔻 as carrier — Dual<R> closes ring-shape under (+, *).
  STATIC_CHECK(
      IsAlgebra<Dual<R>, std::plus<Dual<R>>, std::multiplies<Dual<R>>>);

  // ---- Top row of the front face: Matrix layer over each scalar ----
  //
  // M_2(R) = End_R(Free_2(R)) — the rank-functor image at n=2 over
  // each base scalar.  Pinning IsAlgebra on Matrix2x2V<...> at the
  // closure tier makes the rank-axis of Figure 1 firm: same shape as
  // the bottom-row scalar witnesses, one rank up.
  STATIC_CHECK(IsAlgebra<Matrix2x2V<R>, std::plus<Matrix2x2V<R>>,
                         std::multiplies<Matrix2x2V<R>>>);
  STATIC_CHECK(
      IsAlgebra<Matrix2x2V<Complex<R>>, std::plus<Matrix2x2V<Complex<R>>>,
                std::multiplies<Matrix2x2V<Complex<R>>>>);
  STATIC_CHECK(IsAlgebra<Matrix2x2V<Dual<R>>, std::plus<Matrix2x2V<Dual<R>>>,
                         std::multiplies<Matrix2x2V<Dual<R>>>>);

  // ---- Back face (single quotient applied, Q-axis = Dual or Cplx) ----
  //
  // Examples of two-step compositions live one functor deep:
  //   - Dual(Complex(R))   — the AD carrier over ℚ-rationals.
  //   - Complex(Dual(R))   — abstractly isomorphic to Dual(Complex(R))
  //                          as the polynomial quotient
  //                          ℚ[i, ε]/(i²+1, ε²); the C++ carriers are
  //                          distinct types with distinct concrete
  //                          operations and no canonical-iso witness
  //                          shipped today (cf. #504/#505 generalisations).
  //
  // The two compositions commute (independent generators i, ε), per
  // the Algebraic Lattice caption.  Either direction works as a
  // closure-tier witness.
  using Z2_over_R = Dual<Complex<R>>;
  STATIC_CHECK(
      IsAlgebra<Z2_over_R, std::plus<Z2_over_R>, std::multiplies<Z2_over_R>>);

  // ---- Far-back-corner: M_2(X(ℂ)) — the lattice top ----
  //
  // The composite carrier sitting at the top-right-back corner of
  // Figure 1: matrix layer (rank axis) over the back-face quotient
  // (Dual ∘ Complex) over ℚ.  Closing IsAlgebra here pins the lattice
  // top --- the upper bound of the worked existence-proof slice (the
  // lattice itself is bounded-below / unbounded-above; this corner is
  // the conventional stop, not a maximum).
  STATIC_CHECK(IsAlgebra<Matrix2x2V<Dual<Complex<R>>>,
                         std::plus<Matrix2x2V<Dual<Complex<R>>>>,
                         std::multiplies<Matrix2x2V<Dual<Complex<R>>>>>);

  // ---- Lattice bottom-front-left corner: 𝔹 = bool ----
  //
  // The Algebraic Lattice caption places 𝔹 at the bottom-front-left
  // corner.  Per the paper §3.4 Rosetta tables, the natural reading
  // of bool's algebraic surface is the Galois field 𝔽₂ under
  // (XOR, AND); the Boolean-lattice reading under (∨, ∧) is the
  // sibling structure.  Both fire on the same carrier; the operation
  // tuple disambiguates.  These witnesses also live upstream
  // (`numbers:boolean`, `algebra:universal`), but pinning them here
  // anchors the lattice bottom to the read-side test that names
  // itself for Figure 1.
  STATIC_CHECK(IsAlgebra<bool, std::bit_xor<bool>, std::bit_and<bool>>);
  STATIC_CHECK(dedekind::category::IsField<bool, std::bit_xor<bool>,
                                           std::bit_and<bool>>);
  STATIC_CHECK(dedekind::order::HasLatticeOperators<bool>);

  // ---- Boundary case: alphabet choice on the 𝔹-rooted cells ----
  //
  // The closure-tier `IsAlgebra<bool, std::plus<bool>, std::multiplies<bool>>`
  // check tests `op(a, b) -> std::same_as<bool>`.  `std::plus<bool>` and
  // `std::multiplies<bool>` go through C++ integer promotion (literal
  // `a + b` on bools yields `int`) but the functor's call operator
  // narrows back to `bool`, so the closure-tier `same_as<bool>` check
  // mechanically succeeds.  What the closure check does *not* see is
  // that the values returned reflect the int-promoted arithmetic
  // (`true + true -> int(2) -> bool(true)`, not the F_2-natural
  // `true + true -> false`).  The textbook (+, *) reading is therefore
  // the *wrong alphabet* for the lattice-bottom cell --- the F_2
  // reading above (XOR, AND) is the operationally correct one;
  // (∨, ∧) is the Boolean-lattice sibling on the same carrier.
  // Paper §3.4 footnote a and `tab:rosetta-textbook-vs-cpp` cover the
  // alphabet choice in detail; the refined-type discipline of #496
  // will make the bool→F_2 channel a typed boundary so the wrong
  // alphabet becomes a type error rather than a silent semantic
  // mismatch.
}
