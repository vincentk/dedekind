/** @file dedekind/linear_algebra/embeddings_test.cpp
 *
 * Unit coverage for the `:embeddings` partition — canonical regular
 * representations ℂ ↪ M₂(ℚ) and 𝔻 ↪ M₂(ℚ). The `static_assert`s in the
 * module itself already pin the ring-homomorphism witnesses at build
 * time; this file mirrors them into Catch2 `STATIC_CHECK`s so they show
 * up in the test report alongside the rest of the linear-algebra suite.
 *
 * Also hosts the operator-shape concept checks
 * (`HasRingOperators`, `HasFieldOperators`) on concrete numeric carriers —
 * those live here because `Rational<long>` cannot be imported into the
 * algebra test directory (module-DAG rule: algebra is upstream of numbers).
 */

#include <catch2/catch_test_macros.hpp>
#include <functional>

import dedekind.algebra;
import dedekind.analysis; // Dual<F> (relocated from :numbers at PR #513)
import dedekind.category; // dedekind::category::IsField on bool (F_2 reading)
import dedekind.linear_algebra;
import dedekind.numbers;
import dedekind.order; // dedekind::order::HasLatticeOperators on bool
import dedekind.sets;

using namespace dedekind::algebra;
using namespace dedekind::analysis;  // Dual<F>
using namespace dedekind::linear_algebra;
using namespace dedekind::numbers;
using dedekind::sets::SignedExtensionalCardinal;

namespace {
// Arbitrary-precision signed rational — the canonical ℚ carrier.
using Rat = Rational<SignedExtensionalCardinal<>>;
}  // namespace

TEST_CASE("algebra:modules — operator-shape witnesses for bona fide scalars",
          "[algebra][modules][scalar][field]") {
  // Operator-shape predicates are what int / unsigned int actually satisfy
  // under the active numeric policy (the strict IsRing / IsField are
  // intentionally not specialised on these carriers — signed-overflow UB,
  // modular wrap).  Rat carries the full HasFieldOperators surface (the
  // canonical concept from algebra:field, == HasRingOperators ∧
  // HasGroupOperatorsMul), and is the textbook ℚ.
  STATIC_CHECK(HasRingOperators<int>);
  STATIC_CHECK(HasRingOperators<unsigned int>);
  STATIC_CHECK(HasFieldOperators<Rat>);
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

// ===========================================================================
// Algebraic-Lattice corners on the canonical numeric tower.
//
// This test is the read-side anchor for Figure 1 (`fig:carrier-lattice`)
// of `paper.tex`.  Each STATIC_CHECK below pins one corner of the
// 3D oblique lattice as satisfying the universal-algebra (A, F)
// pattern at the closure tier (cf. `dedekind.algebra:universal`).
//
// Cells that compile today are pinned with positive STATIC_CHECKs.
// Cells that don't are flagged with FIXME breadcrumbs naming the
// missing surface or the structural reason — those become activation
// targets for #498 (the Algebraic Tower epic) as the trait registry
// (#499), refined types (#496), and the Dual / Complex generalisations
// (#504, #505) ship.
// ===========================================================================

TEST_CASE("Algebraic Lattice (Figure 1): cube corners on the numeric tower",
          "[linear_algebra][algebraic_lattice][universal]") {
  using dedekind::algebra::IsAlgebra;

  // ---- Front face (no quotient applied) ----
  //   Bottom row: scalars Q, R, C.

  // ℚ as carrier — the canonical exact rational.
  STATIC_CHECK(IsAlgebra<Rat, std::plus<Rat>, std::multiplies<Rat>>);

  // ℂ as carrier — Complex<Rat> closes ring-shape under (+, *).
  STATIC_CHECK(IsAlgebra<Complex<Rat>, std::plus<Complex<Rat>>,
                         std::multiplies<Complex<Rat>>>);

  // 𝔻 as carrier — Dual<Rat> closes ring-shape under (+, *).
  STATIC_CHECK(
      IsAlgebra<Dual<Rat>, std::plus<Dual<Rat>>, std::multiplies<Dual<Rat>>>);

  // ---- Top row of the front face: Matrix layer over each scalar ----
  // FIXME(#498/NEW-B / #500): pin IsAlgebra on Matrix2x2V<Rat>,
  // Matrix2x2V<Complex<Rat>>, Matrix2x2V<Dual<Rat>>.  Requires
  // std::plus<Matrix2x2V<...>> / std::multiplies<Matrix2x2V<...>>
  // functor-tier closure to be exercised; currently the matrix
  // operator surface is exercised at the value level
  // (`Matrix2x2V<...>::operator+`, `::operator*`) but the std::plus<>
  // /std::multiplies<> functor instantiations have not been pinned
  // here.  Add when NEW-B (#500) lands the trait specialisations.

  // ---- Back face (single quotient applied, Q-axis = Dual or Cplx) ----
  //
  // Examples of two-step compositions live one functor deep:
  //   - Dual(Complex(Rat))   — the AD carrier over ℚ-rationals.
  //   - Complex(Dual(Rat))   — abstractly isomorphic to Dual(Complex(Rat))
  //                            as the polynomial quotient
  //                            ℚ[i, ε]/(i²+1, ε²); the C++ carriers are
  //                            distinct types with distinct concrete
  //                            operations and no canonical-iso witness
  //                            shipped today (cf. #504/#505 generalisations).
  //
  // The two compositions commute (independent generators i, ε), per
  // the Algebraic Lattice caption.  Either direction works as a
  // closure-tier witness.
  using Z2_over_Rat = Dual<Complex<Rat>>;
  STATIC_CHECK(IsAlgebra<Z2_over_Rat, std::plus<Z2_over_Rat>,
                         std::multiplies<Z2_over_Rat>>);

  // ---- Far-back-corner: M_2(X(ℂ)) — the lattice top ----
  // FIXME(#498/NEW-B / #500): pin IsAlgebra on
  // Matrix2x2V<Dual<Complex<Rat>>>.  Same reason as the matrix-row
  // FIXME above — std::plus/std::multiplies functor-tier closure on
  // the composite Matrix2x2V<...> carrier is not pinned.  When NEW-B
  // (#500) lands, this becomes the canonical lattice-top witness:
  //
  //   STATIC_CHECK(
  //       IsAlgebra<Matrix2x2V<Dual<Complex<Rat>>>,
  //                 std::plus<Matrix2x2V<Dual<Complex<Rat>>>>,
  //                 std::multiplies<Matrix2x2V<Dual<Complex<Rat>>>>>);

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
  STATIC_CHECK(IsAlgebra<𝔹, std::bit_xor<bool>, std::bit_and<bool>>);
  STATIC_CHECK(
      dedekind::category::IsField<𝔹, std::bit_xor<bool>, std::bit_and<bool>>);
  STATIC_CHECK(dedekind::order::HasLatticeOperators<𝔹>);

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
