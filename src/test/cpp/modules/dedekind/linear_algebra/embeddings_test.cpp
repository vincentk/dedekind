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

// The "Algebraic Lattice (Figure 1)" read-side anchor moved to
// linear_algebra/matrix_test.cpp at PR #500 — the test grew matrix-
// tier corners (M_2(R), M_2(X(ℂ))) and the matrix-tier file is the
// natural home, since the lattice-corner story is broader than the
// ℂ ↪ M₂ / 𝔻 ↪ M₂ regular-representation embeddings this file is
// otherwise about.
