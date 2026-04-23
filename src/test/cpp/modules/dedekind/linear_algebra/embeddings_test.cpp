/** @file dedekind/linear_algebra/embeddings_test.cpp
 *
 * Unit coverage for the `:embeddings` partition — canonical regular
 * representations ℂ ↪ M₂(ℚ) and 𝔻 ↪ M₂(ℚ). The `static_assert`s in the
 * module itself already pin the ring-homomorphism witnesses at build
 * time; this file mirrors them into Catch2 `STATIC_CHECK`s so they show
 * up in the test report alongside the rest of the linear-algebra suite.
 *
 * Also hosts the operational algebra-modules concept checks
 * (`IsScalarLike`, `IsFieldElementLike`) on concrete numeric carriers
 * — those live here because `Rational<long>` cannot be imported into
 * the algebra test directory (module-DAG rule: algebra is upstream of
 * numbers).
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
