#include <catch2/catch_test_macros.hpp>
#include <functional>  // std::plus / std::multiplies for the IsSemiring witness

// SPIKE (folds in #818): the even Reynolds projector as an OPERATOR on the
// existing bra-ket surface — P = ½(I + U), U the inversion unitary — over the
// finite ℤ/8 grid (the computable/materialised layer of the Figure-6 descent).
// Confirms the operator view builds on linear_algebra's Ket/Bra/SemimoduleVec
// (⊕/⊗) over exact ℂ = Complex<ℚ(√2)>, not a parallel geometry construction.
// The intensional-index generalisation (symbolic Σ_d) is the follow-up.

import dedekind.category;
import dedekind.linear_algebra;
import dedekind.morphologies;
import dedekind.numbers;

using dedekind::linear_algebra::Bra;
using dedekind::linear_algebra::geometric_sum;
using dedekind::linear_algebra::inner_product;
using dedekind::linear_algebra::Ket;
using dedekind::morphologies::Modular;
using dedekind::numbers::Complex;
using dedekind::numbers::QuadraticReal;
using dedekind::numbers::root8;

namespace {
using M8 = Modular<8u>;
using R2 = QuadraticReal<2>;  // ℝ = ℚ(√2), the coat-hanger
using Cx = Complex<R2>;       // ℂ over ℚ(√2), exact

// The plane wave ζ₈^k sampled on ℤ/8, as a Ket |v⟩ (index k → scalar).
// Runtime const: filling all 8 exact ℚ(√2) values would strain the
// constant-evaluation budget; the shallow value-side pin is a scalar
// static_assert below.
const Ket<Cx, 8> plane_wave = [] {
  Ket<Cx, 8> v{};
  for (unsigned k = 0; k < 8; ++k) v.c[k] = root8(M8{k});
  return v;
}();

// The inversion U on ℤ/8 as an operator on kets: (U|v⟩)_k = v_{−k}.
constexpr Ket<Cx, 8> apply_inversion(const Ket<Cx, 8>& v) {
  Ket<Cx, 8> u{};
  for (unsigned k = 0; k < 8; ++k) u.c[k] = v.c[(8u - k) % 8u];
  return u;
}

// The even Reynolds projector P = ½(I + U), spelled on the bra-ket surface:
//   P|v⟩ = ½ ⊗ (|v⟩ ⊕ U|v⟩),   ⊕ and ⊗ the SemimoduleVec ops (semiring_ops<ℂ>).
constexpr Ket<Cx, 8> project_even(const Ket<Cx, 8>& v) {
  const Cx half{R2{1} / R2{2}, R2{}};
  return half * (v + apply_inversion(v));
}
}  // namespace

TEST_CASE("Bra-ket: P=½(I+U) projects ζ₈ onto cos (the operator S-leg)",
          "[linear_algebra][braket][symmetrize][fourier][exact]") {
  const Ket<Cx, 8> cos_ket = project_even(plane_wave);

  // Shallow COMPILE-TIME witness of the projector at the node k=2:
  //   ½(ζ² + ζ⁶) = ½(i + (−i)) = 0 = cos(π/2).
  static_assert(Cx{R2{1} / R2{2}, R2{}} * (root8(M8{2}) + root8(M8{6})) == Cx{},
                "P at k=2 is cos(π/2) = 0 (exact, compile-time)");

  // The even projection IS the cosine: (P|v⟩)_k = cos(2πk/8) = Re ζ₈^k,
  // exactly.
  for (unsigned k = 0; k < 8; ++k)
    CHECK(cos_ket.c[k] == Cx{root8(M8{k}).real(), R2{}});

  // Idempotent (P² = P): a genuine projection onto the even subspace (S-leg).
  CHECK(project_even(cos_ket) == cos_ket);

  // Keystone 1: ℂ = Complex<ℚ(√2)> is now a certified semiring, so the
  // semiring bra-ket inner_product ⟨·|·⟩ works over exact ℂ.
  static_assert(
      dedekind::category::IsSemiring<Cx, std::plus<Cx>, std::multiplies<Cx>>,
      "the exact coat-hanger ℂ is a rig (registered by propagation "
      "from ℚ(√2)).");

  // The discriminant classification of Complex<R> = R[i]/(i²+1) (the H-leg):
  // R a formally-real field ⇒ x²+1 irreducible ⇒ ℂ is a FIELD; the rig
  // structure propagates unconditionally, field-ness only for the supported
  // formally-real bases.  Assert the public IsField concept (the whole chain).
  static_assert(
      dedekind::category::IsField<Cx, std::plus<Cx>, std::multiplies<Cx>>,
      "ℂ = Complex<ℚ(√2)> IS a field (x²+1 irreducible over the formally-real "
      "ℚ(√2)).");
  static_assert(
      !dedekind::category::IsField<Complex<Cx>, std::plus<Complex<Cx>>,
                                   std::multiplies<Complex<Cx>>>,
      "Complex<ℂ> SPLITS (its base ℂ already contains i) — NOT a field.");
  static_assert(
      !dedekind::category::IsSemiring<Complex<double>,
                                      std::plus<Complex<double>>,
                                      std::multiplies<Complex<double>>>,
      "Complex<double> is not even a semiring (IEEE breaks associativity).");
  Ket<Cx, 8> e0{};
  e0.c[0] = Cx{R2{1}, R2{}};
  const Bra<Cx, 8> e0_bra{e0.c};
  CHECK(inner_product<8>(e0_bra, plane_wave) ==
        Cx{R2{1}, R2{}});  // ⟨e₀|v⟩ = v₀ = ζ⁰ = 1
}

TEST_CASE(
    "Symbolic Σ: geometric_sum collapses the DFT / character orthogonality",
    "[linear_algebra][braket][symbolic-sum][fourier][exact]") {
  // The structure-directed sum over an enumerable index, by the division-free
  // O(log N) doubling (correct over any rig, unsigned included):
  //   Σ_{k<8} 2^k = 2^8 − 1 = 255, no walk of k.
  static_assert(geometric_sum<unsigned>(2u, 8) == 255u, "Σ 2^k = 2^8 − 1");

  // The DFT / character orthogonality over EXACT ℂ, decided symbolically:
  //   ⟨χ_m|χ_n⟩ = Σ_{k∈ℤ/8} ζ₈^{Δk} = geometric_sum(ζ₈^Δ, 8) = 8·[Δ≡0], else 0.
  // NO enumeration of k — the group structure (ζ₈⁸ = 1) collapses the sum.
  // Δ = 0 (m = n): the diagonal, 8·1.  (Runtime: the exact ℚ(√2) doubling
  // recursion exceeds the constant-evaluation budget; the unsigned sum above is
  // the compile-time pin.)
  CHECK(geometric_sum(root8(M8{0}), 8) == Cx{R2{8}, R2{}});
  // Δ ≠ 0 (m ≠ n): orthogonal, exactly 0.
  CHECK(geometric_sum(root8(M8{1}), 8) == Cx{});
  CHECK(geometric_sum(root8(M8{2}), 8) == Cx{});
  CHECK(geometric_sum(root8(M8{3}), 8) == Cx{});
  CHECK(geometric_sum(root8(M8{5}), 8) == Cx{});
}
