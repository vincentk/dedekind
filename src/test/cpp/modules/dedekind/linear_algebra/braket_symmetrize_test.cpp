#include <catch2/catch_test_macros.hpp>

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

  // FIXME(#818-followup): the bra-ket inner_product ⟨·|·⟩ is gated on
  // IsSemiring<S>, which ℂ = Complex<ℚ(√2)> does NOT yet satisfy (the transfer
  // machinery registered only tropical / bool).  ℂ is a field, hence a
  // semiring — registering semiring_ops/IsSemiring for Complex<R> is a small
  // follow-up that lights up ⟨·|·⟩ (and the Hermitian form via a bra dagger)
  // on this exact carrier.
}
