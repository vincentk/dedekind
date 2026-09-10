#include <catch2/catch_test_macros.hpp>

// The constructive S-leg over the exact coat-hanger: the even Reynolds
// projection P₊ = ½(id + inversion) turns the plane wave ζ₈^k into the real
// cosine cos(2πk/8), EXACTLY over ℂ = Complex<ℚ(√2)>.  This is the
// symmetry-adapted classification the Figure-6 descent needs (ψ₁ = a sum of
// orthogonal plane waves; ψ₂ = its even part = cosines), and the value-side
// witness for the pointwise/symmetrization surface added under #537 slice 2.

import dedekind.category;
import dedekind.geometry;
import dedekind.morphologies;
import dedekind.numbers;

using dedekind::category::arrow;
using dedekind::geometry::even_part;
using dedekind::geometry::orbit_sum;
using dedekind::morphologies::Modular;
using dedekind::numbers::Complex;
using dedekind::numbers::QuadraticReal;
using dedekind::numbers::root8;

namespace {
using M8 = Modular<8u>;
using R2 = QuadraticReal<2>;  // ℝ = ℚ(√2), the coat-hanger
using Cx = Complex<R2>;       // ℂ over ℚ(√2), exact

// The inversion on ℤ/8: k ↦ −k = (8 − k) mod 8 — the generator of the ℤ₂
// point group whose invariant subalgebra is the even functions.
constexpr auto inv8 =
    arrow<M8, M8>([](const M8& k) { return M8(8u - k.value); });
constexpr auto id8 = arrow<M8, M8>([](const M8& k) { return k; });
}  // namespace

TEST_CASE("Symmetrization: the even Reynolds projection turns ζ₈ into cos",
          "[analysis][geometry][function][symmetrize][fourier][exact]") {
  const R2 half = R2{1} / R2{2};
  const auto cos_wave = even_part(half, root8, inv8);  // ½(ζ^k + ζ^{-k})

  // Shallow COMPILE-TIME witness (node k=2: cos(π/2)=0), pinning the
  // symmetrization value-side without touching the deep ℚ(√2) grid.
  static_assert(even_part(R2{1} / R2{2}, root8, inv8)(M8{2}) == Cx{},
                "even projection at k=2 is cos(π/2) = 0 (exact, compile-time)");

  // At every k the projection reproduces the real part (the cosine), exactly:
  //   ½(ζ^k + ζ^{-k}) = ½(ζ^k + conj ζ^k) = Re ζ^k = cos(2πk/8).
  for (unsigned k = 0; k < 8u; ++k)
    CHECK(cos_wave(M8{k}) == Cx{root8(M8{k}).real(), R2{}});

  // It lands in the even subalgebra (the S-leg): inversion-invariant.
  for (unsigned k = 0; k < 8u; ++k)
    CHECK(cos_wave(M8{k}) == cos_wave(inv8(M8{k})));

  // It is a genuine projection: P(P f) = P f (idempotent).
  const auto cos_wave2 = even_part(half, cos_wave, inv8);
  for (unsigned k = 0; k < 8u; ++k) CHECK(cos_wave2(M8{k}) == cos_wave(M8{k}));

  // The un-normalised orbit sum is the doubled cosine.
  for (unsigned k = 0; k < 8u; ++k)
    CHECK(orbit_sum(root8, id8, inv8)(M8{k}) ==
          Cx{root8(M8{k}).real() + root8(M8{k}).real(), R2{}});
}
