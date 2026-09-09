/** @file test/cpp/modules/dedekind/analysis/roots_of_unity_test.cpp
 *
 * SPIKE (Figure-5 "waves on a torus"): exact plane-wave trigonometry over the
 * coat-hanger ℝ = ℚ(√2).
 *
 * The claim under test: an 8-point Fourier / plane-wave basis is *exact* on
 * ℚ(√2), with de Moivre / Euler holding as a symbolic ring identity — no
 * transcendental evaluation, no rounding.  The reason it works is a genuine
 * algebraic coincidence: √2 = ζ₈ + ζ₈⁻¹, so ℚ(√2) = ℝ ∩ ℚ(ζ₈) is exactly the
 * real subfield of the 8th cyclotomic field.  Every 8th root of unity therefore
 * lives in Complex<ℚ(√2)>, and the sole irrational needed is ½√2 = cos(π/4).
 *
 * Two 2-D quotient algebras carry the two "trigonometries":
 *   - ℂ = ℝ[X]/(X²+1): exp(iθ) = cos θ + i sin θ (Euler, circular) — Sections
 * 1-3;
 *   - 𝔻 = ℝ[ε]/(ε²):  exp(εθ) = 1 + εθ (parabolic linearization / autodiff) —
 * §4.
 */
#include <catch2/catch_test_macros.hpp>
#include <vector>

import dedekind.numbers;  // Rational, QuadraticReal, Complex
import dedekind.analysis; // Dual

using namespace dedekind::numbers;
using dedekind::analysis::Dual;

namespace {
using Q = Rational<>;
using R2 = QuadraticReal<2>;  // ℝ = ℚ(√2), the coat-hanger
using Cx = Complex<R2>;       // ℂ over the coat-hanger (the #810 carrier)

// ½√2 = cos(π/4) = sin(π/4): the ONLY irrational an 8-point DFT needs, and it
// lands exactly in ℚ(√2).
constexpr R2 s = R2::of(Q{}, Q{1, 2});  // 0 + ½·√2
constexpr R2 zero = R2{};
constexpr R2 one = R2{1};

constexpr Cx ONE{one, zero};  // 1
constexpr Cx I{zero, one};    // i
constexpr Cx zeta{s, s};      // ζ₈ = e^{2πi/8} = cos45° + i·sin45°

// The discrete exponential map k ↦ ζ₈^k, built by repeated multiplication.
constexpr Cx pow8(unsigned k) {
  Cx r = ONE;
  for (unsigned j = 0; j < k; ++j) r = r * zeta;
  return r;
}
}  // namespace

// ---------------------------------------------------------------------------
// §1  ζ₈ is a primitive 8th root of unity — exactly, over ℚ(√2).
// ---------------------------------------------------------------------------
TEST_CASE("Roots of unity: ζ₈ is a primitive 8th root, exact over ℚ(√2)",
          "[analysis][fourier][roots][exact]") {
  // ζ² = i, ζ⁴ = -1 --- EXACT at COMPILE TIME (½√2 ∈ ℚ(√2); no rounding).
  // NOTE: exact ℚ(√2) arithmetic is constexpr-capable but step-heavy, so deep
  // chains exceed clang's constexpr budget (~8 complex mults / assert).
  // Compile- time witnesses stay shallow; deeper exact checks run at RUNTIME
  // (still exact).
  STATIC_REQUIRE(zeta * zeta == I);           // ζ² = i           (1 mult)
  STATIC_REQUIRE(pow8(4) == Cx{-one, zero});  // ζ⁴ = -1          (4 mults)

  CHECK(pow8(8) == ONE);  // ζ⁸ = 1 (closure) --- runtime (8-mult chain)

  // Primitivity: no proper power is 1.
  for (unsigned k = 1; k < 8; ++k) {
    CHECK_FALSE(pow8(k) == ONE);
  }
}

// ---------------------------------------------------------------------------
// §2  De Moivre / Euler: k ↦ ζ₈^k is an EXACT homomorphism ℤ/8 → ℂ(ℚ(√2)).
//     This is "Euler falls out": angle addition = exponent addition, as a ring
//     identity, with every value exact.
// ---------------------------------------------------------------------------
TEST_CASE("De Moivre / Euler is an exact identity on the 8-grid",
          "[analysis][fourier][demoivre][euler][exact]") {
  // Each grid root equals its exact (cos, sin) pair --- the exponential map is
  // exact, term by term.
  const Cx expected[8] = {
      {one, zero},  {s, s},   {zero, one},  {-s, s},
      {-one, zero}, {-s, -s}, {zero, -one}, {s, -s},
  };
  for (unsigned k = 0; k < 8; ++k) {
    CHECK(pow8(k) == expected[k]);
  }

  // THE headline: ζ^a · ζ^b = ζ^{(a+b) mod 8}, EXACTLY, for every pair.
  // (= cos(A+B) = cosA cosB - sinA sinB and sin(A+B) = ... , as ring
  // arithmetic.)
  for (unsigned a = 0; a < 8; ++a) {
    for (unsigned b = 0; b < 8; ++b) {
      CHECK(pow8(a) * pow8(b) == pow8((a + b) % 8));
    }
  }

  // Angle addition pinned at compile time (shallow, symbolic): ζ²·ζ² = ζ⁴, i.e.
  // e^{iπ/2}·e^{iπ/2} = e^{iπ}, i.e. i·i = -1 --- exact by ring arithmetic.
  STATIC_REQUIRE((zeta * zeta) * (zeta * zeta) == Cx{-one, zero});
}

// ---------------------------------------------------------------------------
// §3  Standing-wave node & critical sets on the 8-grid --- exact, and
//     differentiation is a quarter-turn (multiply by i = ζ²).
// ---------------------------------------------------------------------------
TEST_CASE("Standing wave: exact node set {cos=0} and critical set {sin=0}",
          "[analysis][fourier][nodal][exact]") {
  // g(k) = cos(2πk/8) = Re ζ^k, the 1-D standing wave.
  // Node set {g = 0}: Re ζ^k = 0.
  // Its derivative g'(k) ∝ -sin(2πk/8) = Re(i·ζ^k) = Re ζ^{k+2}:
  // differentiation is a quarter-turn in ℂ.  Critical set {g' = 0}: Re(i·ζ^k) =
  // 0.
  std::vector<unsigned> nodes, crits;
  for (unsigned k = 0; k < 8; ++k) {
    if (pow8(k).real() == zero) nodes.push_back(k);        // cos = 0
    if ((I * pow8(k)).real() == zero) crits.push_back(k);  // -sin = 0 (extrema)
  }
  CHECK(nodes == std::vector<unsigned>{2, 6});  // cos zero at π/2, 3π/2
  CHECK(crits == std::vector<unsigned>{0, 4});  // sin zero at 0, π (max/min)

  // The 2-D C₄ᵥ fundamental mode f(x,y) = cos(2πx/8) + cos(2πy/8): its critical
  // set is {sin_x = 0} × {sin_y = 0} = {0,4}², the 4 extrema/saddles --- pinned
  // by symmetry, exact.
  int crit_count = 0;
  for (unsigned x = 0; x < 8; ++x) {
    for (unsigned y = 0; y < 8; ++y) {
      const bool dfx0 = (I * pow8(x)).real() == zero;
      const bool dfy0 = (I * pow8(y)).real() == zero;
      if (dfx0 && dfy0) ++crit_count;
    }
  }
  CHECK(crit_count == 4);  // (0,0),(0,4),(4,0),(4,4)
}

// ---------------------------------------------------------------------------
// §4  The parabolic sibling 𝔻 = ℝ[ε]/(ε²): exact autodiff over ℚ(√2).
//     exp(εθ) = 1 + εθ is the linearization dual to ℂ's Euler exp.
// ---------------------------------------------------------------------------
TEST_CASE("Dual numbers: exact autodiff over ℚ(√2) (the parabolic sibling)",
          "[analysis][fourier][dual][exact]") {
  using Du = Dual<R2>;
  // Seed x = ½√2 with dx = 1; f(x) = x²; read f(x) and f'(x) exactly.
  constexpr Du x{s, one};
  constexpr Du f = x * x;
  STATIC_REQUIRE(f.value() == s * s);       // (½√2)² = ½
  STATIC_REQUIRE(f.derivative() == s + s);  // f'(x) = 2x = √2
  // and ½√2 squared is exactly ½, √2/2 doubled is exactly √2 --- no rounding.
  STATIC_REQUIRE(s * s == R2::of(Q{1, 2}, Q{}));  // ½
  STATIC_REQUIRE(s + s == R2::root());            // √2
}
