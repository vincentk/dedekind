#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <type_traits>
#include <utility>

// ─────────────────────────────────────────────────────────────────────────────
// SPIKE — Figure 6, rows 2–3 ("the same construction, made exact"), the
// CONTINUOUS/intensional tier, BEFORE any torus quotient or discretization.
//
// The whole of rows 2–3 is "simple arithmetic on plane-wave symbols".  The
// algebraic infrastructure that lets the compiler do it:
//   (1) exact ℂ = Complex<ℚ(√2)> as the scalar — a certified semiring/field
//   (#818); (2) the PLANE WAVE carried SPECTRALLY as its coefficient map  w ↦
//   c_w  — an
//       `IsArrow` over the wave-vector lattice ℤ², i.e. a vector in the
//       function space ℂ^(ℤ²).  This is infinite-dimensional (the
//       "functions-as-vectors" layer), NOT a finite tuple: the domain point
//       k∈ℤ² is the finite (2-D) vector; the wave ψ living in ℂ^(ℤ²) is the
//       infinite one;
//   (3) pointwise ⊕ and scalar · — the semimodule operations of that function
//       space (built here as the `Sum` / `Scaled` combinator rules).  This is
//       the "algebraic infrastructure" in one line: ℂ^(ℤ²) is a ℂ-semimodule;
//   (4) the reflection U : w ↦ −w, giving the even projector P = ½(I + U).
//
// Exactness forces the spectral (symbolic) form: the spatial wave exp(i k·x) is
// transcendental, NOT representable in ℚ(√2); it becomes exactly evaluable only
// on the torus (root8 at ℤ/N) — the NEXT tier.  Here nothing is discretized.
// ─────────────────────────────────────────────────────────────────────────────

import dedekind.category; // IsArrow
import dedekind.numbers;  // Complex, QuadraticReal, Rational
import dedekind.sets; // IsRelation, Graph, graph(·) — the arrow ⟶ relation lift
import dedekind.relational; // relative product on graphs

using dedekind::category::IsArrow;
using dedekind::numbers::Complex;
using dedekind::numbers::QuadraticReal;
using dedekind::numbers::Rational;

namespace {
using R2 = QuadraticReal<2>;  // ℝ = ℚ(√2), the coat-hanger
using Cx = Complex<R2>;       // ℂ over ℚ(√2), exact
using Q = Rational<>;

constexpr Cx operator""_re(unsigned long long n) {
  return Cx{R2{long(n)}, R2{}};
}
constexpr Cx I{R2{}, R2{1}};  // the imaginary unit i

// A 2-D wave-vector on ℤ² — the FINITE (2-D product) domain.  k·x lives in ℝ
// and its exp does not close over ℚ(√2); that is why ψ is carried spectrally
// below.
struct Wave {
  int kx{}, ky{};
  friend constexpr bool operator==(const Wave&, const Wave&) = default;
};
constexpr Wave operator-(const Wave& k) {
  return {-k.kx, -k.ky};
}  // reflection

// Any Wave→ℂ arrow is a vector in the function space ℂ^(ℤ²).  The symbol
// arithmetic (⊕, scalar·, even) is defined exactly on this surface.
template <class F>
concept IsWaveVector =
    IsArrow<F> && std::same_as<typename std::remove_cvref_t<F>::Domain, Wave> &&
    std::same_as<typename std::remove_cvref_t<F>::Codomain, Cx>;

// The PLANE-WAVE SYMBOL χ_k, carried spectrally as the one-hot |k⟩:
// the coefficient map w ↦ [w = k].  A single basis Ket of ℂ^(ℤ²).
struct PlaneWave {
  Wave k;
  using Domain = Wave;
  using Codomain = Cx;
  constexpr Cx operator()(Wave w) const { return w == k ? 1_re : Cx{}; }
};
constexpr PlaneWave wave(Wave k) { return PlaneWave{k}; }

// scalar ·  — the semimodule scaling of ℂ^(ℤ²): (c·f)(w) = c ⊗ f(w).
template <class F>
struct Scaled {
  Cx c;
  F f;
  using Domain = Wave;
  using Codomain = Cx;
  constexpr Cx operator()(Wave w) const { return c * f(w); }
};
template <IsWaveVector F>
constexpr Scaled<std::remove_cvref_t<F>> operator*(Cx c, F f) {
  return Scaled<std::remove_cvref_t<F>>{c, f};
}

// ⊕  — the semimodule addition of ℂ^(ℤ²): (f ⊕ g)(w) = f(w) ⊕ g(w).
template <class F, class G>
struct Sum {
  F f;
  G g;
  using Domain = Wave;
  using Codomain = Cx;
  constexpr Cx operator()(Wave w) const { return f(w) + g(w); }
};
template <IsWaveVector F, IsWaveVector G>
constexpr Sum<std::remove_cvref_t<F>, std::remove_cvref_t<G>> operator+(F f,
                                                                        G g) {
  return Sum<std::remove_cvref_t<F>, std::remove_cvref_t<G>>{f, g};
}

// The even-symmetry projector  P = ½(I + U),  U : w ↦ −w  (parity/reflection).
// (Pψ)(w) = ½(ψ(w) ⊕ ψ(−w)).  ψ is EVEN ⟺ Pψ = ψ ⟺ its spectrum is symmetric
// under k ↦ −k.  Idempotent (P² = P): a genuine projection onto the even
// subspace — the constructive S-leg, here on the continuous spectral vector.
template <class F>
struct Even {
  F f;
  using Domain = Wave;
  using Codomain = Cx;
  constexpr Cx operator()(Wave w) const {
    const Cx half{R2{Q{1, 2}}, R2{}};
    return half * (f(w) + f(-w));
  }
};
template <IsWaveVector F>
constexpr Even<std::remove_cvref_t<F>> even(F f) {
  return Even<std::remove_cvref_t<F>>{f};
}
}  // namespace

TEST_CASE("Figure 6 row 2: ψ is a superposition of plane-wave symbols",
          "[linear_algebra][funcspace][fourier][spike]") {
  constexpr Wave k1{1, 0}, k2{0, 1};  // orthogonal wave-vectors in ℤ²

  // ROW 2 — simple arithmetic on plane-wave symbols: a non-separable wave as a
  // linear combination of unit waves with orthogonal wave-vectors.
  constexpr auto psi = 2_re * wave(k1) + I * wave(k2);

  // ψ IS a vector in the function space ℂ^(ℤ²): an IsArrow, no finiteness.
  static_assert(IsArrow<decltype(psi)>,
                "ψ is a vector in the function space ℂ^(ℤ²) (an IsArrow).");

  // The SAME ψ, lifted by graph(·), is the predicate-on-pairs view:
  //   graph(ψ) = { (w, c) | c = ψ(w) } ⊆ ℤ² × ℂ
  // graph(·) targets the Set<pair> relation ENCODING (IsRelation: Domain =
  // pair<Wave,ℂ>) — NOT the curried 2-arg IsBinaryRelation/IsFunction form; the
  // two are distinct encodings.  GraphPredicate carries ψ, so the lift is
  // lossless (graphs compose by the relative product).  IsArrow stays the
  // carrier; graph(·) is the bridge to the relational/predicate lattice where
  // range/support predicates live.
  static_assert(
      dedekind::sets::IsRelation<decltype(dedekind::sets::graph(psi)), Wave,
                                 Cx>,
      "graph(ψ) is the Set<pair> relation view (a predicate on pairs).");
  // Membership carries ψ:  (k1, 2) ∈ Γ_ψ,  (k1, 0) ∉ Γ_ψ.
  CHECK(dedekind::sets::graph(psi)(std::pair<Wave, Cx>{k1, 2_re}));
  CHECK_FALSE(dedekind::sets::graph(psi)(std::pair<Wave, Cx>{k1, Cx{}}));

  // Its spectrum, read off exactly — 2 at k1, i at k2, 0 elsewhere.
  static_assert(psi(k1) == 2_re, "c_{k1} = 2");
  static_assert(psi(k2) == I, "c_{k2} = i");
  static_assert(psi(Wave{2, 3}) == Cx{}, "off-support coefficient is 0");

  CHECK(psi(k1) == 2_re);
  CHECK(psi(-k1) == Cx{});  // asymmetric: nothing at −k1 yet (see row 3)
}

TEST_CASE("Figure 6 row 3: the even-symmetry projector P = ½(I+U)",
          "[linear_algebra][funcspace][symmetrize][spike]") {
  constexpr Wave k{1, 0};

  // An already-even wave: spectrum symmetric under k ↦ −k  ⇒  P fixes it.
  constexpr auto psi_even = 1_re * wave(k) + 1_re * wave(-k);
  static_assert(even(psi_even)(k) == psi_even(k), "P|ψ⟩ = |ψ⟩ at k (ψ even)");
  static_assert(even(psi_even)(-k) == psi_even(-k), "… and at −k");

  // A single plane wave is NOT even; P projects it onto its even part:
  //   (P|k⟩)(±k) = ½,  so the ½(|k⟩ ⊕ |−k⟩) cosine-like standing wave.
  constexpr auto projected = even(wave(k));
  static_assert(projected(k) == Cx{R2{Q{1, 2}}, R2{}}, "(P|k⟩)_k = ½");
  static_assert(projected(-k) == Cx{R2{Q{1, 2}}, R2{}}, "(P|k⟩)_{−k} = ½");

  // Idempotent P² = P — a genuine projection (witnessed at the support).
  static_assert(even(projected)(k) == projected(k), "P² = P at k");
  static_assert(even(projected)(-k) == projected(-k), "P² = P at −k");

  CHECK(even(psi_even)(k) == psi_even(k));
  CHECK(projected(k) == Cx{R2{Q{1, 2}}, R2{}});
}
