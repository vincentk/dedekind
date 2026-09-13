/** @file test/cpp/modules/dedekind/analysis/roots_of_unity_test.cpp
 *
 * Exact plane-wave trigonometry over the coat-hanger ℝ = ℚ(√2).  Groundwork for
 * the Figure-5 "waves on a torus" exhibit.
 *
 * The claim under test: an 8-point Fourier / plane-wave basis is *exact* on
 * ℚ(√2), with de Moivre / Euler holding as a symbolic ring identity — no
 * transcendental evaluation, no rounding.  The reason it works is a genuine
 * algebraic coincidence: √2 = ζ₈ + ζ₈⁻¹, so ℚ(√2) = ℝ ∩ ℚ(ζ₈) is exactly the
 * real subfield of the 8th cyclotomic field.  Every 8th root of unity therefore
 * lives in Complex<ℚ(√2)>, and the sole irrational needed is ½√2 = cos(π/4).
 *
 * The de Moivre exponential k ↦ ζ₈^k, the conjugate, and the node-set fact are
 * now LIBRARY primitives (dedekind.numbers: `root8`, `ζ₈`, `conj`, and the
 * certified real-zero set = morphologies::Congruence<4,2>).  This exhibit
 * composes them; it no longer reimplements the exponential map.
 *
 * Two 2-D quotient algebras carry the two "trigonometries":
 *   - ℂ = ℝ[X]/(X²+1): exp(iθ) = cos θ + i sin θ (Euler, circular) — Sections
 * 1-3;
 *   - 𝔻 = ℝ[ε]/(ε²):  exp(εθ) = 1 + εθ (parabolic linearization / autodiff) —
 * §4.
 */
#include <catch2/catch_test_macros.hpp>
#include <utility>
#include <vector>

import dedekind.numbers; // Rational, QuadraticReal, Complex, root8, ζ₈, conj
import dedekind.morphologies; // Modular<8u> — the domain ℤ/8 of root8
import dedekind.analysis;     // Dual
import dedekind.sets;         // ambient_set (predicate→set), the native meet &
import dedekind.relational;   // preimage — the residue-class pullback (#816)

using namespace dedekind::numbers;
using dedekind::analysis::Dual;
using dedekind::morphologies::Congruence;
using dedekind::morphologies::Modular;

namespace {
using Q = Rational<>;
using R2 = QuadraticReal<2>;  // ℝ = ℚ(√2), the coat-hanger
using Cx = Complex<R2>;       // ℂ over the coat-hanger (the #810 carrier)
using M8 = Modular<8u>;       // ℤ/8, the domain of the de Moivre map root8

// ½√2 = cos(π/4) = sin(π/4): the ONLY irrational an 8-point DFT needs, and it
// lands exactly in ℚ(√2).
constexpr R2 s = R2::of(Q{}, Q{1, 2});  // 0 + ½·√2
constexpr R2 zero = R2{};
constexpr R2 one = R2{1};

constexpr Cx ONE{one, zero};  // 1
constexpr Cx I{zero, one};    // i

// The discrete exponential map k ↦ ζ₈^k is the library primitive `root8`
// (ℤ/8 → ℂˣ); this thin adapter keeps the call sites reading `ζ(k)`.
constexpr Cx ζ(unsigned k) { return root8(M8{k}); }
}  // namespace

// ---------------------------------------------------------------------------
// §1  ζ₈ is a primitive 8th root of unity — exactly, over ℚ(√2).
// ---------------------------------------------------------------------------
TEST_CASE("Roots of unity: ζ₈ is a primitive 8th root, exact over ℚ(√2)",
          "[analysis][fourier][roots][exact]") {
  // `root8` folds via the half-turn ζ⁴ = −1, so every power is a shallow
  // O(1) table lookup: the whole μ₈ table is a COMPILE-TIME fact.
  STATIC_REQUIRE(ζ(1) == ζ8);              // the library generator
  STATIC_REQUIRE(ζ(2) == I);               // ζ² = i
  STATIC_REQUIRE(ζ(4) == Cx{-one, zero});  // ζ⁴ = -1
  STATIC_REQUIRE(ζ(8) == ONE);             // ζ⁸ = 1 (closure; 8 ≡ 0 mod 8)

  // Primitivity: no proper power is 1.
  for (unsigned k = 1; k < 8; ++k) {
    CHECK_FALSE(ζ(k) == ONE);
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
    CHECK(ζ(k) == expected[k]);
  }

  // THE headline: ζ^a · ζ^b = ζ^{(a+b) mod 8}, EXACTLY, for every pair.  Now
  // stated as ℤ/8 addition inside the domain (M8{a} + M8{b}), matching the
  // library homomorphism witness pinned in :complex.
  for (unsigned a = 0; a < 8; ++a) {
    for (unsigned b = 0; b < 8; ++b) {
      CHECK(ζ(a) * ζ(b) == root8(M8{a} + M8{b}));
    }
  }

  // Conjugate symmetry of the roots: ζ^{-k} = conj(ζ^k) (the ℝ-signal relation
  // c_{-k} = conj(c_k)).  Exercises the newly-exported `conj`.
  for (unsigned k = 0; k < 8; ++k) {
    CHECK(root8(M8{8u - k % 8u}) == conj(ζ(k)));
  }

  // Angle addition pinned at compile time: ζ²·ζ² = ζ⁴, i.e.
  // e^{iπ/2}·e^{iπ/2} = e^{iπ}, i.e. i·i = -1 --- exact by ring arithmetic.
  STATIC_REQUIRE(ζ(2) * ζ(2) == Cx{-one, zero});
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
    if (ζ(k).real() == zero) nodes.push_back(k);        // cos = 0
    if ((I * ζ(k)).real() == zero) crits.push_back(k);  // -sin = 0 (extrema)
  }
  CHECK(nodes == std::vector<unsigned>{2, 6});  // cos zero at π/2, 3π/2
  CHECK(crits == std::vector<unsigned>{0, 4});  // sin zero at 0, π (max/min)

  // The node set IS the library fact: {Re ζ^k = 0} = Congruence<4,2> (k ≡ 2
  // mod 4), certified in :complex.  Re-checked here against `root8` directly.
  for (unsigned k = 0; k < 8; ++k) {
    CHECK((ζ(k).real() == zero) == Congruence<4, 2>{}(k));
  }

  // The 2-D C₄ᵥ fundamental mode f(x,y) = cos(2πx/8) + cos(2πy/8): its critical
  // set is {sin_x = 0} × {sin_y = 0} = {0,4}², the 4 extrema/saddles --- pinned
  // by symmetry, exact.
  int crit_count = 0;
  for (unsigned x = 0; x < 8; ++x) {
    for (unsigned y = 0; y < 8; ++y) {
      const bool dfx0 = (I * ζ(x)).real() == zero;
      const bool dfy0 = (I * ζ(y)).real() == zero;
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

// ---------------------------------------------------------------------------
// §5  The Figure-5 wave: a non-separable even wave on the torus (ℤ/8)², whose
//     node set is a VISIBLE preimage of the residue class {Re ζ₈ᵏ = 0} through
//     the linear forms x±y.  The strength reduction, made point-free: the exact
//     ℚ(√2) node test collapses to the finite-quotient residue class (the §3
//     fact {Re ζ = 0} = Congruence<4,2>), pulled back through x±y and met — no
//     deep ℚ(√2) grid, no bespoke reduce_to, reusing the general preimage
//     (#816).  ψ(x,y) = cos(2π(x+y)/8) + i·cos(2π(x−y)/8): even, NON-separable,
//     so the node set is a DIAMOND (not a product of two 1-D sets) — S is
//     genuinely more than P.
// ---------------------------------------------------------------------------
namespace figure5 {
using dedekind::relational::preimage;

// The linear-form arrows (x,y) ↦ x+y and (x,y) ↦ x−y on the torus (ℤ/8)²
// (named, no lambdas; x−y uses Modular's group subtraction).
struct Sum {
  using Domain = std::pair<M8, M8>;
  using Codomain = M8;
  constexpr M8 operator()(const Domain& p) const { return p.first + p.second; }
};
struct Diff {
  using Domain = std::pair<M8, M8>;
  using Codomain = M8;
  constexpr M8 operator()(const Domain& p) const { return p.first - p.second; }
};
// The node class on the exponent k ∈ ℤ/8: {Re ζ₈ᵏ = 0} = Congruence<4,2>
// (k ≡ 2 mod 4), the certified §3 fact, on the representative k.value.
struct ReZetaZeroClass {
  using Domain = M8;
  using Codomain = bool;
  constexpr bool operator()(M8 k) const { return Congruence<4, 2>{}(k.value); }
};

// The node class {k ∈ ℤ/8 : Re ζ₈ᵏ = 0} as a Set on ℤ/8: ambient_set<M8>(p)
// constructs the set of M8 elements classified by the predicate p (Modular<8>
// is a registered species).  (The "set = ambient + predicate" reading is the
// conceptual model on #824/#826, not a claim about stored fields.)
constexpr auto node_class = dedekind::sets::ambient_set<M8>(ReZetaZeroClass{});

// The diamond = the residue class pulled back through EACH linear form, then
// MET (native Set ∩ Set): {(x,y) : (x+y) ∈ C ∧ (x−y) ∈ C}.  A visible preimage
// of a Trsk composition (#816) --- the honest form of the strength reduction.
constexpr auto diamond =
    preimage(Sum{}, node_class) & preimage(Diff{}, node_class);

// The diamond nodes on the [0,4]² fundamental cell, counted at compile time
// (named helper, no lambda).
constexpr int diamond_cell_count() {
  int n = 0;
  for (unsigned x = 0; x <= 4u; ++x)
    for (unsigned y = 0; y <= 4u; ++y)
      if (diamond(std::pair{M8{x}, M8{y}})) ++n;
  return n;
}
}  // namespace figure5

TEST_CASE(
    "§5 non-separable wave: the diamond node set as a preimage of the residue "
    "class",
    "[analysis][fourier][wave][strength-reduction][exact]") {
  using figure5::diamond;
  using figure5::ReZetaZeroClass;

  // EXACT layer / the strength-reduction justification: the exact ℚ(√2) node
  // test {Re ζ₈ᵏ = 0} IS the finite-quotient residue class Congruence<4,2>
  // (§3).  This equivalence is what lets the diamond run on ℤ/8, never touching
  // the deep ℚ(√2) grid.
  for (unsigned k = 0; k < 8u; ++k)
    CHECK((ζ(k).real() == zero) == ReZetaZeroClass{}(M8{k}));

  // The diamond, point-free: Re ψ = Im ψ = 0 ⟺ x+y and x−y both in the class.
  CHECK(diamond(std::pair{M8{2}, M8{0}}));        // (2,0)
  CHECK(diamond(std::pair{M8{0}, M8{2}}));        // (0,2)
  CHECK(diamond(std::pair{M8{4}, M8{2}}));        // (4,2)
  CHECK_FALSE(diamond(std::pair{M8{1}, M8{0}}));  // off band
  CHECK_FALSE(diamond(std::pair{M8{3}, M8{5}}));  // off band

  // The [0,4]² fundamental cell = the 4 diamond points, materialised at COMPILE
  // TIME on the cheap residue surrogate (the deep ℚ(√2) grid is never touched).
  static_assert(figure5::diamond_cell_count() == 4,
                "4 diamond nodes on the [0,4]² cell, via the preimage of the "
                "residue class --- compile-time.");

  // Runtime materialisation (for coverage).
  std::vector<std::pair<unsigned, unsigned>> cell;
  for (unsigned x = 0; x <= 4u; ++x)
    for (unsigned y = 0; y <= 4u; ++y)
      if (diamond(std::pair{M8{x}, M8{y}})) cell.push_back({x, y});
  CHECK(cell == std::vector<std::pair<unsigned, unsigned>>{
                    {0, 2}, {2, 0}, {2, 4}, {4, 2}});
}
