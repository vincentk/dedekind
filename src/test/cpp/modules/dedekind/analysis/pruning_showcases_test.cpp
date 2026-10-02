/** @file dedekind/analysis/pruning_showcases_test.cpp
 *
 * Analysis-module mirror of the paper-facing pruning showcases (originally
 * under `src/test/cpp/modules/dedekind/python/showcase_0{1..8}_*.cpp`).
 *
 * The `static_assert` witnesses in each showcase are self-sufficient evidence
 * of the compile-time reduction — if the translation unit compiles, the
 * theorem holds. This mirror re-expresses those witnesses as
 * Catch2 `STATIC_CHECK`s so the reductions are exercised by the
 * `test_analysis` suite in addition to the IR fixture harness.
 *
 * IR inspection is retained for the handful of existential proofs in the
 * `python/` directory (the canonical demonstrations that clang actually
 * emits the collapsed form at -O2). Everything else is covered here.
 */

#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <type_traits>
#include <utility>

import dedekind.category;
import dedekind.sets;
import dedekind.algebra;
import dedekind.numbers;
import dedekind.order;

using namespace dedekind::category;
using namespace dedekind::sets;
using namespace dedekind::algebra;
using namespace dedekind::numbers;
using namespace dedekind::order;

namespace {

// Shared with showcase 1 (ℝ² diagonal × strip).
constexpr auto R2 = ℝ_d * ℝ_d;
using R2Point = typename decltype(R2)::Domain;
// The diagonal {x == y}: reuse the set-expression operator== on the projections
// (π1 == π2), not a hand lambda, over the R2 base, so the element type tracks
// R2 (Real<double> under the double-real proxy, else double) and diag & strip
// stays well-typed.  Kept as the exact scout-equivalent @c Comprehension<R2,
// π1==π2> to guarantee the showcase compiles (Trsk `R2 | (π1==π2)` is an
// equivalent point-free spelling; CI-gated).
constexpr auto diag = Set{Comprehension{R2, π1 == π2}};
// FLAG(#895 L3): pair float bounds; a NAMED predicate (not a hand lambda),
// candidate point-free `R2 | (π1 > bound<5.0> && π2 < bound<3.0>)`.
constexpr auto in_strip = [](R2Point p) {
  return (p.first > 5.0) && (p.second < 3.0);
};
constexpr auto strip = Set{Comprehension{R2, in_strip}};

}  // namespace

TEST_CASE("Pruning showcase 1: diagonal × strip on ℝ² is empty",
          "[analysis][pruning][showcase][showcase01]") {
  constexpr auto empty_diagonal_cut = diag & strip;
  using R2Logic = typename decltype(empty_diagonal_cut)::logic_species;

  // On the diagonal x = y, the strip x>5 ∧ y<3 is contradictory.
  STATIC_CHECK(empty_diagonal_cut(R2Point{6.0, 6.0}) == R2Logic::False);
  STATIC_CHECK(empty_diagonal_cut(R2Point{2.0, 2.0}) == R2Logic::False);
}

namespace {

// Shared with showcase 2 (ℂ lattice × square singleton).
// Post-HSP retarget: ℂ is the coat-hanger 𝔸<Complex<QuadraticReal<2>>, ...>, so
// showcase 2 runs on EXACT ℚ(√2) arithmetic; the scout stays element<ℂ> (now a
// Complex<QuadraticReal<2>> scout).
using QR = QuadraticReal<2>;  // exact real carrier (R2 is taken above for ℝ²)
using Q = Rational<>;         // for the exact rational thresholds ½, 1½
// A coordinate is a "small natural" iff it is one of 0,1,2,3 — on the EXACT
// carrier the "integral ∧ 0 ≤ · ≤ 3" test IS membership in {0,1,2,3}.
constexpr bool is_small_natural(const QR& t) {
  return t == QR{} || t == QR{1} || t == QR{2} || t == QR{3};
}

// Complex real()/imag() component predicates are not π-projectable (π projects
// pair coordinates, not Complex parts), so NAMED-predicate comprehensions.
constexpr auto in_natural_lattice = [](const Complex<QR>& z) {
  return is_small_natural(z.real()) && is_small_natural(z.imag());
};
constexpr auto in_unit_square = [](const Complex<QR>& z) {
  return (z.real() >= QR{Q{1, 2}}) && (z.real() <= QR{Q{3, 2}}) &&
         (z.imag() >= QR{Q{1, 2}}) && (z.imag() <= QR{Q{3, 2}});
};
constexpr auto natural_lattice_in_c = Set{Comprehension{ℂ, in_natural_lattice}};
constexpr auto square_c1_c2 = Set{Comprehension{ℂ, in_unit_square}};

}  // namespace

TEST_CASE("Pruning showcase 2: ℕ² lattice × [½,1½]² in ℂ = {1+i}",
          "[analysis][pruning][showcase][showcase02]") {
  constexpr auto lattice_square = natural_lattice_in_c & square_c1_c2;
  using CLogic = typename decltype(lattice_square)::logic_species;

  STATIC_CHECK(lattice_square(Complex<QR>{QR{1}, QR{1}}) == CLogic::True);
  STATIC_CHECK(lattice_square(Complex<QR>{QR{}, QR{1}}) == CLogic::False);
  STATIC_CHECK(lattice_square(Complex<QR>{QR{1}, QR{}}) == CLogic::False);
  STATIC_CHECK(lattice_square(Complex<QR>{QR{2}, QR{2}}) == CLogic::False);
}

TEST_CASE("Pruning showcase 3: halfspace contradiction on ℕ collapses to Ø",
          "[analysis][pruning][showcase][showcase03]") {
  constexpr auto gt_five = ℕ | (χ > fix(5_c));
  constexpr auto lt_three = ℕ | (χ < fix(3_c));

  // The contradiction folds value-first to the empty SetVal (kind Empty).
  constexpr auto empty_meet = gt_five & lt_three;
  STATIC_CHECK(empty_meet.kind == SetKind::Empty);

  SECTION("Reduction folds an intensional pair to a finite value") {
    // gt_five is an intensional predicate on ℕ (no materialised members); the
    // meet folds it value-first to the empty set --- a finite, decided value.
    // The extensionality tightening is now witnessed by the folded kind, not a
    // type-level IsExtensional tag.
    STATIC_CHECK(HasDecidableMembership<decltype(gt_five)>);
    STATIC_CHECK(!static_cast<bool>(empty_meet(4u)));  // empty: no inhabitant
  }
}

TEST_CASE("Pruning showcase 4: cardinality-1 halfspace meet = Singleton<4>",
          "[analysis][pruning][showcase][showcase04]") {
  // Canonical point-free grammar (the paper form): the canonical set ℕ, the
  // sole projection χ (the element scout, reads as x), and a compile-time bound
  // spelled fix(3_c).  No Set{} wrapper.
  constexpr auto gt_3 = ℕ | (χ > fix(3_c));
  constexpr auto lt_5 = ℕ | (χ < fix(5_c));

  // The punch line: the meet COLLAPSES to the point {4} at compile time.  The
  // collapse gates on the NNO's successor / predecessor --- an axiom of the
  // category, which the ℕ proxy witnesses --- so ℕ folds exactly as a machine
  // integer does; the point is a constexpr VALUE, not a distinct type.
  constexpr auto in_between = gt_3 & lt_5;
  STATIC_CHECK(in_between.kind == SetKind::Singleton);
  STATIC_CHECK(in_between.lo == 4);
  STATIC_CHECK(bool(in_between(4)) && !bool(in_between(3)) &&
               !bool(in_between(5)));

  SECTION("An intensional meet folds to a finite value") {
    // A halfspace on ℕ decides membership by comparison, so parents and result
    // are decidable.  The collapse folds the intensional (predicate-shaped)
    // parents to the point value {4} --- the extensionality gain, witnessed
    // value-first by the folded kind + point.
    STATIC_CHECK(HasDecidableMembership<decltype(gt_3)>);
    STATIC_CHECK(bool(in_between(4)));
  }
}

TEST_CASE("Pruning showcase 5: halfspace meet on ℝ collapses to Ø",
          "[analysis][pruning][showcase][showcase05]") {
  // FIXME(#399 slice 4-6): once ℝ becomes a carrier alias, switch to
  // @c element<𝔸<ℝ>>; for now ℝ is still the predicate-set type.
  constexpr auto gt_five = 𝔸<Real<double>>{} | (χ > bound<5.0>);
  constexpr auto lt_three = 𝔸<Real<double>>{} | (χ < bound<3.0>);

  // The contradiction folds value-first to the empty SetVal, on a continuous
  // carrier just as on ℕ.
  constexpr auto empty_meet = gt_five & lt_three;
  STATIC_CHECK(empty_meet.kind == SetKind::Empty);

  SECTION("Continuous carrier: parents not finite, reduced set is empty") {
    STATIC_CHECK(!static_cast<bool>(empty_meet(4.0)));  // empty: no inhabitant
  }
}
