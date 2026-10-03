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
// Diagonal {x = y} and strip {x > 5 ∧ y < 3}, point-free over R2.
constexpr auto diag = R2 | (π1 == π2);
constexpr auto strip = R2 | (π1 > bound<5.0> && π2 < bound<3.0>);

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

// Shared with showcase 2: the {0,…,3}² lattice × [½,1½]² square in ℝ_d × ℝ_d,
// the same spelling as the IR fixture.  Integrality has no point-free
// spelling, so the lattice keeps one named predicate.
constexpr auto on_small_natural_grid = [](const R2Point& p) {
  const double x = p.first.value();
  const double y = p.second.value();
  // Bounds first: the int casts below are only defined inside [0, 3].
  return x >= 0.0 && x <= 3.0 && y >= 0.0 && y <= 3.0 &&
         static_cast<double>(static_cast<int>(x)) == x &&
         static_cast<double>(static_cast<int>(y)) == y;
};
constexpr auto natural_lattice = Comprehension{R2, on_small_natural_grid};
constexpr auto unit_square = R2 | (π1 >= bound<0.5> && π1 <= bound<1.5> &&
                                   π2 >= bound<0.5> && π2 <= bound<1.5>);

}  // namespace

TEST_CASE("Pruning showcase 2: {0..3}² lattice × [½,1½]² in ℝ² = {(1,1)}",
          "[analysis][pruning][showcase][showcase02]") {
  constexpr auto lattice_square = natural_lattice & unit_square;
  using L = typename decltype(lattice_square)::logic_species;

  STATIC_CHECK(lattice_square(R2Point{1.0, 1.0}) == L::True);
  STATIC_CHECK(lattice_square(R2Point{0.0, 1.0}) == L::False);
  STATIC_CHECK(lattice_square(R2Point{1.0, 0.0}) == L::False);
  STATIC_CHECK(lattice_square(R2Point{2.0, 2.0}) == L::False);
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
  constexpr auto gt_five = ℝ_d | (χ > bound<5.0>);
  constexpr auto lt_three = ℝ_d | (χ < bound<3.0>);

  // The contradiction folds value-first to the empty SetVal, on a continuous
  // carrier just as on ℕ.
  constexpr auto empty_meet = gt_five & lt_three;
  STATIC_CHECK(empty_meet.kind == SetKind::Empty);

  SECTION("Continuous carrier: parents not finite, reduced set is empty") {
    STATIC_CHECK(empty_meet(typename decltype(ℝ_d)::Domain{4.0}) ==
                 decltype(empty_meet)::logic_species::False);  // no inhabitant
  }
}
