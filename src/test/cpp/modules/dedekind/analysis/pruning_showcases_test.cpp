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
constexpr auto R2 = R * R;
using R2Point = typename decltype(R2)::Domain;
// FIXME(#399 slice 4-6): see showcase_01 source for the same comment.
constexpr auto xy = element<Ω<R2Point>>;

constexpr auto diagonal =
    Set{xy % R2 | [](R2Point p) { return p.first == p.second; }};
constexpr auto strip = Set{
    xy % R2 | [](R2Point p) { return (p.first > 5.0) && (p.second < 3.0); }};

}  // namespace

TEST_CASE("Pruning showcase 1: diagonal × strip on ℝ² is empty",
          "[analysis][pruning][showcase][showcase01]") {
  constexpr auto empty_diagonal_cut = diagonal & strip;
  using R2Logic = typename decltype(empty_diagonal_cut)::logic_species;

  // On the diagonal x = y, the strip x>5 ∧ y<3 is contradictory.
  STATIC_CHECK(empty_diagonal_cut(R2Point{6.0, 6.0}) == R2Logic::False);
  STATIC_CHECK(empty_diagonal_cut(R2Point{2.0, 2.0}) == R2Logic::False);
}

namespace {

// Shared with showcase 2 (ℂ lattice × square singleton).
// Post-#559: ℂ is the universe value Ω<Complex<machine_real_scalar>,
// ClassicalLogic, ℶ_1>, so the canonical scout spelling is element<ℂ>.
constexpr auto c = element<ℂ>;

constexpr bool is_integral_coordinate(double x) {
  const int xi = static_cast<int>(x);
  return static_cast<double>(xi) == x;
}

constexpr auto natural_lattice_in_c = Set{c | [](const Complex<double>& z) {
  return is_integral_coordinate(z.real()) && is_integral_coordinate(z.imag()) &&
         (z.real() >= 0.0) && (z.real() <= 3.0) && (z.imag() >= 0.0) &&
         (z.imag() <= 3.0);
}};
constexpr auto square_c1_c2 = Set{c | [](const Complex<double>& z) {
  return (z.real() >= 0.5) && (z.real() <= 1.5) && (z.imag() >= 0.5) &&
         (z.imag() <= 1.5);
}};

}  // namespace

TEST_CASE("Pruning showcase 2: ℕ² lattice × [½,1½]² in ℂ = {1+i}",
          "[analysis][pruning][showcase][showcase02]") {
  constexpr auto lattice_square = natural_lattice_in_c & square_c1_c2;
  using CLogic = typename decltype(lattice_square)::logic_species;

  STATIC_CHECK(lattice_square(Complex<double>{1.0, 1.0}) == CLogic::True);
  STATIC_CHECK(lattice_square(Complex<double>{0.0, 1.0}) == CLogic::False);
  STATIC_CHECK(lattice_square(Complex<double>{1.0, 0.0}) == CLogic::False);
  STATIC_CHECK(lattice_square(Complex<double>{2.0, 2.0}) == CLogic::False);
}

TEST_CASE("Pruning showcase 3: halfspace contradiction on ℕ collapses to Ø",
          "[analysis][pruning][showcase][showcase03]") {
  constexpr auto n = element<ℕ>;
  constexpr auto gt_five = Set{n | (n > bound<5>)};
  constexpr auto lt_three = Set{n | (n < bound<3>)};

  constexpr Ø<Cardinality> empty_meet = gt_five & lt_three;
  STATIC_CHECK(empty_meet == Ø{});

  SECTION("Computability tiers tighten at the reduction boundary") {
    STATIC_CHECK_FALSE(HasDecidableMembership<decltype(gt_five)>);
    STATIC_CHECK_FALSE(IsExtensional<decltype(gt_five)>);
    STATIC_CHECK_FALSE(IsExtensional<decltype(gt_five)>);

    STATIC_CHECK(HasDecidableMembership<decltype(empty_meet)>);
    STATIC_CHECK(IsExtensional<decltype(empty_meet)>);
    STATIC_CHECK(IsExtensional<decltype(empty_meet)>);
  }
}

TEST_CASE("Pruning showcase 4: cardinality-1 halfspace meet = Singleton<4>",
          "[analysis][pruning][showcase][showcase04]") {
  constexpr auto n = element<ℕ>;
  constexpr auto gt_3 = Set{n | n > bound<3>};
  constexpr auto lt_5 = Set{n | n < bound<5>};

  constexpr Singleton<4> in_between = gt_3 & lt_5;
  STATIC_CHECK(in_between == Singleton<4>{});

  SECTION("Elements now live at the type level") {
    STATIC_CHECK_FALSE(IsExtensional<decltype(gt_3)>);
    STATIC_CHECK(IsExtensional<decltype(in_between)>);
  }
}

TEST_CASE("Pruning showcase 5: halfspace meet on ℝ collapses to Ø",
          "[analysis][pruning][showcase][showcase05]") {
  // FIXME(#399 slice 4-6): once ℝ becomes a carrier alias, switch to
  // @c element<Ω<ℝ>>; for now ℝ is still the predicate-set type.
  constexpr auto x = element<Ω<Real<double>>>;
  constexpr auto gt_five = Set{x | (x > bound<5.0>)};
  constexpr auto lt_three = Set{x | (x < bound<3.0>)};

  constexpr Ø<Real<double>> empty_meet = gt_five & lt_three;
  STATIC_CHECK(empty_meet == Ø{});

  SECTION("Continuous carrier: parents not finite, reduced Ø is finite") {
    STATIC_CHECK_FALSE(IsExtensional<decltype(gt_five)>);
    STATIC_CHECK(IsExtensional<decltype(empty_meet)>);
  }
}

TEST_CASE("Pruning showcase 6: (-21, 21] on ℤ has size 42",
          "[analysis][pruning][showcase][showcase06]") {
  constexpr auto n = element<ℤ>;
  constexpr auto above = Set{n | (n > bound<-21>)};
  constexpr auto at_most = Set{n | (n <= bound<21>)};

  constexpr auto iv = above & at_most;
  using Iv = std::decay_t<decltype(iv)>;

  STATIC_CHECK(Iv::lower_pivot == -21);
  STATIC_CHECK(Iv::upper_pivot == 21);
  STATIC_CHECK(Iv::lower_strictness == Strictness::Strict);
  STATIC_CHECK(Iv::upper_strictness == Strictness::NonStrict);
  STATIC_CHECK(iv.size() == 42u);

  SECTION("Finite and decidable but not compile-time-enumerable") {
    STATIC_CHECK(HasDecidableMembership<decltype(iv)>);
    STATIC_CHECK(IsExtensional<decltype(iv)>);
    STATIC_CHECK_FALSE(IsExtensional<decltype(iv)>);
  }
}

TEST_CASE("Pruning showcase 7: ℤ lattice ∩ real interval (-21.0, 21.0]",
          "[analysis][pruning][showcase][showcase07]") {
  // Machine-int carrier explicitly: on int, the standard int↔double
  // promotion is what lets the real-valued bound compare with the
  // integer variable.  The exact ℤ carrier
  // (@c SignedExtensionalCardinal<>) intentionally does not silently
  // narrow to double; real-bound support against the exact carrier
  // is deferred to a SEC<>↔real comparison arrow (follow-up to #399
  // slice 3 / #551).  Showcase 7 keeps the real-bound demonstration
  // distinct from showcase 6 (which uses integer bounds).
  //
  // @c IntsOnInt is the int-Domain universal predicate, defined
  // The pre-#551 surface used a locally-defined IntsOnInt predicate-set
  // because the canonical @c IntegersOf<> carried @c Domain @c =
  // @c SEC<>; under #551 the scout itself knows its ambient (Ω<int>),
  // so no local predicate-set is needed.
  constexpr auto n = element<Ω<int>>;
  constexpr auto above = Set{n | (n > bound<-21.0>)};
  constexpr auto at_most = Set{n | (n <= bound<21.0>)};

  constexpr auto lattice_cut = above & at_most;
  using Iv = std::decay_t<decltype(lattice_cut)>;

  STATIC_CHECK(Iv::lower_pivot == -21.0);
  STATIC_CHECK(Iv::upper_pivot == 21.0);
  STATIC_CHECK(lattice_cut.size() == 42u);
  STATIC_CHECK(HasDecidableMembership<decltype(lattice_cut)>);
  STATIC_CHECK(IsExtensional<decltype(lattice_cut)>);
}

TEST_CASE("Pruning showcase 8: 2D rectangle via IntervalProduct",
          "[analysis][pruning][showcase][showcase08]") {
  constexpr auto n = element<ℤ>;
  constexpr auto I_wide = Set{n | (n > bound<-21>)} & Set{n | (n <= bound<21>)};
  constexpr auto I_tall = Set{n | (n >= bound<0>)} & Set{n | (n <= bound<10>)};

  constexpr auto box = I_wide * I_tall;

  STATIC_CHECK(box.size() == 42u * 11u);
  STATIC_CHECK(box.size() == 462u);
  STATIC_CHECK(HasDecidableMembership<decltype(box)>);
  STATIC_CHECK(IsExtensional<decltype(box)>);

  SECTION("2D membership at a specific point") {
    using Logic = typename decltype(box)::logic_species;
    STATIC_CHECK(box(std::pair{0, 5}) == Logic::True);
  }
}
