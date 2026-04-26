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
constexpr auto xy = var<decltype(R2)>;
using R2Point = typename decltype(R2)::Domain;

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
constexpr auto c = var<ℂ>;

constexpr bool is_integral_coordinate(double x) {
  const int xi = static_cast<int>(x);
  return static_cast<double>(xi) == x;
}

constexpr auto natural_lattice_in_c = Set{c % C | [](const Complex<double>& z) {
  return is_integral_coordinate(z.real()) && is_integral_coordinate(z.imag()) &&
         (z.real() >= 0.0) && (z.real() <= 3.0) && (z.imag() >= 0.0) &&
         (z.imag() <= 3.0);
}};
constexpr auto square_c1_c2 = Set{c % C | [](const Complex<double>& z) {
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
  constexpr auto n = var<ℕ>;
  constexpr auto gt_five = Set{n % N | (n > bound<5>)};
  constexpr auto lt_three = Set{n % N | (n < bound<3>)};

  constexpr Ø<ℕ> empty_meet = gt_five & lt_three;
  STATIC_CHECK(empty_meet == Ø{});

  SECTION("Computability tiers tighten at the reduction boundary") {
    STATIC_CHECK_FALSE(HasDecidableMembership<decltype(gt_five)>);
    STATIC_CHECK_FALSE(IsFiniteSet<decltype(gt_five)>);
    STATIC_CHECK_FALSE(IsCompileTimeEnumerable<decltype(gt_five)>);

    STATIC_CHECK(HasDecidableMembership<decltype(empty_meet)>);
    STATIC_CHECK(IsFiniteSet<decltype(empty_meet)>);
    STATIC_CHECK(IsCompileTimeEnumerable<decltype(empty_meet)>);
  }
}

TEST_CASE("Pruning showcase 4: cardinality-1 halfspace meet = Singleton<4>",
          "[analysis][pruning][showcase][showcase04]") {
  constexpr auto n = var<ℕ>;
  constexpr auto gt_three = Set{n % N | (n > bound<3>)};
  constexpr auto lt_five = Set{n % N | (n < bound<5>)};

  constexpr Singleton<4> in_between = gt_three & lt_five;
  STATIC_CHECK(in_between == Singleton<4>{});

  SECTION("Elements now live at the type level") {
    STATIC_CHECK_FALSE(IsCompileTimeEnumerable<decltype(gt_three)>);
    STATIC_CHECK(IsCompileTimeEnumerable<decltype(in_between)>);
  }
}

TEST_CASE("Pruning showcase 5: halfspace meet on ℝ collapses to Ø",
          "[analysis][pruning][showcase][showcase05]") {
  constexpr auto x = var<ℝ>;
  constexpr auto gt_five = Set{x % R | (x > bound<5.0>)};
  constexpr auto lt_three = Set{x % R | (x < bound<3.0>)};

  constexpr Ø<Real<double>> empty_meet = gt_five & lt_three;
  STATIC_CHECK(empty_meet == Ø{});

  SECTION("Continuous carrier: parents not finite, reduced Ø is finite") {
    STATIC_CHECK_FALSE(IsFiniteSet<decltype(gt_five)>);
    STATIC_CHECK(IsFiniteSet<decltype(empty_meet)>);
  }
}

TEST_CASE("Pruning showcase 6: (-21, 21] on ℤ has size 42",
          "[analysis][pruning][showcase][showcase06]") {
  constexpr auto n = var<ℤ>;
  constexpr auto above = Set{n % Z | (n > bound<-21>)};
  constexpr auto at_most = Set{n % Z | (n <= bound<21>)};

  constexpr auto iv = above & at_most;
  using Iv = std::decay_t<decltype(iv)>;

  STATIC_CHECK(Iv::lower_pivot == -21);
  STATIC_CHECK(Iv::upper_pivot == 21);
  STATIC_CHECK(Iv::lower_strictness == Strictness::Strict);
  STATIC_CHECK(Iv::upper_strictness == Strictness::NonStrict);
  STATIC_CHECK(iv.size() == 42u);

  SECTION("Finite and decidable but not compile-time-enumerable") {
    STATIC_CHECK(HasDecidableMembership<decltype(iv)>);
    STATIC_CHECK(IsFiniteSet<decltype(iv)>);
    STATIC_CHECK_FALSE(IsCompileTimeEnumerable<decltype(iv)>);
  }
}

TEST_CASE("Pruning showcase 7: ℤ lattice ∩ real interval (-21.0, 21.0]",
          "[analysis][pruning][showcase][showcase07]") {
  constexpr auto n = var<ℤ>;
  constexpr auto above = Set{n % Z | (n > bound<-21.0>)};
  constexpr auto at_most = Set{n % Z | (n <= bound<21.0>)};

  constexpr auto lattice_cut = above & at_most;
  using Iv = std::decay_t<decltype(lattice_cut)>;

  STATIC_CHECK(Iv::lower_pivot == -21.0);
  STATIC_CHECK(Iv::upper_pivot == 21.0);
  STATIC_CHECK(lattice_cut.size() == 42u);
  STATIC_CHECK(HasDecidableMembership<decltype(lattice_cut)>);
  STATIC_CHECK(IsFiniteSet<decltype(lattice_cut)>);
}

TEST_CASE("Pruning showcase 8: 2D rectangle via IntervalProduct",
          "[analysis][pruning][showcase][showcase08]") {
  constexpr auto n = var<ℤ>;
  constexpr auto I_wide =
      Set{n % Z | (n > bound<-21>)} & Set{n % Z | (n <= bound<21>)};
  constexpr auto I_tall =
      Set{n % Z | (n >= bound<0>)} & Set{n % Z | (n <= bound<10>)};

  constexpr auto box = I_wide * I_tall;

  STATIC_CHECK(box.size() == 42u * 11u);
  STATIC_CHECK(box.size() == 462u);
  STATIC_CHECK(HasDecidableMembership<decltype(box)>);
  STATIC_CHECK(IsFiniteSet<decltype(box)>);

  SECTION("2D membership at a specific point") {
    using Logic = typename decltype(box)::logic_species;
    STATIC_CHECK(box(std::pair{0, 5}) == Logic::True);
  }
}
