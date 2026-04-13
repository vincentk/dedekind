#include <catch2/catch_test_macros.hpp>

import dedekind.category;
import dedekind.numbers;
import dedekind.sets;

using namespace dedekind::category;
using namespace dedekind::numbers;
using namespace dedekind::sets;

TEST_CASE("Numbers: canonical starter symbols", "[numbers][starter]") {
  STATIC_CHECK(std::same_as<ℕ, NaturalNumbers>);
  STATIC_CHECK(std::same_as<decltype(N), const ℕ>);

  STATIC_CHECK(std::same_as<ℤ, IntegerSet>);
  STATIC_CHECK(std::same_as<decltype(Z), const ℤ>);

  STATIC_CHECK(std::same_as<ℚ, RationalSet>);
  STATIC_CHECK(std::same_as<decltype(Q), const ℚ>);

  STATIC_CHECK(std::same_as<ℝ, RealSet>);
  STATIC_CHECK(std::same_as<decltype(R), const ℝ>);

  STATIC_CHECK(std::same_as<ℂ, ComplexSet>);
  STATIC_CHECK(std::same_as<decltype(C), const ℂ>);

  STATIC_CHECK(std::same_as<𝔻, DualSet>);
  STATIC_CHECK(std::same_as<decltype(D), const 𝔻>);
}

TEST_CASE("Numbers: starter universes construct from ambient values",
          "[numbers][starter][sets]") {
  constexpr auto n = var<ℕ>;
  constexpr auto naturals = Set{n % N};
  static_assert(naturals(7) == Ternary::True);
  static_assert(naturals(-7) == Ternary::False);
  static_assert(naturals(7u) == Ternary::True);

  constexpr auto z = var<ℤ>;
  constexpr auto integers = Set{z % Z};
  static_assert(integers(-7) == Ternary::True);

  constexpr auto q = var<ℚ>;
  constexpr auto rationals = Set{q % Q};
  static_assert(rationals(Rational<int>{1, 2}) == Ternary::True);

  constexpr auto r = var<ℝ>;
  constexpr auto reals = Set{r % R};
  static_assert(reals(Real<double>{1.25}) == Ternary::True);

  constexpr auto c = var<ℂ>;
  constexpr auto complexes = Set{c % C};
  static_assert(complexes(Complex<double>{1.0, 2.0}) == Ternary::True);

  constexpr auto d = var<𝔻>;
  constexpr auto duals = Set{d % D};
  static_assert(duals(Dual<double>{1.0, 1.0}) == Ternary::True);
}

TEST_CASE("Numbers: starter universes satisfy lattice identities",
          "[numbers][starter][algebra]") {
  {
    constexpr auto n = var<ℕ>;
    const auto U = Set{n % N};
    const auto O = !U;
    CHECK((U | O)(7) == Ternary::True);
    CHECK((U & O)(7) == Ternary::False);
    CHECK((U | O)(-3) == Ternary::True);
    CHECK((U & O)(-3) == Ternary::False);
  }

  {
    constexpr auto z = var<ℤ>;
    const auto U = Set{z % Z};
    const auto O = !U;
    CHECK((U | O)(4) == Ternary::True);
    CHECK((U & O)(4) == Ternary::False);
  }

  {
    constexpr auto q = var<ℚ>;
    const auto U = Set{q % Q};
    const auto O = !U;
    CHECK((U | O)(Rational<int>{1, 3}) == Ternary::True);
    CHECK((U & O)(Rational<int>{1, 3}) == Ternary::False);
  }

  {
    constexpr auto r = var<ℝ>;
    const auto U = Set{r % R};
    const auto O = !U;
    CHECK((U | O)(Real<double>{2.0}) == Ternary::True);
    CHECK((U & O)(Real<double>{2.0}) == Ternary::False);
  }

  {
    constexpr auto c = var<ℂ>;
    const auto U = Set{c % C};
    const auto O = !U;
    CHECK((U | O)(Complex<double>{1.0, -1.0}) == Ternary::True);
    CHECK((U & O)(Complex<double>{1.0, -1.0}) == Ternary::False);
  }

  {
    constexpr auto d = var<𝔻>;
    const auto U = Set{d % D};
    const auto O = !U;
    CHECK((U | O)(Dual<double>{3.0, 1.0}) == Ternary::True);
    CHECK((U & O)(Dual<double>{3.0, 1.0}) == Ternary::False);
  }
}