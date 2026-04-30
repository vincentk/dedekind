#include <catch2/catch_test_macros.hpp>

import dedekind.category;
import dedekind.numbers;
import dedekind.sets;

using namespace dedekind::category;
using namespace dedekind::numbers;
using namespace dedekind::sets;

TEST_CASE("Numbers: canonical starter symbols", "[numbers][starter]") {
  // Carrier-vs-predicate-set surface (post-#402; updates #401's reading).
  //   • ℕ is the carrier type itself: ℕ = Cardinality (variant ℕ-proxy
  //     = std::variant<ExtensionalCardinal<>, ℵ_0>; saturating to ℵ_0
  //     on overflow).  Pre-#402 ℕ was the machine carrier `unsigned int`;
  //     callers wanting that bounded reading now spell `unsigned int`
  //     directly.
  //   • N is the value-level universal Natural predicate-set, of type
  //     NaturalNumbersOf<> (= Ω-flavoured classifier).
  //   • The relationship is NaturalNumbersOf<>::Domain = ℕ — the
  //     predicate-set's underlying element type IS the carrier.
  STATIC_CHECK(std::same_as<ℕ, Cardinality>);
  STATIC_CHECK(std::same_as<decltype(N), const NaturalNumbersOf<>>);
  STATIC_CHECK(std::same_as<typename NaturalNumbersOf<>::Domain, ℕ>);

  STATIC_CHECK(std::same_as<ℤ, IntegerSet>);
  STATIC_CHECK(std::same_as<decltype(Z), const ℤ>);

  // ℚ is now the carrier type (the field of rationals); the value-level
  // constant Q is the predicate-set instance (RationalsOf<> / RationalSet).
  STATIC_CHECK(std::same_as<ℚ, Rational<default_integer>>);
  STATIC_CHECK(std::same_as<decltype(Q), const RationalsOf<>>);

  STATIC_CHECK(std::same_as<ℝ, RealSet>);
  STATIC_CHECK(std::same_as<decltype(R), const ℝ>);

  STATIC_CHECK(std::same_as<ℂ, ComplexSet>);
  STATIC_CHECK(std::same_as<decltype(C), const ℂ>);

  // 𝔻 / D / DualSet starter aliases moved to dedekind.analysis:dual at
  // PR ; analogous STATIC_CHECKs live in
  // src/test/cpp/modules/dedekind/analysis/dual_test.cpp.
}

TEST_CASE("Numbers: starter universes construct from ambient values",
          "[numbers][starter][sets]") {
  constexpr auto n = var<ℕ>;
  constexpr auto naturals = Set{n % N};
  // The Set DSL routes calls through Domain = Cardinality (post-#402);
  // unsigned literals lift implicitly into the variant's finite
  // alternative (@c ExtensionalCardinal<>{u}), so callsites passing
  // unsigned values still resolve naturally.  Every Cardinality value
  // is in ℕ by construction (ℕ-as-carrier).
  static_assert(naturals(7u) == Ternary::True);
  static_assert(naturals(0u) == Ternary::True);
  // Classifier reading (ℕ ⊂ ℤ via non-negativity) is preserved on direct
  // calls to the predicate-set, where the int overload is reachable.
  // The int overload returns ClassicalLogic::Ω (= bool) directly, not
  // lifted through TernaryLogic.
  static_assert(N(7) == true, "Predicate-set classifies 7 as natural.");
  static_assert(N(-7) == false, "Predicate-set classifies -7 as not natural.");

  constexpr auto z = var<ℤ>;
  constexpr auto integers = Set{z % Z};
  static_assert(integers(-7) == Ternary::True);

  constexpr auto q = var<ℚ>;
  constexpr auto rationals = Set{q % Q};
  static_assert(rationals(Rational<default_integer>{
                    default_integer{1}, default_integer{2}}) == Ternary::True);

  constexpr auto r = var<ℝ>;
  constexpr auto reals = Set{r % R};
  static_assert(reals(Real<double>{1.25}) == Ternary::True);

  constexpr auto c = var<ℂ>;
  constexpr auto complexes = Set{c % C};
  static_assert(complexes(Complex<double>{1.0, 2.0}) == Ternary::True);

  // 𝔻 starter-universe construction moved to
  // src/test/cpp/modules/dedekind/analysis/dual_test.cpp at PR #513
  // (Dual<F> relocated from :numbers to :analysis).
}

TEST_CASE("Numbers: starter universes satisfy lattice identities",
          "[numbers][starter][algebra]") {
  {
    constexpr auto n = var<ℕ>;
    const auto U = Set{n % N};
    const auto O = !U;
    CHECK((U | O)(7u) == Ternary::True);
    CHECK((U & O)(7u) == Ternary::False);
    CHECK((U | O)(0u) == Ternary::True);
    CHECK((U & O)(0u) == Ternary::False);
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
    CHECK((U | O)(Rational<default_integer>{
              default_integer{1}, default_integer{3}}) == Ternary::True);
    CHECK((U & O)(Rational<default_integer>{
              default_integer{1}, default_integer{3}}) == Ternary::False);
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

  // 𝔻 / D / Dual lattice-identity check moved to
  // src/test/cpp/modules/dedekind/analysis/dual_test.cpp at PR #513
  // (Dual<F> relocated from :numbers to :analysis).
}