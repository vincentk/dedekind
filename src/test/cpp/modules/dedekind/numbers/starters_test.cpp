#include <catch2/catch_test_macros.hpp>
#include <type_traits>

import dedekind.category;
import dedekind.numbers;
import dedekind.sets;

using namespace dedekind::category;
using namespace dedekind::numbers;
using namespace dedekind::sets;

TEST_CASE("Numbers: canonical starter symbols", "[numbers][starter]") {
  // Per #559 (option-A migration, sibling to #551's Ω<carrier> ambient
  // redesign), the canonical species symbols are universe @b values:
  // @c ℕ = @c Ω<Cardinality>, @c ℤ = @c Ω<SignedExtensionalCardinal<>>,
  // and so on.  Carriers are spelled directly (Cardinality, ℤ-as-alias,
  // ℚ-as-alias, ...) in template-type-parameter positions.  Each symbol's
  // STATIC_CHECK below witnesses the universe-over-carrier reading by
  // asserting (a) decltype(symbol) == UniversalSet<carrier> and (b)
  // decltype(symbol)::Domain == carrier.

  STATIC_CHECK(std::same_as<std::remove_cvref_t<decltype(ℕ)>,
                            UniversalSet<Cardinality>>);
  STATIC_CHECK(std::same_as<std::remove_cvref_t<decltype(Ω<Cardinality>)>,
                            UniversalSet<Cardinality>>);

  STATIC_CHECK(std::same_as<std::remove_cvref_t<decltype(ℤ)>,
                            UniversalSet<SignedExtensionalCardinal<>>>);
  STATIC_CHECK(std::same_as<typename std::remove_cvref_t<decltype(ℤ)>::Domain,
                            SignedExtensionalCardinal<>>);
  STATIC_CHECK(std::same_as<
               std::remove_cvref_t<decltype(Ω<SignedExtensionalCardinal<>>)>,
               UniversalSet<SignedExtensionalCardinal<>>>);

  STATIC_CHECK(std::same_as<std::remove_cvref_t<decltype(ℚ)>,
                            UniversalSet<Rational<default_integer>>>);
  STATIC_CHECK(std::same_as<typename std::remove_cvref_t<decltype(ℚ)>::Domain,
                            Rational<default_integer>>);
  STATIC_CHECK(
      std::same_as<std::remove_cvref_t<decltype(Ω<Rational<default_integer>>)>,
                   UniversalSet<Rational<default_integer>>>);

  // 𝔻 / D / DualSet starter aliases moved to dedekind.analysis:dual at
  // PR ; analogous STATIC_CHECKs live in
  // src/test/cpp/modules/dedekind/analysis/dual_test.cpp.
}

TEST_CASE("Numbers: starter universes construct from ambient values",
          "[numbers][starter][sets]") {
  // Per #559 option-A, the canonical scout spelling is:
  //   constexpr auto n = element<ℕ>;         // ℕ is itself the universe value
  //   constexpr auto naturals = Set{n};      // universal Set over ℕ-carrier
  //   static_assert(naturals.contains(7u));  // value-level membership query
  //
  // (Pre-#559 the spelling was @c element<Ω<ℕ>> with ℕ a carrier alias;
  //  the @c % @c N binding step had already gone in #551.)

  constexpr auto n = element<ℕ>;
  constexpr auto naturals = Set{n};
  static_assert(naturals(7u) == Ternary::True);
  static_assert(naturals(0u) == Ternary::True);
  // Direct ambient-call route: ℕ.contains(value) (or equivalently
  // Ω<Cardinality>.contains(value), since ℕ = Ω<Cardinality>) returns
  // L::True for every Cardinality value (the universal-set semantics).
  static_assert(Ω<Cardinality>.contains(7u));
  static_assert(Ω<Cardinality>.contains(0u));

  constexpr auto z = element<ℤ>;
  constexpr auto integers = Set{z};
  static_assert(integers(-7) == Ternary::True);

  // 𝔻 starter-universe construction moved to
  // src/test/cpp/modules/dedekind/analysis/dual_test.cpp at PR #513
  // (Dual<F> relocated from :numbers to :analysis).
}

TEST_CASE("Numbers: starter universes satisfy lattice identities",
          "[numbers][starter][algebra]") {
  {
    constexpr auto n = element<ℕ>;
    const auto U = Set{n};
    const auto O = !U;
    CHECK((U | O)(7u) == Ternary::True);
    CHECK((U & O)(7u) == Ternary::False);
    CHECK((U | O)(0u) == Ternary::True);
    CHECK((U & O)(0u) == Ternary::False);
  }

  {
    constexpr auto z = element<ℤ>;
    const auto U = Set{z};
    const auto O = !U;
    CHECK((U | O)(4) == Ternary::True);
    CHECK((U & O)(4) == Ternary::False);
  }

  // 𝔻 / D / Dual lattice-identity check moved to
  // src/test/cpp/modules/dedekind/analysis/dual_test.cpp at PR #513
  // (Dual<F> relocated from :numbers to :analysis).
}

TEST_CASE(
    "Numbers: ℚ as the textbook quotient (ℤ × ℤ_≠0) / cross-multiplication "
    "(#567 exhibit)",
    "[numbers][starter][quotient][exhibit]") {
  // The textbook construction of ℚ in code, observable as a
  // static_assert chain.  The structural claim is the *type identity*:
  // the quotient's Domain is the carrier we already use for ℚ.
  //
  //   ℚ = (ℤ × ℤ_≠0) / ~,  where (a, b) ~ (c, d) iff a·d = b·c
  //
  // The carrier inhabiting this construction is Rational<I>; equality
  // and canonicalisation (Rational::simplify normalising via gcd;
  // operator== cross-multiplying) provide the equivalence-class
  // semantics the relation specifies.

  // ℤ × ℤ_≠0 — pairs of integers with nonzero second component.
  using I = default_integer;  // SignedExtensionalCardinal<>
  constexpr auto z = element<ℤ>;
  constexpr auto numerators = Set{z};
  // Use an explicit lambda for the nonzero predicate: BoundScout doesn't
  // expose a scalar `!=` lift on the relational surface, and the C++20
  // rewritten-comparison semantics around `!=` would otherwise trip on
  // the universe-value side.
  constexpr auto denominators =
      Set{z | [](const I& v) { return !(v == I{0}); }};
  constexpr auto pairs = cartesian_product(numerators, denominators);

  // Cross-multiplication equivalence relation as a typed tag (so the
  // quotient_carrier trait can specialise on it).
  constexpr auto cross_mult = CrossMultEquiv<I>{};

  // The textbook ℚ-from-construction.
  constexpr auto Q_constructed = quotient(pairs, cross_mult);

  // The structural claim: the quotient's Domain IS Rational<default_integer>
  // — the carrier we already use for ℚ.  Same artefact, structurally
  // exhibited.
  STATIC_CHECK(std::same_as<
               typename std::remove_cvref_t<decltype(Q_constructed)>::Domain,
               Rational<default_integer>>);

  // And it agrees with the universe-value spelling of ℚ (post-#566):
  STATIC_CHECK(std::same_as<
               typename std::remove_cvref_t<decltype(Q_constructed)>::Domain,
               typename std::remove_cvref_t<decltype(ℚ)>::Domain>);
}
