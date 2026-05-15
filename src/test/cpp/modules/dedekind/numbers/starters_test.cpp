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
  // @c ℕ = @c Ω<Cardinality>, @c ℤ = @c Ω<SignedCardinality> (post-#670:
  // saturating-discipline alignment with ℕ), and so on.  Carriers are
  // spelled directly (Cardinality, SignedCardinality, Rational<...>, ...)
  // in template-type-parameter positions.  Each symbol's STATIC_CHECK
  // below witnesses the universe-over-carrier reading by asserting
  // (a) decltype(symbol) == UniversalSet<carrier> and (b)
  // decltype(symbol)::Domain == carrier.

  STATIC_CHECK(std::same_as<std::remove_cvref_t<decltype(ℕ)>,
                            UniversalSet<Cardinality>>);
  STATIC_CHECK(std::same_as<std::remove_cvref_t<decltype(Ω<Cardinality>)>,
                            UniversalSet<Cardinality>>);

  // Post-#670: ℤ uses the saturating SignedCardinality variant
  // (mirroring ℕ = Ω<Cardinality>), not the cyclic finite fragment.
  STATIC_CHECK(std::same_as<std::remove_cvref_t<decltype(ℤ)>,
                            UniversalSet<SignedCardinality>>);
  STATIC_CHECK(std::same_as<typename std::remove_cvref_t<decltype(ℤ)>::Domain,
                            SignedCardinality>);
  STATIC_CHECK(std::same_as<std::remove_cvref_t<decltype(Ω<SignedCardinality>)>,
                            UniversalSet<SignedCardinality>>);

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
  static_assert(naturals(7u));
  static_assert(naturals(0u));
  // Direct ambient-call route: ℕ.contains(value) (or equivalently
  // Ω<Cardinality>.contains(value), since ℕ = Ω<Cardinality>) returns
  // L::True for every Cardinality value (the universal-set semantics).
  static_assert(Ω<Cardinality>.contains(7u));
  static_assert(Ω<Cardinality>.contains(0u));

  constexpr auto z = element<ℤ>;
  constexpr auto integers = Set{z};
  static_assert(integers(-7));

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
    CHECK((U | O)(7u));
    CHECK_FALSE((U & O)(7u));
    CHECK((U | O)(0u));
    CHECK_FALSE((U & O)(0u));
  }

  {
    constexpr auto z = element<ℤ>;
    const auto U = Set{z};
    const auto O = !U;
    CHECK((U | O)(4));
    CHECK_FALSE((U & O)(4));
  }

  // 𝔻 / D / Dual lattice-identity check moved to
  // src/test/cpp/modules/dedekind/analysis/dual_test.cpp at PR #513
  // (Dual<F> relocated from :numbers to :analysis).
}

TEST_CASE(
    "Numbers: ℚ as the textbook quotient (ℤ × ℤ_≠0) / cross-multiplication "
    "(#567 exhibit; post-#673 anchored on canonical ℤ)",
    "[numbers][starter][quotient][exhibit]") {
  // ℚ = (ℤ × ℤ_≠0) / ~,  where (a, b) ~ (c, d) iff a·d = b·c.
  //
  // Post-#673, @c default_integer @c == @c SignedCardinality (the
  // canonical ℤ carrier, mirroring @c ℕ's @c Cardinality).  The
  // exhibit therefore ties directly to the canonical @c ℤ alias,
  // dropping the cyclic-vs-saturating workaround that had briefly
  // anchored the prior version on @c Ω<SignedExtensionalCardinal<>>.

  using I = default_integer;  // SignedCardinality (canonical ℤ carrier)
  constexpr auto z = element<ℤ>;
  constexpr auto numerators = Set{z};
  // Nonzero predicate via the heterogeneous @c SignedCardinality @c ==
  // @c std::integral overload (cardinality.cppm:1326) --- avoids
  // constructing @c I{0} on the variant carrier.
  constexpr auto denominators = Set{z | [](const I& v) { return !(v == 0); }};
  constexpr auto pairs = cartesian_product(numerators, denominators);

  // Cross-multiplication equivalence relation (typed tag so the
  // @c quotient_carrier trait can specialise on it).
  constexpr auto cross_mult = CrossMultEquiv<I>{};

  // The textbook ℚ-from-construction.
  constexpr auto Q_constructed = quotient(pairs, cross_mult);

  // The structural claim: the quotient's @c Domain @b IS
  // @c Rational<default_integer> --- same artefact, structurally
  // exhibited.
  STATIC_CHECK(std::same_as<
               typename std::remove_cvref_t<decltype(Q_constructed)>::Domain,
               Rational<default_integer>>);

  // And it agrees with the universe-value spelling of @c ℚ (post-#566).
  STATIC_CHECK(std::same_as<
               typename std::remove_cvref_t<decltype(Q_constructed)>::Domain,
               typename std::remove_cvref_t<decltype(ℚ)>::Domain>);
}
