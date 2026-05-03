#include <catch2/catch_test_macros.hpp>
#include <limits>
import dedekind.category;
import dedekind.sets;

using namespace dedekind::category;
using namespace dedekind::sets;

TEST_CASE("Level 1 Final Proof: The Mereology Highway",
          "[ontology][mereology][highway]") {
  SECTION("2. Initial Object Proof") {
    Ø<int> empty;
    static_assert(dedekind::category::IsSet<decltype(ambient_set<int>(empty))>);
  }

  SECTION("3. The Extreme Bounds (0 and 1)") {
    Ø<int> empty;
    UniversalSet<int> universe;

    // Verify these are valid sets over the active logic species.
    static_assert(dedekind::category::IsSet<decltype(ambient_set<int>(empty))>);
    static_assert(
        dedekind::category::IsSet<decltype(ambient_set<int>(universe))>);

    // Verify the Logical Truth across species
    REQUIRE(empty(42) == false);
    REQUIRE(universe(42) == true);

    // Verify the Magnitude (The Ruler)
    // Note: UniversalSet<int> is Countable (ℵ_0)
    REQUIRE(empty.cardinality() == Finite{});
  }

  SECTION("4. The Logic Swapping (Topos-Awareness)") {
    // Universal Set over the Ternary Topos (Kleene Logic)
    UniversalSet<int, TernaryLogic> k_universe;

    REQUIRE(k_universe(42) == Ternary::True);
  }
}

TEST_CASE("Boundaries: The Algebra of Extremality", "[sets][boundaries]") {
  // Use SignedExtensionalCardinal<> directly as the carrier for the
  // boundary identities (post-#559 ℤ slice migration; the prior local
  // alias `using ℤ = int` is no longer referenced after the type-site
  // sweep replaced ℤ with the carrier name in the body).

  // The Identities: Ø and Ω as the "North and South Poles"
  constexpr Ø<SignedExtensionalCardinal<>> null;
  constexpr UniversalSet<SignedExtensionalCardinal<>> universe;

  SECTION("Aha! 1: The Law of Absorption (Annihilation)") {
    /**
     * In a Union, the Universe is the Annihilator: Ω | S = Ω.
     * In an Intersection, the Void is the Annihilator: ∅ & S = ∅.
     */
    static_assert(std::is_same_v<decltype((universe | null)),
                                 UniversalSet<SignedExtensionalCardinal<>>>);
    static_assert(std::is_same_v<decltype((null & universe)),
                                 Ø<SignedExtensionalCardinal<>>>);
  }

  // SECTION("Aha! 2: The Law of Identity") {
  //   /**
  //    * In a Union, the Void is the Identity: ∅ | S = S.
  //    * In an Intersection, the Universe is the Identity: Ω & S = S.
  //    */
  //   // Let's use a Singleton to prove it preserves the specific "Body"
  //   constexpr SingletonSet<ℤ>{42} s;

  //   static_assert(std::is_same_v<decltype(null | s), SingletonSet<ℤ>>);
  //   static_assert(std::is_same_v<decltype(universe & s), SingletonSet<ℤ>>);
  // }

  SECTION("Aha! 3: The Involution of the Remainder") {
    /**
     * The Double Negation (Complement of the Complement) is the Identity.
     * !!S = S.
     */
    const auto not_null = !null;
    const auto not_universe = !universe;

    CHECK(not_null(7) == true);
    CHECK(not_universe(7) == false);
  }

  SECTION("Cardinality bounds for extensional sets are computed explicitly") {
    constexpr SingletonSet<SignedExtensionalCardinal<>> s1{1};
    constexpr SingletonSet<SignedExtensionalCardinal<>> s2{2};
    constexpr Ø<SignedExtensionalCardinal<>> empty;

    CHECK(bound_meet(s1, s2) == 1);
    CHECK(bound_join(s1, s2) == 2);

    CHECK(bound_meet(empty, s1) == 0);
    CHECK(bound_join(empty, s1) == 1);
  }

  SECTION("Cardinality bounds for intensional/transfinite sets use sentinel") {
    // ℕ is now the carrier (unsigned int) post-#401; the predicate-set
    // value is the namespace-level constant @c N (NaturalNumbersOf<>).
    constexpr auto naturals = N;
    constexpr Ø<SignedExtensionalCardinal<>> empty;
    constexpr SingletonSet<SignedExtensionalCardinal<>> singleton{7};
    const auto max_v = std::numeric_limits<std::size_t>::max();

    CHECK(bound_meet(universe, naturals) == max_v);
    CHECK(bound_join(universe, naturals) == max_v);

    CHECK(bound_meet(empty, naturals) == 0);
    CHECK(bound_meet(singleton, naturals) == 1);
  }

  SECTION("Identity optimizations remain structurally valid") {
    constexpr SingletonSet<SignedExtensionalCardinal<>> s{42};
    constexpr Ø<SignedExtensionalCardinal<>> empty;

    CHECK(std::is_same_v<decltype(empty | s),
                         SingletonSet<SignedExtensionalCardinal<>>>);
    CHECK(std::is_same_v<decltype(universe & s),
                         SingletonSet<SignedExtensionalCardinal<>>>);
  }
}
