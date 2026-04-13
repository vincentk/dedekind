#include <catch2/catch_test_macros.hpp>
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
    Ω<int> universe;

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
    Ω<int, TernaryLogic> k_universe;

    REQUIRE(k_universe(42) == Ternary::True);
  }
}

TEST_CASE("Boundaries: The Algebra of Extremality", "[sets][boundaries]") {
  using ℤ = int;  // Using a raw type as a Species proxy

  // The Identities: Ø and Ω as the "North and South Poles"
  constexpr Ø<ℤ> null;
  constexpr Ω<ℤ> universe;

  SECTION("Aha! 1: The Law of Absorption (Annihilation)") {
    /**
     * In a Union, the Universe is the Annihilator: Ω | S = Ω.
     * In an Intersection, the Void is the Annihilator: ∅ & S = ∅.
     */
    static_assert(std::is_same_v<decltype((universe | null)), Ω<ℤ>>);
    static_assert(std::is_same_v<decltype((null & universe)), Ø<ℤ>>);
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
}
