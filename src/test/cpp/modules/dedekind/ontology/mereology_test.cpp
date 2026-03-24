#include <catch2/catch_test_macros.hpp>
import dedekind.ontology;

using namespace dedekind::ontology;

TEST_CASE("Level 1 Final Proof: The Mereology Highway",
          "[ontology][mereology][highway]") {
  SECTION("1. The Singleton Lifting") {
    // Target the struct directly to satisfy Clang's template-template rules
    auto atom = 42 >> into<SetMonad<int>>;
    REQUIRE(atom.contains(42) == true);
  }

  SECTION("2. Initial Object Proof") {
    EmptySet<int> empty;
    // This will now pass because cardinality() exists
    static_assert(IsInitialObject<decltype(empty)>);
  }

  SECTION("2. The Pull from the Identity (ε)") {
    // Pushing into the set and pulling the value back out
    // Note: SingletonSet must provide extract_v to satisfy IsPreComonad
    int value = 42 >> into<SetMonad<int>> << extract<>;

    REQUIRE(value == 42);
    static_assert((7 >> into<SetMonad<int>> << extract<>) == 7,
                  "The Round-trip Axiom.");
  }

  SECTION("3. The Extreme Bounds (0 and 1)") {
    EmptySet<int> empty;
    UniversalSet<int> universe;

    // Verify the Categorical Roles
    static_assert(IsInitialObject<decltype(empty)>, "∅ is the Initial Object.");
    static_assert(IsTerminalObject<decltype(universe)>,
                  "U is the Terminal Object.");

    // Verify the Logical Truth across species
    REQUIRE(empty.contains(42) == false);
    REQUIRE(universe.contains(42) == true);

    // Verify the Magnitude (The Ruler)
    // Note: UniversalSet<int> is Countable (ℵ_0)
    REQUIRE(empty.cardinality() == Finite{});
  }

  SECTION("4. The Logic Swapping (Topos-Awareness)") {
    // Universal Set over the Ternary Topos (Kleene Logic)
    UniversalSet<int, TernaryLogic> k_universe;

    static_assert(IsKleeneSet<decltype(k_universe)>,
                  "Must be an Indeterminate Set.");
    REQUIRE(k_universe.contains(42) == Ternary::True);
  }
}
