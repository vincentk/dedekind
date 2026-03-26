#include <catch2/catch_test_macros.hpp>
import dedekind.ontology;
import dedekind.sets;

using namespace dedekind::ontology;
using namespace dedekind::sets;

TEST_CASE("Level 1 Final Proof: The Mereology Highway",
          "[ontology][mereology][highway]") {
  SECTION("2. Initial Object Proof") {
    Ø<int> empty;
    // This will now pass because cardinality() exists
    static_assert(IsInitialObject<decltype(empty)>);
  }

  SECTION("3. The Extreme Bounds (0 and 1)") {
    Ø<int> empty;
    Ω<int> universe;

    // Verify the Categorical Roles
    static_assert(IsInitialObject<decltype(empty)>, "Ø is the Initial Object.");
    static_assert(IsTerminalObject<decltype(universe)>,
                  "Ω is the Terminal Object.");

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
