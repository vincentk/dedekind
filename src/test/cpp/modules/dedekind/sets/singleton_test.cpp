#include <catch2/catch_test_macros.hpp>
import dedekind.ontology;

using namespace dedekind::ontology;

TEST_CASE("Level 1 Final Proof: The Mereology Highway",
          "[ontology][mereology][highway]") {
  SECTION("1. The Singleton Lifting") {
    // Target the struct directly to satisfy Clang's template-template rules
    auto atom = 42 >> into<SingletonSet>;
    REQUIRE(atom.contains(42) == true);
  }

  SECTION("2. The Pull from the Identity (ε)") {
    // Pushing into the set and pulling the value back out
    // Note: SingletonSet must provide extract_v to satisfy IsPreComonad
    int value = 42 >> into<SingletonSet> << extract<SingletonSet>;

    REQUIRE(value == 42);
    static_assert((7 >> into<SingletonSet> << extract<SingletonSet>) == 7,
                  "The Round-trip Axiom.");
  }
}
