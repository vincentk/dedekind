#include <catch2/catch_test_macros.hpp>
#include <type_traits>

import dedekind.ontology;

using namespace dedekind::ontology;

TEST_CASE("Level 0 Final Proof: The Box Monad", "[ontology][category][monad]") {
  SECTION("1. Structural Identity (IsMonad)") {
    // We verify the concept at compile-time within the test body
    static_assert(IsMonad<Box, int, std::plus<int>>,
                  "Ontology: Box must be recognized as a formal Monad.");
    SUCCEED("Box satisfies the IsMonad concept for (Z, +)");
  }

  SECTION("2. Action Proof: Join (μ) Collapse") {
    // Exercise the "Monadic Highway" notation
    auto nested = 42 >> into<> >> into<>;  // Box<Box<int>>
    auto collapsed = nested >> join<>;     // Box<int>
    auto expected = 42 >> into<>;          // Box<int>

    REQUIRE(collapsed == expected);

    // Static version for the 'Nail Down'
    static_assert((42 >> into<> >> into<> >> join<>) == (42 >> into<>),
                  "Ontology: The Monadic Join (μ) failed the Action Proof.");
  }

  SECTION("3. Action Proof: Unit (η) Lift") {
    auto lifted = 42 >> into<>;
    Box<int> explicit_box{42};

    REQUIRE(lifted == explicit_box);

    static_assert((42 >> into<>) == Box{42},
                  "Ontology: The Monadic Unit (η) failed the Action Proof.");
  }
}
