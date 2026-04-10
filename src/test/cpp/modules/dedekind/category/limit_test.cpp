/** @file test/cpp/modules/dedekind/category/discrete_test.cpp */
#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <variant>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE("Discrete: Terminal Object (1) - Pure Existence", "[category][discrete][terminal]") {
  // Table 1: Terminal Object (1) maps to std::monostate
  using One = std::monostate;

  SECTION("Existence Axiom") {
    STATIC_CHECK(IsTerminalObject<One>);
    // Every species T must have exactly one morphism to the Terminal Object (T -> 1)
    STATIC_CHECK(HasUniqueMorphismTo<int, One>);
    STATIC_CHECK(HasUniqueMorphismTo<bool, One>);
  }

  SECTION("Global Elements (Morphisms 1 -> X)") {
    // A constant value '5' is a mapping from the state of pure existence to 'int'
    auto five = Morphism<One, int>([](One) { return 5; });
    
    STATIC_CHECK(IsArrow<decltype(five)>);
    CHECK(five(One{}) == 5);
  }
}

TEST_CASE("Discrete: Initial Object (0) - The Annihilator", "[category][discrete][initial]") {
  // Table 1: Initial Object (0) maps to std::nullptr_t (representing the unreachable)
  using Zero = std::nullptr_t;

  SECTION("Unreachability Axiom") {
    STATIC_CHECK(IsInitialObject<Zero>);
    // Dually, there is a unique morphism from the Initial Object to any species (0 -> T)
    STATIC_CHECK(HasUniqueMorphismFrom<Zero, int>);
    STATIC_CHECK(HasUniqueMorphismFrom<Zero, double>);
  }

  SECTION("Annihilation Logic") {
    // A morphism out of Zero represents an unreachable code path
    auto unreachable = Morphism<Zero, int>([](Zero) -> int { 
        // This body is logically never entered
        return 0; 
    });

    STATIC_CHECK(IsArrow<decltype(unreachable)>);
  }
}

TEST_CASE("Discrete: Product and Coproduct (Cartesian Bridge)", "[category][discrete][universal]") {
  // Section 2.3.5: Mapping categorical products to C++ primitives
  
  SECTION("Product (A x B) via std::pair") {
    using P = std::pair<int, bool>;
    STATIC_CHECK(IsProduct<P, int, bool>);
    
    P p{42, true};
    CHECK(project<0>(p) == 42);
    CHECK(project<1>(p) == true);
  }

  SECTION("Coproduct (A + B) via std::variant") {
    using C = std::variant<int, bool>;
    STATIC_CHECK(IsCoproduct<C, int, bool>);
    
    auto choice = inject<int>(10);
    STATIC_CHECK(std::same_as<decltype(choice), C>);
    CHECK(std::get<int>(choice) == 10);
  }
}
