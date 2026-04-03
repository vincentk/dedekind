/** @file test/cpp/modules/dedekind/category/logic_test.cpp */
#include <catch2/catch_test_macros.hpp>
#include <concepts>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE("Logic: The Binary Prime (Classical)", "[category][logic][boolean]") {
  SECTION("Classical Invariants") {
    CHECK((true && true) == true);
    CHECK((true && false) == false);
    CHECK(!true == false);
    CHECK((true || false) == true);
  }

  SECTION("Species Promotion (Boolean Wrapper)") {
    Boolean t{true};
    Boolean f{false};

    // Verify our 'operator+' bypasses the int-promotion trap
    STATIC_CHECK(std::same_as<decltype(t + f), Boolean>);
    CHECK((t + f).value == true);
  }
}

TEST_CASE("Logic: The Indeterminacy (Kleene)", "[category][logic][kleene]") {
  using enum Ternary;

  SECTION("Kleene Truth Tables") {
    // Conjunction (AND)
    CHECK((True && Unknown) == Unknown);
    CHECK((False && Unknown) == False);

    // Disjunction (OR)
    CHECK((True || Unknown) == True);
    CHECK((False || Unknown) == Unknown);

    // Negation (NOT)
    CHECK(!Unknown == Unknown);
    CHECK(!True == False);
    CHECK(!False == True);
  }

  SECTION("Structural Identities (De Morgan's Laws)") {
    // !(A && B) == !A || !B
    CHECK(!(True && Unknown) == (!True || !Unknown));
    CHECK(!(False && Unknown) == (!False || !Unknown));
  }

  SECTION("Morphism Lifting") {
    // Verifying the lift_logic bridge
    CHECK(lift_logic<TernaryLogic>(true) == True);
    CHECK(lift_logic<TernaryLogic>(false) == False);

    // Identity lifting
    CHECK(lift_logic<TernaryLogic>(Unknown) == Unknown);
  }
}

TEST_CASE("Logic: Archimedean Successor", "[category][logic][peano]") {
  SECTION("Boolean Successor") {
    Boolean b{false};
    // Successor S(0) = 1
    CHECK((b + Boolean::one()).value == true);
    // Saturating S(1) = 1
    CHECK((Boolean{true} + Boolean::one()).value == true);
  }
}
