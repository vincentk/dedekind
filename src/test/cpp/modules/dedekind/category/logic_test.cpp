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

/** @file test/cpp/modules/dedekind/category/logic_test.cpp */

TEST_CASE("Logic: The Lattice Order (Relational Honesty)",
          "[category][logic][order]") {
  SECTION("Boolean Lattice Order") {
    using B = Boolean;
    B t{true}, f{false};

    // Axiom: a <= b iff (a + b) == b
    CHECK((f <= t));        // (false || true) == true
    CHECK((f <= f));        // (false || false) == false
    CHECK((t <= t));        // (true || true) == true
    CHECK_FALSE((t <= f));  // (true || false) != false
  }

  SECTION("Kleene Information/Truth Order") {
    using K = Kleene;
    using enum Ternary;
    K T{True}, F{False}, U{Unknown};

    // Verifying the Linear Truth Chain: False < Unknown < True
    CHECK((F <= U));  // (False || Unknown) == Unknown
    CHECK((U <= T));  // (Unknown || True) == True
    CHECK((F <= T));  // Transitivity

    // Reflexivity
    CHECK((U <= U));

    // Antisymmetry (Strictly different values cannot be <= each other both
    // ways)
    CHECK_FALSE((T <= U));
  }

  SECTION("Predicate Composition (Rule-Anchored)") {
    using P = Rule<Boolean, Boolean>;

    // 1. Explicitly return Boolean to match the Rule's Codomain
    // 2. Use the constructor directly instead of { }
    P is_true([](const Boolean& b) -> Boolean { return b; });
    P is_false([](const Boolean& b) -> Boolean { return !b; });

    // 3. This should now find your Rule-specific operator&&
    auto both = is_true && is_true;

    CHECK(both(Boolean{true}).value == true);
    CHECK(both(Boolean{false}).value == false);

    auto either = is_true || is_false;
    CHECK(either(Boolean{false}).value == true);
  }
}