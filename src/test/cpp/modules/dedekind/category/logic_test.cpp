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

  SECTION("Textbook operator symmetries (Boolean algebra)") {
    constexpr bool values[] = {false, true};
    for (bool a : values) {
      for (bool b : values) {
        CHECK((a && b) == (b && a));
        CHECK((a || b) == (b || a));
      }
    }

    for (bool a : values) {
      for (bool b : values) {
        for (bool c : values) {
          CHECK(((a && b) && c) == (a && (b && c)));
          CHECK(((a || b) || c) == (a || (b || c)));
          CHECK((a && (b || c)) == ((a && b) || (a && c)));
          CHECK((a || (b && c)) == ((a || b) && (a || c)));
        }
      }
    }
  }

  SECTION("Species Promotion (Boolean Wrapper)") {
    Boolean t{true};
    Boolean f{false};

    // Verify our 'operator+' bypasses the int-promotion trap
    STATIC_CHECK(std::same_as<decltype(t + f), Boolean>);
    CHECK((t + f).value == true);

    // De Morgan's laws for Boolean wrapper
    CHECK(!(t && f) == (!t || !f));
    CHECK(!(t || f) == (!t && !f));
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

  SECTION("Textbook operator symmetries (K3 lattice laws)") {
    constexpr Ternary values[] = {False, Unknown, True};
    for (auto a : values) {
      for (auto b : values) {
        CHECK((a && b) == (b && a));
        CHECK((a || b) == (b || a));
      }
    }

    for (auto a : values) {
      for (auto b : values) {
        for (auto c : values) {
          CHECK(((a && b) && c) == (a && (b && c)));
          CHECK(((a || b) || c) == (a || (b || c)));
          CHECK((a && (b || c)) == ((a && b) || (a && c)));
          CHECK((a || (b && c)) == ((a || b) && (a || c)));
        }
      }
    }
  }

  SECTION("Structural Identities (De Morgan's Laws)") {
    constexpr Ternary values[] = {False, Unknown, True};
    // !(A && B) == !A || !B
    for (auto a : values) {
      for (auto b : values) {
        CHECK(!(a && b) == (!a || !b));
        CHECK(!(a || b) == (!a && !b));
      }
    }
  }

  SECTION("Morphism Lifting") {
    // Verifying the lift_logic bridge from Boolean to Ternary
    CHECK(lift_logic<TernaryLogic>(true) == True);
    CHECK(lift_logic<TernaryLogic>(false) == False);

    // Identity lifting
    CHECK(lift_logic<TernaryLogic>(Unknown) == Unknown);

    // Consistency: lifting preserves order
    CHECK(lift_logic<TernaryLogic>(false) <= lift_logic<TernaryLogic>(true));
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
    CHECK(holds(f <= t));    // (false || true) == true
    CHECK(holds(f <= f));    // (false || false) == false, so equality holds
    CHECK(holds(t <= t));    // (true || true) == true
    CHECK(refutes(t <= f));  // (true || false) != false
  }

  SECTION("Kleene Information/Truth Order") {
    using K = Kleene;
    using enum Ternary;
    K T{True}, F{False}, U{Unknown};

    // Verifying the Linear Truth Chain: False < Unknown < True
    CHECK(holds(F <= U));  // (False || Unknown) == Unknown, hence <= is True
    CHECK(holds(U <= T));  // (Unknown || True) == True
    CHECK(holds(F <= T));  // Transitivity

    // Reflexivity
    CHECK(holds(U <= U));

    // Antisymmetry (Strictly different values cannot be <= each other both
    // ways)
    CHECK(refutes(T <= U));
  }
}