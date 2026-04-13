#include <catch2/catch_test_macros.hpp>

import dedekind.category;
import dedekind.sets;

using namespace dedekind::category;
using namespace dedekind::sets;

TEST_CASE("Dedekind MVP: Basic Membership and Symbols", "[sets]") {
  SECTION("Integer Universe Membership") {
    auto x = var<Ω<int>>;  // A variable representing an element of the integer
                           // universe

    // Should be Set<int, ClassicalLogic>
    auto finite = Set{x % singleton(1) | (x == 1)};
    REQUIRE(finite(1) == true);
    REQUIRE(finite(2) == false);

    // Should be Set<int, TernaryLogic> (because ℕ is transfinite)
    auto infinite = Set{x % ℕ | (x > 0)};
    REQUIRE(infinite(5) == Ternary::True);
    REQUIRE(infinite(-5) == Ternary::False);
  }
}

TEST_CASE("Dedekind Identities: Extremal Collapse", "[sets][identities]") {
  auto x = var<NaturalNumbers>;

  SECTION("Identity: Set{Ω} is Ω") {
    // The terminal object should remain terminal
    auto U = Set{ℕ};

    // It must satisfy the 'Total Presence' axiom
    static_assert(std::is_same_v<decltype(U)::logic_species, TernaryLogic>);
    REQUIRE(U(42) == Ternary::True);
    REQUIRE(U(-1) == Ternary::True);
  }

  SECTION("Contradiction: {x ∈ ℕ | x > 10 ∧ x < 5} is ∅") {
    // Here we combine the symbolic predicates
    auto S = Set{x % ℕ | (x > 10 && x < 5)};

    // For a non-trivial polish, we verify it is 'Total Absence'
    REQUIRE(S(0) == Ternary::False);
    REQUIRE(S(7) == Ternary::False);
    REQUIRE(S(12) == Ternary::False);
  }

  SECTION("Tautology: {x ∈ ℕ | x > 10 ∨ x <= 10} is Ω") {
    auto S = Set{x % ℕ | (x > 10 || x <= 10)};
    REQUIRE(S(7) == Ternary::True);
  }
}

TEST_CASE("Dedekind Boolean Sets: Concrete Algebraic Starter",
          "[sets][boolean][algebra]") {
  auto b = var<Ω<bool, ClassicalLogic, Finite>>;

  // Two concrete subsets of the boolean universe.
  auto truthy = Set{b % Ω<bool, ClassicalLogic, Finite>{} | (b == true)};
  auto falsy = Set{b % Ω<bool, ClassicalLogic, Finite>{} | (b == false)};

  // Partition check: each value belongs to exactly one side.
  REQUIRE(truthy(true));
  REQUIRE_FALSE(truthy(false));
  REQUIRE_FALSE(falsy(true));
  REQUIRE(falsy(false));

  // Boolean algebra laws over the concrete universe {false, true}.
  auto top = truthy | falsy;
  auto bottom = truthy & falsy;

  REQUIRE(top(true));
  REQUIRE(top(false));
  REQUIRE_FALSE(bottom(true));
  REQUIRE_FALSE(bottom(false));

  auto not_truthy = !truthy;
  REQUIRE_FALSE(not_truthy(true));
  REQUIRE(not_truthy(false));

  // Absorption: A ∨ (A ∧ B) = A and A ∧ (A ∨ B) = A
  auto absorb_join = truthy | (truthy & falsy);
  auto absorb_meet = truthy & (truthy | falsy);

  REQUIRE(absorb_join(true) == truthy(true));
  REQUIRE(absorb_join(false) == truthy(false));
  REQUIRE(absorb_meet(true) == truthy(true));
  REQUIRE(absorb_meet(false) == truthy(false));
}
