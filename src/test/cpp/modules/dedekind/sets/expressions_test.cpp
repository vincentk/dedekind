#include <catch2/catch_test_macros.hpp>

// Import our Dedekind modules
import dedekind.sets;
import dedekind.ontology;

using namespace dedekind::category;
using namespace dedekind::ontology;
using namespace dedekind::sets;

TEST_CASE("Dedekind MVP: Basic Membership and Symbols", "[sets]") {
  SECTION("Integer Universe Membership") {
    auto x = var<Ω<int>>;  // A variable representing an element of the integer
                           // universe

    // Should be Set<int, ClassicalLogic>
    auto finite = Set{x % singleton(1) | (x == 1)};

    // Should be Set<int, TernaryLogic> (because ℕ is transfinite)
    auto infinite = Set{x % ℕ | (x > 0)};
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
