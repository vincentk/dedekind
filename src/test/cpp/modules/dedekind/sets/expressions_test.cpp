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
    auto infinite = Set{x % N | (x > 0)};
    REQUIRE(infinite(5) == Ternary::True);
    REQUIRE(infinite(-5) == Ternary::False);
  }
}

TEST_CASE("Dedekind Identities: Extremal Collapse", "[sets][identities]") {
  auto x = var<ℕ>;

  SECTION("Identity: Set{N} is N") {
    // Naturals remain stable when materialized through Set{...}.
    auto U = Set{N};

    static_assert(std::is_same_v<decltype(U)::logic_species, TernaryLogic>);
    REQUIRE(U(42) == Ternary::True);
    REQUIRE(U(-1) == Ternary::False);
  }

  SECTION("Contradiction: {x ∈ ℕ | x > 10 ∧ x < 5} is ∅") {
    // Here we combine the symbolic predicates
    auto S = Set{x % N | (x > 10 && x < 5)};

    // For a non-trivial polish, we verify it is 'Total Absence'
    REQUIRE(S(0) == Ternary::False);
    REQUIRE(S(7) == Ternary::False);
    REQUIRE(S(12) == Ternary::False);
  }

  SECTION("Tautology: {x ∈ ℕ | x > 10 ∨ x <= 10} is Ω") {
    auto S = Set{x % N | (x > 10 || x <= 10)};
    REQUIRE(S(7) == Ternary::True);
  }
}
