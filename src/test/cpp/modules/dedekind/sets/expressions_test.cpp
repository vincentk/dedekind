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

TEST_CASE("Dedekind Sets: Cartesian product and relation witnesses",
          "[sets][relations][cartesian]") {
  auto x = var<Ω<int>>;

  const auto positive = Set{x % Ω<int>{} | (x > 0)};
  const auto small = Set{x % Ω<int>{} | (x <= 3)};

  const auto product = cartesian_product(positive, small);

  CHECK(product(std::pair<int, int>{1, 2}) == Ternary::True);
  CHECK(product(std::pair<int, int>{-1, 2}) == Ternary::False);
  CHECK(product(std::pair<int, int>{1, 7}) == Ternary::False);

  const auto graph_pred = [](const std::pair<int, int>& p) {
    return p.second == 2 * p.first;
  };
  const Relation<int, int, ClassicalLogic, decltype(graph_pred)> R{graph_pred};

  STATIC_CHECK(IsRelation<decltype(R), int, int>);
  CHECK(relates(R, 3, 6) == true);
  CHECK(relates(R, 3, 7) == false);

  const SetFunction<int, int, ClassicalLogic, decltype(graph_pred)> F{
      graph_pred};
  CHECK(is_single_valued_at(F, 3, 6, 6) == true);
  CHECK(is_single_valued_at(F, 3, 6, 7) == true);
}

TEST_CASE("Dedekind Sets: Power-set witness over homogeneous predicates",
          "[sets][powerset]") {
  auto x = var<Ω<int>>;

  const auto positive = Set{x % Ω<int>{} | (x > 0)};

  const auto p_positive = power_set(positive);

  STATIC_CHECK(std::same_as<typename decltype(p_positive)::Domain,
                            std::remove_cvref_t<decltype(positive)>>);
  CHECK(p_positive(positive) == true);
}
