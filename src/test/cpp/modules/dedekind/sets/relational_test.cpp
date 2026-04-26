#include <catch2/catch_test_macros.hpp>

import dedekind.category;
import dedekind.sets;

using namespace dedekind::category;
using namespace dedekind::sets;

// ---------------------------------------------------------------------------
// Fixtures: small integer-universe sets used across sections.
// ---------------------------------------------------------------------------
namespace {

// Even integers in [0, 10)
constexpr auto evens_0_10 = [] {
  auto x = var<ℕ>;
  return Set{x % N |
             [](unsigned int v) { return (v < 10u) && (v % 2u == 0u); }};
}();

// Multiples of 3 in [0, 10)
constexpr auto threes_0_10 = [] {
  auto x = var<ℕ>;
  return Set{x % N |
             [](unsigned int v) { return (v < 10u) && (v % 3u == 0u); }};
}();

}  // namespace

// ---------------------------------------------------------------------------
// 1. Selection (σ)
// ---------------------------------------------------------------------------
TEST_CASE("Relational Algebra: Selection (σ)", "[sets][relational]") {
  // Keep only elements < 6 from evens_0_10 = {0, 2, 4, 6, 8}
  const auto small_evens =
      select(evens_0_10, [](unsigned int v) { return v < 6u; });

  SECTION("Members of σ_{<6}(evens) are even and below 6") {
    REQUIRE(small_evens(0u) == Ternary::True);
    REQUIRE(small_evens(2u) == Ternary::True);
    REQUIRE(small_evens(4u) == Ternary::True);
  }

  SECTION("Elements >= 6 are excluded even if even") {
    REQUIRE(small_evens(6u) == Ternary::False);
    REQUIRE(small_evens(8u) == Ternary::False);
  }

  SECTION("Odd numbers are excluded regardless of bound") {
    REQUIRE(small_evens(3u) == Ternary::False);
    REQUIRE(small_evens(5u) == Ternary::False);
  }

  SECTION("Logical-valued predicates preserve Ω semantics") {
    const auto flagged = select(evens_0_10, [](unsigned int v) {
      return (v == 2u) ? Ternary::Unknown : Ternary::True;
    });

    REQUIRE(flagged(0u) == Ternary::True);
    REQUIRE(flagged(2u) == Ternary::Unknown);
    REQUIRE(flagged(3u) == Ternary::False);
  }
}

// ---------------------------------------------------------------------------
// 2. Union (∪)
// ---------------------------------------------------------------------------
TEST_CASE("Relational Algebra: Union (∪)", "[sets][relational]") {
  // evens ∪ threes in [0,10) = {0,2,3,4,6,8,9}
  const auto union_set = set_union(evens_0_10, threes_0_10);

  SECTION("Elements in both sets are in the union") {
    REQUIRE(union_set(0u) == Ternary::True);  // 0: even and mult-of-3
    REQUIRE(union_set(6u) == Ternary::True);  // 6: even and mult-of-3
  }

  SECTION("Elements in only one set are in the union") {
    REQUIRE(union_set(2u) == Ternary::True);  // even only
    REQUIRE(union_set(4u) == Ternary::True);  // even only
    REQUIRE(union_set(3u) == Ternary::True);  // mult-of-3 only
    REQUIRE(union_set(9u) == Ternary::True);  // mult-of-3 only
  }

  SECTION("Elements in neither set are excluded") {
    REQUIRE(union_set(1u) == Ternary::False);
    REQUIRE(union_set(5u) == Ternary::False);
    REQUIRE(union_set(7u) == Ternary::False);
  }
}

// ---------------------------------------------------------------------------
// 3. Difference (∖)
// ---------------------------------------------------------------------------
TEST_CASE("Relational Algebra: Difference (∖)", "[sets][relational]") {
  // evens ∖ threes = {2, 4, 8}  (removes 0 and 6 which are multiples of 3)
  const auto diff = set_difference(evens_0_10, threes_0_10);

  SECTION("Pure-even elements remain in the difference") {
    REQUIRE(diff(2u) == Ternary::True);
    REQUIRE(diff(4u) == Ternary::True);
    REQUIRE(diff(8u) == Ternary::True);
  }

  SECTION("Even multiples-of-3 are removed") {
    REQUIRE(diff(0u) == Ternary::False);  // 0 ∈ threes
    REQUIRE(diff(6u) == Ternary::False);  // 6 ∈ threes
  }

  SECTION("Odd multiples-of-3 are not in the difference (not in evens)") {
    REQUIRE(diff(3u) == Ternary::False);
    REQUIRE(diff(9u) == Ternary::False);
  }
}

// ---------------------------------------------------------------------------
// 4. Intersection (∩)
// ---------------------------------------------------------------------------
TEST_CASE("Relational Algebra: Intersection (∩)", "[sets][relational]") {
  // evens ∩ threes = {0, 6}
  const auto inter = set_intersection(evens_0_10, threes_0_10);

  SECTION("Elements in both sets are in the intersection") {
    REQUIRE(inter(0u) == Ternary::True);
    REQUIRE(inter(6u) == Ternary::True);
  }

  SECTION("Elements in only one set are excluded") {
    REQUIRE(inter(2u) == Ternary::False);  // even but not mult-of-3
    REQUIRE(inter(3u) == Ternary::False);  // mult-of-3 but not even
    REQUIRE(inter(4u) == Ternary::False);
    REQUIRE(inter(9u) == Ternary::False);
  }
}

// ---------------------------------------------------------------------------
// 5. Natural Join (⋈)
// ---------------------------------------------------------------------------
TEST_CASE("Relational Algebra: Natural Join (⋈)", "[sets][relational]") {
  // Relation R1: {(a, b) | 0 <= a < 4 and b = a + 1}  (successor pairs)
  auto s1 = var_for_type<std::pair<int, int>>;
  const auto succ =
      Set{s1 % Ω<std::pair<int, int>>{} | [](const std::pair<int, int>& p) {
        return (p.first >= 0) && (p.first < 4) && (p.second == p.first + 1);
      }};

  // Relation R2: {(b, c) | 0 <= b < 5 and c = b * 2}  (double pairs)
  auto s2 = var_for_type<std::pair<int, int>>;
  const auto dbl =
      Set{s2 % Ω<std::pair<int, int>>{} | [](const std::pair<int, int>& p) {
        return (p.first >= 0) && (p.first < 5) && (p.second == p.first * 2);
      }};

  // Join: succ ⋈ dbl = {(a, b, c) | b = a+1 and c = b*2}
  // => (0,1,2), (1,2,4), (2,3,6), (3,4,8)
  const auto joined = natural_join(succ, dbl);

  SECTION("Valid triples are in the join") {
    REQUIRE(joined({0, 1, 2}) == Ternary::True);
    REQUIRE(joined({1, 2, 4}) == Ternary::True);
    REQUIRE(joined({2, 3, 6}) == Ternary::True);
    REQUIRE(joined({3, 4, 8}) == Ternary::True);
  }

  SECTION("Triples with wrong successor are excluded") {
    REQUIRE(joined({0, 2, 4}) == Ternary::False);  // 2 != 0+1
    REQUIRE(joined({1, 3, 6}) == Ternary::False);  // 3 != 1+1
  }

  SECTION("Triples with wrong double are excluded") {
    REQUIRE(joined({0, 1, 3}) == Ternary::False);  // 3 != 1*2
    REQUIRE(joined({1, 2, 5}) == Ternary::False);  // 5 != 2*2
  }
}
