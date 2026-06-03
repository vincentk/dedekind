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
  auto x = element<ℕ>;
  return Set{x | [](const auto& v) { return (v < 10u) && (v % 2u == 0u); }};
}();

// Multiples of 3 in [0, 10)
constexpr auto threes_0_10 = [] {
  auto x = element<ℕ>;
  return Set{x | [](const auto& v) { return (v < 10u) && (v % 3u == 0u); }};
}();

}  // namespace

// ---------------------------------------------------------------------------
// 1. Selection (σ)
// ---------------------------------------------------------------------------
TEST_CASE("Relational Algebra: Selection (σ)", "[sets][relational]") {
  // Keep only elements < 6 from evens_0_10 = {0, 2, 4, 6, 8}
  const auto small_evens =
      select(evens_0_10, [](const auto& v) { return v < 6u; });

  SECTION("Members of σ_{<6}(evens) are even and below 6") {
    REQUIRE(small_evens(0u));
    REQUIRE(small_evens(2u));
    REQUIRE(small_evens(4u));
  }

  SECTION("Elements >= 6 are excluded even if even") {
    REQUIRE_FALSE(small_evens(6u));
    REQUIRE_FALSE(small_evens(8u));
  }

  SECTION("Odd numbers are excluded regardless of bound") {
    REQUIRE_FALSE(small_evens(3u));
    REQUIRE_FALSE(small_evens(5u));
  }

  // FIXME(#693): "Logical-valued predicates preserve Ω semantics" — the
  // pre-#622 test relied on `evens_0_10` being TernaryLogic-routed (ℕ →
  // Ternary under the old NaturalLogic resolver), letting a lambda return
  // Ternary::Unknown straight through `select`.  Under the cardinality
  // cut (#622), ℕ → Classical on the carrier axis, so Ternary preservation
  // requires an explicit-Ternary fixture.  Rebuild under the predicate-
  // level axis (#693), which is the principled home for this witness.
}

// ---------------------------------------------------------------------------
// 2. Union (∪)
// ---------------------------------------------------------------------------
TEST_CASE("Relational Algebra: Union (∪)", "[sets][relational]") {
  // evens ∪ threes in [0,10) = {0,2,3,4,6,8,9}
  const auto union_set = set_union(evens_0_10, threes_0_10);

  SECTION("Elements in both sets are in the union") {
    REQUIRE(union_set(0u));  // 0: even and mult-of-3
    REQUIRE(union_set(6u));  // 6: even and mult-of-3
  }

  SECTION("Elements in only one set are in the union") {
    REQUIRE(union_set(2u));  // even only
    REQUIRE(union_set(4u));  // even only
    REQUIRE(union_set(3u));  // mult-of-3 only
    REQUIRE(union_set(9u));  // mult-of-3 only
  }

  SECTION("Elements in neither set are excluded") {
    REQUIRE_FALSE(union_set(1u));
    REQUIRE_FALSE(union_set(5u));
    REQUIRE_FALSE(union_set(7u));
  }
}

// ---------------------------------------------------------------------------
// 3. Difference (∖)
// ---------------------------------------------------------------------------
TEST_CASE("Relational Algebra: Difference (∖)", "[sets][relational]") {
  // evens ∖ threes = {2, 4, 8}  (removes 0 and 6 which are multiples of 3)
  const auto diff = set_difference(evens_0_10, threes_0_10);

  SECTION("Pure-even elements remain in the difference") {
    REQUIRE(diff(2u));
    REQUIRE(diff(4u));
    REQUIRE(diff(8u));
  }

  SECTION("Even multiples-of-3 are removed") {
    REQUIRE_FALSE(diff(0u));  // 0 ∈ threes
    REQUIRE_FALSE(diff(6u));  // 6 ∈ threes
  }

  SECTION("Odd multiples-of-3 are not in the difference (not in evens)") {
    REQUIRE_FALSE(diff(3u));
    REQUIRE_FALSE(diff(9u));
  }
}

// ---------------------------------------------------------------------------
// 4. Intersection (∩)
// ---------------------------------------------------------------------------
TEST_CASE("Relational Algebra: Intersection (∩)", "[sets][relational]") {
  // evens ∩ threes = {0, 6}
  const auto inter = set_intersection(evens_0_10, threes_0_10);

  SECTION("Elements in both sets are in the intersection") {
    REQUIRE(inter(0u));
    REQUIRE(inter(6u));
  }

  SECTION("Elements in only one set are excluded") {
    REQUIRE_FALSE(inter(2u));  // even but not mult-of-3
    REQUIRE_FALSE(inter(3u));  // mult-of-3 but not even
    REQUIRE_FALSE(inter(4u));
    REQUIRE_FALSE(inter(9u));
  }
}

// ---------------------------------------------------------------------------
// 5. Natural Join (⋈)
// ---------------------------------------------------------------------------
TEST_CASE("Relational Algebra: Natural Join (⋈)", "[sets][relational]") {
  // Relation R1: {(a, b) | 0 <= a < 4 and b = a + 1}  (successor pairs)
  auto s1 = element<Ω<std::pair<int, int>>>;
  const auto succ =
      Set{s1 % UniversalSet<std::pair<int, int>>{} |
          [](const std::pair<int, int>& p) {
            return (p.first >= 0) && (p.first < 4) && (p.second == p.first + 1);
          }};

  // Relation R2: {(b, c) | 0 <= b < 5 and c = b * 2}  (double pairs)
  auto s2 = element<Ω<std::pair<int, int>>>;
  const auto dbl =
      Set{s2 % UniversalSet<std::pair<int, int>>{} |
          [](const std::pair<int, int>& p) {
            return (p.first >= 0) && (p.first < 5) && (p.second == p.first * 2);
          }};

  // Join: succ ⋈ dbl = {(a, b, c) | b = a+1 and c = b*2}
  // => (0,1,2), (1,2,4), (2,3,6), (3,4,8)
  const auto joined = natural_join(succ, dbl);

  SECTION("Valid triples are in the join") {
    REQUIRE(joined({0, 1, 2}));
    REQUIRE(joined({1, 2, 4}));
    REQUIRE(joined({2, 3, 6}));
    REQUIRE(joined({3, 4, 8}));
  }

  SECTION("Triples with wrong successor are excluded") {
    REQUIRE_FALSE(joined({0, 2, 4}));  // 2 != 0+1
    REQUIRE_FALSE(joined({1, 3, 6}));  // 3 != 1+1
  }

  SECTION("Triples with wrong double are excluded") {
    REQUIRE_FALSE(joined({0, 1, 3}));  // 3 != 1*2
    REQUIRE_FALSE(joined({1, 2, 5}));  // 5 != 2*2
  }
}
