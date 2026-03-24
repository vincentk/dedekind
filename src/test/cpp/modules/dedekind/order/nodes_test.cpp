#include <catch2/catch_test_macros.hpp>
#include <type_traits>

import dedekind.order;
import dedekind.sets;

TEST_CASE("Symbolic Order: Ray Algebra", "[order][static]") {
  using namespace dedekind::sets;
  using namespace dedekind::order;

  auto U = UniversalSet<int, ℕ64>(ℕ64());

  SECTION("Negation Theorem: ¬[Min, ∞) == (-∞, Min)") {
    auto ray = lower_closed_ray<int, decltype(U), 10>(U);
    auto negated = !ray;

    using Expected =
        PredicateNode<int, decltype(U), UpperBound<int, 10, Open>, ℕ64>;
    STATIC_REQUIRE(std::is_same_v<decltype(negated), Expected>);
  }
  auto L = lower_closed_ray<int, decltype(U), 10>(U);
  auto R = upper_closed_ray<int, decltype(U), 5>(U);

  SECTION("Disjointness Theorem: [10, ∞) ∩ (-∞, 5] == ø") {
    auto result = L & R;
    STATIC_REQUIRE(std::is_same_v<decltype(result), ø<int, decltype(U)>>);
  }

  SECTION("Commutative Disjointness Proof") {
    // Both directions result in the same Empty Set type
    STATIC_REQUIRE(std::is_same_v<decltype(L & R), ø<int, decltype(U)>>);
    STATIC_REQUIRE(std::is_same_v<decltype(R & L), ø<int, decltype(U)>>);
  }

  SECTION("Interval Factory: [0, 10]") {
    auto I = closed_interval<int, decltype(U), 0, 10>(U);

    REQUIRE(I.contains(0));
    REQUIRE(I.contains(5));
    REQUIRE(I.contains(10));
    REQUIRE_FALSE(I.contains(11));
    REQUIRE_FALSE(I.contains(-1));
  }
}
