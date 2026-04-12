/** @file dedekind/order/order_test.cpp */
#include <catch2/catch_test_macros.hpp>
import dedekind.order;
import dedekind.category;

using namespace dedekind::order;
using namespace dedekind::category;

TEST_CASE("Order: The Geography of Species", "[order][axioms]") {
  SECTION("The Integral Chain (int)") {
    // Does int satisfy the full stack of Level 1.5?
    STATIC_CHECK(IsPreOrdered<int>);
    STATIC_CHECK(IsPartiallyOrdered<int>);
    STATIC_CHECK(IsTotallyOrdered<int>);
    // Archimedean and density contracts are experimental in this layer.
  }

  SECTION("The Logical Lattice (bool)") {
    // Booleans are a Poset, but are they a Total Order?
    // (Depends on if you've vouched for false < true)
    STATIC_CHECK(IsPartiallyOrdered<bool>);
  }
}

TEST_CASE("Order: Archimedean Scales", "[order][archimedean]") {
  using namespace dedekind::category;

  SECTION("Discrete Logic Scales") {
    SUCCEED("Boolean/Kleene order traits are tracked as experimental.");
  }

  SECTION("Discrete Integral Scales") {
    STATIC_CHECK(IsPreOrdered<unsigned int>);
    STATIC_CHECK(IsPreOrdered<int>);
  }
}