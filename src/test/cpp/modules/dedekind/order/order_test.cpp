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
    STATIC_CHECK(IsArchimedean<int>);
    // Note: int is NOT IsDense, which is correct!
    STATIC_CHECK_FALSE(IsDense<int>);
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
    // Boolean: 0 + 1 = 1 (Saturating)
    STATIC_CHECK(IsArchimedean<Boolean>);

    // Kleene: U + 1 = 1
    STATIC_CHECK(IsArchimedean<Kleene>);
  }

  SECTION("Discrete Integral Scales") {
    STATIC_CHECK(IsArchimedean<unsigned int>);
    STATIC_CHECK(IsArchimedean<int>);
  }
}