/** @file dedekind/order/order_test.cpp */
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
import dedekind.order;
import dedekind.category;

using namespace dedekind::order;

TEST_CASE("Order: The Geography of Species", "[order][axioms]") {
  SECTION("The Integral Chain (int)") {
    // Does int satisfy the full stack of Level 1.5?
    STATIC_CHECK(IsPreOrdered<int>);
    STATIC_CHECK(IsPartiallyOrdered<int>);
    STATIC_CHECK(IsTotallyOrdered<int>);
    STATIC_CHECK(dedekind::order::IsOrderMeetSemilattice<int>);
    STATIC_CHECK(dedekind::order::IsOrderJoinSemilattice<int>);
    // int is a totally-ordered lattice under min / max.  The
    // post-#393 `IsOrderLattice` is the bundled Boolean-ring lattice
    // (Add = bit_xor, Mult = bit_and), which int does NOT satisfy ---
    // int is an integer ring under +/*, not a Boolean ring under
    // XOR/AND.  The order-theoretic min/max claim is now made
    // directly via the underlying certified-lattice concept.
    STATIC_CHECK(dedekind::category::IsCertifiedOrderLatticeOperations<int>);
    STATIC_CHECK(dedekind::order::IsOrderDistributiveLattice<int>);
    // Archimedean and density contracts are experimental in this layer.
  }

  SECTION("The Logical Lattice (bool)") {
    // Booleans are a Poset, but are they a Total Order?
    // (Depends on if you've vouched for false < true)
    STATIC_CHECK(IsPartiallyOrdered<bool>);
    STATIC_CHECK(dedekind::order::IsOrderLattice<bool>);
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

TEST_CASE("Order: Posetal lattice reuse", "[order][lattice]") {
  constexpr auto meet = std::ranges::min;
  constexpr auto join = std::ranges::max;

  SECTION("Certified lattice operations are re-exported") {
    STATIC_CHECK(dedekind::order::IsOrderMeetSemilattice<int, decltype(meet)>);
    STATIC_CHECK(dedekind::order::IsOrderJoinSemilattice<int, decltype(join)>);
    // Order-theoretic certified lattice on int (Join = max, Meet = min);
    // post-#393, `IsOrderLattice` is the bundled Boolean-ring lattice and
    // does not fire on integer carriers.
    STATIC_CHECK(dedekind::category::IsCertifiedOrderLatticeOperations<
                 int, decltype(join), decltype(meet)>);
    STATIC_CHECK(
        dedekind::order::IsOrderDistributiveLattice<int, decltype(join),
                                                    decltype(meet)>);
  }
}