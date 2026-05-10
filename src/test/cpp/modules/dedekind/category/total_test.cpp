/** @file test/cpp/modules/dedekind/category/total_test.cpp */
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <functional>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE("Total: The Path to Symmetry (Table 2)",
          "[category][total][algebra]") {
  SECTION("The Ideal: Unsigned Integers (Z/2^nZ)") {
    // Table 2: uint addition is marked 'M' (Modular success)
    // Verify maturation from Magma up to Abelian Group
    STATIC_CHECK(IsMagma<unsigned int, std::plus<unsigned int>>);
    STATIC_CHECK(IsSemigroup<unsigned int, std::plus<unsigned int>>);
    STATIC_CHECK(IsMonoid<unsigned int, std::plus<unsigned int>>);
    STATIC_CHECK(IsGroup<unsigned int, std::plus<unsigned int>>);
    STATIC_CHECK(IsAbelianGroup<unsigned int, std::plus<unsigned int>>);

    // Multiplicative Monoid (No integer inverse for 2)
    STATIC_CHECK(
        IsCommutativeMonoid<unsigned int, std::multiplies<unsigned int>>);
    STATIC_CHECK_FALSE(IsGroup<unsigned int, std::multiplies<unsigned int>>);
  }

  SECTION("The Honest Rejection: Signed Integers") {
    // Table 2: int + is marked 'HzU' (Undefined Behavior / Overflow)
    // Rejection Check: A species with UB is NOT a Total Magma.
    STATIC_CHECK_FALSE(IsMagma<int, std::plus<int>>);

    // Identity (0) is safe and discoverable, but Associativity is rejected
    STATIC_CHECK(IsPointed<int, std::plus<int>>);
    STATIC_CHECK_FALSE(IsAssociative<int, std::plus<int>>);
  }

  SECTION("Multi-operation Species: Rigs & Rings") {
    // bool (OR/AND) -> Table 2 'Logic' row
    // Valid Rig (Semiring), but OR has no inverse (Not a Ring)
    STATIC_CHECK(IsRig<bool, std::logical_or<bool>, std::logical_and<bool>>);
    STATIC_CHECK_FALSE(
        IsRing<bool, std::logical_or<bool>, std::logical_and<bool>>);

    // uint (Modular +, *) -> Valid Ring
    STATIC_CHECK(IsRing<unsigned int, std::plus<unsigned int>,
                        std::multiplies<unsigned int>>);
  }

  SECTION("Pointed factories: zero<A,B,Op>() and unit<A,B,Op>() at runtime") {
    // Runtime witnesses for the @c IsPointed-gated constant-morphism factories
    // re-homed from @c :discrete to @c :total under #637.  Compile-time
    // witnesses are pinned alongside the definitions in @c :total; this section
    // exercises the function bodies at runtime so coverage instrumentation
    // sees the move.

    // Additive zero: maps everything to identity_v<int, std::plus<int>> = 0.
    auto z_int = zero<int, int, std::plus<int>>();
    CHECK(z_int(0) == 0);
    CHECK(z_int(7) == 0);
    CHECK(z_int(-42) == 0);

    // Multiplicative unit: maps everything to identity_v<int,
    // std::multiplies<int>> = 1.
    auto u_int = unit<int, int, std::multiplies<int>>();
    CHECK(u_int(0) == 1);
    CHECK(u_int(99) == 1);
    CHECK(u_int(-7) == 1);

    // Cross-codomain: A=double, B=int, Op=std::plus<int>.
    auto z_dbl_to_int = zero<double, int, std::plus<int>>();
    CHECK(z_dbl_to_int(3.14) == 0);
    CHECK(z_dbl_to_int(-1.5) == 0);

    // Boolean multiplicative unit: identity_v<bool, std::logical_and<bool>> =
    // true.
    auto u_int_to_bool = unit<int, bool, std::logical_and<bool>>();
    CHECK(u_int_to_bool(0) == true);
    CHECK(u_int_to_bool(123) == true);
  }
}

TEST_CASE("Total: Lattice Structures (Relational Presence)",
          "[category][total][lattice]") {
  SECTION("Boolean Distributive Lattice") {
    STATIC_CHECK(IsJoinSemilattice<bool, std::logical_or<bool>>);
    STATIC_CHECK(IsMeetSemilattice<bool, std::logical_and<bool>>);
    STATIC_CHECK(IsDistributiveLattice<bool, std::logical_or<bool>,
                                       std::logical_and<bool>>);
  }

  SECTION("Order Lattices (Total Order Species)") {
    // Fix: Capture the type of the niebloid/algorithm
    using Max = decltype(std::ranges::max);
    using Min = decltype(std::ranges::min);

    // Verify structural properties of the operations
    STATIC_CHECK(IsSemilattice<int, Max>);
    STATIC_CHECK(IsSemilattice<int, Min>);
  }
}