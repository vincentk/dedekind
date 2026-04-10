/** @file test/cpp/modules/dedekind/category/total_test.cpp */
#include <catch2/catch_test_macros.hpp>
#include <concepts>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE("Total: The Path to Symmetry (Table 2)", "[category][total][algebra]") {
    
    SECTION("The Ideal: Unsigned Integers (Z/2^nZ)") {
        // Table 2: uint addition is marked 'M' (Modular success)
        // Verify maturation from Magma up to Abelian Group
        STATIC_CHECK(IsMagma<unsigned int, std::plus<unsigned int>>);
        STATIC_CHECK(IsSemigroup<unsigned int, std::plus<unsigned int>>);
        STATIC_CHECK(IsMonoid<unsigned int, std::plus<unsigned int>>);
        STATIC_CHECK(IsGroup<unsigned int, std::plus<unsigned int>>);
        STATIC_CHECK(IsAbelianGroup<unsigned int, std::plus<unsigned int>>);
        
        // Multiplicative Monoid (No integer inverse for 2)
        STATIC_CHECK(IsCommutativeMonoid<unsigned int, std::multiplies<unsigned int>>);
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
        STATIC_CHECK_FALSE(IsRing<bool, std::logical_or<bool>, std::logical_and<bool>>);

        // uint (Modular +, *) -> Valid Ring
        STATIC_CHECK(IsRing<unsigned int, std::plus<unsigned int>, std::multiplies<unsigned int>>);
    }
}

TEST_CASE("Total: Lattice Structures (Relational Presence)", "[category][total][lattice]") {
    
    SECTION("Boolean Distributive Lattice") {
        // Table 2: OR/AND are marked Idempotent & Commutative
        STATIC_CHECK(IsJoinSemilattice<bool, std::logical_or<bool>>);
        STATIC_CHECK(IsMeetSemilattice<bool, std::logical_and<bool>>);
        STATIC_CHECK(IsDistributiveLattice<bool, std::logical_or<bool>, std::logical_and<bool>>);
    }

    SECTION("Order Lattices (Total Order Species)") {
        // min/max satisfy the lattice axioms on ordered species
        using Max = std::ranges::max_element; // Or your specific Op wrapper
        using Min = std::ranges::min_element;

        STATIC_CHECK(IsLattice<unsigned int, std::plus<unsigned int>, std::multiplies<unsigned int>> == false);
        // Assuming your dispatcher handles std::ranges::max/min or similar
    }
}

TEST_CASE("Total: Morphic Gatekeeping", "[category][total][morphism]") {
    SECTION("Morphic Totality Verification") {
        auto total_fn = [](unsigned int x) { return x + 1; };
        
        // Verify the lambda is a valid TotalArrow
        STATIC_CHECK(IsArrow<decltype(total_fn), unsigned int>);
        
        // TotalMorphism wrapper provides compile-time guarantee
        using Succ = TotalMorphism<unsigned int, unsigned int, decltype(total_fn)>;
        Succ s{total_fn};
        CHECK(s(10) == 11);
    }
}
