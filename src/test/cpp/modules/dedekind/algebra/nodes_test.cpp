#include <catch2/catch_test_macros.hpp>
#include <type_traits>

import dedekind.algebra;
import dedekind.sets;

using namespace dedekind::algebra;


TEST_CASE("Symbolic Algebra: Axiomatic Proofs", "[algebra][static]") {

    SECTION("Monoid Proof: Strings") {
        // Theorem: std::string is a Monoid under concatenation (+)
        // It has identity ("") and is associative, but no inverse.
        STATIC_REQUIRE(IsMonoid<std::string, std::plus<std::string>>);
        STATIC_REQUIRE_FALSE(IsGroup<std::string, std::plus<std::string>>);
        STATIC_REQUIRE_FALSE(is_commutative_v<std::string, std::plus<std::string>>);
    }

    SECTION("Ring Proof: Integers") {
        // Theorem: int is a Ring (Additive Abelian Group + Multiplicative Monoid)
        STATIC_REQUIRE(IsAbelianGroup<int, std::plus<int>>);
        STATIC_REQUIRE(IsMonoid<int, std::multiplies<int>>);
        STATIC_REQUIRE(IsRing<int>);
        
        // Theorem: int is NOT a Field (no multiplicative inverse for 2)
        STATIC_REQUIRE_FALSE(IsField<int>);
    }

    SECTION("Field Proof: Floating Point") {
        // Theorem: double satisfies the Field axioms (approximate math aside)
        STATIC_REQUIRE(IsField<double>);
    }
}
