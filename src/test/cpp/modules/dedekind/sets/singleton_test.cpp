#include <catch2/catch_test_macros.hpp>
import dedekind.category;
import dedekind.sets;

using namespace dedekind::category;
using namespace dedekind::sets;

TEST_CASE("Sets: Singleton Final Proof: The Highway",
          "[sets][singleton][highway]") {
  SECTION("1. The Singleton Lifting") {
    // Target the struct directly to satisfy Clang's template-template rules
    auto atom = 42 >> into<SingletonSet>;
    REQUIRE(atom(42) == true);
  }

  SECTION("2. The Pull from the Identity (ε)") {
    // Pushing into the set and pulling the value back out
    // Note: SingletonSet must provide extract_v to satisfy IsPreComonad
    int value = 42 >> into<SingletonSet> << extract<SingletonSet>;

    REQUIRE(value == 42);
    static_assert((7 >> into<SingletonSet> << extract<SingletonSet>) == 7,
                  "The Round-trip Axiom.");
  }
}

/*
TEST_CASE("Sets: Composition of Operations: The Functor Highway",
          "[sets][composition][monad]") {

  SECTION("1. Forward Monadic Composition (Bind)") {
    // Define our atomic operations as lambda predicates/transformers
    auto plus_one = [](int x) { return x + 1; };
    auto times_two = [](int x) { return x * 2; };

    // Start with a value, lift it into a SingletonSet,
    // and compose the operations using the monadic "Push" (>>=)
    // Math: η(5) >>= (x -> η(x + 1)) >>= (x -> η(x * 2))
    auto result_set = 5 >> into<SingletonSet>
                        >>= [plus_one](int x) { return plus_one(x) >>
into<SingletonSet>; }
                        >>= [times_two](int x) { return times_two(x) >>
into<SingletonSet>; };

    // The result should be a SingletonSet containing (5 + 1) * 2 = 12
    REQUIRE(result_set(12) == true);
    REQUIRE(result_set(6) == false);
  }

  SECTION("2. Compile-Time Semantic Mapping of Composition") {
    // Validating the "Zero-Overhead" claim from Page 1
    // The compiler should resolve the composition to a constant 12
    static_assert((5 >> into<SingletonSet>
                     >>= [](int x) { return (x + 1) >> into<SingletonSet>; }
                     >>= [](int x) { return (x * 2) >> into<SingletonSet>; }
                     << extract<SingletonSet>) == 12,
                  "The Composition Axiom must be resolved at compile-time.");
  }

}
                    */
