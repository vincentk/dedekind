#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <functional>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE("Category: Morphisms and Arrow Factories", "[category][morphisms]") {
  SECTION("Identity Morphism (id)") {
    auto identity = id<int>();

    // Exercise the Identity struct's operator()
    CHECK(identity(42) == 42);
    CHECK(identity(-1) == -1);

    // Verify type traits at runtime
    STATIC_CHECK(std::same_as<typename decltype(identity)::Domain, int>);
  }

  SECTION("Arrow Factory (Morphism Tagging)") {
    // Tagging a lambda as a Morphism from int to bool
    auto is_even = arrow<int, bool>([](int x) { return x % 2 == 0; });

    CHECK(is_even(2) == true);
    CHECK(is_even(3) == false);

    // Verify it satisfies the IsArrow concept
    STATIC_CHECK(IsArrow<decltype(is_even)>);
  }

  SECTION("Endomorphism Factory (endo)") {
    // Endomorphisms preserve the species (Domain == Codomain)
    auto square = endo<int>([](int x) { return x * x; });

    CHECK(square(5) == 25);
    CHECK(square(-3) == 9);
  }

  SECTION("Zero Morphism (Absorption)") {
    // Zero morphism maps everything to the identity of the target monoid
    // For (int, +), identity is 0.
    auto absorb_to_zero = zero<double, int, std::plus<int>>();

    CHECK(absorb_to_zero(3.14) == 0);
    CHECK(absorb_to_zero(-99.9) == 0);

    // For (bool, &&), identity is true.
    auto absorb_to_true = zero<int, bool, std::logical_and<bool>>();

    CHECK(absorb_to_true(42) == true);
  }

  SECTION("Composition (The Highway Flow)") {
    auto f = endo<int>([](int x) { return x + 1; });
    auto g = endo<int>([](int x) { return x * 2; });

    // Compose: h = g . f (x -> (x+1)*2)
    auto h = f >> g;

    CHECK(h(5) == 12);
    CHECK(h(0) == 2);
  }
}

TEST_CASE("Category: Algebraic Proofs (Runtime Witnesses)",
          "[category][morphisms][algebra]") {
  SECTION("Small Category Identity Proof") {
    // Verify that the identity_trait is correctly resolved for primitive
    // species.
    CHECK(identity_v<int, std::plus<int>> == 0);
    CHECK(identity_v<bool, std::logical_and<bool>> == true);
    CHECK(identity_v<bool, std::logical_or<bool>> == false);
  }
}
