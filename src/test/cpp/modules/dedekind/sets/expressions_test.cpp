#include <catch2/catch_test_macros.hpp>

// Import our Dedekind modules
import dedekind.sets;

using namespace dedekind::sets;

TEST_CASE("Dedekind MVP: Basic Membership and Symbols", "[sets]") {
  SECTION("Integer Universe Membership") {
auto x = var<Ω<int>>; // A variable representing an element of the integer universe

    // Should be Set<int, ClassicalLogic>
    auto finite = Set{ x % singleton(1) | (x == 1) };

    // Should be Set<int, TernaryLogic> (because ℕ is transfinite)
    auto infinite = Set{ x % ℕ | (x > 0) }; 
  }
  }
