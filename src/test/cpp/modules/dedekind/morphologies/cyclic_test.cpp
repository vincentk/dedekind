#include <catch2/catch_test_macros.hpp>

import dedekind.category;
import dedekind.algebra;
import dedekind.sets;
import dedekind.morphologies;

using namespace dedekind::category;
using namespace dedekind::algebra;
using namespace dedekind::sets;
using namespace dedekind::morphologies;

TEST_CASE("Morphology Verification: The Cyclic Ring",
          "[morphologies][cyclic]") {
  // Reifying Z/100Z using our new proper class
  using ℤ100 = CyclicRing<int, 100>;

  SECTION("Axiomatic Verification") {
    // Does it satisfy the Level 3.5 Morphology concepts?
    static_assert(IsCyclic<ℤ100>);
    static_assert(IsCyclicRing<ℤ100>);

    // Ensure it is NOT simply infinite (it is bounded/finite)
    static_assert(!IsSimplyInfinite<ℤ100>);
  }

  SECTION("Successor Morphism (The Dedekind Chain)") {
    // Verify the internal value wrapping logic
    constexpr ℤ100 boundary_val{99};

    // Static check: f(99) = 0
    static_assert(ℤ100::successor(boundary_val) == 0);

    // Generator check: The unit of the chain
    static_assert(ℤ100::generator() == 1);
  }

  SECTION("Minkowski Summation with Boundaries") {
    /**
     * Test: The Minkowski sum over a Cyclic Species.
     * Using the Empty Set (Ø) and Universal Set (Ω).
     */
    constexpr Ø<ℤ100> null;
    constexpr Ω<ℤ100> universe;

    // 1. The Annihilator: AnySet + ∅ = ∅
    // Even the entire "Universe" of Z100 becomes nothing when summed with
    // nothing.
    static_assert(std::is_same_v<decltype(universe + null), Ø<ℤ100>>);

    // 2. The Identity: Since we haven't defined a SingletonSet{0} here,
    // we verify that the boundaries maintain their types.
    static_assert(std::is_same_v<decltype(null + null), Ø<ℤ100>>);

    // 3. The Whole: Ω + Ω = Ω
    static_assert(std::is_same_v<decltype(universe + universe), Ω<ℤ100>>);
  }
}
