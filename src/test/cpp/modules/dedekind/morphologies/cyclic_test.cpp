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
  }

  SECTION("Successor Morphism (The Dedekind Chain)") {
    // Verify the internal value wrapping logic
    constexpr ℤ100 boundary_val{99};

    // Static check: f(99) = 0
    static_assert(ℤ100::successor(boundary_val) == 0);

    // Generator check: The unit of the chain
    static_assert(ℤ100::generator() == 1);
  }

  SECTION("Modular Arithmetic") {
    constexpr ℤ100 a{70};
    constexpr ℤ100 b{50};
    static_assert(static_cast<int>(a + b) == 20);
    static_assert(static_cast<int>(a * b) == 0);
  }
}
