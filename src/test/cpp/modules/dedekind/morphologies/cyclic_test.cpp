#include <catch2/catch_test_macros.hpp>

import dedekind.category;
import dedekind.algebra;
import dedekind.sets;
import dedekind.order; // the translation graph (Ω*Ω | π1+fix==π2) for preimage
import dedekind.morphologies;

using namespace dedekind::category;
using namespace dedekind::algebra;
using namespace dedekind::sets;
using namespace dedekind::order;
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

// The residue-class preimage (integral.cppm, FIXME #797): pulling a Congruence
// back through the translation x↦x+K rotates the class by −K (mod N).  The
// bound-PRESERVING closed form the torus needs — the residue sibling of the
// halfspace preimage.
TEST_CASE("Congruence residue preimage: {x≡R mod N} ⟵ x↦x+K rotates by −K",
          "[morphologies][congruence][preimage][residue]") {
  constexpr auto ℤ = Ω<SignedCardinality>;
  // preimage(x↦x+1, {x≡2 mod4}) = {x≡1 mod4}: the class rotates by −1.
  constexpr auto pre =
      preimage(ℤ * ℤ | π1 + fix(1_c) == π2, Congruence<4, 2>{});
  CHECK(pre(1));        // 1 ≡ 1 (mod 4)
  CHECK(pre(5));        // 5 ≡ 1 (mod 4)
  CHECK_FALSE(pre(2));  // 2 ≢ 1 (mod 4)
  // Defining property: preimage(x↦x+K, C)(a) ⟺ C(a+K), over a full period.
  for (int a = 0; a < 12; ++a) CHECK(pre(a) == Congruence<4, 2>{}(a + 1));
}
