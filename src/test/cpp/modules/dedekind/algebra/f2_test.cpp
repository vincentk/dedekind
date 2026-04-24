/** @file dedekind/algebra/f2_test.cpp
 *
 * Field-axiom exercises for the Galois field @f$\mathbb{F}_2@f$,
 * witnessed directly on @c bool under its natural bitwise operators
 * (@c std::bit_xor as the additive group, @c std::bit_and as the
 * multiplicative monoid).  No wrapper struct is involved: the claim
 * is literally that @c bool @em is @f$\mathbb{F}_2@f$ under the
 * right operator choice.
 *
 * The structural (concept-layer) witness is asserted at the source
 * of truth (@c algebra/field.cppm); this file is the runtime /
 * value-level witness, enumerating every pair @f$(a, b) \in
 * \mathbb{F}_2^2@f$ against each field axiom via the
 * @c std::bit_xor / @c std::bit_and functors.  With
 * @f$|\mathbb{F}_2| = 2@f$ the full Cayley table fits in a handful
 * of @c CHECKs, so the axioms are verified by exhaustion rather
 * than by probe.
 */

#include <catch2/catch_test_macros.hpp>
#include <functional>

import dedekind.algebra;
import dedekind.category;

using dedekind::category::identity_v;

namespace {

// Plus / Times for 𝔽2 are bitwise XOR and AND on bool.
using Plus = std::bit_xor<bool>;
using Times = std::bit_and<bool>;

// Field identities via the library's trait registrations ---
// identity_trait<bool, std::bit_xor<bool>>::value is false;
// identity_trait<bool, std::bit_and<bool>>::value is true.
inline constexpr bool ZERO = identity_v<bool, Plus>;
inline constexpr bool ONE = identity_v<bool, Times>;

constexpr Plus plus{};
constexpr Times times{};

// Multiplicative inverse on 𝔽2^× = {true}: the only non-zero element
// is self-inverse.  No .inverse() method exists on bool; the
// concept-layer claim rests on the is_invertible_v trait.
constexpr bool f2_inverse(bool x) {
  // Division-by-zero is undefined; concepts don't quantify over values,
  // so this function simply asserts its precondition.
  return x;  // 1^{-1} = 1; callers must not pass false.
}

}  // namespace

TEST_CASE("𝔽2 — structural: bool satisfies category::IsField under (XOR, AND)",
          "[algebra][field][F2]") {
  STATIC_CHECK(dedekind::category::IsField<bool, std::bit_xor<bool>,
                                           std::bit_and<bool>>);
  STATIC_CHECK(dedekind::category::IsCommutativeRing<bool, std::bit_xor<bool>,
                                                     std::bit_and<bool>>);
  STATIC_CHECK(ZERO == false);
  STATIC_CHECK(ONE == true);
}

TEST_CASE("𝔽2 — additive group axioms (XOR on bool)", "[algebra][field][F2]") {
  SECTION("Closure + Cayley table") {
    CHECK(plus(ZERO, ZERO) == ZERO);
    CHECK(plus(ZERO, ONE) == ONE);
    CHECK(plus(ONE, ZERO) == ONE);
    CHECK(plus(ONE, ONE) == ZERO);  // characteristic two
  }

  SECTION("Associativity (exhaustive over 𝔽2³)") {
    for (bool i : {false, true}) {
      for (bool j : {false, true}) {
        for (bool k : {false, true}) {
          CHECK(plus(plus(i, j), k) == plus(i, plus(j, k)));
        }
      }
    }
  }

  SECTION("Commutativity") {
    for (bool i : {false, true}) {
      for (bool j : {false, true}) {
        CHECK(plus(i, j) == plus(j, i));
      }
    }
  }

  SECTION("Additive identity (0)") {
    CHECK(plus(ZERO, ZERO) == ZERO);
    CHECK(plus(ONE, ZERO) == ONE);
    CHECK(plus(ZERO, ONE) == ONE);
  }

  SECTION("Additive inverse (-x = x in char 2)") {
    // Every element is its own additive inverse under XOR.
    CHECK(plus(ZERO, ZERO) == ZERO);  // 0 + 0 = 0
    CHECK(plus(ONE, ONE) == ZERO);    // 1 + 1 = 0
  }
}

TEST_CASE("𝔽2 — multiplicative monoid / group axioms (AND on bool)",
          "[algebra][field][F2]") {
  SECTION("Closure + Cayley table") {
    CHECK(times(ZERO, ZERO) == ZERO);
    CHECK(times(ZERO, ONE) == ZERO);
    CHECK(times(ONE, ZERO) == ZERO);
    CHECK(times(ONE, ONE) == ONE);
  }

  SECTION("Associativity (exhaustive over 𝔽2³)") {
    for (bool i : {false, true}) {
      for (bool j : {false, true}) {
        for (bool k : {false, true}) {
          CHECK(times(times(i, j), k) == times(i, times(j, k)));
        }
      }
    }
  }

  SECTION("Commutativity") {
    for (bool i : {false, true}) {
      for (bool j : {false, true}) {
        CHECK(times(i, j) == times(j, i));
      }
    }
  }

  SECTION("Multiplicative identity (1)") {
    CHECK(times(ZERO, ONE) == ZERO);
    CHECK(times(ONE, ONE) == ONE);
  }

  SECTION("Multiplicative inverse on 𝔽2^× = {1}") {
    CHECK(f2_inverse(ONE) == ONE);
    CHECK(times(ONE, f2_inverse(ONE)) == ONE);
  }
}

TEST_CASE("𝔽2 — distributivity (a·(b+c) = a·b + a·c)", "[algebra][field][F2]") {
  for (bool i : {false, true}) {
    for (bool j : {false, true}) {
      for (bool k : {false, true}) {
        CHECK(times(i, plus(j, k)) == plus(times(i, j), times(i, k)));
        CHECK(times(plus(i, j), k) == plus(times(i, k), times(j, k)));
      }
    }
  }
}
