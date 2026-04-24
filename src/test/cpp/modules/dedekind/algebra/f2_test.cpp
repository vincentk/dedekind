/** @file dedekind/algebra/f2_test.cpp
 *
 * Field-axiom exercises for the prototypical carrier @c 𝔽2, the
 * Galois field of order two.  The concrete witness for
 * @c algebra::IsField --- the smallest possible carrier that reaches
 * through the whole axiomatic / operator stack (abelian group under
 * addition, abelian group on non-zero multiplicative elements,
 * distributivity, division, inverses).
 *
 * Structural witness (the concept chain proper) is already asserted
 * at the source of truth, alongside the struct in
 * @c algebra/field.cppm.  This file is the runtime / value-level
 * witness: it enumerates every pair (a, b) ∈ 𝔽2² and checks each
 * field axiom.  With |𝔽2| = 2 the whole Cayley table fits in a
 * handful of @c CHECKs, so the axioms can be verified by exhaustion
 * rather than by probe.
 */

#include <catch2/catch_test_macros.hpp>
#include <functional>
#include <stdexcept>

import dedekind.algebra;
import dedekind.category;

using dedekind::algebra::𝔽2;
using dedekind::category::identity_v;

namespace {

// The field identities come from the library's trait registrations
// (`identity_trait<𝔽2, std::plus<𝔽2>>` and
// `identity_trait<𝔽2, std::multiplies<𝔽2>>`, defined alongside the
// struct in `algebra/field.cppm`). Exposing them here as `ZERO` /
// `ONE` is a readability alias, not a fresh definition: every
// operator assertion in this file is a fact \emph{about} the
// canonical identities the concept chain already certifies.
inline constexpr auto ZERO = identity_v<𝔽2, std::plus<𝔽2>>;
inline constexpr auto ONE = identity_v<𝔽2, std::multiplies<𝔽2>>;

}  // namespace

TEST_CASE("𝔽2 — structural: IsField holds at both layers",
          "[algebra][field][F2]") {
  STATIC_CHECK(
      dedekind::category::IsField<𝔽2, std::plus<𝔽2>, std::multiplies<𝔽2>>);
  STATIC_CHECK(
      dedekind::algebra::IsField<𝔽2, std::plus<𝔽2>, std::multiplies<𝔽2>>);
}

TEST_CASE("𝔽2 — additive group axioms", "[algebra][field][F2]") {
  SECTION("Closure + Cayley table") {
    CHECK(ZERO + ZERO == ZERO);
    CHECK(ZERO + ONE == ONE);
    CHECK(ONE + ZERO == ONE);
    CHECK(ONE + ONE == ZERO);  // characteristic two
  }

  SECTION("Associativity (exhaustive over 𝔽2³)") {
    for (bool i : {false, true}) {
      for (bool j : {false, true}) {
        for (bool k : {false, true}) {
          const 𝔽2 a{i}, b{j}, c{k};
          CHECK((a + b) + c == a + (b + c));
        }
      }
    }
  }

  SECTION("Commutativity") {
    for (bool i : {false, true}) {
      for (bool j : {false, true}) {
        const 𝔽2 a{i}, b{j};
        CHECK(a + b == b + a);
      }
    }
  }

  SECTION("Additive identity (0)") {
    CHECK(ZERO + ZERO == ZERO);
    CHECK(ONE + ZERO == ONE);
  }

  SECTION("Additive inverse (-x = x in char 2)") {
    CHECK(-ZERO == ZERO);
    CHECK(-ONE == ONE);
    CHECK(ZERO + (-ZERO) == ZERO);
    CHECK(ONE + (-ONE) == ZERO);
  }

  SECTION("Subtraction coincides with addition") {
    CHECK(ONE - ONE == ZERO);
    CHECK(ONE - ZERO == ONE);
    CHECK(ZERO - ONE == ONE);
  }
}

TEST_CASE("𝔽2 — multiplicative monoid / group axioms", "[algebra][field][F2]") {
  SECTION("Closure + Cayley table") {
    CHECK(ZERO * ZERO == ZERO);
    CHECK(ZERO * ONE == ZERO);
    CHECK(ONE * ZERO == ZERO);
    CHECK(ONE * ONE == ONE);
  }

  SECTION("Associativity (exhaustive over 𝔽2³)") {
    for (bool i : {false, true}) {
      for (bool j : {false, true}) {
        for (bool k : {false, true}) {
          const 𝔽2 a{i}, b{j}, c{k};
          CHECK((a * b) * c == a * (b * c));
        }
      }
    }
  }

  SECTION("Commutativity") {
    for (bool i : {false, true}) {
      for (bool j : {false, true}) {
        const 𝔽2 a{i}, b{j};
        CHECK(a * b == b * a);
      }
    }
  }

  SECTION("Multiplicative identity (1)") {
    CHECK(ZERO * ONE == ZERO);
    CHECK(ONE * ONE == ONE);
  }

  SECTION("Multiplicative inverse on 𝔽2^× = {1}") {
    CHECK(ONE.inverse() == ONE);
    CHECK(ONE * ONE.inverse() == ONE);
  }

  SECTION("Inverse of zero is a domain error") {
    CHECK_THROWS_AS(ZERO.inverse(), std::domain_error);
  }
}

TEST_CASE("𝔽2 — distributivity (a·(b+c) = a·b + a·c)", "[algebra][field][F2]") {
  for (bool i : {false, true}) {
    for (bool j : {false, true}) {
      for (bool k : {false, true}) {
        const 𝔽2 a{i}, b{j}, c{k};
        CHECK(a * (b + c) == (a * b) + (a * c));
        CHECK((a + b) * c == (a * c) + (b * c));
      }
    }
  }
}

TEST_CASE("𝔽2 — division surface", "[algebra][field][F2]") {
  SECTION("a / 1 == a") {
    CHECK(ZERO / ONE == ZERO);
    CHECK(ONE / ONE == ONE);
  }

  SECTION("a / b == a · b⁻¹ for b ≠ 0") {
    for (bool i : {false, true}) {
      const 𝔽2 a{i};
      CHECK(a / ONE == a * ONE.inverse());
    }
  }

  SECTION("Division by zero throws") {
    CHECK_THROWS_AS(ONE / ZERO, std::domain_error);
    CHECK_THROWS_AS(ZERO / ZERO, std::domain_error);
  }

  SECTION("std::divides witness is consistent") {
    CHECK(std::divides<𝔽2>{}(ONE, ONE) == ONE);
    CHECK(std::divides<𝔽2>{}(ZERO, ONE) == ZERO);
  }
}

TEST_CASE("𝔽2 — construction from bool", "[algebra][field][F2]") {
  CHECK(𝔽2{false} == ZERO);
  CHECK(𝔽2{true} == ONE);
  CHECK(𝔽2{} == ZERO);  // default constructor
  STATIC_CHECK(sizeof(𝔽2) == sizeof(bool));
}
