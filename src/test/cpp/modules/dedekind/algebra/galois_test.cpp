/** @file dedekind/algebra/galois_test.cpp
 *
 * Tests for the @c :galois partition: the @c IsGaloisField concept,
 * the @c is_galois_field_v / @c galois_order_v traits, and the two
 * registered Galois-field witnesses --- @c bool under @c (XOR, AND)
 * as @f$\mathbb{F}_2@f$, and @c 𝔽64 as
 * @f$\mathrm{GF}(2^6) = \mathbb{F}_2[x]/(x^6 + x + 1)@f$.
 *
 * Structural claims (concept chain + trait opt-ins) are asserted at
 * the source of truth in @c algebra/galois.cppm; this file is the
 * runtime / value-level witness for @c 𝔽64 (the 𝔽2 Cayley tables
 * are already exhausted in @c f2_test.cpp).
 *
 * The tests exercise:
 *   - Structural @c IsGaloisField + order trait for both carriers.
 *   - Full field axioms on @c 𝔽64 by probe (closure, associativity,
 *     commutativity, distributivity, identities).
 *   - Multiplicative-inverse exhaustion over the 63 non-zero
 *     elements of @f$\mathbb{F}_{64}^{\times}@f$ (the cyclic
 *     group).
 *   - Division surface, and Fermat @f$a^{q-1} = 1@f$ for every
 *     non-zero element (with @f$q = 64@f$ the field's order, so
 *     @f$a^{63} = 1@f$ and @f$a^{62} = a^{-1}@f$).
 *   - Characteristic-two identity: @f$-x = x@f$.
 *   - Constructor range canonicalisation (bits 6--7 dropped).
 */

#include <catch2/catch_test_macros.hpp>
#include <algorithm>
#include <cstdint>
#include <functional>
#include <ranges>
#include <set>

import dedekind.algebra;
import dedekind.category;
import dedekind.sequences;

using dedekind::algebra::galois_order_v;
using dedekind::algebra::is_galois_field_v;
using dedekind::algebra::IsGaloisField;
using dedekind::algebra::𝔽64;
using dedekind::category::identity_v;

namespace {

inline constexpr 𝔽64 ZERO64 = identity_v<𝔽64, std::plus<𝔽64>>;
inline constexpr 𝔽64 ONE64 = identity_v<𝔽64, std::multiplies<𝔽64>>;

constexpr 𝔽64 pow(𝔽64 a, int n) {
  𝔽64 result = ONE64;
  𝔽64 base = a;
  while (n > 0) {
    if (n & 1) result = result * base;
    base = base * base;
    n >>= 1;
  }
  return result;
}

}  // namespace

TEST_CASE("Galois — structural: bool is the Galois field 𝔽2",
          "[algebra][galois][F2]") {
  STATIC_CHECK(IsGaloisField<bool, std::bit_xor<bool>, std::bit_and<bool>>);
  STATIC_CHECK(is_galois_field_v<bool, std::bit_xor<bool>, std::bit_and<bool>>);
  STATIC_CHECK(galois_order_v<bool, std::bit_xor<bool>, std::bit_and<bool>> ==
               2);
}

TEST_CASE("Galois — structural: 𝔽64 is the Galois field of order 64",
          "[algebra][galois][F64]") {
  STATIC_CHECK(IsGaloisField<𝔽64, std::plus<𝔽64>, std::multiplies<𝔽64>>);
  STATIC_CHECK(is_galois_field_v<𝔽64, std::plus<𝔽64>, std::multiplies<𝔽64>>);
  STATIC_CHECK(galois_order_v<𝔽64, std::plus<𝔽64>, std::multiplies<𝔽64>> == 64);
}

TEST_CASE("𝔽64 — constructor canonicalises to 6 bits",
          "[algebra][galois][F64]") {
  // Bits above bit 5 are masked off.
  CHECK(𝔽64{static_cast<std::uint8_t>(0x40)} == ZERO64);
  CHECK(𝔽64{static_cast<std::uint8_t>(0x80)} == ZERO64);
  CHECK(𝔽64{static_cast<std::uint8_t>(0x41)} ==
        𝔽64{static_cast<std::uint8_t>(0x01)});
  CHECK(𝔽64{static_cast<std::uint8_t>(0xFF)} ==
        𝔽64{static_cast<std::uint8_t>(0x3F)});
  STATIC_CHECK(sizeof(𝔽64) == sizeof(std::uint8_t));
}

TEST_CASE("𝔽64 — additive group axioms (XOR)", "[algebra][galois][F64]") {
  SECTION("Cayley: x + 0 = x") {
    for (std::uint8_t v = 0; v < 64; ++v) {
      const 𝔽64 x{v};
      CHECK(x + ZERO64 == x);
      CHECK(ZERO64 + x == x);
    }
  }

  SECTION("Characteristic two: x + x = 0, -x = x") {
    for (std::uint8_t v = 0; v < 64; ++v) {
      const 𝔽64 x{v};
      CHECK(x + x == ZERO64);
      CHECK(-x == x);
      CHECK(x - x == ZERO64);
    }
  }

  SECTION("Commutativity + associativity (probe)") {
    const 𝔽64 a{std::uint8_t{0x2D}};
    const 𝔽64 b{std::uint8_t{0x11}};
    const 𝔽64 c{std::uint8_t{0x3B}};
    CHECK(a + b == b + a);
    CHECK((a + b) + c == a + (b + c));
  }
}

TEST_CASE("𝔽64 — multiplicative monoid axioms", "[algebra][galois][F64]") {
  SECTION("Cayley: x · 1 = x, x · 0 = 0") {
    for (std::uint8_t v = 0; v < 64; ++v) {
      const 𝔽64 x{v};
      CHECK(x * ONE64 == x);
      CHECK(ONE64 * x == x);
      CHECK(x * ZERO64 == ZERO64);
      CHECK(ZERO64 * x == ZERO64);
    }
  }

  SECTION("Commutativity + associativity (probe)") {
    const 𝔽64 a{std::uint8_t{0x05}};  // x^2 + 1
    const 𝔽64 b{std::uint8_t{0x13}};  // x^4 + x + 1
    const 𝔽64 c{std::uint8_t{0x2A}};  // x^5 + x^3 + x
    CHECK(a * b == b * a);
    CHECK((a * b) * c == a * (b * c));
  }

  SECTION("Distributivity: a·(b+c) = a·b + a·c (probe)") {
    const 𝔽64 a{std::uint8_t{0x07}};
    const 𝔽64 b{std::uint8_t{0x1E}};
    const 𝔽64 c{std::uint8_t{0x29}};
    CHECK(a * (b + c) == (a * b) + (a * c));
    CHECK((a + b) * c == (a * c) + (b * c));
  }
}

TEST_CASE("𝔽64 — multiplicative inverse exhaustive over 𝔽64^× (63 elements)",
          "[algebra][galois][F64]") {
  for (std::uint8_t v = 1; v < 64; ++v) {  // skip zero
    const 𝔽64 x{v};
    const 𝔽64 xi = x.inverse();
    CHECK(x * xi == ONE64);
    CHECK(xi * x == ONE64);
  }
}

TEST_CASE("𝔽64 — Fermat: a^(q-1) = 1 and a^(q-2) = a^{-1} with q = 64",
          "[algebra][galois][F64]") {
  // |𝔽64^×| = q - 1 = 63, so a^63 = 1 (Fermat's little theorem for
  // finite fields); equivalently a^62 = a^{-1}.
  for (std::uint8_t v = 1; v < 64; ++v) {
    const 𝔽64 x{v};
    CHECK(pow(x, 63) == ONE64);
    CHECK(pow(x, 62) == x.inverse());
  }
}

TEST_CASE("𝔽64 — division surface: a / b = a · b^{-1}",
          "[algebra][galois][F64]") {
  SECTION("Division by zero throws") {
    const 𝔽64 x{std::uint8_t{0x17}};
    CHECK_THROWS_AS(x / ZERO64, std::domain_error);
    CHECK_THROWS_AS(ZERO64 / ZERO64, std::domain_error);
    CHECK_THROWS_AS(ZERO64.inverse(), std::domain_error);
  }

  SECTION("a / b = a · b^{-1} (probe; non-zero divisors)") {
    const 𝔽64 a{std::uint8_t{0x2F}};
    for (std::uint8_t v = 1; v < 64; ++v) {
      const 𝔽64 b{v};
      CHECK(a / b == a * b.inverse());
    }
  }

  SECTION("std::divides witness is consistent") {
    const 𝔽64 a{std::uint8_t{0x1A}};
    const 𝔽64 b{std::uint8_t{0x23}};
    CHECK(std::divides<𝔽64>{}(a, b) == a / b);
  }
}

TEST_CASE("𝔽64 — primitive element α = x generates 𝔽64^×",
          "[algebra][galois][F64]") {
  // α = x corresponds to bit pattern 0x02 (polynomial 0·1 + 1·x + 0·x^2 + ...).
  // Since x^6 + x + 1 is primitive, α has order 63 (generates 𝔽64^×).
  const 𝔽64 alpha{std::uint8_t{0x02}};
  𝔽64 power = ONE64;
  // Walk α^0, α^1, ..., α^62; must be all 63 distinct non-zero elements.
  bool seen[64] = {false};
  seen[0] = true;  // zero is not in 𝔽64^×, so it must not appear.
  for (int k = 0; k < 63; ++k) {
    CHECK(!(power == ZERO64));
    seen[power.value] = true;
    power = power * alpha;
  }
  CHECK(power == ONE64);  // α^63 = 1.
  for (int i = 0; i < 64; ++i) {
    CHECK(seen[i]);  // every element appeared exactly once (0 at the start).
  }
}

TEST_CASE(
    "𝔽64^× — f64_primitive_powers() coheres with the manual α^i walk (#388)",
    "[algebra][galois][F64][sequence]") {
  // The library's f64_primitive_powers() returns a FinitePath<𝔽64> of
  // size 63 whose i-th element is α^i.  This test pins the agreement
  // between the FinitePath surface and the manual walk: faithfulness
  // of the wrapper.  If a future edit reshuffled the at(i) semantics
  // (say, returned α^(i+1) by mistake), this test catches it.
  const auto seq = dedekind::algebra::f64_primitive_powers();
  REQUIRE(seq.size() == 63);

  const 𝔽64 alpha{std::uint8_t{0x02}};
  𝔽64 manual = 𝔽64{std::uint8_t{1}};
  for (std::size_t i = 0; i < 63; ++i) {
    CHECK(seq.at(i) == manual);
    manual = manual * alpha;
  }
  // After 63 multiplications by α we land on α^63 = 1 — the cyclic
  // group closes (the same fact the existing primitive-element test
  // proves manually, but here cross-checked through the FinitePath).
  CHECK(manual == 𝔽64{std::uint8_t{1}});
}

TEST_CASE("𝔽64^× — std::ranges anchor walks all 63 non-zero elements (#388)",
          "[algebra][galois][F64][sequence][ranges]") {
  // The math ↔ stdlib bidirectional anchor in action: drive the
  // FinitePath via std::ranges machinery and verify the cyclic-group
  // walk.  This is the today-available anchor (std::ranges::input_range)
  // that the IsSequence_vs_stdlib_388 doc block in :sequences:net
  // describes; this test is the runtime witness that the anchor
  // genuinely admits standard-algorithm consumers.
  const auto seq = dedekind::algebra::f64_primitive_powers();

  // std::ranges::distance over a sized_range yields the size.
  CHECK(std::ranges::distance(seq) == 63);

  // Walk via std::ranges::for_each: collect distinct values.
  std::set<std::uint8_t> seen;
  std::ranges::for_each(seq, [&](𝔽64 x) { seen.insert(x.value); });
  CHECK(seen.size() == 63);
  // Zero must NOT appear (the enumeration covers 𝔽64^× = 𝔽64 \ {0}).
  CHECK(!seen.contains(std::uint8_t{0}));
  // Every non-zero residue 1..63 must appear exactly once.
  for (std::uint8_t v = 1; v <= 63; ++v) {
    CHECK(seen.contains(v));
  }
}

TEST_CASE("𝔽64^× — IsFiniteSequence cardinality matches cyclic-group order",
          "[algebra][galois][F64][sequence][cyclic]") {
  // Cross-check between the categorical witness
  //   cyclic_order_v<𝔽64, std::multiplies<𝔽64>> == 63
  // (axiomatic, in :galois) and the operational FinitePath::size()
  // (the value-level walk).  Both must agree; if they didn't, the
  // operational and axiomatic views of 𝔽64^× would have drifted apart.
  const auto seq = dedekind::algebra::f64_primitive_powers();
  CHECK(seq.size() ==
        dedekind::category::cyclic_order_v<𝔽64, std::multiplies<𝔽64>>);
  CHECK(seq.size() ==
        dedekind::algebra::galois_order_v<𝔽64, std::plus<𝔽64>,
                                          std::multiplies<𝔽64>> -
            1);  // |𝔽_q^×| = q - 1.
}
