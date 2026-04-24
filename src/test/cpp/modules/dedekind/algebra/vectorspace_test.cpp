/** @file dedekind/algebra/vectorspace_test.cpp
 *
 * Tests for the @c :vectorspace partition's two vector-space
 * witnesses:
 *
 *   - @c 𝔽64 as a 6-D vector space over @f$\mathbb{F}_2@f$
 *     (scalar side witnessed on @c bool under @c (XOR, AND)).
 *   - @c uint64_t as @f$\mathbb{F}_2^{64}@f$, the 64-D vector space
 *     over @f$\mathbb{F}_2@f$ (additive group @c (uint64_t, XOR);
 *     scalar action @c Bit2U64Action).
 *
 * Structural @c IsVectorSpace claims are asserted at the source of
 * truth in @c algebra/vectorspace.cppm; this file witnesses the
 * four vector-space axioms at value level on each carrier:
 *
 *   (V1)  (α + β) · v  =  α · v + β · v        (scalar additivity)
 *   (V2)   α · (v + w) = α · v + α · w         (vector additivity)
 *   (V3)  (α · β) · v  =  α · (β · v)          (action composition)
 *   (V4)      1 · v    =  v                    (unit axiom)
 *
 * For @c 𝔽64 over @c bool the scalar side is tiny (2 elements); for
 * @c uint64_t over @c bool the carrier is 2^64 but the scalar is
 * still just 2 elements, so all axioms are value-exhaustive in the
 * scalar and probe-based in the vector.
 */

#include <catch2/catch_test_macros.hpp>
#include <cstdint>
#include <functional>

import dedekind.algebra;
import dedekind.category;

using dedekind::algebra::Bit2U64Action;
using dedekind::algebra::𝔽64;
using dedekind::category::identity_v;

namespace {

inline constexpr 𝔽64 ZERO64 = identity_v<𝔽64, std::plus<𝔽64>>;
inline constexpr bool F2_ZERO = identity_v<bool, std::bit_xor<bool>>;
inline constexpr bool F2_ONE = identity_v<bool, std::bit_and<bool>>;

constexpr std::bit_xor<bool> f2_plus{};
constexpr std::bit_and<bool> f2_times{};

// A small pool of 𝔽64 probe vectors chosen to exercise all bit
// positions (low bits, middle bits, high bits, mixed).
constexpr std::uint8_t F64_PROBES[] = {
    0x00, 0x01, 0x02, 0x0B, 0x17, 0x2A, 0x3F,
};

// And a pool of 𝔽_2^{64} (uint64_t) probes.
constexpr std::uint64_t U64_PROBES[] = {
    0x0000000000000000ull, 0x0000000000000001ull, 0xFFFFFFFFFFFFFFFFull,
    0xDEADBEEFDEADBEEFull, 0x0123456789ABCDEFull, 0xCAFEBABEBAADF00Dull,
};

}  // namespace

// =====================================================================
// 𝔽64 as a 6-D vector space over 𝔽2 (scalar = bool under (XOR, AND))
// =====================================================================

TEST_CASE("VS(𝔽64, 𝔽2) — unit axiom: 1 · v = v",
          "[algebra][vectorspace][F64]") {
  for (std::uint8_t v : F64_PROBES) {
    const 𝔽64 x{v};
    CHECK(F2_ONE * x == x);
    CHECK(F2_ZERO * x == ZERO64);  // annihilator: 0 · v = 0
  }
}

TEST_CASE("VS(𝔽64, 𝔽2) — vector additivity: α · (v + w) = α · v + α · w",
          "[algebra][vectorspace][F64]") {
  for (bool a : {false, true}) {
    for (std::uint8_t i : F64_PROBES) {
      for (std::uint8_t j : F64_PROBES) {
        const 𝔽64 x{i}, y{j};
        CHECK(a * (x + y) == (a * x) + (a * y));
      }
    }
  }
}

TEST_CASE("VS(𝔽64, 𝔽2) — scalar additivity: (α + β) · v = α · v + β · v",
          "[algebra][vectorspace][F64]") {
  for (bool a : {false, true}) {
    for (bool b : {false, true}) {
      for (std::uint8_t v : F64_PROBES) {
        const 𝔽64 x{v};
        CHECK(f2_plus(a, b) * x == (a * x) + (b * x));
      }
    }
  }
}

TEST_CASE("VS(𝔽64, 𝔽2) — action composition: (α · β) · v = α · (β · v)",
          "[algebra][vectorspace][F64]") {
  for (bool a : {false, true}) {
    for (bool b : {false, true}) {
      for (std::uint8_t v : F64_PROBES) {
        const 𝔽64 x{v};
        CHECK(f2_times(a, b) * x == a * (b * x));
      }
    }
  }
}

// =====================================================================
// uint64_t as 𝔽_2^{64}, the 64-D vector space over 𝔽2
// =====================================================================

TEST_CASE("VS(𝔽_2^{64}, 𝔽2) — action functor: Bit2U64Action",
          "[algebra][vectorspace][F2_64]") {
  constexpr Bit2U64Action act{};
  // false · v = 0, true · v = v for every uint64_t v.
  for (std::uint64_t v : U64_PROBES) {
    CHECK(act(false, v) == 0ull);
    CHECK(act(true, v) == v);
  }
}

TEST_CASE("VS(𝔽_2^{64}, 𝔽2) — unit axiom: 1 · v = v",
          "[algebra][vectorspace][F2_64]") {
  constexpr Bit2U64Action act{};
  for (std::uint64_t v : U64_PROBES) {
    CHECK(act(F2_ONE, v) == v);
    CHECK(act(F2_ZERO, v) == 0ull);  // annihilator
  }
}

TEST_CASE(
    "VS(𝔽_2^{64}, 𝔽2) — vector additivity: α · (v ⊕ w) = (α · v) ⊕ (α · w)",
    "[algebra][vectorspace][F2_64]") {
  constexpr Bit2U64Action act{};
  constexpr std::bit_xor<std::uint64_t> add64{};
  for (bool a : {false, true}) {
    for (std::uint64_t v : U64_PROBES) {
      for (std::uint64_t w : U64_PROBES) {
        CHECK(act(a, add64(v, w)) == add64(act(a, v), act(a, w)));
      }
    }
  }
}

TEST_CASE(
    "VS(𝔽_2^{64}, 𝔽2) — scalar additivity: (α ⊕ β) · v = (α · v) ⊕ (β · v)",
    "[algebra][vectorspace][F2_64]") {
  constexpr Bit2U64Action act{};
  constexpr std::bit_xor<std::uint64_t> add64{};
  for (bool a : {false, true}) {
    for (bool b : {false, true}) {
      for (std::uint64_t v : U64_PROBES) {
        CHECK(act(f2_plus(a, b), v) == add64(act(a, v), act(b, v)));
      }
    }
  }
}

TEST_CASE("VS(𝔽_2^{64}, 𝔽2) — action composition: (α ∧ β) · v = α · (β · v)",
          "[algebra][vectorspace][F2_64]") {
  constexpr Bit2U64Action act{};
  for (bool a : {false, true}) {
    for (bool b : {false, true}) {
      for (std::uint64_t v : U64_PROBES) {
        CHECK(act(f2_times(a, b), v) == act(a, act(b, v)));
      }
    }
  }
}

TEST_CASE("VS(𝔽_2^{64}, 𝔽2) — additive group: XOR on uint64_t",
          "[algebra][vectorspace][F2_64]") {
  constexpr std::bit_xor<std::uint64_t> add64{};
  SECTION("Identity: v ⊕ 0 = v") {
    for (std::uint64_t v : U64_PROBES) {
      CHECK(add64(v, 0ull) == v);
    }
  }
  SECTION("Self-inverse: v ⊕ v = 0") {
    for (std::uint64_t v : U64_PROBES) {
      CHECK(add64(v, v) == 0ull);
    }
  }
  SECTION("Associativity + commutativity (probe)") {
    const std::uint64_t a = U64_PROBES[3];
    const std::uint64_t b = U64_PROBES[4];
    const std::uint64_t c = U64_PROBES[5];
    CHECK(add64(a, b) == add64(b, a));
    CHECK(add64(add64(a, b), c) == add64(a, add64(b, c)));
  }
}
