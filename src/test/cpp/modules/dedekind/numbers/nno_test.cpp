/**
 * @file nno_test.cpp
 * @brief Test suite for the @c category:nno partition.
 *
 * @section Scope
 * Exercises the Natural Numbers Object reification (closes part of
 * #445): the @c IsNNO concept's structural shape, the @c
 * iterate operational discharge of the universal property, and
 * the canonical @c Numeral witness registered in @c
 * numbers:natural.
 *
 * The architecture under test:
 *
 *   @c NNO  →  @c Numeral  →  @c ℕ
 *
 * — NNO is the Form (universal property), Numeral is the
 * canonical carrier witness, ℕ is the universe over it.
 */
#include <catch2/catch_test_macros.hpp>
#include <limits>
#include <optional>  // the cover / Lambek answers, 1 + N
#include <utility>

import dedekind.category;
import dedekind.numbers;
import dedekind.sets;

using namespace dedekind::category;
using namespace dedekind::numbers;
using namespace dedekind::sets;

// ===========================================================================
// (1) The successor on the canonical witness is an arrow (the IsNNO shape
//     itself is asserted at the definition site, numbers:natural)
// ===========================================================================

TEST_CASE("category:nno — Successor<Numeral> is an arrow ℕ → ℕ",
          "[category][nno][cardinality][witness]") {
  STATIC_CHECK(IsArrow<Successor<Numeral>>);
  STATIC_CHECK(std::same_as<Dom<Successor<Numeral>>, Numeral> &&
               std::same_as<Cod<Successor<Numeral>>, Numeral>);
}

// ===========================================================================
// (2) Zero and successor on Numeral behave as the textbook NNO requires
// ===========================================================================

TEST_CASE("category:nno — ZeroElement<Numeral> returns Numeral{0u}",
          "[category][nno][cardinality][zero]") {
  constexpr ZeroElement<Numeral> z{};
  CHECK(z() == Numeral{0u});
}

TEST_CASE("category:nno — Successor<Numeral> generates 1, 2, 3, ...",
          "[category][nno][cardinality][successor]") {
  constexpr ZeroElement<Numeral> z{};
  constexpr Successor<Numeral> s{};
  const auto zero = z();
  const auto one = s(zero);
  const auto two = s(one);
  const auto three = s(two);
  CHECK(one == Numeral{1u});
  CHECK(two == Numeral{2u});
  CHECK(three == Numeral{3u});
}

// ===========================================================================
// (3) iterate discharges the universal property at finite indices
// ===========================================================================

TEST_CASE(
    "category:nno — iterate(g, a₀, n) realises f(n) for the "
    "induced morphism f : ℕ → A",
    "[category][nno][universal-property][iterate]") {
  // Identity recursion: a₀ = 0, g = succ.  The unique f : ℕ → ℕ
  // produced by the universal property is the identity, materialised
  // at finite indices via iterate.
  const auto zero = ZeroElement<Numeral>{}();
  const auto step = Successor<Numeral>{};
  CHECK(dedekind::category::iterate(step, zero, 0u) == Numeral{0u});
  CHECK(dedekind::category::iterate(step, zero, 1u) == Numeral{1u});
  CHECK(dedekind::category::iterate(step, zero, 5u) == Numeral{5u});
  CHECK(dedekind::category::iterate(step, zero, 42u) == Numeral{42u});
}

TEST_CASE(
    "category:nno — Fibonacci as a coalgebra for F(X) = ℕ × X via "
    "iterate",
    "[category][nno][universal-property][iterate][fibonacci][coalgebra]") {
  // The proper categorical framing of two-term recurrences (per the
  // Gemini exchange in PR #444): Fibonacci is a coalgebra for the
  // functor F(X) = ℕ × X with state (F_n, F_{n-1}) and step
  //   (a, b) ↦ (a + b, a).
  // The unique morphism f : ℕ → A produced by the NNO universal
  // property over this coalgebra is the Fibonacci sequence;
  // iterate materialises f(n) for finite n.
  using State = std::pair<Numeral, Numeral>;
  // Initial state (F_1, F_0) = (1, 0); we step n times to get
  // (F_{n+1}, F_n), then read off the second component for F_n.
  const State a0 = {Numeral{1u}, Numeral{0u}};
  const auto g = [](const State& s) -> State {
    return {s.first + s.second, s.first};
  };
  // F_0 .. F_10 = 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55.
  CHECK(dedekind::category::iterate(g, a0, 0u).second == Numeral{0u});
  CHECK(dedekind::category::iterate(g, a0, 1u).second == Numeral{1u});
  CHECK(dedekind::category::iterate(g, a0, 2u).second == Numeral{1u});
  CHECK(dedekind::category::iterate(g, a0, 3u).second == Numeral{2u});
  CHECK(dedekind::category::iterate(g, a0, 5u).second == Numeral{5u});
  CHECK(dedekind::category::iterate(g, a0, 10u).second == Numeral{55u});
}

// ===========================================================================
// (4) The Form_Bias chain: NNO → Numeral → ℕ
// ===========================================================================

TEST_CASE(
    "category:nno — three-layer chain: NNO Form → Numeral carrier → ℕ "
    "alias",
    "[category][nno][form-bias][architecture]") {
  // Layer 1: NNO is the Form (universal property; concept-level), asserted
  // where the witness is declared (numbers:natural).

  // Layer 2: Numeral is the canonical carrier inhabiting the Form.
  STATIC_CHECK(std::same_as<decltype(ZeroElement<Numeral>{}()), Numeral>);
  STATIC_CHECK(
      std::same_as<decltype(Successor<Numeral>{}(Numeral{})), Numeral>);

  // Layer 3: ℕ is the universe 𝔸<Numeral>.  The textbook
  // symbol practitioners use; the NNO universal property is what it
  // _means_ via the chain.
  STATIC_CHECK(
      std::same_as<typename std::remove_cvref_t<decltype(ℕ)>::Domain, Numeral>);
}

// ===========================================================================
// (5) The cover, Lambek, and ++: the posture at the bounds, at runtime
// ===========================================================================

TEST_CASE("category:nno — the cover is partial at ⊤",
          "[category][nno][cover][saturation]") {
  const auto c41 = cover(Numeral{41u});
  REQUIRE(c41.has_value());
  CHECK(*c41 == Numeral{42u});
  CHECK(cover(Ternary::Unknown) == Ternary::True);
  CHECK_FALSE(cover(Ternary::True).has_value());
}

TEST_CASE(
    "category:nno — Lambek's lemma separates the NNO from its look-alikes",
    "[category][nno][lambek]") {
  // ℕ: [Z, S] is an iso.
  for (std::size_t k = 0; k < 6; ++k) {
    const auto n = Numeral{k};
    CHECK(In<Numeral>{}(Prior<Numeral>{}(n)) == n);
    const auto back = Prior<Numeral>{}(In<Numeral>{}(n));
    REQUIRE(back.has_value());
    CHECK(*back == n);
  }
  // K₃: S saturates at ⊤, so out ∘ in misses True.
  CHECK(Prior<Ternary>{}(In<Ternary>{}(Ternary::True)) == Ternary::Unknown);
  // int (ℤ): Z collides with S(−1).
  CHECK_FALSE(Prior<int>{}(In<int>{}(-1)).has_value());
}

TEST_CASE("category:nno — Lambek on Numeral: In and Prior are inverse",
          "[category][nno][numeral][lambek]") {
  const auto in_ = In<Numeral>{};
  const auto prior_ = inverse(in_);
  const auto back = inverse(prior_);
  const Numeral top{std::numeric_limits<std::size_t>::max()};
  const Numeral past = top + Numeral{1u};
  CHECK(prior_(in_(std::nullopt)) == std::nullopt);
  CHECK(prior_(in_(top)) == top);
  CHECK(back(prior_(Numeral{})) == Numeral{});
  CHECK(back(prior_(past)) == past);
  CHECK(prior_(past) == top);
}
