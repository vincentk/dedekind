/**
 * @file nno_test.cpp
 * @brief Test suite for the @c category:nno partition.
 *
 * @section Scope
 * Exercises the Natural Numbers Object reification (closes part of
 * #445): the @c IsNNO concept's structural shape, the @c
 * iterate operational discharge of the universal property, and
 * the canonical @c NatExpr witness registered in @c
 * numbers:natural.
 *
 * The architecture under test:
 *
 *   @c NNO  →  @c NatExpr  →  @c ℕ
 *
 * — NNO is the Form (universal property), NatExpr is the
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

TEST_CASE("category:nno — Successor<NatExpr> is an arrow ℕ → ℕ",
          "[category][nno][cardinality][witness]") {
  STATIC_CHECK(IsArrow<Successor<NatExpr<>>>);
  STATIC_CHECK(std::same_as<Dom<Successor<NatExpr<>>>, NatExpr<>> &&
               std::same_as<Cod<Successor<NatExpr<>>>, NatExpr<>>);
}

// ===========================================================================
// (2) Zero and successor on NatExpr behave as the textbook NNO requires
// ===========================================================================

TEST_CASE("category:nno — ZeroElement<NatExpr> returns NatExpr{0u}",
          "[category][nno][cardinality][zero]") {
  constexpr ZeroElement<NatExpr<>> z{};
  CHECK(z() == NatExpr<>{0u});
}

TEST_CASE("category:nno — Successor<NatExpr> generates 1, 2, 3, ...",
          "[category][nno][cardinality][successor]") {
  constexpr ZeroElement<NatExpr<>> z{};
  constexpr Successor<NatExpr<>> s{};
  const auto zero = z();
  const auto one = s(zero);
  const auto two = s(one);
  const auto three = s(two);
  CHECK(one == NatExpr<>{1u});
  CHECK(two == NatExpr<>{2u});
  CHECK(three == NatExpr<>{3u});
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
  const auto zero = ZeroElement<NatExpr<>>{}();
  const auto step = Successor<NatExpr<>>{};
  CHECK(dedekind::category::iterate(step, zero, 0u) == NatExpr<>{0u});
  CHECK(dedekind::category::iterate(step, zero, 1u) == NatExpr<>{1u});
  CHECK(dedekind::category::iterate(step, zero, 5u) == NatExpr<>{5u});
  CHECK(dedekind::category::iterate(step, zero, 42u) == NatExpr<>{42u});
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
  using State = std::pair<NatExpr<>, NatExpr<>>;
  // Initial state (F_1, F_0) = (1, 0); we step n times to get
  // (F_{n+1}, F_n), then read off the second component for F_n.
  const State a0 = {NatExpr<>{1u}, NatExpr<>{0u}};
  const auto g = [](const State& s) -> State {
    return {s.first + s.second, s.first};
  };
  // F_0 .. F_10 = 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55.
  CHECK(dedekind::category::iterate(g, a0, 0u).second == NatExpr<>{0u});
  CHECK(dedekind::category::iterate(g, a0, 1u).second == NatExpr<>{1u});
  CHECK(dedekind::category::iterate(g, a0, 2u).second == NatExpr<>{1u});
  CHECK(dedekind::category::iterate(g, a0, 3u).second == NatExpr<>{2u});
  CHECK(dedekind::category::iterate(g, a0, 5u).second == NatExpr<>{5u});
  CHECK(dedekind::category::iterate(g, a0, 10u).second == NatExpr<>{55u});
}

// ===========================================================================
// (4) The cover, Lambek, and ++: the posture at the bounds, at runtime
// ===========================================================================

TEST_CASE("category:nno — the cover is partial at ⊤",
          "[category][nno][cover][saturation]") {
  const auto c41 = cover(NatExpr<>{41u});
  REQUIRE(c41.has_value());
  CHECK(*c41 == NatExpr<>{42u});
  CHECK(cover(Ternary::Unknown) == Ternary::True);
  CHECK_FALSE(cover(Ternary::True).has_value());
}

TEST_CASE(
    "category:nno — Lambek's lemma separates the NNO from its look-alikes",
    "[category][nno][lambek]") {
  // ℕ: [Z, S] is an iso.
  for (std::size_t k = 0; k < 6; ++k) {
    const auto n = NatExpr<>{k};
    CHECK(In<NatExpr<>>{}(Prior<NatExpr<>>{}(n)) == n);
    const auto back = Prior<NatExpr<>>{}(In<NatExpr<>>{}(n));
    REQUIRE(back.has_value());
    CHECK(*back == n);
  }
  // K₃: S saturates at ⊤, so out ∘ in misses True.
  CHECK(Prior<Ternary>{}(In<Ternary>{}(Ternary::True)) == Ternary::Unknown);
  // int (ℤ): Z collides with S(−1).
  CHECK_FALSE(Prior<int>{}(In<int>{}(-1)).has_value());
}

TEST_CASE("category:nno — Lambek on NatExpr: In and Prior are inverse",
          "[category][nno][numeral][lambek]") {
  const auto in_ = In<NatExpr<>>{};
  const auto prior_ = inverse(in_);
  const auto back = inverse(prior_);
  const NatExpr<> top{std::numeric_limits<std::size_t>::max()};
  const NatExpr<> past = top + NatExpr<>{1u};
  CHECK(prior_(in_(std::nullopt)) == std::nullopt);
  CHECK(prior_(in_(top)) == top);
  CHECK(back(prior_(NatExpr<>{})) == NatExpr<>{});
  CHECK(back(prior_(past)) == past);
  CHECK(prior_(past) == top);
}
