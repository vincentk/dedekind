/**
 * @file nno_test.cpp
 * @brief Test suite for the @c category:nno partition.
 *
 * @section Scope
 * Exercises the Natural Numbers Object reification (closes part of
 * #445): the @c IsNNO concept's structural shape, the @c
 * nno_iterate operational discharge of the universal property, and
 * the canonical @c Cardinality witness registered in @c
 * numbers:natural.
 *
 * The architecture under test:
 *
 *   @c NNO  →  @c Cardinality  →  @c ℕ
 *
 * — NNO is the Form (universal property), Cardinality is the
 * canonical carrier witness, ℕ is the project-level alias.
 */
#include <catch2/catch_test_macros.hpp>
#include <optional>  // the cover / Lambek answers, 1 + N
#include <utility>
#include <variant>  // load-bearing for std::variant's operator== via ADL

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

TEST_CASE("category:nno — Successor<Cardinality> is an arrow ℕ → ℕ",
          "[category][nno][cardinality][witness]") {
  STATIC_CHECK(IsArrow<Successor<Cardinality>>);
  STATIC_CHECK(std::same_as<Dom<Successor<Cardinality>>, Cardinality> &&
               std::same_as<Cod<Successor<Cardinality>>, Cardinality>);
}

// ===========================================================================
// (2) Zero and successor on Cardinality behave as the textbook NNO requires
// ===========================================================================

TEST_CASE(
    "category:nno — ZeroElement<Cardinality> returns finite_cardinality(0)",
    "[category][nno][cardinality][zero]") {
  constexpr ZeroElement<Cardinality> z{};
  CHECK(z() == finite_cardinality(0));
}

TEST_CASE("category:nno — Successor<Cardinality> generates 1, 2, 3, ...",
          "[category][nno][cardinality][successor]") {
  constexpr ZeroElement<Cardinality> z{};
  constexpr Successor<Cardinality> s{};
  const auto zero = z();
  const auto one = s(zero);
  const auto two = s(one);
  const auto three = s(two);
  CHECK(one == finite_cardinality(1));
  CHECK(two == finite_cardinality(2));
  CHECK(three == finite_cardinality(3));
}

TEST_CASE(
    "category:nno — Successor<Cardinality> saturates honestly at ℵ_0 (the "
    "transfinite element)",
    "[category][nno][cardinality][saturation]") {
  // The abstract NNO admits no transfinite element; @c Cardinality
  // does (the @c ℵ_0 alternative).  Honest carrier behaviour: @c
  // succ(ℵ_0) stays at @c ℵ_0 (saturating absorbing element on the
  // monoid side).  This is the carrier's deviation from the
  // textbook NNO reified honestly rather than papered over.
  constexpr Successor<Cardinality> s{};
  const auto inf = Cardinality{ℵ_0{}};
  CHECK(s(inf) == inf);
}

// ===========================================================================
// (3) nno_iterate discharges the universal property at finite indices
// ===========================================================================

TEST_CASE(
    "category:nno — nno_iterate(a₀, g, n) realises f(n) for the "
    "induced morphism f : ℕ → A",
    "[category][nno][universal-property][iterate]") {
  // Identity recursion: a₀ = 0, g = succ.  The unique f : ℕ → ℕ
  // produced by the universal property is the identity, materialised
  // at finite indices via nno_iterate.
  const auto zero = ZeroElement<Cardinality>{}();
  const auto step = Successor<Cardinality>{};
  CHECK(nno_iterate(zero, step, 0u) == finite_cardinality(0));
  CHECK(nno_iterate(zero, step, 1u) == finite_cardinality(1));
  CHECK(nno_iterate(zero, step, 5u) == finite_cardinality(5));
  CHECK(nno_iterate(zero, step, 42u) == finite_cardinality(42));
}

TEST_CASE(
    "category:nno — Fibonacci as a coalgebra for F(X) = ℕ × X via "
    "nno_iterate",
    "[category][nno][universal-property][iterate][fibonacci][coalgebra]") {
  // The proper categorical framing of two-term recurrences (per the
  // Gemini exchange in PR #444): Fibonacci is a coalgebra for the
  // functor F(X) = ℕ × X with state (F_n, F_{n-1}) and step
  //   (a, b) ↦ (a + b, a).
  // The unique morphism f : ℕ → A produced by the NNO universal
  // property over this coalgebra is the Fibonacci sequence;
  // nno_iterate materialises f(n) for finite n.
  using State = std::pair<Cardinality, Cardinality>;
  // Initial state (F_1, F_0) = (1, 0); we step n times to get
  // (F_{n+1}, F_n), then read off the second component for F_n.
  const State a0 = {finite_cardinality(1), finite_cardinality(0)};
  const auto g = [](const State& s) -> State {
    return {s.first + s.second, s.first};
  };
  // F_0 .. F_10 = 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55.
  CHECK(nno_iterate(a0, g, 0u).second == finite_cardinality(0));
  CHECK(nno_iterate(a0, g, 1u).second == finite_cardinality(1));
  CHECK(nno_iterate(a0, g, 2u).second == finite_cardinality(1));
  CHECK(nno_iterate(a0, g, 3u).second == finite_cardinality(2));
  CHECK(nno_iterate(a0, g, 5u).second == finite_cardinality(5));
  CHECK(nno_iterate(a0, g, 10u).second == finite_cardinality(55));
}

// ===========================================================================
// (4) The Form_Bias chain: NNO → Cardinality → ℕ
// ===========================================================================

TEST_CASE(
    "category:nno — three-layer chain: NNO Form → Cardinality carrier → ℕ "
    "alias",
    "[category][nno][form-bias][architecture]") {
  // Layer 1: NNO is the Form (universal property; concept-level), asserted
  // where the witness is declared (numbers:natural).

  // Layer 2: Cardinality is the canonical carrier inhabiting the Form.
  STATIC_CHECK(
      std::same_as<decltype(ZeroElement<Cardinality>{}()), Cardinality>);
  STATIC_CHECK(std::same_as<decltype(Successor<Cardinality>{}(Cardinality{})),
                            Cardinality>);

  // Layer 3: ℕ is the universe 𝔸<Cardinality> (post-#559).  The textbook
  // symbol practitioners use; the NNO universal property is what it
  // _means_ via the chain.
  STATIC_CHECK(std::same_as<typename std::remove_cvref_t<decltype(ℕ)>::Domain,
                            Cardinality>);
}

// ===========================================================================
// (5) The cover, Lambek, and ++: the posture at the bounds, at runtime
// ===========================================================================

TEST_CASE("category:nno — the cover is partial at ⊤ and ++ saturates there",
          "[category][nno][cover][saturation]") {
  const auto c41 = cover(finite_cardinality(41));
  REQUIRE(c41.has_value());
  CHECK(*c41 == finite_cardinality(42));
  CHECK_FALSE(cover(Cardinality{ℵ_0{}}).has_value());
  CHECK(cover(Ternary::Unknown) == Ternary::True);
  CHECK_FALSE(cover(Ternary::True).has_value());
  Cardinality n = finite_cardinality(0);
  for (int i = 0; i < 5; ++i) ++n;
  CHECK(n == finite_cardinality(5));
  Cardinality top{ℵ_0{}};
  ++top;
  CHECK(top == Cardinality{ℵ_0{}});
}

TEST_CASE(
    "category:nno — Lambek's lemma separates the NNO from its look-alikes",
    "[category][nno][lambek]") {
  // ℕ's finite fragment: [Z, S] is an iso.
  for (std::size_t k = 0; k < 6; ++k) {
    const auto n = finite_cardinality(k);
    CHECK(In<Cardinality>{}(Out<Cardinality>{}(n)) == n);
    const auto back = Out<Cardinality>{}(In<Cardinality>{}(n));
    REQUIRE(back.has_value());
    CHECK(*back == n);
  }
  // K₃: S saturates at ⊤, so out ∘ in misses True.
  CHECK(Out<Ternary>{}(In<Ternary>{}(Ternary::True)) == Ternary::Unknown);
  // int (ℤ): Z collides with S(−1).
  CHECK_FALSE(Out<int>{}(In<int>{}(-1)).has_value());
}
