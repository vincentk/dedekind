/**
 * @file dedekind/numbers/collatz.cppm
 * @partition :collatz
 * @brief Bounded Collatz reachability — a §4 relational-closure exhibit on ℕ.
 *
 * The set @f$\{\, n : \text{the Collatz orbit of } n \text{ reaches } 1 \,\}@f$
 * is @b de @b facto @b undecidable: whether @e every @c n reaches 1 is the open
 * Collatz conjecture, and the generalized problem is provably undecidable
 * (Conway).  Reachability is the relational closure of the step map's graph
 * @f$T@f$: @c reaches1 @c = @c dom(T* ; η(1)).  The @b constraint that
 * collapses it to something decidable is purely relational --- @b bound the
 * closure
 * @f$T^{\le N}@f$ (finitely many relative products).
 *
 * Architecture mirrors @c :mandelbrot (the ℕ sibling of the ℂ orbit): the
 * "reaches 1" indicator is the exact analogue of Mandelbrot's escape indicator,
 * a monotone @c {Unknown,True}-valued absorptive @c Path<Ternary>.
 *
 *   Layer 1 (intensional):  @c collatz_reach_path(n) : Path<Ternary>
 *       True    at depth k  if 1 appears in the orbit by step k (reached)
 *       Unknown at depth k  if 1 has not appeared yet
 *
 *   Layer 2 (bounded, computable):  @c reaches_1_within(n, N) : Ternary
 *       True    = reached 1 within N steps                 (IN)
 *       Unknown = not within N (budget short, or divergent) (U)
 *       (there is @b no False: "never reaches 1" has no finite certificate for
 *        standard Collatz --- that @e is the open problem, rendered honestly.)
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "Mathematics is not yet ready for such problems."
 *       -- Paul Erdős, on the Collatz conjecture.
 */
module;

#include <cstddef>
#include <optional>

export module dedekind.numbers:collatz;

import dedekind.category;
import dedekind.sequences;

namespace dedekind::numbers {
using namespace dedekind::category;
using namespace dedekind::sequences;

// ─── The step relation (a total map ℕ → ℕ) ───────────────────────────────

/**
 * @brief The Collatz step @f$T@f$: @c n even @c → @c n/2, @c n odd @c → @c
 * 3n+1.
 * @note A @b total function, hence a map in @c Trsk; its orbit is a @c Path.
 *       @c 3n+1 overflows for @c n near @c SIZE_MAX --- keep the exhibit window
 *       bounded (small @c n) or move to a saturating carrier.
 */
export constexpr std::size_t collatz_step(std::size_t n) {
  return (n % 2 == 0) ? n / 2 : 3 * n + 1;
}

/** @brief The orbit @f$\{n\} ; T^*@f$ presented as the map's iterate --- a lazy
 *         @c Path<std::size_t> @c [n, T(n), T²(n), …]. */
export constexpr auto collatz_orbit(std::size_t n) {
  return iterate(n, collatz_step);
}

// ─── Layer 1: the intensional "reaches 1" indicator ──────────────────────

/** @brief @c True once 1 is present, @c Unknown before --- a monotone
 *         @c {Unknown,True} @c Path<Ternary> (@c True is the absorbing element
 *         of Kleene OR, so it is eventually constant: an @c
 * IsAbsorptiveSequence for every @c n, exactly as Mandelbrot's escape
 * indicator). */
export constexpr auto collatz_reach_path(std::size_t n) {
  return scan(
      [](const FinitePath<std::size_t>& p) -> Ternary {
        return exists(p, [](std::size_t v) { return v == 1; })
                   ? Ternary::True
                   : Ternary::Unknown;
      },
      collatz_orbit(n));
}

// ─── Layer 2: the bounded, computable classifier ─────────────────────────

/** @brief The first index @f$k \le@f$ @c budget at which the orbit hits 1, else
 *         @c nullopt.  The efficient materialization of
 *         @c first_where(collatz_orbit(n), @c ·==1, @c budget): a single
 * forward pass, returning as soon as 1 appears. */
export constexpr std::optional<std::size_t> collatz_reach_time(
    std::size_t n, std::size_t budget) {
  std::size_t v = n;
  for (std::size_t i = 0; i <= budget; ++i) {
    if (v == 1) return i;
    v = collatz_step(v);
  }
  return std::nullopt;
}

/** @brief The Rosolini-dominance verdict: reaches 1 within @c budget @c ⟹
 *         @c True (IN); not yet @c ⟹ @c Unknown (U).  Never @c False --- see
 *         the file note. */
export constexpr Ternary reaches_1_within(std::size_t n, std::size_t budget) {
  return collatz_reach_time(n, budget).has_value() ? Ternary::True
                                                   : Ternary::Unknown;
}

/** @brief The @b collapse: the open @f$\forall@f$-conjecture, restricted to a
 *         finite window @f$[1, W)@f$ and a budget @c B, is a decidable
 *         compile-time fact. */
export template <std::size_t W, std::size_t B>
constexpr bool all_reach_1_within() {
  for (std::size_t n = 1; n < W; ++n)
    if (reaches_1_within(n, B) != Ternary::True) return false;
  return true;
}

// ─── Formal verification: the money witnesses ────────────────────────────

// 27 famously climbs to 9232 and reaches 1 in 111 steps: a budget short of the
// trajectory says U, a sufficient budget says IN.  (Margins avoid a brittle
// dependence on the exact stopping time.)
static_assert(reaches_1_within(27, 120) == Ternary::True,
              "27 reaches 1 (in ~111 steps) — IN within budget 120");
static_assert(reaches_1_within(27, 50) == Ternary::Unknown,
              "27 not decided at budget 50 — honest U");
// 6 → 3 → 10 → 5 → 16 → 8 → 4 → 2 → 1  (reaches 1 at step 8):
static_assert(reaches_1_within(6, 8) == Ternary::True, "6 reaches 1 at step 8");
static_assert(reaches_1_within(6, 7) == Ternary::Unknown,
              "6 not decided one step short");
// The collapse: every n < 1000 reaches 1 within 300 steps — a decidable ∀,
// checked at compile time (the open conjecture, windowed + budgeted; the known
// maximum stopping time below 1000 is 178, at n = 871, so 300 is comfortable).
static_assert(all_reach_1_within<1000, 300>(),
              "every 1 <= n < 1000 reaches 1 within 300 steps");

// The reach indicator is a genuine (absorptive) sequence.
static_assert(IsSequence<decltype(collatz_reach_path(27))>,
              "the reaches-1 indicator is a Path<Ternary>");

}  // namespace dedekind::numbers
