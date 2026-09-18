/**
 * @file dedekind/numbers/collatz.cppm
 * @partition :collatz
 * @brief Bounded Collatz reachability — a §4 relational exhibit on ℕ.
 *
 * The recurrence is spelled @b once as an @c IsFunction on ℕ (the arrow
 * @c collatz_step, whose graph @c collatz is the @c Trsk relation
 * @f$T \subseteq \mathbb{N}\times\mathbb{N}@f$), and then @b finitely iterated.
 *
 * The set @f$\{\, n : \text{the orbit of } n \text{ reaches } 1 \,\}@f$ is @b de
 * @b facto @b undecidable: universality is the open Collatz conjecture, the
 * generalized problem is provably undecidable (Conway), and the search is
 * unbounded a priori.  We make @b no closure statement --- reachability would be
 * @f$\mathrm{dom}(T^{*} ; \eta(1))@f$, but @f$T^{*}@f$ is not a point-free
 * operator (the relative product @c >> is Boolean-middle only, and the Kleene
 * star is @c FIXME(#786)).  So beyond @b one relational step (the @c preimage
 * below), reachability @b strength-reduces to the finite iteration of the arrow
 * on its native @c size_t shadow --- the honest boundary this exhibit names.
 *
 * The "reaches 1" indicator mirrors @c :mandelbrot's escape indicator (its ℕ
 * sibling): a monotone @c {Unknown,True}-valued absorptive @c Path<Ternary>.
 *   Layer 2 (bounded):  @c reaches_1_within(n, N) : Ternary
 *       True    = reached 1 within N steps        (IN)
 *       Unknown = not within N                     (U)
 *       (no False: "never reaches 1" has no finite certificate --- the open
 *        problem, rendered honestly.)
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
#include <utility>

export module dedekind.numbers:collatz;

import dedekind.category;
import dedekind.relational;
import dedekind.sequences;
import dedekind.sets;

namespace dedekind::numbers {
using namespace dedekind::category;
using namespace dedekind::relational;
using namespace dedekind::sequences;
using namespace dedekind::sets;

// ─── The recurrence, spelled once as an IsFunction on ℕ (Trsk) ────────────

/** @brief The recurrence, spelled out @b explicitly as a named function (not a
 *         lambda): @c n even @c ↦ @c n/2, @c n odd @c ↦ @c 3n+1.  Computed on the
 *         native @c size_t shadow (halving and @c 3n+1 are exact below the
 *         wrap); the canonical ℕ is the ideal it faithfully shadows. */
export constexpr std::size_t collatz_rule(std::size_t n) {
  return (n % 2 == 0) ? n / 2 : 3 * n + 1;
}

/** @brief The Collatz step as an arrow ℕ → ℕ --- the named rule lifted into a
 *         @c Trsk morphism.  A total function, hence a map. */
export inline constexpr auto collatz_step =
    arrow<std::size_t, std::size_t>(collatz_rule);

/** @brief The Trsk relation @f$T = \{(n, \text{collatz\_step}(n))\}@f$ --- the
 *         graph of the arrow.  @c graph(f) is @c IsFunctional and @c IsEntire by
 *         construction, so @c T is an @c IsFunction with no opt-in flag. */
export inline constexpr auto collatz = graph(collatz_step);

static_assert(IsSet<decltype(collatz)>,
              "graph(collatz_step) is the ETCS relation ℕ × ℕ | m == step(n)");
static_assert(collatz(std::pair{std::size_t{6}, std::size_t{3}}),
              "6 is even: 6 ↦ 3");
static_assert(collatz(std::pair{std::size_t{7}, std::size_t{22}}),
              "7 is odd: 7 ↦ 3·7+1");
static_assert(!collatz(std::pair{std::size_t{6}, std::size_t{4}}),
              "6 ↦ 3, not 4");

/** @brief One relational step, point-free: the @c preimage of @f$\{1\}@f$ under
 *         the arrow is exactly @f$\{2\}@f$ (the only @c n with @c step(n)=1).
 *         The @e full reachability would iterate this (@f$T^{*}@f$), which is
 *         not point-free (#786); beyond one step it strength-reduces to the
 *         finite iteration below. */
export inline constexpr auto reaches_1_in_one_step =
    preimage(collatz_step, ℕ | (π == fix(1_c)));

static_assert(reaches_1_in_one_step(std::size_t{2}), "2 → 1");
static_assert(!reaches_1_in_one_step(std::size_t{3}), "3 → 10, not 1");
static_assert(!reaches_1_in_one_step(std::size_t{4}), "4 → 2, not 1");

// ─── The finite iteration (the strength-reduced shadow) ───────────────────

/** @brief The orbit @f$\{n\} ; T^{\le N}@f$ presented as the arrow's iterate ---
 *         a lazy @c Path<std::size_t>. */
export constexpr auto collatz_orbit(std::size_t n) {
  return iterate(n, collatz_rule);
}

/** @brief @c True once 1 is present, @c Unknown before --- a monotone
 *         @c {Unknown,True} absorptive @c Path<Ternary> (the ℕ analogue of
 *         Mandelbrot's escape indicator). */
export constexpr auto collatz_reach_path(std::size_t n) {
  return scan(
      [](const FinitePath<std::size_t>& p) -> Ternary {
        return exists(p, [](std::size_t v) { return v == 1; })
                   ? Ternary::True
                   : Ternary::Unknown;
      },
      collatz_orbit(n));
}

/** @brief The first index @f$k \le@f$ @c budget at which the orbit hits 1, else
 *         @c nullopt --- the efficient materialization (one forward pass). */
export constexpr std::optional<std::size_t> collatz_reach_time(
    std::size_t n, std::size_t budget) {
  std::size_t v = n;
  for (std::size_t i = 0; i <= budget; ++i) {
    if (v == 1) return i;
    v = collatz_rule(v);
  }
  return std::nullopt;
}

/** @brief The Rosolini-dominance verdict: reaches 1 within @c budget @c ⟹
 *         @c True (IN); not yet @c ⟹ @c Unknown (U).  Never @c False. */
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

// 27 climbs to 9232 and reaches 1 in ~111 steps: a short budget says U, a
// sufficient budget says IN (margins avoid a brittle exact-count dependence).
static_assert(reaches_1_within(27, 120) == Ternary::True,
              "27 reaches 1 — IN within budget 120");
static_assert(reaches_1_within(27, 50) == Ternary::Unknown,
              "27 not decided at budget 50 — honest U");
// 6 → 3 → 10 → 5 → 16 → 8 → 4 → 2 → 1  (reaches 1 at step 8):
static_assert(reaches_1_within(6, 8) == Ternary::True, "6 reaches 1 at step 8");
static_assert(reaches_1_within(6, 7) == Ternary::Unknown,
              "6 not decided one step short");
// The collapse: every n < 1000 reaches 1 within 300 steps (max stopping time
// below 1000 is 178, at n = 871) — a decidable ∀, at compile time.
static_assert(all_reach_1_within<1000, 300>(),
              "every 1 <= n < 1000 reaches 1 within 300 steps");
// The reach indicator is a genuine (absorptive) sequence.
static_assert(IsSequence<decltype(collatz_reach_path(27))>,
              "the reaches-1 indicator is a Path<Ternary>");

}  // namespace dedekind::numbers
