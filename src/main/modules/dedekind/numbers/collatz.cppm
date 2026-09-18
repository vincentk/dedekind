/**
 * @file dedekind/numbers/collatz.cppm
 * @partition :collatz
 * @brief Bounded Collatz reachability — a §4 relational exhibit on ℕ.
 *
 * The recurrence is spelled @b once, @b point-free, as the @c Trsk relation
 * @c collatz @f$= T \subseteq \mathbb{N}\times\mathbb{N}@f$ (an @c IsRelation
 * --- whether the even/odd guard-union is inferred @c IsFunction is a
 * follow-up), and then @b finitely iterated via its native @c size_t shadow @c
 * collatz_rule.
 *
 * The set @f$\{\, n : \text{the orbit of } n \text{ reaches } 1 \,\}@f$ is @b
 * de
 * @b facto @b undecidable: universality is the open Collatz conjecture, the
 * generalized problem is provably undecidable (Conway), and the search is
 * unbounded a priori.  We make @b no closure statement --- reachability would
 * be
 * @f$\mathrm{dom}(T^{*} ; \eta(1))@f$, but @f$T^{*}@f$ is not a point-free
 * operator (the relative product @c >> is Boolean-middle only, and the Kleene
 * star is @c FIXME(#786)).  So reachability @b strength-reduces to the finite
 * iteration on its native @c size_t shadow --- the honest boundary this exhibit
 * names, and a motivating case for point-free @f$R^{*}@f$ (#786).
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
import dedekind.order;
import dedekind.relational;
import dedekind.sequences;
import dedekind.sets;

namespace dedekind::numbers {
using namespace dedekind::category;
using namespace dedekind::order;  // operator""_c (the fix(N_c) literals)
using namespace dedekind::relational;
using namespace dedekind::sequences;
using namespace dedekind::sets;

// ─── The recurrence, spelled once as an IsFunction on ℕ (Trsk) ────────────

/** @brief The recurrence, spelled out @b explicitly as a named function (not a
 *         lambda): @c n even @c ↦ @c n/2, @c n odd @c ↦ @c 3n+1.  Computed on
 * the native @c size_t shadow (halving and @c 3n+1 are exact below the wrap);
 * the canonical ℕ is the ideal it faithfully shadows. */
export constexpr std::size_t collatz_rule(std::size_t n) {
  return (n % 2 == 0) ? n / 2 : 3 * n + 1;
}

/** @brief The parity discriminant @f$\{(n,m) : n \text{ even}\}@f$ --- the axis
 *  restriction spelled @b once, so @f$T@f$ never repeats the @f$\pi_1 \bmod
 * 2@f$ test.  Its complement @f$\sim@f$@c n_even is @f$\{n \text{ odd}\}@f$ (on
 * ℕ the two parities exhaust and are disjoint), which makes the case split
 *  @b structural rather than two independent guards. */
constexpr auto n_even = ℕ * ℕ | π1 % fix(2_c) == fix(0_c);
/** @brief The even step @f$m = n/2@f$, spelled without division as
 *  @f$2\pi_2 = \pi_1@f$ (the doubling graph read backwards). */
constexpr auto halve = ℕ * ℕ | π2 * fix(2_c) == π1;
/** @brief The odd step @f$m = 3n+1@f$ --- the affine graph (@c :order). */
constexpr auto triple_plus_1 = ℕ * ℕ | π1 * fix(3_c) + fix(1_c) == π2;

/** @brief The Trsk relation @f$T \subseteq \mathbb{N}\times\mathbb{N}@f$,
 *  spelled @b point-free as the McCarthy conditional
 *  @f$T = (\text{even} \cap \text{halve}) \cup (\text{odd} \cap
 * \text{triple})@f$
 *  --- @b no lambda, @b no graph, the recurrence @e is the relation.  Mirrors
 *  the divides relation of §4; the parity test appears exactly once.
 * @note Whether the DSL infers @c IsFunction across the (disjoint, total)
 *       case split is the next investigation --- functionality is inferred from
 * a graph-shaped leaf or through @c >>, not yet across @f$\cap/\cup@f$. */
export inline constexpr auto collatz =
    (n_even & halve) + (~n_even & triple_plus_1);

static_assert(IsSet<decltype(collatz)>,
              "the point-free recurrence is an ETCS Set on ℕ × ℕ");
static_assert(
    IsRelation<decltype(collatz), Cardinality, Cardinality>,
    "T ⊆ ℕ × ℕ is an IsRelation (IsFunction is the next investigation)");
static_assert(collatz(std::pair{finite_cardinality(6), finite_cardinality(3)}),
              "6 is even: 2·3 == 6, so 6 ↦ 3");
static_assert(collatz(std::pair{finite_cardinality(7), finite_cardinality(22)}),
              "7 is odd: 3·7+1 == 22, so 7 ↦ 22");
static_assert(!collatz(std::pair{finite_cardinality(6), finite_cardinality(4)}),
              "6 ↦ 3, not 4");

// ─── The finite iteration (the strength-reduced shadow) ───────────────────

/** @brief The orbit @f$\{n\} ; T^{\le N}@f$ presented as the arrow's iterate
 * --- a lazy @c Path<std::size_t>. */
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
