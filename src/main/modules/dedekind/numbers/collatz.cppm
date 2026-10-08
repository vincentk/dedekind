/**
 * @file dedekind/numbers/collatz.cppm
 * @partition :collatz
 * @brief Bounded Collatz reachability — a §4 relational exhibit on ℕ.
 *
 * The recurrence is spelled @b once, @b point-free, as the @c Trsk relation
 * @c collatz @f$= T \subseteq \mathbb{N}\times\mathbb{N}@f$, a function by
 * structure (the guarded union of two total function graphs over complementary
 * left cylinders, @c :dyadic), and then @b finitely iterated as the arrow
 * @c collatz_step on its native @c size_t shadow, the same term.
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
#include <limits>
#include <optional>
#include <type_traits>
#include <utility>

export module dedekind.numbers:collatz;

import dedekind.category;
import dedekind.order;
import dedekind.relational;
import dedekind.sequences;
import dedekind.sets;
import :natural;  // Affine / FloorDiv / Mod, the shadow's arrows

namespace dedekind::numbers {
using namespace dedekind::category;
using namespace dedekind::order;  // operator""_c (the fix(N_c) literals)
using namespace dedekind::relational;
using namespace dedekind::sequences;
using namespace dedekind::sets;

// ─── The recurrence, spelled once as an IsFunction on ℕ (Trsk) ────────────

/** @brief The recurrence as an @b arrow, a term in the shadow's arithmetic:
 *  McCarthy's conditional over the parity test, @c n/2 where even, @c 3n+1
 *  where odd.  The affine leg saturates at the word's top, which is odd and so
 *  a fixpoint: an orbit that leaves the word never reaches 1, and the verdict
 *  below stays @c Unknown rather than wrapping into a false @c True.  The
 *  Python exhibit rebuilds this very term from the same combinators. */
export inline constexpr auto collatz_step =
    Cond{Compose{Mod<std::size_t>{2}, η(std::size_t{0})},
         FloorDiv<std::size_t>{2}, Affine<std::size_t>{3, 1}};
static_assert(collatz_step(6) == 3 && collatz_step(7) == 22, "6 ↦ 3, 7 ↦ 22");
static_assert(collatz_step(std::numeric_limits<std::size_t>::max()) ==
                  std::numeric_limits<std::size_t>::max(),
              "the word's top is a fixpoint of the saturating step: no wrap");

/** @brief The parity discriminant @f$\{(n,m) : n \text{ even}\}@f$ --- the axis
 *  restriction spelled @b once, so @f$T@f$ never repeats the @f$\pi_1 \bmod
 * 2@f$ test.  Its complement @f$\sim@f$@c n_even is @f$\{n \text{ odd}\}@f$ (on
 * ℕ the two parities exhaust and are disjoint), which makes the case split
 *  @b structural rather than two independent guards. */
constexpr auto n_even = ℕ * ℕ | π1 % fix(2_c) == fix(0_c);
/** @brief The even step @f$m = \lfloor n/2 \rfloor@f$, the floor-division
 *  graph: a @b total function on ℕ that is the exact half on the evens, where
 *  the guard admits it (@c FloorDiv on the shadow, the same arrow). */
constexpr auto halve = ℕ * ℕ | π1 / fix(2_c) == π2;
/** @brief The odd step @f$m = 3n+1@f$ --- the affine graph (@c :order). */
constexpr auto triple_plus_1 = ℕ * ℕ | π1 * fix(3_c) + fix(1_c) == π2;

/** @brief The Trsk relation @f$T \subseteq \mathbb{N}\times\mathbb{N}@f$,
 *  spelled @b point-free as the McCarthy conditional
 *  @f$T = (\text{even} \cap \text{halve}) \cup (\text{odd} \cap
 * \text{triple})@f$
 *  --- @b no lambda, @b no graph, the recurrence @e is the relation, spelled in
 *  uniform set-grammar: @c & (meet), @c ~ (complement), @c | (join, the
 *  structural @c OrPredicate union of #365).  Mirrors the divides relation of
 *  §4; the parity test appears exactly once.
 * @note @c IsFunction is @b inferred: the guards are complementary left
 *       cylinders and both branches are total function graphs, so the guarded
 *       union is functional and entire by the @c :dyadic rule (the relational
 *       twin of @c Cond). */
export inline constexpr auto collatz =
    (n_even & halve) | (~n_even & triple_plus_1);

static_assert(dedekind::sets::IsSetObject<decltype(collatz)>,
              "the point-free recurrence is a set object on ℕ × ℕ (a lattice "
              "node over set objects, structurally)");
static_assert(IsRelation<decltype(collatz), Cardinality, Cardinality>,
              "T ⊆ ℕ × ℕ is an IsRelation");
static_assert(IsFunctional<std::remove_cvref_t<decltype(collatz)>> &&
                  IsEntire<std::remove_cvref_t<decltype(collatz)>>,
              "T is a FUNCTION by structure: the guarded union of two total "
              "function graphs over complementary left cylinders");
static_assert(collatz(std::pair{finite_cardinality(6), finite_cardinality(3)}),
              "6 is even: 2·3 == 6, so 6 ↦ 3");
static_assert(collatz(std::pair{finite_cardinality(7), finite_cardinality(22)}),
              "7 is odd: 3·7+1 == 22, so 7 ↦ 22");
static_assert(!collatz(std::pair{finite_cardinality(6), finite_cardinality(4)}),
              "6 ↦ 3, not 4");

// ─── Backward reachability, relationally (no shadow, no graph) ─────────────
//
// Restricting @c collatz to a target codomain and reading the PRE-IMAGE is a
// point-free, decidable @c Trsk step: the pre-image of a set @f$S@f$ under a
// relation @f$R@f$ is @f$\{a \mid \exists b \in S.\ (a,b) \in R\}@f$, which for
// a @b singleton target @f$S=\{t\}@f$ is the converse fibre
// @f$R^{\circ}(t) = \{a \mid (a,t) \in R\}@f$ --- @c fibre(converse(R), t).  No
// existential over an infinite codomain is needed for a singleton target.

/** @brief @c converges_in_1 @f$= \mathrm{collatz}^{\circ}(1) = \{n \mid
 *  \mathrm{collatz}(n) = 1\}@f$ --- the pre-image of the fixed-point target
 *  @f$\{1\}@f$: the naturals that reach 1 in exactly one step.  Point-free, via
 *  the converse fibre. */
// `collatz` is a lattice node --- a set object structurally --- and the
// pair-relational entry points (`converse`, `fibre`, `| relpred`) take it as
// is.
export inline constexpr auto converges_in_1 =
    fibre(converse(collatz), finite_cardinality(1));

static_assert(converges_in_1(finite_cardinality(2)),
              "2 is even, 2/2 = 1: 2 → 1 in one step (2 ∈ collatz°(1))");
static_assert(!converges_in_1(finite_cardinality(1)),
              "1 is odd, 3·1+1 = 4: 1 does NOT reach 1 in one step");
static_assert(!converges_in_1(finite_cardinality(4)),
              "4 is even, 4/2 = 2 ≠ 1: 4 ∉ collatz°(1)");

// ─── Two steps: the relative product over a finite ℕ-prefix middle (#795) ──
//
// @c collatz;collatz needs @f$\exists b@f$ over the ℕ middle --- the Rice wall
// on all of ℕ.  Bounding the domain to a finite prefix makes it DECIDABLE: the
// half-space contraction @c collatz @c | @c (π1 @c < @c fix(M_c)) tags the
// relation with the bound @c M in its pivot value, and the bounded @c >> (@c
// :sequences) reads @c M and streams the @f$\exists@f$-over-the-middle as an
// OR-fold over @f$[0,M)@f$ --- O(1) memory, O(M) steps, no array.  The result
// is @e itself bounded, so the squaring chain (@c collatz4 = @c collatz2 @c >>
// @c collatz2, …) composes without re-supplying the middle: bounded transitive
// closure, the §4 collapse.

/** @brief @c collatz restricted to the finite ℕ-prefix @f$[0,64)@f$ by a
 *  half-space domain cut --- the bound @c 64 rides in the pivot VALUE, so @c >>
 *  recovers it. */
constexpr auto collatzM = collatz | (π1 < fix(64_c));

/** @brief @c collatz2 @f$= \mathrm{collatz};\mathrm{collatz}@f$ over the finite
 *  @f$[0,64)@f$ middle --- two steps, the bare relative product, middle
 * inferred from the bound (no argument, no shadow, no graph, no array). */
export inline constexpr auto collatz2 = collatzM >> collatzM;

static_assert(collatz2(std::pair{finite_cardinality(4), finite_cardinality(1)}),
              "4 → 2 → 1: (4,1) ∈ collatz;collatz (middle b=2)");
static_assert(collatz2(std::pair{finite_cardinality(8), finite_cardinality(2)}),
              "8 → 4 → 2: (8,2) ∈ collatz2 (middle b=4)");
static_assert(collatz2(std::pair{finite_cardinality(6),
                                 finite_cardinality(10)}),
              "6 → 3 → 10: (6,10) ∈ collatz2 (middle b=3)");
static_assert(!collatz2(std::pair{finite_cardinality(6),
                                  finite_cardinality(5)}),
              "6 → 3 → 10 ≠ 5: (6,5) ∉ collatz2");

// ─── Who has converged?  The pre-image of the attractor (#795) ─────────────
//
// A number @f$n@f$ has @b converged (been captured by the cycle 1 → 4 → 2 → 1)
// within @f$2^k@f$ steps iff @f$\mathrm{collatz}^{2^k}(n)@f$ lands in the
// @b absorbing set @f$\{1,2,4\}@f$.  Because that cycle is CLOSED under
// @c collatz, membership at @b exactly step @f$2^k@f$ already certifies capture
// --- the orbit can only rotate within the cycle thereafter --- so the
// exact-step product suffices, with @b no reflexive closure.
//
// Read it as @c converges_in_1 reads its one-step target, one rung up: the
// converged set is the @b pre-image of the attractor, the union of
// converse-fibres over its three points.  @c fibre(converse(R), t) pins
// @f$\pi_2 = t@f$ and returns the domain elements reaching it, so the
// membership test is purely on @f$\pi_2@f$ (which cycle point) --- a unary
// @c Set on ℕ, the shape the prefix tracker folds over.

/** @brief @c converged_2 @f$= \mathrm{collatz2}^{\circ}(\{1,2,4\})@f$ --- the
 *  naturals captured by the attractor within two steps: the pre-image of the
 *  cycle, spelled as the @b finite relational image (@c fibre of the converse
 *  over the three cycle points), which distributes to the union of the
 *  point-fibres. */
constexpr auto back2 = converse(collatz2);
export inline constexpr auto converged_2 = fibre(
    back2, finite_cardinality(1), finite_cardinality(2), finite_cardinality(4));

/** @brief @c pending_2 @f$= \sim@f$@c converged_2 --- the naturals @b not yet
 *  captured within two steps: the honest "Unknown", complemented on the domain
 *  (unary), not on the pair universe. */
export inline constexpr auto pending_2 = ~converged_2;

static_assert(converged_2(finite_cardinality(4)),
              "4 → 2 → 1: captured within two steps");
static_assert(converged_2(finite_cardinality(8)),
              "8 → 4 → 2: captured within two steps");
static_assert(converged_2(finite_cardinality(1)),
              "1 → 4 → 2: already inside the cycle");
static_assert(!converged_2(finite_cardinality(3)),
              "3 → 10 → 5: not captured within two steps");
static_assert(pending_2(finite_cardinality(3)), "3 is still pending at step 2");
static_assert(!pending_2(finite_cardinality(4)),
              "4 has converged, so it is not pending");

// ─── Squaring stops here: the relational form is exponential (#795) ────────
//
// @c collatz4 = @c collatz2 @c >> @c collatz2 TYPE-CHECKS (the chain composes,
// @c ComposePrefixPred carries the bound), but its convergence queries blow the
// @c constexpr step limit: the nested @f$\exists@f$-over-the-middle is
// @b exponential in the number of squarings (each rung re-folds the whole
// @f$[0,M)@f$ middle inside the previous rung's fold), and the @b pre-image /
// @b pending queries cannot short-circuit --- proving @f$3@f$ is NOT captured
// scans the entire middle for every target.  So the relational closure exhibits
// the STRUCTURE of @f$T^{\le 2^k}@f$ but computes in the wrong complexity
// class; the transitive-closure convergence tracking below runs on the
// linear-time
// @c size_t shadow, which is exactly the strength-reduction this exhibit names.

// ─── The finite iteration (the strength-reduced shadow) ───────────────────

/** @brief The orbit @f$\{n\} ; T^{\le N}@f$ presented as the arrow's iterate
 * --- a lazy @c Path<std::size_t>. */
export constexpr auto collatz_orbit(std::size_t n) {
  return iterate(n, collatz_step);
}

/** @brief The reach indicator's type: a @c Path<Ternary> monotone in
 *  @c {Unknown, True} (@c True absorbs), constructible only through
 *  @c collatz_reach_path, which guarantees that shape; registered absorptive
 *  in @c sequences below, as @c :mandelbrot's @c DivergencePath is. */
export struct ReachPath : Path<Ternary> {
 private:
  constexpr explicit ReachPath(Path<Ternary> p) : Path<Ternary>{std::move(p)} {}
  friend constexpr ReachPath collatz_reach_path(std::size_t n);
};
/** @brief @c True once 1 is present, @c Unknown before --- a monotone
 *         @c {Unknown,True} absorptive path (the ℕ analogue of Mandelbrot's
 *         escape indicator).  @param n the seed. */
export constexpr ReachPath collatz_reach_path(std::size_t n) {
  return ReachPath{scan(
      [](const FinitePath<std::size_t>& p) -> Ternary {
        return exists(p, [](std::size_t v) { return v == 1; })
                   ? Ternary::True
                   : Ternary::Unknown;
      },
      collatz_orbit(n))};
}

/** @brief The first index @f$k \le@f$ @c budget at which the orbit hits 1, else
 *         @c nullopt --- the efficient materialization (one forward pass). */
export constexpr std::optional<std::size_t> collatz_reach_time(
    std::size_t n, std::size_t budget) {
  std::size_t v = n;
  for (std::size_t i = 0;; ++i) {
    if (v == 1) return i;
    if (i == budget) return std::nullopt;  // the budget's step is never taken
    v = collatz_step(v);
  }
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
// The collapse: every n < 100 reaches 1 within 118 steps and not within 117
// (the max stopping time below 100 is 118, at n = 97) — a decidable ∀ with a
// sharp edge, at compile time.  The wider window [1, 1000) at budget 178 is the
// runtime companion (collatz_test): the arrow term costs constexpr steps per
// iteration that the compiler's budget does not stretch to a thousand seeds.
static_assert(all_reach_1_within<100, 118>() && !all_reach_1_within<100, 117>(),
              "every 1 <= n < 100 reaches 1 within 118 steps, not within 117");
// An orbit that leaves the word saturates at its top and stays undecided.
static_assert(reaches_1_within(std::numeric_limits<std::size_t>::max(), 8) ==
                  Ternary::Unknown,
              "the saturated orbit never reaches 1: U, not a wrapped True");

}  // namespace dedekind::numbers

namespace dedekind::sequences {
/** @brief Opt-in: every @c ReachPath is absorptive (eventually constant:
 *  @c True once 1 is seen, or the constant @c Unknown).  The shape is
 *  guaranteed by its only constructor, so the registration is sound. */
export template <>
inline constexpr bool is_absorptive_sequence_v<dedekind::numbers::ReachPath> =
    true;
}  // namespace dedekind::sequences

namespace dedekind::numbers {
// The reach indicator is a genuine absorptive sequence, as a type-level fact.
static_assert(IsAbsorptiveSequence<decltype(collatz_reach_path(27))>,
              "the reaches-1 indicator is an absorptive Path<Ternary>");
}  // namespace dedekind::numbers
