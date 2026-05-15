/**
 * @file
 * src/test/cpp/modules/dedekind/python/showcase_alpha_prime.cpp
 * @brief Showcase α′ — the §3 single-listing nugget walk
 *        (rule → tested → trimmed → collapsed → contradicted → certified).
 *
 * Source companion to paper Listing α′ (#603).  Each line lands one
 * structural nugget exhibiting a different ingredient of the
 * type-system-as-set-DSL story:
 *
 *   1. intensional ℕ-comprehension (post-#620 @c in<...> alias),
 *   2. compile-time membership query,
 *   3. set difference via @c set_difference (substituted from textbook ∖),
 *   4. cardinality reduction → 1 (intensional → extensional, halfspace
 *      collapse to @c Singleton),
 *   5. complement-via-LEM: @c S @c ∩ @c ¬S @c = @c ∅ at compile time,
 *   6. tier elevation lands on both reductions.
 *
 * The α′ candidate listing in #603 enumerates seven base nuggets,
 * with an eighth @em transformation nugget @c {2x @c | @c x @c ∈ @c S}
 * added in the 2026-05-06 design update.  This showcase ships the six
 * that compile against current main: of the seven base nuggets, one
 * (the cross-carrier @c embed_𝔹_ℕ embedding) is dropped because
 * @c embed_𝔹_ℕ is not yet reified.
 *
 * Post-#664 (Slice 2 + Slice 3 + PR #674's @c IsField cert) the @b
 * transformation nugget is @b realised, parallel-sectioned on ℚ
 * (since ℕ doesn't admit the algebraic gates: not a group under @c +,
 * not a field under @c *).  ℚ admits both: additive translation
 * @c in<ℚ> @c + @c bound<k> via @c IsOrderedAdditiveGroup<ℚ>, and
 * multiplicative scaling @c in<ℚ> @c * @c bound<k> via
 * @c IsOrderedMultiplicativeGroup<ℚ>.  The transformations are
 * type-level halfspace-pivot transports, just like the §3 reductions.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */

#include <concepts>
#include <type_traits>

import dedekind.category;
import dedekind.sets;
import dedekind.algebra;
import dedekind.numbers;
import dedekind.order;

// Deliberately omitting `using namespace dedekind::category;`: the soft-alias
// `dedekind::sets::in<Ambient>` (post-#620) collides with the free function
// `dedekind::category::in(x, S)` under unqualified name lookup.  `category`
// names used here are spelled fully qualified.
using namespace dedekind::sets;
using namespace dedekind::algebra;
using namespace dedekind::numbers;
using namespace dedekind::order;

// (1) Rule.  Intensional ℕ-comprehension via the soft alias `in<>` (post-#620).
//     Reads "the set of x ∈ ℕ such that x > 5" — bar, "in" on the LHS, the
//     textbook membership shape.
constexpr auto S = Set{in<ℕ> | in<ℕ> > bound<5>};

// Post-#622: ℕ = Ω<Cardinality> is countable on the carrier axis
// (ℵ_0), so NaturalLogic routes the comprehension @c S to
// @c ClassicalLogic — @c S.contains(...) lands @c bool directly, no
// Kleene lift required.  Rice's theorem still caps further promotion of
// the opaque λ inside the comprehension, but the carrier-axis witness
// is sufficient at this layer.

// (2) Tested.  Compile-time membership query — the rule is the type, so
//     `S.contains(7u)` is constant-evaluable and reads as bare @c bool.
static_assert(S.contains(7u));

// (3) Trimmed.  Set difference: A ∖ B = {x | x ∈ A ∧ x ∉ B}.  The textbook
//     `S \ T` would be the natural spelling; substituted here as
//     `set_difference(S, T)` (free function in :sets:relational) since `\`
//     is not a C++ operator.  T is a lazy lambda-Set: structural reduction
//     across `NegatedPredicate` (so `T & {<7}` would collapse to the literal
//     interval [6, 6]) is a future DSL refinement; membership on T still
//     constant-folds via the predicate.
constexpr auto T = set_difference(S, Set{in<ℕ> | in<ℕ> > bound<10>});
static_assert(T.contains(8u));    // 5 < 8 ≤ 10 ✓
static_assert(!T.contains(11u));  // 11 > 10 ✗

// (4) Collapsed.  Cardinality reduction → 1 (intensional → extensional).
//     `S` (= {x : x > 5}) intersected with {x : x < 7} on ℕ has cardinality
//     exactly 1, and `structured_and` reduces the meet to the `Singleton<6>`
//     extensional carrier at compile time.  (The candidate listing's
//     `T & {== bound<6>}` form would land the same Singleton via the
//     trimmed `T` and an equality-classifier reduction; both refinements
//     are separate future slices.)
constexpr Singleton<6> a = S & Set{in<ℕ> | in<ℕ> < bound<7>};

// (5) Contradicted.  Complement-via-LEM: any S has empty meet with its
//     complement.  `structured_and` reduces this to `Ø<Cardinality>` at
//     compile time — the law-of-excluded-middle made type-level.
constexpr Ø<Cardinality> b = S & !S;

// (6) Certified.  Tier elevation: both reductions (Singleton<6>,
// Ø<Cardinality>)
//     are compile-time enumerable, even though their parent S is not — the
//     reduction crosses the realisation boundary, lifting an intensional
//     description on a transfinite carrier into decidable, finite,
//     type-level semantics.
static_assert(IsExtensional<decltype(a)>);
static_assert(IsExtensional<decltype(b)>);

// (7) Transformed.  The deferred eighth nugget from #603's design,
//     realised post-#664 + #674 on ℚ (the algebraic gates fail on ℕ
//     --- not a group under +, not a field under *; on ℚ both fire).
//     Each scout-algebra DSL form is a halfspace-pivot transport at
//     compile time: the source predicate's pivot is shifted (+) or
//     scaled (*) by the scout's Element under the group action.
constexpr auto S_translated = Set{in<ℚ> + bound<3> | in<ℚ> > bound<2>};
constexpr auto S_scaled = Set{in<ℚ> * bound<2> | in<ℚ> > bound<5>};
// Pivot transport: 2 + 3 = 5 (translation); 5 * 2 = 10 (scaling).
// The reduced predicate IS a Halfspace with the transported pivot at
// the type level; pinned via full-type equality.  ℚ is countable (ℵ_0)
// so the carrier-axis cut (#622) routes through ClassicalLogic.
using QCarrier =
    dedekind::numbers::Rational<dedekind::numbers::default_integer>;
static_assert(
    std::same_as<
        std::remove_cvref_t<decltype(S_translated)>,
        Set<QCarrier, dedekind::category::ClassicalLogic,
            Halfspace<QCarrier, 5, Direction::Upward, Strictness::Strict>>>);
static_assert(
    std::same_as<
        std::remove_cvref_t<decltype(S_scaled)>,
        Set<QCarrier, dedekind::category::ClassicalLogic,
            Halfspace<QCarrier, 10, Direction::Upward, Strictness::Strict>>>);

/**
 * @brief Showcase α′: the §3 walk's compile-time payoff lifted to a
 *        runtime witness.
 *
 * @c S.contains(7u) is constant-evaluable (7 > 5 lands in the halfspace).
 * Post-#622 the result IS @c bool directly (ℕ → ClassicalLogic on the
 * carrier axis), so the body returns the membership query as-is — no
 * Kleene comparison lift required.
 *
 * Expected IR: `ret i1 true`
 */
extern "C" __attribute__((noinline)) bool witness_alpha_prime() {
  return S.contains(7u);
}
