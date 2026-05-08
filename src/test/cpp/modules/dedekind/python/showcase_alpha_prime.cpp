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
 * The α′ candidate listing in #603 enumerates seven nuggets; this
 * showcase ships the six that compile against current main.  The
 * remaining two — a cross-carrier @c embed_𝔹_ℕ embedding and a
 * transformation nugget @c {2x @c | @c x @c ∈ @c S} — are deferred
 * follow-ups (the embedding function is not yet reified; the
 * transformation nugget is gated on the expression-template DSL leg
 * of #603's four-leg ordering).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */

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
constexpr auto S = Set{in<ℕ> | (in<ℕ> > bound<5>)};

// ℕ = Ω<Cardinality> routes through NaturalLogic → TernaryLogic (because
// ℵ_0 is transfinite), so `S.contains(...)` returns Ternary rather than bool;
// the comparison against `Logic::True` lifts the result back into
// boolean / static_assert-eligible form.  A halfspace-elevation refinement
// (carry the membership decidability through the carrier's logic-routing so
// `static_assert(S.contains(7u))` reads bare) is on the future-DSL list.
using NLogic = typename decltype(S)::logic_species;

// (2) Tested.  Compile-time membership query — the rule is the type, so
//     `S.contains(7u) == True` is constant-evaluable.
static_assert(S.contains(7u) == NLogic::True);

// (3) Trimmed.  Set difference: A ∖ B = {x | x ∈ A ∧ x ∉ B}.  The textbook
//     `S \ T` would be the natural spelling; substituted here as
//     `set_difference(S, T)` (free function in :sets:relational) since `\`
//     is not a C++ operator.  T is a lazy lambda-Set: structural reduction
//     across `NegatedPredicate` (so `T & {<7}` would collapse to the literal
//     interval [6, 6]) is a future DSL refinement; membership on T still
//     constant-folds via the predicate.
constexpr auto T = set_difference(S, Set{in<ℕ> | (in<ℕ> > bound<10>)});
static_assert(T.contains(8u) == NLogic::True);    // 5 < 8 ≤ 10 ✓
static_assert(T.contains(11u) == NLogic::False);  // 11 > 10 ✗

// (4) Collapsed.  Cardinality reduction → 1 (intensional → extensional).
//     `S` (= {x : x > 5}) intersected with {x : x < 7} on ℕ has cardinality
//     exactly 1, and `structured_and` reduces the meet to the `Singleton<6>`
//     extensional carrier at compile time.  (The candidate listing's
//     `T & {== bound<6>}` form would land the same Singleton via the
//     trimmed `T` and an equality-classifier reduction; both refinements
//     are separate future slices.)
constexpr Singleton<6> a = S & Set{in<ℕ> | (in<ℕ> < bound<7>)};

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
static_assert(IsCompileTimeEnumerable<decltype(a)>);
static_assert(IsCompileTimeEnumerable<decltype(b)>);

/**
 * @brief Showcase α′: the §3 walk's compile-time payoff lifted to a
 *        runtime witness.
 *
 * @c S.contains(7u) is constant-evaluable (7 > 5 lands in the halfspace),
 * so the compiler should constant-fold the body to @c true.
 *
 * Expected IR: `ret i1 true`
 */
extern "C" __attribute__((noinline)) bool witness_alpha_prime() {
  return S.contains(7u) == NLogic::True;
}
