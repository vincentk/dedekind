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
 *   1. intensional ℕ-comprehension (point-free @c ℕ @c | @c pred),
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
 * The eighth @b transformation nugget was formerly realised here via the
 * symbolic scout-algebra affine pipe (@c element<ℚ> @c + @c bound<k> /
 * @c element<ℚ> @c * @c bound<k>).  That test-only layer was retired
 * under #895 (scout sunset), so the transformation nugget is dropped
 * again until a point-free affine-transport spelling lands.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */

#include <concepts>
#include <type_traits>

import dedekind.category;
import dedekind.sets;
import dedekind.relational;
import dedekind.algebra;
import dedekind.numbers;
import dedekind.order;

// `using namespace dedekind::category;` is deliberately omitted; the `category`
// names used here are spelled fully qualified.
using namespace dedekind::sets;
using namespace dedekind::relational;  // set_difference (∖) — relational-only
using namespace dedekind::algebra;
using namespace dedekind::numbers;
using namespace dedekind::order;

// (1) Rule.  Intensional ℕ-comprehension, point-free: the ambient set ℕ
//     refined by the projection predicate `π > fix(5)`.  Reads "the set of
//     x ∈ ℕ such that x > 5".  This is the textbook membership shape.
constexpr auto S = ℕ | (π > fix(5_c));

// Post-#622: ℕ = 𝔸<Cardinality> is countable on the carrier axis
// (ℵ_0), so NaturalLogic routes the comprehension @c S to
// @c Boole — @c S.contains(...) lands @c bool directly, no
// Kleene lift required.  Rice's theorem still caps further promotion of
// the opaque λ inside the comprehension, but the carrier-axis witness
// is sufficient at this layer.

// (2) Tested.  Compile-time membership query: the rule is the type, so
//     `S(7u)` is constant-evaluable and reads as bare @c bool.
static_assert(S(7u));

// (3) Trimmed.  Set difference: A ∖ B = {x | x ∈ A ∧ x ∉ B}.  The textbook
//     `S \ T` would be the natural spelling; substituted here as
//     `set_difference(S, T)` (free function in :sets:relational) since `\`
//     is not a C++ operator.  T is a lazy lambda-Set: structural reduction
//     across `NegatedPredicate` (so `T & {<7}` would collapse to the literal
//     interval [6, 6]) is a future DSL refinement; membership on T still
//     constant-folds via the predicate.
constexpr auto T = set_difference(Set{S}, Set{ℕ | (π > fix(10_c))});
static_assert(T(8u));    // 5 < 8 ≤ 10 ✓
static_assert(!T(11u));  // 11 > 10 ✗

// (4) Collapsed.  The value-first meet of two bare halfspaces folds through
//     `structured_and` to a `SetVal`: `S` (= {x > 5}) meet {x < 7} on ℕ folds
//     to the bounded interval (5, 7), whose sole ℕ-inhabitant is 6.  On ℕ (the
//     `Cardinality` variant, not `std::integral`) the interval does not yet
//     shrink to the `Singleton` value: that predecessor/successor collapse is
//     the heterogeneous-pivot restoration deferred in the value-carrying port.
//     Membership already decides the point exactly.
constexpr auto a = S & (ℕ | (π < fix(7_c)));  // (5, 7) on ℕ, sole inhabitant 6
static_assert(a.kind == SetKind::Interval && a.lo == 5 && a.hi == 7,
              "the meet folds to the bounded interval (5, 7) at compile time");
static_assert(static_cast<bool>(a(6u)) && !static_cast<bool>(a(5u)) &&
                  !static_cast<bool>(a(7u)),
              "the folded interval's sole ℕ-inhabitant is 6");

// (5) Contradicted.  Complement-via-LEM: any S has empty meet with its
//     complement.  The value-first meet folds `S ∩ ¬S` to the empty `SetVal` at
//     compile time — the law of excluded middle, value-directed.  (Emptiness is
//     the crossing-bound test, so it fires on ℕ regardless of the deferred
//     integer collapse above.)
constexpr auto b = S & ~S;
static_assert(b.kind == SetKind::Empty,
              "S ∩ ¬S folds to the empty set at compile time");

// (6) Certified.  Both reductions are finite constexpr `SetVal`s — the interval
//     (5, 7) (one ℕ-inhabitant) and the empty set (none) — decided at compile
//     time even though their parent `S` is a transfinite intensional
//     description.  The reduction crosses the realisation boundary value-first:
//     the folded values, not an elevated result type.
static_assert(!static_cast<bool>(b(6u)), "the folded contradiction is empty");

// The deferred eighth "transformed" nugget was previously realised here
// via the symbolic scout-algebra affine pipe (element<ℚ> + bound<k> /
// element<ℚ> * bound<k>).  That test-only layer was retired under #895
// (scout sunset); the nugget is dropped from this showcase until a
// point-free affine-transport spelling lands.

/**
 * @brief Showcase α′: the §3 walk's compile-time payoff lifted to a
 *        runtime witness.
 *
 * @c S(7u) is constant-evaluable (7 > 5 lands in the halfspace).
 * Post-#622 the result IS @c bool directly (ℕ → Boole on the
 * carrier axis), so the body returns the membership query as-is — no
 * Kleene comparison lift required.
 *
 * Expected IR: `ret i1 true`
 */
extern "C" __attribute__((noinline)) bool witness_alpha_prime() {
  return S(7u);
}
