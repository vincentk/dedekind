/**
 * @file boolean.cppm
 * @partition :boolean
 * @brief Boolean Starter Package: the algebra on the carrier @c bool and the
 *        canonical embedding @c 𝔹 @c ↪ @c 𝕂3.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section algebra_boolean__Starter_Intent
 * This partition offers a small, explicit entry point for Boolean algebra in
 * the set-builder DSL: the algebraic witnesses on the carrier @c bool and the
 * mono @c 𝔹 @c ↪ @c 𝕂3.  The universe itself is @c dedekind::sets::𝔹
 * (@c 𝔸<bool>{}), defined once upstream in @c :sets:boundaries.
 *
 * @section algebra_boolean__Notation
 * - `𝔹`: the Boolean @b universe @c 𝔸<bool>{} over the carrier @c bool
 *   (@c dedekind::sets::𝔹); carrier-type positions use @c bool directly.
 *   @c static_assert(IsField<bool, bit_xor, bit_and>) carries the algebra
 *   on the carrier.  Bool is the @b bottom of the algebraic tower, so the
 *   characteristic morphism of 𝔹-as-subobject coincides with the universe
 *   (no proper ambient super-object).  Every other species symbol
 *   (@c ℕ, @c ℤ, @c ℚ, @c ℝ, @c ℂ, @c 𝔻) is likewise the universe over its
 *   carrier; a subobject such as ℕ ⊂ ℤ is an @c :order halfspace.
 *
 * @section algebra_boolean__Paper_Alignment
 * In the paper's Feature Cube (bool row), logical (`||`, `&&`) and bitwise
 * (`|`, `&`) operators over bool share the same lattice behavior (join/meet,
 * identities, absorbers, and distributivity). The test suite validates this
 * alignment explicitly.
 *
 * Element scouts are post-#559 spelled @c element<𝔹> (BoundScout factory
 * over the Boolean universe value @c 𝔹 = @c 𝔸<bool>); the legacy
 * @c var<...> family was retired in Phase 2e.3 of the Ω-ambient redesign
 * (#551), and the @c element<𝔸<𝔹>> intermediate spelling went away in
 * #559's option-A migration once @c 𝔹 stopped being a carrier alias.
 *
 * @note "La matematica non e una collezione di trucchi: e grammatica delle
 * forme." (Mathematics is not a bag of tricks; it is a grammar of forms.) —
 * Emma Castelnuovo as quoted by B. L. van der Waerden (1975)
 */
module;

#include <functional>
#include <utility>  // std::forward (used in embed_𝔹_𝕂3's set-level lift)

export module dedekind.algebra:boolean;

import dedekind.category;
import dedekind.order;
import dedekind.sets;
import :universal;

namespace dedekind::algebra {
using namespace dedekind::category;
using namespace dedekind::sets;

static_assert(
    IsAlgebraOnSet<decltype(𝔹),
                   std::logical_and<bool>,  // ∧  ┐
                   std::logical_or<bool>,   // ∨  ├ F = element-level ops
                   std::logical_not<bool>   // ¬  ┘   on the carrier bool
                   >);

/**
 * @brief Canonical embedding @c 𝔹 @c ↪ @c 𝕂3: bool → Ternary.
 * @details The two-valued-to-three-valued Kleene lift: @c false @c
 *          ↦ @c Ternary::False (@c -1), @c true @c ↦ @c
 *          Ternary::True (@c 1).  The @c Ternary::Unknown (@c 0)
 *          value is @b not in the image — it represents the third
 *          truth-value that @c 𝔹 lacks.  This is the canonical
 *          inclusion of two-valued classical logic into three-
 *          valued Kleene logic; structurally a monomorphism.
 */
export inline constexpr auto embed_𝔹_𝕂3_ =
    arrow<bool, dedekind::category::Ternary>(
        [](const bool& b) noexcept -> dedekind::category::Ternary {
          return b ? dedekind::category::Ternary::True
                   : dedekind::category::Ternary::False;
        });

/**
 * @brief Set-level lift of @c embed_𝔹_𝕂3_: image of a Boolean set
 *        @c S under the canonical mono 𝔹 ↪ 𝕂3.
 *
 * @details Layer-1 entry per #602 (sister to @c embed_𝔹_ℕ in
 * @c :numbers:natural, PR #624).  Names the construction at the call
 * site rather than re-spelling @c image(embed_𝔹_𝕂3_, S).  Accepted
 * input @c S is anything @c dedekind::sets::image already dispatches
 * on --- @c Singleton (@c :sets:singleton),
 * @c std::set<bool> / @c std::unordered_set<bool> (@c :sets:extensional);
 * lazy predicate sets join the dispatch table when #602's layer 2
 * lands.
 *
 * Mathematically: the image of @c S under the canonical mono
 * 𝔹 ↪ 𝕂3 is a subset of @c {Ternary::False, @c Ternary::True} ⊂ 𝕂3
 * containing whichever @c bool elements are in @c S.
 * @c Ternary::Unknown is by construction @b not in the image --- the
 * structural reason the arrow is monic but not surjective.
 */
export template <typename S>
  requires requires(S&& s) {
    dedekind::sets::image(embed_𝔹_𝕂3_, std::forward<S>(s));
  }
constexpr auto embed_𝔹_𝕂3(S&& s) {
  return dedekind::sets::image(embed_𝔹_𝕂3_, std::forward<S>(s));
}

// Set-level value-witnesses for @c embed_𝔹_𝕂3 — pinned at the @b value
// level so the pivot equality is constant-evaluated, not just the codomain
// type.  Sister anchor to PR #624's @c embed_𝔹_ℕ witness in @c :natural ---
// same shape, different codomain.  Lives next to the arrow itself so the
// value-pin moves with the canonical surface.
static_assert(origin(embed_𝔹_𝕂3(η(true))) == dedekind::category::Ternary::True,
              "embed_𝔹_𝕂3(η(true)) lands at Ternary::True on the 𝕂3 "
              "carrier.");
static_assert(origin(embed_𝔹_𝕂3(η(false))) ==
                  dedekind::category::Ternary::False,
              "embed_𝔹_𝕂3(η(false)) lands at Ternary::False on the 𝕂3 "
              "carrier.");

// Concept-level witness: the result realises the categorical image of
// the source set under the canonical mono 𝔹 ↪ 𝕂3 — Subobject of
// @c Cod<embed_𝔹_𝕂3_> = Ternary per @c :category:image.
static_assert(
    IsImageOf<decltype(embed_𝔹_𝕂3(η(true))), decltype(embed_𝔹_𝕂3_)>,
    "embed_𝔹_𝕂3(S) realises IsImageOf<result, embed_𝔹_𝕂3_>: result is "
    "a Subobject of Cod<embed_𝔹_𝕂3_> = Ternary, witnessing the "
    "categorical image of S under the canonical mono 𝔹 ↪ 𝕂3.");

// The logical-operator shape concept already lives at the lower layer as
// @c dedekind::category::HasLogicalOperators<T> (in @c :logic, introduced
// under #393), as a sibling of @c HasRingOperators / @c HasFieldOperators
// / @c HasLatticeOperators in the shape-concept family.  Reusing it here
// rather than duplicating; the L-parametric variant (where @c && / @c ||
// close to @c L::Ω rather than to @c T) is a future refinement that can
// be added on top of the category-layer concept if a non-Boolean truth-
// value carrier carrier ever calls for it.
static_assert(dedekind::category::HasLogicalOperators<bool>);

/** @section algebra_boolean__Formal_Verification */

// `bool` under (min, max) is a (distributive) lattice --- the Boolean
// lattice 𝔹.  Witnessed at the source so downstream code does not
// have to rederive the claim.  These complement the algebraic
// witnesses elsewhere: `bool` under (XOR, AND) is the Galois field
// 𝔽_2 (see :galois), and `bool` under (OR, AND) is the Boolean rig
// (see :ring).  All three views agree on the underlying carrier.
static_assert(dedekind::order::IsOrderJoinSemilattice<bool>,
              "bool under max is a join-semilattice (the Boolean "
              "lattice's join).");
static_assert(dedekind::order::IsOrderMeetSemilattice<bool>,
              "bool under min is a meet-semilattice (the Boolean "
              "lattice's meet).");
static_assert(dedekind::order::IsOrderLattice<bool>,
              "bool under (min, max) is a lattice --- the Boolean "
              "lattice 𝔹.");
static_assert(dedekind::order::IsOrderDistributiveLattice<bool>,
              "bool under (min, max) is a distributive lattice "
              "(meet and join distribute over each other).");

// Order witnesses: bool with `<=` is totally ordered (false ≤ true).
// The `is_reflexive_v` / `is_transitive_v` / `is_antisymmetric_v`
// specs covering integral types in `:species` lift here, plus
// `std::totally_ordered<bool>` from the standard library.
static_assert(dedekind::order::IsPreOrdered<bool>,
              "bool with <= is a pre-order (reflexive + transitive).");
static_assert(dedekind::order::IsPartiallyOrdered<bool>,
              "bool with <= is a partial order (adds antisymmetry).");
static_assert(dedekind::order::IsTotallyOrdered<bool>,
              "bool with <= is totally ordered (false ≤ true).");

// `bool` is also a directed set: every pair has a common upper bound
// (trivially: `true` dominates).  This makes `bool` a valid \emph{net
// domain} in the @c sequences sense (cf.\ Munkres / Kelley: a net is
// a function from a directed set, not just from ℕ).  Witnessed here
// rather than in @c order:poset because the lattice structure on
// @c bool is anchored in the algebraic Boolean partition.
static_assert(dedekind::order::IsDirectedSet<bool>,
              "bool with <= is a directed set --- a valid net domain.");
static_assert(dedekind::order::IsDirectedPoset<bool>,
              "bool is a directed poset (directed + antisymmetric).");

}  // namespace dedekind::algebra

// ---------------------------------------------------------------------------
// Trait registrations for @c embed_𝔹_𝕂3_ — live next to the arrow itself
// so callers importing @c dedekind.algebra get @c IsMonicArrow /
// @c IsEmbeddingFunctor witnesses without also needing @c dedekind.numbers.
// ---------------------------------------------------------------------------
namespace dedekind::category {
template <>
inline constexpr bool
    is_monic_arrow_v<std::decay_t<decltype(dedekind::algebra::embed_𝔹_𝕂3_)>> =
        true;
static_assert(
    IsInjective<std::decay_t<decltype(dedekind::algebra::embed_𝔹_𝕂3_)>>,
    "embed_𝔹_𝕂3_ (𝔹 ↪ 𝕂3) is registered injective.");

// IsEmbeddingFunctor witness (#633): @c embed_𝔹_𝕂3_ lifts the
// two-element 𝔹 into the three-element 𝕂3 truth surface monically
// (False ↦ False, True ↦ True; the Unknown value is unreached).
// Fully faithful + injective on objects per Mac Lane CWM §IV.4.
template <>
inline constexpr bool is_embedding_functor_v<
    std::decay_t<decltype(dedekind::algebra::embed_𝔹_𝕂3_)>> = true;
static_assert(
    IsEmbeddingFunctor<std::decay_t<decltype(dedekind::algebra::embed_𝔹_𝕂3_)>>,
    "embed_𝔹_𝕂3_ realises IsEmbeddingFunctor per #633's Mac Lane reading.");

// IsMonotone witness (#664 morphism vocabulary): @c false @c ↦ @c False
// (@c -1), @c true @c ↦ @c True (@c 1).  Under the canonical Kleene order
// @c False @c < @c Unknown @c < @c True (which @c std::less_equal lifts
// to the underlying @c int-valued enum), the embedding sends @c false @c
// ≤ @c true to @c False @c ≤ @c True — order is preserved.
template <>
inline constexpr bool is_monotone_v<
    std::decay_t<decltype(dedekind::algebra::embed_𝔹_𝕂3_)>, std::less_equal<>> =
    true;
static_assert(
    IsMonotone<std::decay_t<decltype(dedekind::algebra::embed_𝔹_𝕂3_)>>,
    "embed_𝔹_𝕂3_ (𝔹 ↪ 𝕂3) is monotone — preserves the Boolean order "
    "under the canonical Kleene three-value order.");
}  // namespace dedekind::category
