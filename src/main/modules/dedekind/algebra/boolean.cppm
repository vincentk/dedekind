/**
 * @file boolean.cppm
 * @partition :boolean
 * @brief Boolean Starter Package: canonical Boolean ambient species aliases.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section algebra_boolean__Starter_Intent
 * This partition offers a small, explicit entry point for Boolean algebra in
 * the set-builder DSL. It exports canonical Boolean universe aliases so
 * examples remain readable and stable.
 *
 * @section algebra_boolean__Notation
 * - `𝔹`: canonical Unicode symbol for the Boolean @b universe over the
 *   carrier @c bool (post-#559).  Spelled as the value @c Ω<bool> below;
 *   carrier-type positions use @c bool directly.
 *   @c static_assert(IsField<bool, bit_xor, bit_and>) carries the algebra
 *   on the carrier, while @c element<𝔹> is the canonical scout spelling
 *   over the universe.
 * - `BooleanSetOf<L, C>` ≡ `UniversalSet<bool, L, C>`: parameterised
 *   predicate-set template alias.  Bool is the @b bottom of the algebraic
 *   tower, so the characteristic morphism χ_𝔹 of 𝔹-as-subobject coincides
 *   with the universe Ω<bool> (no proper ambient super-object); the alias
 *   makes this collapse explicit.  Contrast with @c NaturalNumbersOf,
 *   @c IntegersOf, @c RationalsOf, @c RealsOf, @c ComplexesOf,
 *   @c DualSetOf, where χ_T : tower-ambient → Ω is a non-trivial
 *   classifier with multi-overload cross-carrier embedding-aware
 *   membership (e.g.\ @c N(-7) returns @c False because -7 ∈ ℤ does not
 *   land in ℕ ↪ ℤ).
 * - `B`: value-level instance @c BooleanSetOf<>{}, named for paper-listing
 *   readability and direct membership-test calls (e.g.\ @c B(true)).
 * - `BooleanSet` (non-exported): an internal convenience alias for
 *   `BooleanSetOf<>` used inside this partition.  Not part of the
 *   public surface — external callers should spell `BooleanSetOf<>` or
 *   `decltype(B)` for the same type.
 *
 * @section algebra_boolean__Paper_Alignment
 * In the paper's Feature Cube (bool row), logical (`||`, `&&`) and bitwise
 * (`|`, `&`) operators over bool share the same lattice behavior (join/meet,
 * identities, absorbers, and distributivity). The test suite validates this
 * alignment explicitly.
 *
 * Element scouts are post-#559 spelled @c element<𝔹> (BoundScout factory
 * over the Boolean universe value @c 𝔹 = @c Ω<bool>); the legacy
 * @c var<...> family was retired in Phase 2e.3 of the Ω-ambient redesign
 * (#551), and the @c element<Ω<𝔹>> intermediate spelling went away in
 * #559's option-A migration once @c 𝔹 stopped being a carrier alias.
 *
 * @note "La matematica non e una collezione di trucchi: e grammatica delle
 * forme." (Mathematics is not a bag of tricks; it is a grammar of forms.) —
 * Emma Castelnuovo as quoted by B. L. van der Waerden (1975)
 */
module;

#include <functional>

export module dedekind.algebra:boolean;

import dedekind.category;
import dedekind.order;
import dedekind.sets;
import :universal;

namespace dedekind::algebra {
using namespace dedekind::category;
using namespace dedekind::sets;

export template <typename L = dedekind::category::ClassicalLogic,
                 typename C = Finite>
using BooleanSetOf = UniversalSet<bool, L, C>;

// Non-exported convenience alias used by the value-level B constant
// below.  The exported public surface is `BooleanSetOf<L, C>` (the
// parameterised template); callers naming the default form should
// either use `BooleanSetOf<>` directly or @c decltype(B).  This keeps
// the namespace surface small, per Copilot review on PR #407.
using BooleanSet = BooleanSetOf<>;

/** @brief The canonical Boolean universe @c 𝔹 = @c Ω<bool> (post-#559).
 *
 *  @details Per #559's chosen direction (option A): the named species
 *  symbols (@c 𝔹 / @c ℕ / @c ℤ / @c ℚ / @c ℝ / @c ℂ / @c 𝔻) denote the
 *  @b universe values (constexpr instances of @c UniversalSet over the
 *  carrier), not carrier @b types.  Carrier types are spelled directly
 *  (@c bool, @c Cardinality, etc.) in template-type-parameter positions;
 *  the math symbols denote the sets.
 *
 *  This makes @c element<𝔹> the canonical scout spelling — closer to
 *  textbook math notation than the previous @c element<Ω<𝔹>> form (which
 *  required @c 𝔹 to be a type alias for @c bool).  The Boolean structures
 *  @c bool carries — the Boolean rig (@c bool, @c ∨, @c ∧), the Galois
 *  field 𝔽₂ (@c bool, @c ⊕, @c ∧), the order lattice — are still witnessed
 *  on the carrier @c bool directly (see formal-verification block below
 *  and @c numbers:boolean).
 *
 *  Pre-#559 the spelling was @c using @c 𝔹 @c = @c bool (carrier-type
 *  alias); the ~25 type-context sites of @c 𝔹 in concept gates and
 *  static_asserts were migrated to @c bool directly in step 1 of this
 *  PR (#559, slice 𝔹).
 */
export inline constexpr UniversalSet<bool, ClassicalLogic, Finite> 𝔹 =
    sets::Ω<bool>;

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
 * on --- @c SingletonSet (@c :sets:singleton),
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

// The logical-operator shape concept already lives at the lower layer as
// @c dedekind::category::HasLogicalOperators<T> (in @c :logic, introduced
// under #393), as a sibling of @c HasRingOperators / @c HasFieldOperators
// / @c HasLatticeOperators in the shape-concept family.  Reusing it here
// rather than duplicating; the L-parametric variant (where @c && / @c ||
// close to @c L::Ω rather than to @c T) is a future refinement that can
// be added on top of the category-layer concept if a non-Boolean truth-
// value carrier carrier ever calls for it.
static_assert(dedekind::category::HasLogicalOperators<bool>);

export inline constexpr BooleanSet B{};

/** @section algebra_boolean__Formal_Verification */

// BooleanSet is the canonical IsSet witness for the Boolean ambient universe.
static_assert(dedekind::category::IsSet<
                  decltype(dedekind::category::ambient_set<bool>(B))>,
              "BooleanSet must be the canonical IsSet anchor for bool.");

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
