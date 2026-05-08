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
 * @concept HasPartialOrderOperators
 * @brief @b Pure @b syntactic @b shape: T supports the partial-order
 *        operators @c <, @c <=, @c >, @c >= with results in the
 *        truth-value carrier @c L::Ω of a chosen logical species.
 *
 * @details
 * Use this concept where a callsite needs the three logical operators
 * to compile and yield a value in the truth-value carrier of a chosen
 * @c IsLogicalSpecies, but does @b not want to bind to a particular
 * logical species (classical / Boolean, ternary / Kleene, ...). No claim about
 * reflexivity, antisymmetry, transitivity, or comparability is made
 * here; for those, use @c IsPreOrdered / @c IsPartiallyOrdered /
 * @c IsTotallyOrdered (the last in @c :order:total).
 *
 * The @c L parameter defaults to @c ClassicalLogic (so @c L::Ω is
 * @c bool); supply a different @c IsLogicalSpecies to constrain the
 * return type to a non-Boolean truth-value carrier (e.g.\ Kleene
 * @c TernaryLogic).  Mirrors the @c L-parametric pattern already used
 * by @c IsPreOrdered / @c IsPartiallyOrdered.
 *
 * Sibling of @c dedekind::algebra::HasRingOperators (in @c
 * algebra:ring), @c dedekind::algebra::HasFieldOperators (in @c
 * algebra:field), and @c HasLatticeOperators (in @c order:lattice)
 * in the shape-concept family.  The split between @b shape and @b
 * axiom mirrors the literal-vs-strict tier introduced under PR #394.
 */
export template <typename T, typename L = ClassicalLogic>
concept HasLogicalOperators = requires(const T a, const T b) {
  { a && b } -> std::same_as<typename L::Ω>;
  { a || b } -> std::same_as<typename L::Ω>;
  { !a } -> std::same_as<T>;
};

static_assert(HasLogicalOperators<bool>);

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
