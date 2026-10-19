/**
 * @file dedekind/category/filtered.cppm
 * @partition :filtered
 * @brief Filtered categories — categories whose finite diagrams have a
 *        cocone.
 *
 * @section filtered__Categorical_Definition
 * A @b filtered category is one in which every finite diagram has a
 * @b cocone (an upper bound, in poset terms).  The standard textbook
 * definition is three-fold:
 *
 * - The category is @b nonempty.
 * - For every pair of objects @c (a, b), there exists an object @c c
 *   and morphisms @c a @c → @c c, @c b @c → @c c (binary upper bound).
 * - For every parallel pair @c f, @c g @c : @c a @c → b, there exists
 *   @c h @c : @c b @c → @c c equalising them.
 *
 * @section filtered__Thin_Case
 * In a thin category (hom-sets propositional), the third clause is
 * automatic: parallel morphisms are equal by thinness, so any choice
 * of @c h coequalises trivially.  Filtered then reduces to:
 *
 *   @b nonempty + ∀ a, b ∈ T. ∃ c ∈ T. Rel(a, c) ∧ Rel(b, c)
 *
 * By induction on finite-subset size, the binary case extends to
 * arbitrary finite subsets — that's exactly the @b directedness
 * axiom captured by @c is_directed_v (below).
 *
 * @section filtered__Form_Chain_Row_3
 * @c IsFilteredCategory is row 3 of the lattice Form-chain (#698):
 *
 * @code
 *   IsThinCategory        (row 1, preorder)
 *      ↓ + antisymmetry
 *   IsPosetal             (row 2, poset)
 *
 *   IsThinCategory        (row 1, parallel branch)
 *      ↓ + directedness
 *   IsFilteredCategory    (row 3, this partition)
 *
 *   row 2 + row 3 + cofiltered + universality
 *      ↓
 *   IsLatticeCategory     (row 4, Form-chain home — Slice 3)
 * @endcode
 *
 * Filtered cats are @b not refinements of posetal — they need only
 * thin + directed, NOT antisymmetric.  The textbook example of a
 * filtered non-poset is a preorder with two distinct maximal elements
 * that are mutually @c ≤; under thin+directed it counts as filtered,
 * but it is not a poset.
 *
 * @section filtered__Why_Filtered_Matters
 * Filtered categories are the natural indexing shape for @b filtered
 * @b colimits, which preserve finite limits.  In the context of
 * @c :sets::mereology and the subobject classifier reading (#698):
 *
 * - @b Unrestricted-fusion mereology (Leśniewskian gunk-friendly
 *   variants) needs arbitrary colimits in Sub(X) — these require
 *   filtered cocompleteness of the ambient.
 * - @b Filtered Ind-objects on Sub(X) name "limits of finite parts" —
 *   the categorical machinery behind "arbitrary fusion".
 *
 * Whether the project ever needs this gunk-axis machinery is open;
 * @c :filtered names the row regardless, so the @c IsLatticeCategory
 * assembly in Slice 3 can encode the faithful inclusion
 * @c IsLatticeCategory @c ⊊ @c IsFilteredCategory definitionally.
 *
 * @section filtered__Boolean_Witness
 * @c bool with @c std::less_equal is filtered: @c true is the upper
 * bound of every pair (its reflexive, connex @c <= witnesses
 * @c is_directed_v through the specialisation below).  The
 * 2-element thin category is also filtered.
 *
 * @see https://en.wikipedia.org/wiki/Filtered_category
 * @see https://ncatlab.org/nlab/show/filtered+category
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "Pas à pas, on va loin."  (Step by step, one goes far.)
 *       — French proverb; here the steps are slices of #698.
 */
module;

#include <concepts>
#include <functional>

export module dedekind.category:filtered;

import :logic;
import :mereology;  // is_reflexive_v, is_transitive_v
import :thin;       // IsThinCategory — the faithful row-1 inclusion

namespace dedekind::category {

/** @section filtered__Directedness: ∀a,b ∃c. a ≤ c ∧ b ≤ c
 *
 *  @brief Trait to mark a relation as @b directed: every pair of
 *         elements has an upper bound.
 *
 *  @details Directedness is the one-sided existence axiom that
 *  upgrades a preorder / thin category to a @b filtered category
 *  (#698 row 3): every finite subset has SOME upper bound (induction
 *  reduces this to the pairwise case).
 *
 *  Filtered ⊊ thin: every filtered category is thin, but filtered
 *  additionally requires the upper-bound existence claim.  Note that
 *  filtered does @b not require antisymmetry — it is parallel to,
 *  not downstream of, @c :posetal in the Form-chain.
 */
export template <typename T, typename Rel>
struct is_directed : std::false_type {};

/** @brief Discovery: types may opt in via a nested @c is_directed_v
 *         template member, mirroring the @c is_reflexive_v /
 *         @c is_transitive_v / @c is_antisymmetric_v discovery pattern. */
template <typename T, typename Rel>
  requires requires { T::template is_directed_v<Rel>; }
struct is_directed<T, Rel>
    : std::bool_constant<T::template is_directed_v<Rel>> {};

export template <typename T, typename Rel>
inline constexpr bool is_directed_v = is_directed<T, Rel>::value;

/** @brief A reflexive, connex @c <= is directed: of @c a and @c b one is
 *         above the other, and it bounds both (reflexivity bounds itself).
 *         Derived from the certificates (@c :mereology), not from the
 *         syntactic @c std::totally_ordered<T>, which @c double satisfies
 *         (#1043). */
template <typename T>
  requires(is_reflexive_v<T, std::less_equal<T>> &&
           is_connex_v<T, std::less_equal<T>>)
struct is_directed<T, std::less_equal<T>> : std::true_type {};

/**
 * @concept IsDirected
 * @brief Formal verification of the directedness axiom:
 *        ∀ a, b ∈ T. ∃ c ∈ T. Rel(a, c) ∧ Rel(b, c).
 *
 * @details At the value level, every pair has an upper bound under
 * @c Rel.  Specialised at trait level rather than constructively
 * because exhibiting @c c for arbitrary @c a, @c b in a generic
 * concept body is awkward; the trait specialisation captures the
 * mathematical content for the canonical witnesses
 * (the certified chains: reflexive and connex).
 */
export template <typename T, typename Rel, typename Ω = bool>
concept IsDirected =
    IsTruthObject<Ω> && requires(const Rel& rel, const T& a, const T& b) {
      { rel(a, b) } -> std::convertible_to<Ω>;
    } && requires { requires is_directed_v<T, Rel>; };

/** @section filtered__Codirectedness: ∀a,b ∃c. c ≤ a ∧ c ≤ b
 *
 *  @brief Trait to mark a relation as @b codirected: every pair of
 *         elements has a lower bound.
 *
 *  @details Codirectedness is the dual of directedness — every pair has
 *  SOME lower bound, not necessarily a greatest lower bound (meet).
 *  Combined with directedness and posetal structure (antisymmetry), it
 *  upgrades a filtered category to a @b lattice category (#698 row 4):
 *
 *    lattice = posetal + filtered (UB exists) + cofiltered (LB exists)
 *            + universality (UB / LB are unique by antisymmetry)
 *
 *  Filtered and cofiltered are dual existence axioms; together with
 *  antisymmetry they suffice for the Form-witness assembly of
 *  @c IsLatticeCategory in @c :lattice.
 */
export template <typename T, typename Rel>
struct is_codirected : std::false_type {};

/** @brief Discovery: types may opt in via a nested @c is_codirected_v
 *         template member, mirroring the sibling trait patterns. */
template <typename T, typename Rel>
  requires requires { T::template is_codirected_v<Rel>; }
struct is_codirected<T, Rel>
    : std::bool_constant<T::template is_codirected_v<Rel>> {};

export template <typename T, typename Rel>
inline constexpr bool is_codirected_v = is_codirected<T, Rel>::value;

/** @brief A reflexive, connex @c <= is codirected: the lesser of @c a and
 *         @c b bounds both from below (#1043, as @c is_directed). */
template <typename T>
  requires(is_reflexive_v<T, std::less_equal<T>> &&
           is_connex_v<T, std::less_equal<T>>)
struct is_codirected<T, std::less_equal<T>> : std::true_type {};

/**
 * @concept IsCodirected
 * @brief Formal verification of the codirectedness axiom:
 *        ∀ a, b ∈ T. ∃ c ∈ T. Rel(c, a) ∧ Rel(c, b).
 */
export template <typename T, typename Rel, typename Ω = bool>
concept IsCodirected =
    IsTruthObject<Ω> && requires(const Rel& rel, const T& a, const T& b) {
      { rel(a, b) } -> std::convertible_to<Ω>;
    } && requires { requires is_codirected_v<T, Rel>; };

/**
 * @concept IsFilteredCategory
 * @brief A thin category whose every finite subset of objects has an
 *        upper bound (directedness).
 *
 * @details
 * Faithful inclusion: @c IsFilteredCategory ⊊ @c IsThinCategory.  Every
 * filtered category is thin (the relation IS the morphism, propositional
 * hom-sets), but filtered additionally requires the directedness axiom:
 *
 *   ∀ a, b ∈ T. ∃ c ∈ T. Rel(a, c) ∧ Rel(b, c).
 *
 * The pairwise statement extends to all finite subsets by induction.
 *
 * Antisymmetry is @b not required — filtered is parallel to, not
 * downstream of, @c :posetal::IsPosetal in the Form-chain.
 *
 * @tparam T   The Domain (Objects).
 * @tparam Rel The Relation (the unique-morphism witness).
 * @tparam L   The Logic Species (the Subobject Classifier Ω).
 */
export template <typename T, typename Rel = std::less_equal<T>,
                 typename L = Boole>
concept IsFilteredCategory =
    IsThinCategory<T, Rel, L> &&  // Faithful inclusion: filtered ⊊ thin.
    requires {
      /** @brief Directedness: every pair has an upper bound. */
      requires is_directed_v<T, Rel>;
    };

/** @section filtered__Canonical_Witnesses */

static_assert(IsFilteredCategory<bool>,
              "bool with std::less_equal is the canonical 2-element "
              "filtered category (true is the upper bound of every pair).");

static_assert(IsFilteredCategory<int>,
              "int with std::less_equal is filtered (totally ordered "
              "carriers are trivially directed).");

static_assert(!is_directed_v<double, std::less_equal<double>> &&
                  !is_codirected_v<double, std::less_equal<double>>,
              "<= on double is not directed: NaN has no upper bound "
              "(#1043; std::totally_ordered<double> certifies nothing)");

}  // namespace dedekind::category
