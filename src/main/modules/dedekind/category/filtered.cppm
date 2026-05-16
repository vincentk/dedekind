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
 * axiom captured by @c :species::is_directed_v.
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
 * bound of every pair (and @c std::totally_ordered<bool> witnesses
 * @c is_directed_v through the @c :species specialisation).  The
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
import :species;  // is_directed_v, is_reflexive_v, is_transitive_v
import :thin;     // IsThinCategory — the faithful row-1 inclusion

namespace dedekind::category {

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
                 typename L = ClassicalLogic>
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

}  // namespace dedekind::category
