/**
 * @file dedekind/sets/family.cppm
 * @partition :family
 * @brief The Collective Body: Realized Families of Sets (Systems of Parts).
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @dependency dedekind.ontology, dedekind.sets:boundaries
 *
 * @section family__The_Family
 * In the Dedekind topos, a Family is a 'Set of Sets'—a realized mereological
 * 'System' that collects various parts (subsets) of a common Ambient Species.
 * It acts as the higher-order structural layer where sets themselves
 * become the elements of a larger Lattice.
 *
 * @details
 * A Family is defined by its adherence to the Bounded Lattice axioms:
 * - Membership: An element of a Family must satisfy the IsSet concept.
 * - Integrity: All member sets must share the same 'Ambient Species' context.
 * - Extremality: The Family is bounded by the Empty Set (⊥) and the
 *   Universal Set (⊤) provided by the :boundaries partition.
 *
 * @section family__Structural_Mapping
 * While individual sets define 'Presence', the Family defines 'Space'.
 * By collecting singletons, intervals, and rule-based bodies, the Family
 * provides the topological foundation for advanced Dedekind structures
 * like Atlases and Manifolds.
 *
 * @tparam Species The underlying domain (e.g., Integers) that constrains
 *                 the membership of the constituent sets.
 * @tparam L The Subobject Classifier (Ω) governing the internal logic.
 *
 * Wikipedia: Family of sets, Power set, System of sets, Bounded lattice
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "The art of doing mathematics consists in finding that special case
 * which contains all the germs of generality."
 *       -- David Hilbert, quoted in Constance Reid, Hilbert (1970)
 */
module;

#include <compare>
#include <concepts>
#include <functional>
#include <variant>

export module dedekind.sets:family;

import dedekind.category;

import :mereology;

using namespace dedekind::category;

/**
 * @section family__Mereology
 * @section family__Mereology_2
 */
namespace dedekind::sets {

/** @section family__Set_Type_Erasure */

/** @brief A @b set @b family: a set whose elements are themselves sets,
 *  all sharing the same ambient species @c Species.
 *
 *  Detects "F is set-shaped (has @c Domain) AND F::Domain is set-shaped
 *  (has its own @c Domain) AND F::Domain::Domain is @c Species" — i.e.
 *  one level of nesting, common-species-uniform.
 *
 *  Matches @c Set<Set<Species, L, P>, L, λ> (the return type of
 *  @c power_set), and any other carrier whose elements are
 *  @c Domain-typedef-bearing carriers over @c Species.
 *
 *  Does NOT match the legacy @c Family<Species, L>::Domain =
 *  @c AnySetOver = @c std::variant<…> — the variant has no
 *  @c Domain typedef. Replacing @c Family with this concept-gated
 *  form is exactly the migration this concept enables.
 */
template <typename F>
concept IsSetFamily =
    IsSystem<F, typename F::Domain> && IsSet<F> && IsSet<typename F::Domain>;

//
// static_assert(IsSystem<Family<int>, int>, "Family must satisfy IsSystem.");

}  // namespace dedekind::sets
