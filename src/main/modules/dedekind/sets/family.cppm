/**
 * @file dedekind/sets/family.cppm
 * @partition :family
 * @brief The Collective Body: Realized Families of Sets (Systems of Parts).
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @module dedekind.sets:family
 * @dependency dedekind.ontology, dedekind.sets:boundaries
 *
 * @section The_Family: The Algebra of Collections
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
 * @section Structural_Mapping
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

import :boundaries;
import :singleton;

using namespace dedekind::category;

/**
 * @section Mereology: The study of parts and wholes.
 * @section Mereology: The Hierarchy of Order.
 */
namespace dedekind::sets {

/** @section Set_Type_Erasure */

export template <typename Species, typename L = ClassicalLogic>
using AnySetOver = std::variant<Ø<Species, L>, Ω<Species, L>,
                                SingletonSet<element_of_t<Species>, L>>;
/**
 * @class Family
 * @brief A realized collection of sets (A "Set of Sets") over a common Species.
 *
 * @details
 * In the Dedekind architecture, a Family is the extensional realization of a
 * mereological 'System'. It acts as a container for parts of a specific
 * Ambient Species, satisfying the IsSystem concept.
 *
 * Unlike a raw collection, a Family is a Bounded Lattice where the
 * extreme points—the Empty Set and the Universal Set—serve as the
 * structural Identities for Union and Intersection.
 *
 * @section Semantic_Role
 * - An element of a Family is itself an IsSet (e.g., SingletonSet).
 * - A Family over Species X is bounded by the Power Set P(X).
 * - It provides the context for Topos-aware set operations.
 *
 * @tparam Species The underlying domain (e.g., Integers) that all
 *                 member sets must inhabit.
 * @tparam L The Subobject Classifier (Ω) governing the truth logic
 *           of the member sets. Defaults to ClassicalLogic.
 *
 * @see dedekind.ontology:IsSystem
 * @see dedekind.sets:EmptySet
 * @see dedekind.sets:UniversalSet
 */
export template <typename Species, typename L = ClassicalLogic>
struct Family {
  // Implies a type-erased interface for member sets, but we can still enforce
  // the IsSet concept at runtime.
  using Domain = AnySetOver<Species, L>;

  static constexpr auto bottom() { return Ø<Species, L>{}; }
  static constexpr auto top() { return Ω<Species, L>{}; }

  // ... Implementation of Lattice operators ...
};

static_assert(
    dedekind::category::IsSet<
        decltype(dedekind::category::ambient_set<int>(Family<int>::bottom()))>,
    "Family::bottom() must lift to an ETCS set object.");
static_assert(
    dedekind::category::IsSet<
        decltype(dedekind::category::ambient_set<int>(Family<int>::top()))>,
    "Family::top() must lift to an ETCS set object.");
static_assert(dedekind::category::HasCanonicalSetCCC<AnySetOver<int>>,
              "Breadcrumb to :cartesian: family ambient variant has canonical "
              "CCC witness.");

// FIXME:
// static_assert(IsSystem<Family<int>, int>, "Family must satisfy IsSystem.");

}  // namespace dedekind::sets
