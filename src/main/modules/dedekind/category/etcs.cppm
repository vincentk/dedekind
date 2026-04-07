/**
 * @file etcs.cppm
 * @partition :etcs
 * @brief Level 0c: Elementary Theory of the Category of Sets (ETCS).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 *
 * @section ETCS: The Categorical Axiomatization of Set Theory
 * "In the mathematical development of recent decades, the notion of
 *  set has not only played a fundamental role, but it has itself
 *  passed through a long process of refinement."
 *  — F. William Lawvere, "An Elementary Theory of the Category of Sets"
 *
 * @details
 * This partition provides the axiomatic foundations of ETCS, allowing
 * the Dedekind Universe to treat "Sets" as objects within a category
 * rather than collections defined by membership (ZF).
 *
 * We utilize the 'Ontology Bridge' (SpeciesTraits) to ensure that
 * primitive types (int, bool, size_t) can be treated as first-class
 * categorical objects without violating their C++ nature.
 *
 * @build_order 0
 * @dependency :species, :logic
 *
 * Wikipedia: Elementary Theory of the Category of Sets
 */
module;

#include <concepts>
#include <functional>

export module dedekind.category:etcs;

import :logic;
import :morphism;
import :species;

namespace dedekind::category {

/**
 * @concept IsCharacteristic
 * @brief The Morphic identity of a Set-like species (ETCS Compliance).
 * @details S : Domain → Ω.
 */
export template <typename S, typename Ω = ClassicalLogic>
concept IsCharacteristic =
    IsArrow<S, typename SpeciesTraits<S>::Domain, typename Ω::type>;

}  // namespace dedekind::category