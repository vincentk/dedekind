/**
 * @file etcs.cppm
 * @brief Level 0c: Elementary Theory of the Category of Sets (ETCS).
 *
 * @partition :etcs
 * @section ETCS: The Categorical Axiomatization of Set Theory
 * This partition provides the 10 axioms of ETCS, allowing the Dedekind
 * Universe to treat "Sets" as objects within a category rather than
 * collections defined by membership (ZF).
 *
 * Wikipedia: Elementary Theory of the Category of Sets
 */
module;

#include <concepts>
#include <functional>

export module dedekind.category:etcs;

import :logic;
import :species;

namespace dedekind::category {

/**
 * @concept IsCharacteristic
 * @brief The Morphic identity of a Set-like species (ETCS Compliance).
 * @details S : Domain → Ω.
 */
export template <typename S, typename Ω = ClassicalLogic>
concept IsCharacteristic = IsArrow<S, typename S::Domain, typename Ω::type>;

}  // namespace dedekind::category