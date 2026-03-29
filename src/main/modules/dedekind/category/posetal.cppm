/**
 * @file posetal.cppm
 * @brief Level 0b: Posetal Categories (Morphisms as Relations).
 *
 * @partition :posetal
 * @section Posetal: Categories as Partially Ordered Sets
 * A Posetal Category is a structure where there is at most one morphism
 * between any two objects. This partition enables the interpretation
 * of "A -> B" as the relation "A ≤ B".
 *
 * Wikipedia: Poset, Preorder (category theory)
 */
export module dedekind.category:posetal;

import :logic;

namespace dedekind::category {

/**
 * @section Zero_Morphism
 * The unique morphism existing between any two objects in a collapsed
 * posetal structure.
 */

}  // namespace dedekind::category
