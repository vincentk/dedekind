/**
 * @file small.cppm
 * @brief Level 0b: Small Categories (Enumerated Morphism Sets).
 *
 * @partition :small
 * @section Small: Categories with Set-Sized Morphisms
 * A Small Category is a category where the collection of morphisms is a Set.
 * This partition enables the definition of finite categories, diagrams,
 * and discrete structures that can be explicitly navigated.
 *
 * Wikipedia: Small category, Discrete category
 */
export module dedekind.category:small;

import :logic;
import :functorial;

namespace dedekind::category {

/**
 * @section Small_Category_Concepts
 * Concepts for categories with enumerable Objects (Obj) and
 * Hom-sets (Hom<A, B>).
 */

/**
 * @section Discrete_Categories
 * Special cases where the only morphisms are identities.
 */

}  // namespace dedekind::category
