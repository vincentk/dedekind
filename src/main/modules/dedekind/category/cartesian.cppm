/**
 * @file cartesian.cppm
 * @brief Level 0b: Cartesian Closed Category (CCC) Foundations.
 *
 * @partition :cartesian
 * @section Cartesian: Products, Exponentials, and Terminal Objects
 * This partition defines the structures required for a category to be
 * Cartesian Closed. In the Dedekind universe, this provides the "Product"
 * and "Function Space" (Exponential) mechanics necessary for ETCS.
 *
 * Wikipedia: Cartesian closed category, Exponential object
 */
export module dedekind.category:cartesian;

import :logic;
import :discrete;

namespace dedekind::category {

/**
 * @concept IsProduct
 * @brief categorification of std::pair as the categorical product (A × B).
 * @details A product of A and B is an object P equipped with projection morphisms
 * π₁: P -> A and π₂: P -> B such that for any object X with morphisms f: X -> A and g: X -> B, there exists a unique morphism u: X -> P making the following diagram commute:
 *       X
 *      / \
 *     f   g
 *    /     \
 *   A       B
 *    \     /
 *     π₁   π₂
 *      \   /
 *       P
*/
export template <typename T>
concept IsProduct = requires {
  typename T::Left;
  typename T::Right;
};

/**
 * @concept IsCoproduct
 * @brief categorification of std::variant as the categorical coproduct (A + B).
 * @details A coproduct of A and B is an object C equipped with injection morphisms
 * ι₁: A -> C and ι₂: B -> C such that for any object X with morphisms f: A -> X and g: B -> X, there exists a unique morphism v: C -> X making the following diagram commute:
 *       A       B
 *        \     /
 *         ι₁   ι₂
 *          \   /
 *           C
 *          / \
 *         f   g
 *        /     \
 *       X       X  
 */
export template <typename A>
concept IsCoproduct = requires {
  typename A::First;
  typename A::Second;
};

/**
 * @section Terminal_Object
 * The unique object '1' from which there is exactly one morphism to any object.
 * In C++, this is typically reified as a unit or monostate type.
 */

/**
 * @section Cartesian_Closed_Concepts
 * Concepts for Products (A × B) and Exponentials (B^A).
 */

}  // namespace dedekind::category
