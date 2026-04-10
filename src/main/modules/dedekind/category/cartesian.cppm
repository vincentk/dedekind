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
module;

#include <concepts>
#include <utility>
#include <variant>

export module dedekind.category:cartesian;

import :logic;
import :discrete;

namespace dedekind::category {

/**
 * @concept IsProduct
 * @brief categorification of std::pair as the categorical product (A × B).
 * @details A product of A and B is an object P equipped with projection
 * morphisms π₁: P -> A and π₂: P -> B such that for any object X with morphisms
 * f: X -> A and g: X -> B, there exists a unique morphism u: X -> P making the
 * following diagram commute:
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
export template <typename P, typename A, typename B>
concept IsProduct = requires(P p) {
  { p.first } -> std::convertible_to<A>;
  { p.second } -> std::convertible_to<B>;
};

static_assert(
    IsProduct<std::pair<int, bool>, int, bool>,
    "Verification Failed: std::pair<int, bool> must satisfy IsProduct.");

/**
 * @concept IsCoproduct
 * @brief categorification of std::variant of exactly two typesas the
 * categorical coproduct (A + B).
 * @details A coproduct of A and B is an object C equipped with injection
 * morphisms ι₁: A -> C and ι₂: B -> C such that for any object X with morphisms
 * f: A -> X and g: B -> X, there exists a unique morphism v: C -> X making the
 * following diagram commute: A       B
 *        \     /
 *         ι₁   ι₂
 *          \   /
 *           C
 *          / \
 *         f   g
 *        /     \
 *       X       X
 */
template <typename T, typename A, typename B>
concept IsCoproduct = requires(A a, B b) {
  { T(a) } -> std::same_as<T>;
  { T(b) } -> std::same_as<T>;
};

export template <typename T, typename V>
auto inject(V&& value) {
  // In a true topos, this finds the unique injector into the coproduct
  return std::variant<std::decay_t<V>, T>(std::forward<V>(value));
}

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
