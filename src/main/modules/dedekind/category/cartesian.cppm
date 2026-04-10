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
#include <functional>
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
 * @concept IsExponential
 * @brief Structural property of an Internal Hom-set (B^A).
 * @details Identifies a type as an inhabitant of the function space
 * from A to B based on its moveability and invocability.
 */
export template <typename F, typename A, typename B>
concept IsExponential =
    std::move_constructible<F> && std::same_as<std::invoke_result_t<F, A>, B>;

/**
 * @section Evaluation_Morphism
 * The canonical 'eval' for the CCC: (B^A x A) -> B.
 */
export template <typename F, typename A>
  requires IsExponential<F, A, std::invoke_result_t<F, A>>
auto eval(F&& f, A&& a) -> decltype(auto) {
  return std::invoke(std::forward<F>(f), std::forward<A>(a));
}

/** @section Structural_Exponential_Verification */

// 1. The "Heavy" Exponential: Type-erased function space
static_assert(IsExponential<std::function<bool(int)>, int, bool>,
              "Axiom: std::function must satisfy the Internal Hom-set.");

#if __has_include(<functional>) && defined(__cpp_lib_move_only_function)
// 2. The "Move-Only" Exponential: C++23's modern function space
#include <functional>  // for std::move_only_function if available
static_assert(IsExponential<std::move_only_function<bool(int)>, int, bool>,
              "Axiom: move_only_function is a valid Exponential inhabitant.");
#endif

// 3. The "Light" Exponential: Anonymous Structural Closure
// This captures the 'essence' without the 'lineage'
auto closure = [limit = 42](int x) { return x < limit; };
static_assert(
    IsExponential<decltype(closure), int, bool>,
    "Structural: A lambda is discovered as an Exponential by its scent.");

// 4. The "Honest Rejection": Mismatched Signature
// Valid function, but wrong mapping for this specific Hom-set
static_assert(!IsExponential<std::function<int(int)>, int, bool>,
              "Verification: Rejected due to Codomain mismatch.");

}  // namespace dedekind::category
