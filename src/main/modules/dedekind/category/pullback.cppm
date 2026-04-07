/**
 * @concept IsPosetal
 * @brief Represents a Posetal Category (A Category derived from a Partially
 * Ordered Set).
 *
 * @section Categorical_Definition
 * A Posetal Category is a category where for any two objects A and B, there is
 * **at most one** morphism from A to B. In this framework:
 * - Objects are elements of the set.
 * - Morphisms represent the relation (a ≤ b).
 * - Identity morphisms correspond to Reflexivity (a ≤ a).
 * - Composition corresponds to Transitivity (a ≤ b and b ≤ c implies a ≤ c).
 * - Skeletality in the category corresponds to Antisymmetry (a ≤ b and b ≤ a
 * implies a = b).
 *
 * @section Order_Structure
 * This structure corresponds to a **Partially Ordered Set (Poset)**. Unlike a
 * Preorder, a Posetal Category is skeletal, meaning isomorphic objects are
 * identical.
 *
 * @quote
 * "In a sense, the most basic category is a partially ordered set;
 *  the arrows are just the instances of the order relation."
 *  — Saunders Mac Lane, *Categories for the Working Mathematician*
 *
 * @tparam T The type of objects in the poset.
 * @tparam Rel The relation defining the order (the Morphism Generator).
 */
module;

#include <concepts>
#include <functional>

export module dedekind.category:pullback;

import :limit;

namespace dedekind::category {
/**
 * @concept IsPullback
 * @brief The universal construction of the pullback (fiber product).
 * @details Given morphisms f: X -> Z and g: Y -> Z, a pullback is an object P
 *          along with morphisms p1: P -> X and p2: P -> Y such that f ∘ p1 =
 *          g ∘ p2, and for any other object Q with morphisms q1: Q -> X and
 *          q2: Q -> Y satisfying the same commutativity, there exists a unique
 *          morphism u: Q -> P making the entire diagram commute.
 *
 * @tparam P  The candidate pullback object.
 * @tparam Z  The codomain of both f and g.
 * @tparam f  The morphism from X to Z.
 * @tparam g  The morphism from Y to Z.
 */
export template <typename P, typename f, typename g>
concept IsPullback = IsArrow<f> && IsArrow<g> &&
                     std::same_as<typename f::Codomain, typename g::Codomain> &&
                     requires(P p, typename f::Domain x, typename g::Domain y) {
                       // The pullback must provide morphisms to X and Y
                       { p.p1(x) } -> std::same_as<typename f::Domain>;
                       { p.p2(y) } -> std::same_as<typename g::Domain>;

                       // The pullback must satisfy the commutativity condition:
                       // f ∘ p1 = g ∘ p2
                       requires(f(p.p1(x)) == g(p.p2(y)));

                       // For any other object Q with morphisms q1: Q -> X and
                       // q2: Q -> Y satisfying the same commutativity, there
                       // must exist a unique morphism u: Q -> P.
                       requires requires(typename P::Domain q) {
                         { p.u(q) } -> std::same_as<P>;
                         requires(f(p.p1(q)) == g(p.p2(q)));
                       };
                     };

}  // namespace dedekind::category
