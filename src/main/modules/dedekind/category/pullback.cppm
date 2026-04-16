/**
 * @file dedekind/category/pullback.cppm
 * @partition :pullback
 * @brief Limits — Equalizers and Fiber Products (Pullbacks).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @quote
 * "The pullback is the categorical generalisation of the fiber product:
 *  it is the universal solution to finding pairs (x, y) that agree in Z."
 *  — Saunders Mac Lane, *Categories for the Working Mathematician*
 *
 * @section Pullbacks_and_Equalizers
 * This partition defines the two primary limit constructions used in the
 * Dedekind Topos:
 *
 * - **Equalizer** (`IsEqualizer`): The kernel of a parallel pair (f, g: A ⟶ B).
 *   Selects exactly those members of A where f and g coincide.
 *
 * - **Pullback** (`IsPullback`, `pullback`): The Fiber Product X ×_Z Y.
 *   Given f: X ⟶ Z and g: Y ⟶ Z, the pullback is a Subobject of X × Y
 *   containing exactly those pairs (x, y) where f(x) = g(y).
 *
 * In this implementation both are realised as `Subobject` species whose
 * characteristic morphism χ encodes the membership predicate.
 *
 * Wikipedia: Pullback (category theory), Equaliser (mathematics)
 *
 * @note "In these days the angel of topology and the devil of abstract algebra
 * fight for the soul of each individual mathematical domain."
 *       -- Hermann Weyl, Invariants (1939)
 */
module;

#include <concepts>
#include <functional>

export module dedekind.category:pullback;

import :morphism;   // For arrow<>
import :logic;      // For LogicalValue
import :cartesian;  // For Product
import :topoi;

namespace dedekind::category {

/**
 * @concept IsPullback
 * @brief The Fiber Product (X ×_Z Y) as a Subobject of the Product (X × Y).
 *
 * @details A pullback of f: X ⟶ Z and g: Y ⟶ Z is an object P that represents
 * the subset of the product species X × Y where the images in Z coincide.
 *
 * In this implementation, the pullback is realized as a Subobject:
 * - ι: P ↣ X × Y is the inclusion into the product space.
 * - π₁ = p1 ∘ ι and π₂ = p2 ∘ ι are the projections to the domains.
 * - Commutativity (f ∘ π₁ = g ∘ π₂) is guaranteed by the characteristic
 *   morphism χ used to classify the subobject.
 *
 * @tparam P The candidate pullback species (a Subobject).
 * @tparam F The morphism f: X ⟶ Z.
 * @tparam G The morphism g: Y ⟶ Z.
 */
export template <typename P, typename F, typename G>
concept IsPullback = IsArrow<F> && IsArrow<G> && std::same_as<Cod<F>, Cod<G>> &&
                     IsSubobject<P, typename P::Ambient> &&
                     IsProduct<typename P::Ambient, Dom<F>, Dom<G>> &&
                     requires(P p, typename P::Member m) {
                       // π₁: P ⟶ X and π₂: P ⟶ Y (Projections via the inclusion
                       // ι)
                       { p.π1(m) } -> std::same_as<Dom<F>>;
                       { p.π2(m) } -> std::same_as<Dom<G>>;
                     };

/**
 * @brief The Characteristic Morphism χ: (X × Y) ⟶ Ω.
 *
 * In a Topos, every subobject is uniquely determined by a morphism into the
 * subobject classifier Ω. For a pullback, χ identifies pairs (x, y)
 * that map to the same image in the codomain Z.
 *
 * @tparam L The Logic Species defining the Subobject Classifier Ω.
 * @tparam Π The Product Species (e.g., std::pair<X, Y>).
 * @tparam F Morphism type for f: X ⟶ Z (must share codomain with G).
 * @tparam G Morphism type for g: Y ⟶ Z (must share codomain with F).
 * @param f The first morphism f: X ⟶ Z.
 * @param g The second morphism g: Y ⟶ Z.
 * @return An arrow χ: Π ⟶ Ω that is `L::True` iff f(π₁(pair)) == g(π₂(pair)).
 * @note Cod<F> and Cod<G> must be the same type and equality-comparable.
 */
template <typename L, typename Π, typename F, typename G>
  requires IsLogicalSpecies<L> && IsArrow<F> && IsArrow<G> &&
           IsProduct<Π, Dom<F>, Dom<G>> && std::same_as<Cod<F>, Cod<G>> &&
           std::equality_comparable<Cod<F>>
auto make_χ(F f, G g) {
  using Ω = typename L::Ω;

  // We capture by value because Morphism is a lightweight skeletal struct.
  // If F or G contain large lambdas, Morphism handles the storage.
  return arrow<Π, Ω>([f, g](const Π& pair) -> Ω {
    return (f(pair.first) == g(pair.second)) ? L::True : L::False;
  });
}

/**
 * @brief The Pullback Factory — constructs the Fiber Product X ×_Z Y.
 *
 * Given two morphisms f: X ⟶ Z and g: Y ⟶ Z, constructs the Fiber Product
 * P ≅ X ×_Z Y as the Subobject of Π = X × Y classified by the characteristic
 * morphism χ(x, y) = (f(x) == g(y)).
 *
 * @tparam L  The Logic Species used for the Subobject Classifier Ω.
 *            Defaults to `ClassicalLogic` (bool).
 * @tparam Π  The Product Species (must satisfy IsProduct<Π, Dom<F>, Dom<G>>).
 *            Typically `std::pair<X, Y>`.
 * @tparam F_Raw The (possibly raw callable) type for the first morphism f: X ⟶
 * Z.
 * @tparam G_Raw The (possibly raw callable) type for the second morphism g: Y ⟶
 * Z.
 * @param f_raw The first morphism (or raw callable) f: X ⟶ Z.
 * @param g_raw The second morphism (or raw callable) g: Y ⟶ Z.
 * @return A `Subobject<Π, χ>` representing P = X ×_Z Y.
 */
export template <typename L = ClassicalLogic, typename Π, typename F_Raw,
                 typename G_Raw>
auto pullback(F_Raw&& f_raw, G_Raw&& g_raw) {
  // 1. Lift raw callables into formal Morphisms using your skeletal factory.
  auto f = arrow(std::forward<F_Raw>(f_raw));
  auto g = arrow(std::forward<G_Raw>(g_raw));

  // 2. Construct the characteristic morphism χ using the formal Arrows.
  auto χ = make_χ<L, Π>(f, g);

  // 3. The Pullback is the subobject of Π classified by χ.
  return classify<Π>(χ);
}

/**
 * @concept IsEqualizer
 * @brief The kernel of a parallel pair (f, g: A ⟶ B).
 *
 * @details In the Dedekind Topos, an Equalizer is a `Subobject` E ↣ A
 * containing exactly those members where the morphisms f and g coincide.
 * It is the internal "Solution Set" for the equation f(x) = g(x).
 *
 * @section Universal_Property
 * For any object Q and morphism q: Q ⟶ A such that f ∘ q = g ∘ q,
 * there exists a unique morphism u: Q ⟶ E such that ι ∘ u = q.
 *
 * @tparam E The candidate Equalizer species.  Must satisfy
 *           `IsSubobject<E, typename F::Domain>`, guaranteeing:
 *           - an inclusion ι: E ↣ A into the shared domain A,
 *           - a characteristic morphism χ: A ⟶ Ω (an `IsPredicate`)
 *             with `Dom<χ> == A`, classifying the solution set.
 * @tparam F The first parallel morphism f: A ⟶ B.
 * @tparam G The second parallel morphism g: A ⟶ B.
 */
export template <typename E, typename F, typename G>
concept IsEqualizer =
    IsArrow<F> && IsArrow<G> &&
    std::same_as<typename F::Domain, typename G::Domain> &&
    std::same_as<typename F::Codomain, typename G::Codomain> &&
    IsSubobject<E, typename F::Domain>;

}  // namespace dedekind::category
