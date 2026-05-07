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
 * @section pullback__Pullbacks_and_Equalizers
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
 * @section pullback__Kernel_Pair_and_Coequalizer_dual
 * Two structurally-named extensions of the above, anchoring the
 * @em image-as-coequalizer-of-kernel-pair reading the §3 paper outline
 * (`Functions on Sets: Filtering and Transforming`) refers to:
 *
 * - **Kernel pair** (`IsKernelPair<P, F>`): the pullback of an arrow F
 *   against itself, i.e.\ the equivalence relation @em "have the same
 *   F-image" on `Dom<F>`.  Concretely
 *   @c IsKernelPair<P, @c F> @c ≡ @c IsPullback<P, @c F, @c F>; the
 *   alias exists to give the kernel-pair role its textbook name at the
 *   concept level.
 *
 * - **Coequalizer** (`IsCoequalizer<Q, F, G>`): the categorical dual of
 *   `IsEqualizer`.  Where the equalizer is the largest @em subobject
 *   of @c Dom<F> on which two parallel arrows agree, the coequalizer
 *   is the smallest @em quotient of @c Cod<F> that equates them.
 *   Image of a single arrow @c F is the coequalizer of its kernel
 *   pair: @f$\mathrm{Im}(F) @f$ @f$=@f$
 *   @f$\mathrm{coeq}(\pi_1, \pi_2 : \mathrm{KernelPair}(F) \rightrightarrows
 *   \mathrm{Dom}(F))@f$, the canonical epi-mono factorisation reading.
 *
 * The coequalizer concept here pins only the structural-shape contract
 * (parallel arrows with matching @c Dom and @c Cod).  The universality
 * (existence and uniqueness of the unique factoring map for any
 * equalising arrow) is the engineer's honesty obligation, as for
 * @c IsNNO --- C++ concepts cannot quantify over the universal
 * property's $\forall$, only over the structural shape.  The dual side
 * (an `IsQuotient` sister to `IsSubobject`) is not yet reified in the
 * codebase; until it is, the @em quotient half of "Q is a quotient of
 * Cod<F>" lives as documentation rather than as a type-level
 * constraint.
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
 * @section pullback__Universal_Property
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
// Per the picking policy in :morphism (#411): use Dom / Cod in
// IsArrow-strict contexts; the IsArrow<F> / IsArrow<G> guards above
// cover the precondition.
export template <typename E, typename F, typename G>
concept IsEqualizer =
    IsArrow<F> && IsArrow<G> && std::same_as<Dom<F>, Dom<G>> &&
    std::same_as<Cod<F>, Cod<G>> && IsSubobject<E, Dom<F>>;

/**
 * @concept IsKernelPair
 * @brief The pullback of an arrow @c F against itself --- the
 *        equivalence relation @em "have the same F-image" on @c Dom<F>.
 *
 * @details For an arrow @c F @c : @c A @c → @c B, the kernel pair is
 * the pullback @c A @c ×_B @c A: pairs @c (x, @c x') @c ∈ @c A @c × @c A
 * such that @c F(x) @c = @c F(x').  Categorically this is the @em kernel
 * (in the equivalence-relation sense) of @c F, treated as an
 * equivalence-relation object via the pullback.
 *
 * Structurally, @c IsKernelPair<P, @c F> is exactly @c IsPullback<P,
 * @c F, @c F>; the alias gives the kernel-pair role its textbook
 * name at the concept level so downstream witnesses can refer to it
 * by intent.
 *
 * The @b image of an arrow @c F is the coequalizer of @c F's kernel
 * pair (the canonical epi-mono factorisation of @c F), which is the
 * structural payoff this naming sets up.  See the @c IsCoequalizer
 * concept below.
 *
 * @tparam P The candidate kernel-pair species (a Subobject of
 *           @c Dom<F> @c × @c Dom<F>).
 * @tparam F The arrow whose kernel pair is being named.
 */
export template <typename P, typename F>
concept IsKernelPair = IsPullback<P, F, F>;

/**
 * @concept IsCoequalizer
 * @brief The categorical dual of @c IsEqualizer --- a quotient of
 *        @c Cod<F> that equates two parallel arrows @c F, @c G.
 *
 * @details For parallel arrows @c F, @c G @c : @c A @c → @c B, the
 * coequalizer is the smallest quotient @c Q of @c B together with
 * a regular epi @c q @c : @c B @c → @c Q satisfying
 *
 *   @c q @c ∘ @c F @c = @c q @c ∘ @c G                 (equalising)
 *
 * and universal among such: any other @c B @c → @c X equating @c F
 * and @c G factors uniquely through @c q.  In a topos the
 * coequalizer is the regular-epi onto the quotient by the smallest
 * equivalence relation containing the image of the parallel pair.
 *
 * Where @c IsEqualizer pins the structural shape via
 * @c IsSubobject<E, @c Dom<F>>, the dual @c IsCoequalizer would pin
 * @c Q via an @em IsQuotient sister concept --- which the codebase
 * does not yet reify.  Until it does, the structural shape pinned
 * here is the @em parallel-arrow precondition; the @em quotient
 * half lives as documentation.  This is consistent with @c IsNNO's
 * approach: the structural shape is what the type system can pin;
 * the universal property (existence and uniqueness of the unique
 * factoring map) is the engineer's honesty obligation.
 *
 * @section pullback__Image_as_coequalizer
 * For a single arrow @c F @c : @c A @c → @c B with kernel pair
 * @c π_1, @c π_2 @c : @c P @c ⇒ @c A (@c P satisfying
 * @c IsKernelPair<P, @c F> above), the @em image of @c F is the
 * coequalizer of @c (π_1, @c π_2) --- the canonical epi-mono
 * factorisation
 *
 *   @c A @c → @c Im(F) @c → @c B
 *
 * read off as "collapse the equivalence relation 'same F-image'".
 * This is the structural definition of image referenced by the §3
 * paper outline (Functions on Sets: Filtering and Transforming).
 *
 * @tparam Q The candidate coequalizer species (target quotient of
 *           @c Cod<F>); structurally not yet pinnable beyond
 *           parallel-arrow preconditions, see above.
 * @tparam F The first parallel arrow @c F @c : @c A @c → @c B.
 * @tparam G The second parallel arrow @c G @c : @c A @c → @c B.
 */
export template <typename Q, typename F, typename G>
concept IsCoequalizer =
    IsArrow<F> && IsArrow<G> && std::same_as<Dom<F>, Dom<G>> &&
    std::same_as<Cod<F>, Cod<G>>;

}  // namespace dedekind::category
