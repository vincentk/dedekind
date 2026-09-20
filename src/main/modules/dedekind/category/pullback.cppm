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
 * @section pullback__Kernel_Pair_and_Parallel_Pair
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
 * - **Parallel pair** (`IsParallelPair<F, G>`): the structural
 *   precondition shared by an equalizer's input @b and a coequalizer's
 *   input --- two arrows @c F, @c G with matching @c Dom and @c Cod.
 *   Used here as the input shape for the image-as-coequalizer reading:
 *   the kernel pair of @c F supplies two parallel projections
 *   @c π_1, @c π_2 @c : @c P @c ⇒ @c Dom<F>, and the image is the
 *   coequalizer of that parallel pair.
 *
 * The full coequalizer concept ( @c IsCoequalizer<Q, F, G> ---
 * @c IsParallelPair<F, G> @c && @c IsQuotient<Q, @c Cod<F>>) lives
 * in @c :image alongside @c IsImageOf, since the image-as-coequalizer
 * reading is the structural payoff that ties the two sides of the
 * canonical epi-mono factorisation together.  @c IsQuotient itself
 * (the dual of @c IsSubobject, the "co-classifier" surface) lives in
 * @c :topoi where the subobject classifier already lives.
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
import :logic;      // For IsΩ
import :cartesian;  // For Product
import :topoi;

namespace dedekind::category {

/**
 * @concept IsPullback
 * @brief The apex of a pullback square over the cospan f: X ⟶ Z, g: Y ⟶ Z:
 *        a subobject carrying the two projection legs π₁ ⟶ X, π₂ ⟶ Y.
 *
 * @details A pullback of f: X ⟶ Z and g: Y ⟶ Z is an object P with legs
 * π₁: P ⟶ X and π₂: P ⟶ Y making the square commute (f ∘ π₁ = g ∘ π₂),
 * universal among such cones.  This concept pins the @b structural shape (an
 * apex subobject with the two legs), @b not the representation of the apex and
 * @b not the universal property.  Both of those are the engineer's-honesty
 * obligation, exactly as for @c IsEqualizer / @c IsImageOf / @c IsNNO
 * (C++ concepts reason about shape, not the ∀ that makes the cone terminal).
 *
 * @note Deliberately representation-agnostic (#881).  The @b canonical apex is
 * the fiber product @c X ×_Z Y, a subobject of the @b product @c X × Y with
 * @c π₁/@c π₂ the pair projections (this is what the @c pullback factory below
 * builds).  But the same universal object appears in @b reduced presentations
 * that are @b not subobjects of the product: the meet @c A ∩ B in @c Sub(U) is
 * the pullback of the inclusions @c A ↪ U ↩ B with apex a subobject of @c U
 * (legs the co-restrictions @c A∩B ↪ A, @c A∩B ↪ B); Codd's @c natural_join is
 * the fiber product over a shared column.  Requiring the apex to @b be the
 * product (the earlier @c IsProduct<P::Domain, X, Y> clause) matched only the
 * one canonical realization and rejected these; dropping it lets any apex that
 * carries the two legs qualify, while the fiber-product apex still satisfies it
 * (its @c π₁/@c π₂ are the legs).
 *
 * @tparam P The candidate pullback apex (a Subobject carrying π₁, π₂).
 * @tparam F The morphism f: X ⟶ Z.
 * @tparam G The morphism g: Y ⟶ Z.
 */
export template <typename P, typename F, typename G>
concept IsPullback = IsArrow<F> && IsArrow<G> && std::same_as<Cod<F>, Cod<G>> &&
                     IsSubobject<P, typename P::Domain> &&
                     requires(const P& p, typename P::Member m) {
                       // The two legs π₁: P ⟶ X and π₂: P ⟶ Y.  Probed through
                       // const P& --- IsArrow purity requires const invocation.
                       // Their existence is the shape; commutativity +
                       // universality are the honesty obligation (cf.
                       // IsEqualizer).
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
 * structural payoff this naming sets up.  The kernel pair's
 * projections @c π_1, @c π_2 form an @c IsParallelPair (below) on
 * @c Dom<F>; the coequalizer-of-parallel-pair half awaits an
 * @c IsQuotient surface (see the partition header).
 *
 * @tparam P The candidate kernel-pair species (a Subobject of
 *           @c Dom<F> @c × @c Dom<F>).
 * @tparam F The arrow whose kernel pair is being named.
 */
export template <typename P, typename F>
concept IsKernelPair = IsPullback<P, F, F>;

/**
 * @concept IsPushout
 * @brief The categorical dual of @c IsPullback: the apex of a pushout square
 *        over the @b span f: Z ⟶ X, g: Z ⟶ Y, carrying the two coprojection
 *        colegs ι1: X ⟶ P, ι2: Y ⟶ P.
 *
 * @details Every arrow of @c IsPullback is reversed.  The cospan (shared
 * @c Cod<F> @c = @c Cod<G>) becomes a @b span (shared @c Dom<F> @c = @c
 * Dom<G>); the projection legs P ⟶ X, P ⟶ Y become @b coprojection colegs X ⟶
 * P, Y ⟶ P (so the apex exposes @c p.ι1(x), @c p.ι2(y) rather than @c p.π1(m),
 * @c p.π2(m)).  In the poset @c Sub(U) the pushout = coproduct = @b join (the
 * least upper bound, taken over the initial object @c ∅), exactly dual to
 * product = pullback = @b meet (the greatest lower bound, over the terminal
 * @c U).  So @c A @c | @c B is the pushout of the span @c A ← @c ∅ → @c B, with
 * colegs @c A ↪ A∪B, @c B ↪ A∪B.
 *
 * Structural shape only, exactly as @c IsPullback: commutativity and the
 * ∀-universal property remain the engineer's-honesty obligation (C++ concepts
 * reason about shape, not the ∀ that makes the cocone initial).  #881.
 *
 * @note @b Scope: this is the @c Sub(U)-specific pushout, whose apex is a
 * @c IsSubobject just as @c IsPullback's is.  In a general category the strict
 * dual of a mono (subobject) apex is an @b epi (quotient) apex, so a general
 * colimit is not a subobject; @c category::IsQuotient (@c topoi.cppm) is the
 * dual representation earmarked for that general pushout / colimit.  Here the
 * join @c A @c | @c B genuinely @b is a subobject of the ambient @c U (in the
 * thin poset @c Sub(U) every object is a subobject of @c U), so the
 * subobject-apex constraint is exact for this setting, not an over-restriction.
 *
 * @tparam P The candidate pushout apex (a @c Sub(U) subobject carrying the
 *           colegs; the general colimit apex is a @c IsQuotient instead).
 * @tparam F The span arrow f: Z ⟶ X.
 * @tparam G The span arrow g: Z ⟶ Y.
 */
export template <typename P, typename F, typename G>
concept IsPushout = IsArrow<F> && IsArrow<G> && std::same_as<Dom<F>, Dom<G>> &&
                    IsSubobject<P, typename P::Domain> &&
                    requires(const P& p, const Cod<F>& x, const Cod<G>& y) {
                      // The two colegs ι1: X ⟶ P and ι2: Y ⟶ P (dual to π1/π2),
                      // probed through const P& (IsArrow purity requires const
                      // invocation).  Both land in the apex, so both return P's
                      // Member (dual to IsPullback's legs returning
                      // Dom<F>/Dom<G>); a void coleg must not qualify.
                      // Commutativity + universality remain the honesty
                      // obligation (cf. IsPullback / IsEqualizer).
                      { p.ι1(x) } -> std::same_as<typename P::Member>;
                      { p.ι2(y) } -> std::same_as<typename P::Member>;
                    };

/**
 * @concept IsParallelPair
 * @brief @c F and @c G are a parallel pair of arrows --- shared
 *        @c Dom and shared @c Cod.
 *
 * @details The structural precondition shared by an @em equalizer's
 * input ( @c IsEqualizer<E, @c F, @c G> via the @c IsSubobject leg
 * above) and a (future) @em coequalizer's input.  Used here as the
 * shape of the kernel pair's two projections @c π_1, @c π_2 @c :
 * @c KernelPair(F) @c ⇒ @c Dom<F> --- the input to the
 * image-as-coequalizer reading.
 *
 * @b Why @b not @c IsCoequalizer here?
 * A coequalizer concept proper would constrain a @em quotient
 * witness @c Q (with a regular epi @c q @c : @c Cod<F> @c → @c Q
 * satisfying @c q∘F @c = @c q∘G), dual to how @c IsEqualizer
 * constrains a @em subobject witness @c E via @c IsSubobject.
 * That requires an @c IsQuotient sister to @c IsSubobject which
 * the codebase has not yet reified.  Naming an @c IsCoequalizer
 * concept whose body checked only parallel-arrow shape would
 * overpromise (a downstream caller could read the name as a
 * verified coequalizer when only the input shape was checked) ---
 * so the structurally-honest shape lives here as
 * @c IsParallelPair, and the @c IsCoequalizer name is reserved for
 * the future @c Q-witnessed concept.
 *
 * Mathematically, image-as-coequalizer still holds: for an arrow
 * @c F @c : @c A @c → @c B with kernel pair
 * @c π_1, @c π_2 @c : @c P @c ⇒ @c A,
 *
 *   @c Im(F) @c = @c coeq(π_1, π_2) @c
 *
 * the canonical epi-mono factorisation @c A @c → @c Im(F) @c → @c B.
 * The @em mono side ( @c Im(F) @c ↪ @c B) is what the @c :image
 * partition pins as @c IsImageOf<S, F>; the @em quotient side
 * awaits @c IsQuotient.
 *
 * @tparam F The first arrow @c F @c : @c A @c → @c B.
 * @tparam G The second arrow @c G @c : @c A @c → @c B.
 */
export template <typename F, typename G>
concept IsParallelPair =
    IsArrow<F> && IsArrow<G> && std::same_as<Dom<F>, Dom<G>> &&
    std::same_as<Cod<F>, Cod<G>>;

}  // namespace dedekind::category
