/**
 * @file dedekind/category/concrete.cppm
 * @partition :concrete
 * @brief Concrete categories: faithful @c U @c : @c 𝒞 @c → @c Set, sub-object
 *        machinery, lattice operations on χ-classified sets.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section concrete__What_Concreteness_Pins
 *
 * A category @c 𝒞 is @em concrete (in the textbook sense; Mac Lane CWM §IV.6,
 * Pierce §1.6, Awodey ch 6) when there is a faithful functor
 * @c U @c : @c 𝒞 @c → @c Set, i.e. each object @b is a set and each arrow
 * @b is a set-function preserving the relevant structure.  This is the
 * @em ontological reading of "small category" (every object is a set),
 * complementing the @em cardinality reading pinned by @c :small
 * (the object-collection and Hom-collection are themselves sets).
 *
 * The two readings are independent (Reading 1 / Reading 2 in the @c :small
 * docstring; cf. paper §2.3):
 *   - Small but not concrete: @c 𝟚 (the two-arrow category), finite-state-
 *     machines-as-categories, posetal categories with abstract tag objects.
 *   - Concrete but not small: @c Set itself, @c Grp, @c Alg(Σ), @c Top.
 *
 * @c :concrete sits @b between @c :small and @c :etcs on the chain
 * @c Cat @c → @c Ddk: @c IsSmallCategory pins the size axis; @c IsConcrete
 * pins the concreteness axis (objects are sets); @c :etcs further
 * axiomatises @em which concrete category is @c Set itself via the topos
 * structure (well-pointedness, NNO, choice, ...).  @c :etcs imports
 * @c :concrete and treats it as the prerequisite layer.
 *
 * @section concrete__Std_Namespace_Mappings
 *
 *  - @c IsSubobject<S, @c A>: per-object concreteness ---
 *    @c S is a Subobject of ambient @c A.
 *  - @c IsConcrete<C>: per-category concreteness ---
 *    @c C is small AND its objects are sets.
 *  - @c IsCompatibleSetPair / @c HasTernarySupport: subobject-pair shape gates.
 *  - @c compose_embedding: compose two embedding arrows (pullback path checks).
 *
 * The set-lattice OPERATIONS (@c set_intersection / @c set_union /
 * @c set_complement, @c meet / @c join / @c complement, membership @c in /
 * @c in_via) moved to @c dedekind.sets (#834): set operations belong in
 * @c :sets, where they collapse through @c operator& / @c | / @c !.
 *
 * Concept-as-predicate framing (cf. #635, #637).  The chain ranges over
 * CATEGORIES C: among small categories, the concrete ones (a faithful
 * U: C→Set) contain the category @c Set itself (all 10 ETCS axioms).
 *
 * @code
 *   IsSmallCategory<C>      ⊇    IsConcrete<C>      ⊇    C = Set (ETCS)
 *   (size axis: small)        (concreteness)         (full ETCS axiomatisation)
 * @endcode
 *
 * @warning This is not @c IsSet<A> @c ⟹ @c IsSmallCategory<A>.  The concept
 * @c IsSet<A> is a predicate on a set OBJECT A, not on a category: an
 * individual set is an @em object of @c Set (Lawvere), a category only via
 * the discrete embedding @c Disc: Set ↪ Cat.  @c IsSet<A> requires the
 * categorical axioms of @c CanonicalSetCCC<A::Domain> (the @c Set-witness
 * over the carrier), never @c IsSmallCategory<A>.
 *
 * @note "A set is a Many that allows itself to be thought of as a One."
 *  — Georg Cantor, letter to Richard Dedekind (1899)
 */

module;

#include <concepts>
#include <type_traits>
#include <utility>

export module dedekind.category:concrete;

import :cartesian;  // Set<T> --- target of the IsConcrete witnesses below
import :logic;      // classify, classifier_true, Ternary, IsΩ
import :morphism;   // IsArrow, IsSpokeArrow, Dom, Cod, operator>>
import :small;      // IsSmallCategory --- size-axis prerequisite for IsConcrete
import :species;    // IsSpecies --- "object collection is set-shaped"
import :topoi;      // IsSubobject, IsPredicate --- subobject machinery

namespace dedekind::category {

/**
 * @concept SetAsProduct
 * @brief Set := (Underlying, Classifier) read as a product, in the
 *        projection-access sense.
 *
 * @details
 * Names the @em product reading of a set object explicitly --- a set is a
 * pair of (i) its underlying ambient species @p Underlying, accessed
 * type-level via @c S::Domain, and (ii) its classifier codomain
 * @p Classifier, recognised structurally via the call shape
 * @c S(a) @c -> @c Classifier.  Sibling to @c IsSubobject, which names
 * the @em predicate reading "S is a subobject of A".  The two readings
 * carve the same surface from different angles.
 *
 * @par Post-#681 semantic shift
 * Prior to the structural refactor, @c Classifier named the @em type of
 * the characteristic-morphism projector (@c decltype(s.χ)).  The static
 * @c χ projector has been retired in favour of structural recognition
 * (@c S @b is the characteristic morphism via @c operator()), so
 * @c Classifier is now @em the codomain @c L::Ω that @c S(a) produces,
 * not the predicate-type wrapper.  Concrete examples: for a Set under
 * @c Boole, @c Classifier @c = @c bool; for a Set under
 * @c Kleene, @c Classifier @c = @c Ternary.
 *
 * @par Why this concept exists (#573 / #644 --- Sollbruchstelle)
 * The user's framing for the HAS-A maturation: "a set should be a product
 * in the @c :cartesian sense of an underlying and predicates."  This
 * concept names that seam.  It refines @c IsSubobject by adding a typed
 * obligation that the classifier codomain @b is @p Classifier ---
 * a structural witness that the (Underlying, Classifier) decomposition
 * is recoverable.  Downstream slices (#573 slice 3 @c AlgebraAsProduct,
 * slice 4 pilot witness) compose on this name.
 *
 * @par Relation to @c IsProductParthood
 * The bridge concept @c IsProductParthood in @c :limit asserts the
 * structural product-as-parthood reading uniformly via
 * @c IsProjectedProduct.  @c SetAsProduct is the set-level specialisation,
 * but the asymmetry "Underlying is a type, Classifier is a codomain"
 * keeps a direct @c IsProductParthood composition awkward at this
 * slice; the concepts are logical siblings, not typed dependencies.
 *
 * @tparam S           The set object.
 * @tparam Underlying  The ambient species (an @c IsSpecies object).
 * @tparam Classifier  The codomain of @c S-as-predicate (@c L::Ω).
 */
export template <typename S, typename Underlying, typename Classifier>
concept SetAsProduct = IsSubobject<S, Underlying> && requires {
  // Post-#681 structural refactor: the carrier IS the characteristic
  // morphism, so there is no named @c s.χ projector to match against
  // @c Classifier.  We capture the (Underlying, Classifier) seam
  // instead by matching @c Classifier to the codomain of @c S-as-
  // predicate — i.e.\ @c L::Ω.  Slightly different semantic surface
  // (Classifier = the codomain, not the predicate-type itself), but
  // operationally equivalent: the Classifier dimension is the L::Ω
  // value at any @c a, and downstream consumers consume the codomain
  // not the predicate-type wrapper.
  requires std::same_as<std::invoke_result_t<S const&, Underlying const&>,
                        Classifier>;
};

/**
 * @concept IsConcrete
 * @brief A small category @c C is @em concrete when its objects are
 *        themselves sets --- equivalently, when there is a faithful functor
 *        @c U @c : @c C @c → @c Set.
 *
 * @details Concept-as-predicate / @c &&-as-set-intersection framing:
 *          @c IsConcrete narrows @c IsSmallCategory by intersecting with
 *          the constraint that the category's object-collection is
 *          set-shaped (its @c Species type is registered in the species
 *          atlas).  In the project's Juliet posture, where @c Species is
 *          a C++ type whose values @em are the objects, registration in
 *          the species atlas (via @c SpeciesTraits) is exactly the
 *          structural witness that those objects can be viewed as
 *          elements of @c Set.
 *
 *          Position in the size-and-structure lattice (read top-down as
 *          narrowing intersections; cf. paper §2.3):
 *
 * @code
 *   IsSmallCategory<C>
 *     ∧ IsSpecies<C::Species>   (objects sit in the species atlas)
 *   = IsConcrete<C>
 * @endcode
 *
 *          @c :etcs further narrows by adding the full topos / ETCS axiom
 *          surface (subobject classifier, NNO, choice, ...): @c IsSet =
 *          @c IsConcrete + ETCS axioms.
 *
 *          Examples in the project:
 *            - @c Set<T> --- the canonical CCC in @c :cartesian; concrete
 *              for any @c T that satisfies @c IsSpecies (the @c :species
 *              primary registers integral / floating-point / bool types;
 *              other types opt in via explicit @c SpeciesTraits
 *              specialisations).
 *            - @c DiscreteCategory<S> in @c :discrete --- concrete when
 *              @c S is a registered species.
 */
export template <typename C>
concept IsConcrete = IsSmallCategory<C> && IsSpecies<typename C::Species>;

/**
 * @concept HasTernarySupport
 * @brief True when a set object's classifier returns ternary truth values.
 */
export template <typename S>
concept HasTernarySupport =
    IsSubobject<S, typename S::Domain> &&
    std::same_as<std::invoke_result_t<S const&, typename S::Domain const&>,
                 Ternary>;

/**
 * @concept IsCompatibleSetPair
 * @brief Two set objects over the same ambient species and same Ω codomain.
 */
export template <typename S1, typename S2>
concept IsCompatibleSetPair =
    IsSubobject<S1, typename S1::Domain> &&
    IsSubobject<S2, typename S2::Domain> &&
    std::same_as<typename S1::Domain, typename S2::Domain> &&
    std::same_as<std::invoke_result_t<S1 const&, typename S1::Domain const&>,
                 std::invoke_result_t<S2 const&, typename S2::Domain const&>>;

// The set-lattice operations --- @c set_intersection / @c set_union /
// @c set_complement, membership @c in / @c in_via, the @c meet / @c join /
// @c complement aliases, and the @c ConjunctionChi predicate --- were RELOCATED
// to @c dedekind.sets (@c :sets:expressions), #834 / #946 S2: set operations
// belong in @c :sets, where they converge onto the collapsing @c operator& /
// @c | / @c ! (a set meet now COLLAPSES instead of staying an opaque
// @c ConjunctionChi).  ETCS Axiom 10 was decoupled from the @c meet / @c join
// free-function names (#834) so this move is DAG-clean.  This partition keeps
// only the carrier-agnostic @c compose_embedding arrow helper below.

/**
 * @brief Compose two embedding arrows for pullback naturality path checks.
 * @details Produces @c h @c = @c f @c >> @c g @c : @c A @c → @c C,
 *          preserving @c IsArrow compatibility.  Use instead of an ad-hoc
 *          lambda when building composed-path witnesses for downstream
 *          axiom-7 reindexing surfaces in @c :etcs.  Raw lambdas do not
 *          carry @c Domain / @c Codomain typedefs, so they fail the
 *          @c IsArrow concept; this wrapper delegates to @c operator>>
 *          which returns a properly typed @c Morphism.
 */
export template <IsArrow F, IsArrow G>
  requires IsSpokeArrow<std::decay_t<F>> && IsSpokeArrow<std::decay_t<G>> &&
           std::same_as<Cod<std::decay_t<F>>, Dom<std::decay_t<G>>>
constexpr auto compose_embedding(F&& f, G&& g) {
  return std::forward<F>(f) >> std::forward<G>(g);
}

/** @section concrete__Witnesses
 *
 * @details Compiler-validated witnesses pinning that the canonical
 * carriers of the project's CCC layer satisfy @c IsConcrete.  These
 * close the AC of #636 ("at least one downstream @c IsConcrete witness")
 * and serve as audit-trail evidence that the concreteness concept binds
 * structurally on the project's actual carriers.
 */

// @c Set<T> (the canonical CCC over @c T from @c :cartesian) is concrete
// for any species type @c T --- its @c Species = @c T and the species
// atlas registers all integral / floating-point / bool primaries by
// default.
static_assert(IsConcrete<Set<int>>,
              "Set<int> is concrete: Species = int is a registered species.");
static_assert(IsConcrete<Set<bool>>,
              "Set<bool> is concrete: Species = bool is a registered species.");
static_assert(
    IsConcrete<Set<double>>,
    "Set<double> is concrete: Species = double is a registered species.");

}  // namespace dedekind::category
