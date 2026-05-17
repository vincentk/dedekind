/**
 * @file dedekind/category/etcs.cppm
 * @partition :etcs
 * @brief Elementary Theory of the Category of Sets (ETCS) facade.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.

 *
 * @details
 * This partition lifts topoi-level subobject operations into a focused ETCS
 * set interface. In this codebase, a "set" is represented as a subobject
 * classified by a characteristic morphism χ: A → Ω.
 *
 * @section etcs__ETCS_Axiom_Mapping
 *
 * The following table maps each of the 10 axioms of Lawvere's ETCS to the
 * corresponding C++23 implementation. Items marked (asp.) are aspirational.
 *
 * | ETCS Axiom                    | C++23 Implementation                 |
 * |:------------------------------|:-------------------------------------|
 * | **1. Composition**            | `operator>>` / `IsArrow`             |
 * | **2. Identity**               | `Identity<T>` / `id<T>()`            |
 * | **3. Terminal Object (1)**    | `One` (`std::monostate`)             |
 * | **4. Well-Pointedness**       | `s.χ(x) → Ω` (global element eval)   |
 * | **5. Cartesian Product**      | `std::pair` / `IsProduct`            |
 * | **6. Exponentiation (B^A)**   | `Exponential<A,B>` / `IsExponential` |
 * | **7. Subobject Classifier**   | `Subobject<A,χ>` / `classify<A>(p)`  |
 * | **8. Empty Set (∅)**          | `Zero` (`std::nullptr_t`)            |
 * | **9. NNO (ℕ)**               | `SpeciesTraits<unsigned>`             |
 * | **10. Axiom of Choice** (asp.) | `meet` / `join` power-object lattice |
 *
 * @section etcs__What_Concreteness_Pins
 *
 * @c :small establishes the @em cardinality reading of "small category":
 * the collection of objects and the collection of arrows are each sets in
 * the metatheory.  @c :concrete (re-homed from this partition under #636)
 * pins the @em ontological reading: each object @b is a set, equivalently
 * a faithful forgetful functor @c U @c : @c 𝒞 @c → @c Set.
 *
 * Concept-as-predicate framing (cf. paper §2.3):
 *
 * @code
 *   IsSmallCategory<C>      ⊇    IsConcrete<C>      ⊇    IsSet (= ETCS)
 *   (size axis: small)       (ontology: objects-are-   (full ETCS
 *                            sets; faithful U: C→Set)   axiomatisation)
 * @endcode
 *
 * @c :etcs sits one step down from @c :concrete on this chain: every
 * ETCS set @em is a concrete set object, plus the 10 ETCS axioms
 * (well-pointedness, NNO, choice, ...) that pick out @c Set specifically
 * among concrete categories.  The @c IsSetObject, @c IsConcrete, and
 * @c χ-based set-operation machinery (@c set_intersection / @c set_union
 * / @c set_complement / @c in / @c in_via / @c meet / @c join) all live
 * in @c :concrete; this partition imports it and adds the ETCS-specific
 * axiom witnesses on top.
 *
 * @see Lawvere, F.W. (1964) "An Elementary Theory of the Category of Sets"
 * @see McLarty, C. (1993) "Numbers can be just what they have to"
 *
 * @note "In the mathematical development of recent decades, the notion of
 *  set has not only played a fundamental role, but it has itself
 *  passed through a long process of refinement."
 *  — F. William Lawvere, "An Elementary Theory of the Category of Sets"
 */

module;

#include <concepts>
#include <functional>
#include <set>
#include <type_traits>
#include <unordered_set>
#include <utility>

export module dedekind.category:etcs;

import :adjunction;  // HasAdjunctionShape / IsAdjunction — the bona fide
                     // adjunction machinery used to witness Disc ⊣ U at the
                     // type level (#572 review).
import :cartesian;
import :concrete;  // IsSetObject, IsConcrete, set_intersection / set_union /
                   // set_complement / in / in_via / meet / join — the
                   // concreteness layer (#636), prerequisite for ETCS axioms.
import :discrete;  // DiscreteCategory<T> — target of the Set ↪ Cat lift (#572)
import :functor;   // identity_functor — the structural-shape witness for
                   // the discrete-restriction Disc ⊣ U adjunction (#572).
import :limit;
import :logic;
import :morphism;
import :natural;
import :pullback;
import :species;
import :topoi;

namespace dedekind::category {

// NOTE (#636 re-home): the concreteness layer --- @c IsSetObject,
// @c HasTernarySupport, @c IsCompatibleSetPair, @c set_intersection / @c
// set_union / @c set_complement, @c in / @c in_via, @c meet / @c join,
// @c compose_embedding, and the new @c IsConcrete<C> umbrella ---
// moved to @c :concrete.  @c :etcs retains the ETCS-specific axiom
// witnesses (@c HasAxiom1..10, @c HasETCSAxioms, @c IsSet) and imports
// @c :concrete as the structural prerequisite.  Concept-as-predicate
// reading: @c IsSet @c = @c IsConcrete @c + ETCS axioms.

/** @brief ETCS axiom 1 witness: composition is available for ambient arrows. */
export template <typename A>
concept HasAxiom1Composition =
    IsArrow<Identity<A>> && IsArrow<decltype(id<A>() >> id<A>())>;

/** @brief ETCS axiom 2 witness: identity arrow acts neutrally on A. */
export template <typename A>
concept HasAxiom2Identity =
    IsArrow<Identity<A>> && std::same_as<Dom<Identity<A>>, A> &&
    std::same_as<Cod<Identity<A>>, A>;

/** @brief ETCS axiom 3 witness: terminal object 1 is present. */
export template <typename A>
concept HasAxiom3TerminalObject = IsTerminalObject<One>;

/** @brief ETCS axiom 4 witness: membership is evaluation through χ. */
export template <typename S>
concept HasAxiom4WellPointedness =
    IsSubobject<S, typename S::Ambient> &&
    IsPredicate<std::remove_cvref_t<decltype(std::declval<S>().χ)>>;

/** @brief ETCS axiom 5 witness: products exist for ambient species A. */
export template <typename A>
concept HasAxiom5CartesianProduct = IsProduct<std::pair<A, A>, A, A>;

/** @brief ETCS axiom 6 witness: exponentials B^A exist (here A^A witness). */
export template <typename A>
concept HasAxiom6Exponentiation =
    IsExponential<Exponential<A, A>, A, A> && IsArrow<Exponential<A, A>>;

/** @brief ETCS axiom 7 witness: every set is represented by a subobject. */
export template <typename S>
concept HasAxiom7SubobjectClassifier =
    IsSubobject<S, typename S::Ambient> && requires {
      requires IsSetObject<decltype(classify<typename S::Ambient>(
                               classifier_true<typename S::Ambient>())),
                           typename S::Ambient>;
      requires IsSetObject<decltype(classify<typename S::Ambient>(
                               classifier_false<typename S::Ambient>())),
                           typename S::Ambient>;
      requires IsPullback<
          decltype(pullback<ClassicalLogic, std::pair<typename S::Ambient,
                                                      typename S::Ambient>>(
              id<typename S::Ambient>(), id<typename S::Ambient>())),
          Identity<typename S::Ambient>, Identity<typename S::Ambient>>;
    };

/**
 * @concept HasAxiom7ClassifierNaturalityWitness
 * @brief Optional naturality witness for Axiom 7 classifier behavior.
 * @details Encodes that classifier-level structure for ambient species A can
 * be paired with a natural-transformation witness over functors rooted in A.
 * This strengthens the proof surface for pullback/reindexing narratives while
 * keeping `IsSet` backward compatible. This witness is intentionally additive
 * and is not required by `IsSet`.
 */
export template <typename S, typename Α, typename F, typename G>
concept HasAxiom7ClassifierNaturalityWitness =
    HasAxiom7SubobjectClassifier<S> && IsNaturalTransformation<Α, F, G> &&
    std::same_as<typename F::Σ_cat::Species, typename S::Ambient>;

/** @brief ETCS axiom 8 witness: initial object 0 is present. */
export template <typename A>
concept HasAxiom8EmptySet = IsInitialObject<Zero>;

/** @brief ETCS axiom 9 witness: arithmetic species atlas exposes ℕ witness. */
export template <typename A>
concept HasAxiom9NNO = IsSpecies<unsigned>;

/**
 * @concept IsSplitEpicPair
 * @brief Structural witness of a split epimorphism e with section s.
 * @details This concept captures the textbook shape e: A ↠ B and s: B → A.
 * Semantic equations (e ∘ s = id_B) remain a proof obligation supplied by
 * tests or user-declared witnesses.
 */
export template <typename Epi, typename Section>
concept IsSplitEpicPair = IsEpicArrow<Epi> && IsArrow<Section> &&
                          std::same_as<Dom<Section>, Cod<Epi>> &&
                          std::same_as<Cod<Section>, Dom<Epi>>;

/**
 * @concept HasAxiom10ChoiceSplitEpicWitness
 * @brief Axiom 10 (choice) support via an explicit split-epi witness shape.
 * @details This is an opt-in structural contract that exposes where choice-like
 * splitting witnesses exist today.
 */
export template <typename S, typename Epi, typename Section>
concept HasAxiom10ChoiceSplitEpicWitness =
    IsSetObject<S, typename S::Ambient> && IsSplitEpicPair<Epi, Section> &&
    std::same_as<Cod<Epi>, typename S::Ambient>;

/**
 * @brief Law check for split epimorphisms at a concrete codomain element.
 * @details Encodes the semantic Axiom 10 equation `e ∘ s = id` at value `b`.
 */
export template <typename Epi, typename Section>
  requires IsSplitEpicPair<Epi, Section> && std::equality_comparable<Cod<Epi>>
constexpr bool split_epi_section_law_at(const Epi& epi, const Section& section,
                                        const Cod<Epi>& b) {
  return epi(section(b)) == b;
}

/**
 * @concept HasAxiom10ChoiceSplitEpicLawSurface
 * @brief Axiom 10 semantic-law surface (split-epi shape + equality codomain).
 * @details Law truth itself is established by tests via
 * `split_epi_section_law_at` on concrete witnesses.
 */
export template <typename S, typename Epi, typename Section>
concept HasAxiom10ChoiceSplitEpicLawSurface =
    HasAxiom10ChoiceSplitEpicWitness<S, Epi, Section> &&
    std::equality_comparable<Cod<Epi>>;

/**
 * @brief Definitional witness check for Axiom 7 reindexing through embedding.
 * @details Confirms that χ_S(e(x)) is evaluable at value x. The equality
 * χ_S(e(x)) = in_via(x, e, S) holds by definition of in_via; this function
 * evaluates the common expression once and checks reflexivity, serving as a
 * concrete evaluability witness for
 * HasAxiom7PullbackReindexingDefinitionalSurface. Assumes referential
 * transparency of the embedding arrow.
 */
export template <typename S, IsArrow E>
  requires IsSubobject<S, typename S::Ambient> &&
           std::same_as<Cod<E>, typename S::Ambient> &&
           std::equality_comparable<Cod<decltype(std::declval<S>().χ)>>
constexpr bool classifier_reindexing_definitional_witness_at(const S& s,
                                                             const E& embedding,
                                                             const Dom<E>& x) {
  const auto embedded = embedding(x);
  const auto via_classifier = s.χ(embedded);
  return via_classifier == s.χ(embedded);
}

/**
 * @concept HasAxiom7PullbackReindexingDefinitionalSurface
 * @brief Axiom 7 definitional reindexing witness surface.
 * @details This surface encodes well-typed availability of the definitional
 * reindexing identity and does not by itself prove full naturality closure.
 */
export template <typename S, typename E>
concept HasAxiom7PullbackReindexingDefinitionalSurface =
    HasAxiom7SubobjectClassifier<S> && IsArrow<E> &&
    std::same_as<Cod<E>, typename S::Ambient> &&
    std::equality_comparable<Cod<decltype(std::declval<S>().χ)>>;

/**
 * @brief ETCS axiom 10 witness: power-object lattice completeness.
 * @details meet/join/complement on subobjects follows from the subobject
 * classifier (Axiom 7) inducing a Heyting algebra on Sub(A) (Boolean if
 * the topos is Boolean, i.e.\ L = ClassicalLogic).  The Axiom of Choice
 * proper (every epimorphism splits) is aspirational and not yet encoded
 * here.
 *
 * @section etcs__Axiom_10_Slice_9_Generalisation
 * #698 Slice 9 adds a single typedef requirement to the body — the
 * carrier must expose its @c logic_species (verified as an
 * @c IsLogicalSpecies) — alongside the pre-existing @c meet / @c join
 * structural shape.  The classical-direction Boolean refinement
 * (Diaconescu) and the @c complement clause live in the parallel
 * @c :lattice::IsSubobjectLattice / @c IsBooleanSubobjectLattice
 * concepts rather than inside Axiom 10's body — see the Diaconescu
 * note below for why.
 *
 * Concrete carriers (@c Set<T, L, P>, @c Subobject<A, Chi>) provide:
 *   - @c logic_species typedef (the @c L for Sub's classifier);
 *   - free functions @c meet, @c join (and @c complement at the
 *     parallel @c :lattice concept) returning same-family
 *     @c IsSubobjectFamilyMember-shaped results.
 */
export template <typename S>
concept HasAxiom10PowerObjectLattice =
    IsSetObject<S, typename S::Ambient> && IsCompatibleSetPair<S, S> &&
    requires {
      /** @brief @c logic_species typedef anchors the classifier @c L
       *  (Slice 9 — required by @c :lattice::IsSubobjectLattice).
       *  Verified as an @c IsLogicalSpecies so unrelated types with
       *  an accidental @c logic_species typedef don't satisfy the
       *  axiom by accident (#713 review, Copilot). */
      typename S::logic_species;
      requires IsLogicalSpecies<typename S::logic_species>;
    } && requires(S lhs, S rhs) {
      requires IsSetObject<decltype(meet(lhs, rhs)), typename S::Ambient>;
      requires IsSetObject<decltype(join(lhs, rhs)), typename S::Ambient>;
    };

/** @section etcs__Axiom_10_Diaconescu_Note
 *
 *  @brief Diaconescu's classical direction (#698 Slice 9 review).
 *
 *  @details The standard textbook ETCS Axiom 10 is the @b Axiom of
 *  Choice: every epimorphism splits.  Diaconescu (1975) showed that
 *  in a topos, AC implies the topos is Boolean (classical internal
 *  logic).  The codebase commits to one direction of this
 *  bi-implication only:
 *
 *      @c L @c = @c ClassicalLogic @c ⟹ Sub(A) Boolean   (Diaconescu's
 * classical-Ω direction)
 *      @c L @c = @c TernaryLogic   @c ⟹ Sub(A) Heyting   (constructive-collapse
 * path)
 *
 *  The Boolean refinement is exposed via the parallel
 *  @c :lattice::IsBooleanSubobjectLattice<S> concept rather than
 *  baked into Axiom 10's body.  An earlier attempt to gate the
 *  conditional inside @c HasAxiom10PowerObjectLattice triggered
 *  instantiation of @c set_complement(s) → @c !s.χ → @c Set::χ's
 *  static-init for capturing-lambda Predicates produced by the
 *  comprehension DSL, which fails default-construction.  The
 *  parallel-track design avoids that ODR-use cascade; the static
 *  asserts in @c :sets carrier files pin the L-parametric route
 *  type-checked at the carrier sites without inducing the cascade.
 *
 *  The reverse direction (Boolean Sub(A) @c ⟹ full AC) is the
 *  splitting-epi witness, which is genuinely aspirational and not yet
 *  encoded.  The header table's @c (asp.) marker on row 10 names this
 *  remaining gap.  Tracked as a separate concept (e.g.\
 *  @c HasAxiom10AxiomOfChoiceFull) if and when the project commits to
 *  it. */

/**
 * @concept HasETCSAxioms
 * @brief Aggregates the 10 ETCS axiom witnesses over a carrier T.
 *
 * @details Each axiom is its own concept-level witness
 * (@c HasAxiom1Composition through @c HasAxiom10PowerObjectLattice); this
 * concept conjoins them into a single gate.  Used by @c IsSet below as
 * the axioms half of the ETCS set predicate (the other half being the
 * canonical CCC witness over the ambient species).
 */
export template <typename T>
concept HasETCSAxioms =
    IsSetObject<T, typename T::Ambient> &&
    HasAxiom1Composition<typename T::Ambient> &&
    HasAxiom2Identity<typename T::Ambient> &&
    HasAxiom3TerminalObject<typename T::Ambient> &&
    HasAxiom4WellPointedness<T> &&
    HasAxiom5CartesianProduct<typename T::Ambient> &&
    HasAxiom6Exponentiation<typename T::Ambient> &&
    HasAxiom7SubobjectClassifier<T> && HasAxiom8EmptySet<typename T::Ambient> &&
    HasAxiom9NNO<typename T::Ambient> && HasAxiom10PowerObjectLattice<T>;

/**
 * @concept IsSet
 * @brief ETCS set: a carrier satisfying the 10 ETCS axioms AND admitting
 *        the canonical Set-CCC witness over its ambient species.
 *
 * @details Conjoins @c HasETCSAxioms<A> with @c IsCartesianClosed over
 * @c CanonicalSetCCC<A::Ambient>.  Every ETCS category is a CCC --- axioms
 * 3 (terminal), 5 (products), 6 (exponentials) are exactly the structural
 * ingredients of a CCC --- so the second half of the conjunction is
 * structurally entailed by the first; carrying it in the body makes the
 * CCC guarantee discoverable at the @c IsSet site rather than per-carrier
 * opt-in.  This is the Lambek--Scott payoff: Cartesian-closedness is
 * simply-typed lambda calculus, and the ETCS subset relation lifts that
 * to the typed lambda calculus of the set-builder DSL.  Tracked
 * historically under #389.
 *
 * @section etcs__ETCS_subset_CCC
 * Standard categorical fact: every ETCS category is a Cartesian-closed
 * category, but most CCCs are not ETCS.  The additional axioms beyond CCC
 * (4 well-pointed, 7 subobject classifier, 9 NNO, 10 choice / power-object
 * lattice) are precisely what fails for general CCCs.
 */
export template <typename A>
concept IsSet =
    HasETCSAxioms<A> && IsCartesianClosed<CanonicalSetCCC<typename A::Ambient>>;

/**
 * @brief Construct a set object over ambient species A from a characteristic
 * predicate.
 */
export template <typename A, typename Pred>
  requires IsSpecies<A> && std::invocable<std::decay_t<Pred>, const A&> &&
           LogicalValue<std::invoke_result_t<std::decay_t<Pred>, const A&>>
constexpr auto ambient_set(Pred&& predicate) {
  return classify<A>(std::forward<Pred>(predicate));
}

// ---------------------------------------------------------------------------
// Juliet-clean lifts: std::set / std::unordered_set ↪ IsSet directly (#607).
//
// These overloads let std-container values lift to IsSet without going
// through a project-shipped wrapper (no @c ExtensionalSet, no
// @c FiniteBooleanSet, just std).  Each overload wraps the container's
// @c contains member as a callable predicate and forwards to the
// universal @c ambient_set<A>(Pred) above.
//
// Lifetime contract:
//   * lvalue overload — captures the container by reference; the caller
//     keeps the container alive for as long as the lifted Subobject is
//     reachable.  Zero-copy.
//   * rvalue overload — moves the container into the lambda; the lifted
//     Subobject owns its predicate's data.  One std-level move, no
//     wrapper-level copy.
//
// This is slice 1 of #607's wrapper-dissolution plan: no removals yet,
// but std types are now first-class IsSet citizens via the lift.
// Subsequent slices will dissolve @c ExtensionalSet, @c SingletonSet,
// @c UniversalSet, @c Ø in favour of these overloads (and an analogous
// SingletonSet-replacement that takes a single @c T value).
// ---------------------------------------------------------------------------

/** @brief Lift @c std::unordered_set<T, ...> by const-ref into an IsSet
 *         object.  Borrows lifetime; zero copy. (#607 slice 1) */
export template <typename T, typename Hash, typename Equal, typename Alloc>
constexpr auto ambient_set(const std::unordered_set<T, Hash, Equal, Alloc>& s)
  requires IsSpecies<T>
{
  return ambient_set<T>([&s](const T& x) -> bool { return s.contains(x); });
}

/** @brief Lift @c std::unordered_set<T, ...> by rvalue.  Moves the
 *         container into the predicate; lifted Subobject owns the data.
 *         (#607 slice 1) */
export template <typename T, typename Hash, typename Equal, typename Alloc>
constexpr auto ambient_set(std::unordered_set<T, Hash, Equal, Alloc>&& s)
  requires IsSpecies<T>
{
  return ambient_set<T>(
      [s = std::move(s)](const T& x) -> bool { return s.contains(x); });
}

/** @brief Lift @c std::set<T, ...> by const-ref.  Borrows lifetime;
 *         zero copy. (#607 slice 1) */
export template <typename T, typename Compare, typename Alloc>
constexpr auto ambient_set(const std::set<T, Compare, Alloc>& s)
  requires IsSpecies<T>
{
  return ambient_set<T>([&s](const T& x) -> bool { return s.contains(x); });
}

/** @brief Lift @c std::set<T, ...> by rvalue.  Moves the container
 *         into the predicate. (#607 slice 1) */
export template <typename T, typename Compare, typename Alloc>
constexpr auto ambient_set(std::set<T, Compare, Alloc>&& s)
  requires IsSpecies<T>
{
  return ambient_set<T>(
      [s = std::move(s)](const T& x) -> bool { return s.contains(x); });
}

// Witnesses: std-containers satisfy IsSet directly via the lift.
static_assert(IsSet<decltype(ambient_set(
                  std::declval<const std::unordered_set<int>&>()))>,
              "std::unordered_set<int> lifts to IsSet via ambient_set (lvalue) "
              "without a project-shipped wrapper.");

static_assert(
    IsSet<decltype(ambient_set(std::declval<std::unordered_set<int>&&>()))>,
    "std::unordered_set<int> lifts to IsSet via ambient_set (rvalue).");

static_assert(
    IsSet<decltype(ambient_set(std::declval<const std::set<int>&>()))>,
    "std::set<int> lifts to IsSet via ambient_set (lvalue).");

static_assert(IsSet<decltype(ambient_set(std::declval<std::set<int>&&>()))>,
              "std::set<int> lifts to IsSet via ambient_set (rvalue).");

/**
 * @section etcs__IsSet_entails_CCC_directional_witness
 * Representative sanity check for the ETCS \f$\subset\f$ CCC
 * implication.  The second @c static_assert checks the implication
 * @c IsSet<S> ==> HasCanonicalSetCCC<typename S::Ambient> for the
 * chosen witness type @c _isset_witness_t by spelling it as
 * @c !IsSet<_isset_witness_t> || HasCanonicalSetCCC<...>.
 *
 * Because this is pinned to a single representative carrier (a
 * trivially-true predicate over @c int) and @c HasCanonicalSetCCC<int>
 * is independently true, the assertion is a local regression sanity
 * check kept next to the @c IsSet definition --- not a universal
 * proof that every future model of @c IsSet has a canonical CCC
 * ambient.
 */
namespace {
using _isset_witness_t = decltype(ambient_set<int>([](int) { return true; }));
}  // namespace
static_assert(IsSet<_isset_witness_t>,
              "Witness: representative carrier satisfies IsSet "
              "(axioms + canonical CCC over the ambient).");
static_assert(
    IsCartesianClosed<CanonicalSetCCC<typename _isset_witness_t::Ambient>>,
    "Directional witness: the CCC half of IsSet pinned independently --- "
    "the canonical Set-CCC over the witness's ambient species is "
    "Cartesian-closed.  (Pre-#629 this was carried by the "
    "IsSetInCanonicalCCC mnemonic alias; IsSet now subsumes it "
    "structurally, so the alias is gone but the directional check stays.)");

/**
 * @brief The canonical Set ↪ Cat lift for any IsSet-witnessing carrier.
 *
 * @details For every @c IsSet<S>, the lift @c S @c → @c Disc(S) sits in
 * @c Cat as a discrete category.  The lift uses @c S itself (not its
 * ambient species) as the carrier of the discrete category, so distinct
 * @c IsSet types over the same ambient remain distinct after lifting:
 * a refined subset and an unrefined ambient are different sets and
 * therefore different discrete categories.  This mirrors the textbook:
 * @c Disc(\{0, 1, 2\}) and @c Disc(\mathbb{N}) are not the same
 * category.  The resulting type follows the @c IsDiscreteCategory
 * concept definition by construction; the per-instance witnesses
 * below pin it on the project's representative @c IsSet carrier.
 *
 * Companion to:
 *   - The Set_to_Cat_Embedding doc-section in @c :discrete.
 *   - The @c IsSet aggregator above (the source side of the lift).
 *   - @c HasAdjunctionShape / @c IsAdjunction in @c :adjunction (the
 *     bona fide adjunction machinery, instantiated for this lift below).
 *   - @c disc_self_endofunctor_t / @c disc_self_unit_t in @c :natural
 *     (the named entities reifying the textbook @c Disc / @c U / @c η /
 *     @c ε under the discrete restriction; consumed by the static_asserts
 *     below).
 *
 * @section etcs__Disc_dashv_U_adjunction
 *
 * In the textbook @c Disc @c : @c Set @c → @c Cat is the left adjoint
 * of the underlying-objects functor @c U @c : @c Cat @c → @c Set.  The
 * adjunction @c Disc @c ⊣ @c U lives between the @b meta-categories
 * @c Set and @c Cat.  This codebase encodes categories per-ambient
 * (one @c Species per @c IsSmallCategory type), so the @b meta-categorical
 * statement is one level above what the type system can express
 * directly --- there is no @c category_of_sets or @c
 * category_of_categories type whose @c Species ranges over all sets
 * or all categories.  What @b is type-level expressible is the @b
 * restriction of the adjunction to a single discrete carrier: on
 * @c Disc(S) every functor that respects the discrete shape is the
 * identity on objects and arrows, so @c Disc and @c U restricted to
 * @c Disc(S) collapse to the trivial self-adjunction
 *
 *   @c Id_{Disc(S)} @c ⊣ @c Id_{Disc(S)}
 *
 * with identity unit / counit naturals.  The witnesses below
 * mechanically exhibit this restriction using the bona fide
 * @c HasAdjunctionShape / @c IsAdjunction concepts from
 * @c :adjunction --- the same surfaces that gate the trivial
 * self-adjunction exhibit in @c functor_test.cpp:138.  The full
 * meta-categorical @c Disc @c ⊣ @c U is left as future work (it
 * needs @c category_of_sets / @c category_of_categories types that
 * the project does not yet model); tracked under #587.
 *
 * Discrete-restriction lift filed and addressed under #572.
 */
export template <typename S>
  requires IsSet<S>
using discrete_lift_t = DiscreteCategory<S>;

// Witness that the lift produces a discrete category for the
// representative @c IsSet-witnessing carrier from above.
static_assert(IsSmallCategory<discrete_lift_t<_isset_witness_t>>,
              "Set ↪ Cat lift: discrete_lift_t<S> satisfies the "
              "general Category contract.");
static_assert(IsDiscreteCategory<discrete_lift_t<_isset_witness_t>>,
              "Set ↪ Cat lift: discrete_lift_t<S> is a DiscreteCategory "
              "for every IsSet-witnessing S.");
static_assert(std::same_as<discrete_lift_t<_isset_witness_t>,
                           DiscreteCategory<_isset_witness_t>>,
              "Set ↪ Cat lift: discrete_lift_t<S> resolves to "
              "DiscreteCategory<S>; subobject information in S is "
              "preserved by lifting S itself rather than S::Ambient.");

// Bona fide adjunction-machinery witnesses on the representative
// IsSet carrier.  These are the type-level mechanical realisation of
// the prose claim above --- every IsSet S has a @c Disc(S) that
// participates in the @c HasAdjunctionShape / @c IsAdjunction surface
// from @c :adjunction, anchored on the @c disc_self_endofunctor_t /
// @c disc_self_unit_t named entities reified in @c :natural (#583
// review).  The trivial-self-adjunction shape is the most the type
// system can certify here; the full meta-categorical
// @c Disc @c ⊣ @c U is acknowledged as future work in the doc block.
static_assert(
    IsFunctor<disc_self_endofunctor_t<discrete_lift_t<_isset_witness_t>>>,
    "Set ↪ Cat lift: the discrete-restriction Disc-functor is a bona "
    "fide functor on Disc(S).");
static_assert(HasAdjunctionShape<
                  disc_self_endofunctor_t<discrete_lift_t<_isset_witness_t>>,
                  disc_self_endofunctor_t<discrete_lift_t<_isset_witness_t>>>,
              "Set ↪ Cat lift: the discrete-restriction Disc and U satisfy the "
              "structural shape of an adjunction (Σ_cat / Τ_cat cross-pair).");
static_assert(IsNaturalTransformation<
                  disc_self_unit_t<discrete_lift_t<_isset_witness_t>>,
                  disc_self_endofunctor_t<discrete_lift_t<_isset_witness_t>>,
                  disc_self_endofunctor_t<discrete_lift_t<_isset_witness_t>>>,
              "Set ↪ Cat lift: the identity unit/counit is a bona fide natural "
              "transformation between the discrete-restriction functors.");
static_assert(
    IsAdjunction<disc_self_endofunctor_t<discrete_lift_t<_isset_witness_t>>,
                 disc_self_endofunctor_t<discrete_lift_t<_isset_witness_t>>,
                 disc_self_unit_t<discrete_lift_t<_isset_witness_t>>,
                 disc_self_unit_t<discrete_lift_t<_isset_witness_t>>>,
    "Set ↪ Cat lift: discrete_lift_t<S> participates in the bona fide "
    "IsAdjunction surface --- the discrete-restriction encoding of the "
    "textbook Disc ⊣ U adjunction.");

}  // namespace dedekind::category
