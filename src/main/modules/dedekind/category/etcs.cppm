/**
 * @file dedekind/category/etcs.cppm
 * @partition :etcs
 * @brief Level 4: Elementary Theory of the Category of Sets (ETCS) facade.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.

 *
 * @details
 * This partition lifts topoi-level subobject operations into a focused ETCS
 * set interface. In this codebase, a "set" is represented as a subobject
 * classified by a characteristic morphism χ: A → Ω.
 *
 * @section ETCS_Axiom_Mapping Lawvere's 10 Axioms mapped to dedekind
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
#include <type_traits>
#include <utility>

export module dedekind.category:etcs;

import :cartesian;
import :limit;
import :logic;
import :morphism;
import :natural;
import :pullback;
import :species;
import :topoi;

namespace dedekind::category {

/**
 * @concept IsSetObject
 * @brief A categorical set object represented as a subobject S ↣ A.
 */
export template <typename S, typename A>
concept IsSetObject = IsSubobject<S, A> && requires {
  typename S::Ambient;
  requires std::same_as<typename S::Ambient, A>;
};

/**
 * @concept HasTernarySupport
 * @brief True when a set object's classifier returns ternary truth values.
 */
export template <typename S>
concept HasTernarySupport =
    IsSubobject<S, typename S::Ambient> &&
    std::same_as<Cod<decltype(std::declval<S>().χ)>, Ternary>;

/**
 * @concept IsCompatibleSetPair
 * @brief Two set objects over the same ambient species and same Ω codomain.
 */
export template <typename S1, typename S2>
concept IsCompatibleSetPair =
    IsSubobject<S1, typename S1::Ambient> &&
    IsSubobject<S2, typename S2::Ambient> &&
    std::same_as<typename S1::Ambient, typename S2::Ambient> &&
    std::same_as<Cod<decltype(std::declval<S1>().χ)>,
                 Cod<decltype(std::declval<S2>().χ)>>;

/** @brief ETCS intersection: materialize A ∩ B from χ_A ∧ χ_B. */
export template <typename S1, typename S2>
  requires IsCompatibleSetPair<S1, S2>
constexpr auto set_intersection(const S1& lhs, const S2& rhs) {
  using A = typename S1::Ambient;
  return classify<A>(lhs.χ && rhs.χ);
}

/** @brief ETCS union: materialize A ∪ B from χ_A ∨ χ_B. */
export template <typename S1, typename S2>
  requires IsCompatibleSetPair<S1, S2>
constexpr auto set_union(const S1& lhs, const S2& rhs) {
  using A = typename S1::Ambient;
  return classify<A>(lhs.χ || rhs.χ);
}

/** @brief ETCS complement: materialize A^c from ¬χ_A. */
export template <typename S>
  requires IsSubobject<S, typename S::Ambient>
constexpr auto set_complement(const S& s) {
  using A = typename S::Ambient;
  return classify<A>(!s.χ);
}

/** @brief ETCS membership: x ∈ S evaluated via χ_S(x). */
export template <typename S>
  requires IsSubobject<S, typename S::Ambient>
constexpr auto in(const typename S::Ambient& x, const S& s) {
  return s.χ(x);
}

/**
 * @brief Membership through an embedding arrow e: X -> A, then χ_S.
 * @details Evaluates x ∈_e S as χ_S(e(x)).
 */
export template <typename S, IsArrow E>
  requires IsSubobject<S, typename S::Ambient> &&
           std::same_as<Cod<E>, typename S::Ambient>
constexpr auto in_via(const Dom<E>& x, E&& embedding, const S& s) {
  return s.χ(std::forward<E>(embedding)(x));
}

/**
 * @brief Compose two embedding arrows for pullback naturality path checks.
 * @details Produces h = f >> g : A -> C, preserving `IsArrow` compatibility.
 * Use instead of an ad-hoc lambda when building composed-path witnesses for
 * `HasAxiom7PullbackReindexingDefinitionalSurface`. Raw lambdas do not carry
 * `Domain`/`Codomain` typedefs, so they fail the `IsArrow` concept; this
 * wrapper delegates to `operator>>` which returns a properly typed `Morphism`.
 */
export template <IsArrow F, IsArrow G>
  requires IsSpokeArrow<std::decay_t<F>> && IsSpokeArrow<std::decay_t<G>> &&
           std::same_as<Cod<std::decay_t<F>>, Dom<std::decay_t<G>>>
constexpr auto compose_embedding(F&& f, G&& g) {
  return std::forward<F>(f) >> std::forward<G>(g);
}

/** @brief Lattice alias: meet = intersection. */
export template <typename S1, typename S2>
  requires IsCompatibleSetPair<S1, S2>
constexpr auto meet(const S1& lhs, const S2& rhs) {
  return set_intersection(lhs, rhs);
}

/** @brief Lattice alias: join = union. */
export template <typename S1, typename S2>
  requires IsCompatibleSetPair<S1, S2>
constexpr auto join(const S1& lhs, const S2& rhs) {
  return set_union(lhs, rhs);
}

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
export template <typename S, typename Alpha, typename F, typename G>
concept HasAxiom7ClassifierNaturalityWitness =
    HasAxiom7SubobjectClassifier<S> && IsNaturalTransformation<Alpha, F, G> &&
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
 * @details meet/join on subobjects follows from the subobject classifier
 * (Axiom 7) inducing a Heyting algebra on Sub(A). The Axiom of Choice proper
 * (every epimorphism splits) is aspirational and not yet encoded here.
 */
export template <typename S>
concept HasAxiom10PowerObjectLattice =
    IsSetObject<S, typename S::Ambient> && IsCompatibleSetPair<S, S> &&
    requires(S lhs, S rhs) {
      requires IsSetObject<decltype(meet(lhs, rhs)), typename S::Ambient>;
      requires IsSetObject<decltype(join(lhs, rhs)), typename S::Ambient>;
    };

/**
 * @concept IsSet
 * @brief ETCS set concept that aggregates all 10 axiom witnesses in one place.
 *
 * @details
 * IsSet keeps the elegant Subobject representation (axiom 7) while making
 * the ETCS axiom mapping explicit and discoverable as concept-level witnesses.
 */
export template <typename T>
concept IsSet =
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
 * @concept IsSetInCanonicalCCC
 * @brief Object/category bridge: ETCS set object with canonical CCC ambient.
 */
export template <typename S>
concept IsSetInCanonicalCCC =
    IsSet<S> && HasCanonicalSetCCC<typename S::Ambient>;

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

static_assert(
    IsSetInCanonicalCCC<decltype(ambient_set<int>([](int) { return true; }))>,
    "Mnemonic check: ETCS set objects live over a canonical CCC ambient.");

}  // namespace dedekind::category
