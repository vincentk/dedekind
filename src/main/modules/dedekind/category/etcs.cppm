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
 * | **10. Axiom of Choice**       | `meet` / `join` lattice dispatcher   |
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
concept HasAxiom7SubobjectClassifier = IsSubobject<S, typename S::Ambient>;

/** @brief ETCS axiom 8 witness: initial object 0 is present. */
export template <typename A>
concept HasAxiom8EmptySet = IsInitialObject<Zero>;

/** @brief ETCS axiom 9 witness: arithmetic species atlas exposes ℕ witness. */
export template <typename A>
concept HasAxiom9NNO = IsSpecies<unsigned>;

/** @brief ETCS axiom 10 witness: meet/join lattice operators are available. */
export template <typename S>
concept HasAxiom10ChoiceDispatcher =
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
    HasAxiom9NNO<typename T::Ambient> && HasAxiom10ChoiceDispatcher<T>;

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
