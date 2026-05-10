/**
 * @file dedekind/category/small.cppm
 * @partition :small
 * @brief Small Categories (Enumerated Morphism Sets).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section small__Small
 * A Small Category is defined by the property that its collections of objects
 * and morphisms are Sets (not Classes). In the context of C++23, "Smallness"
 * is a pragmatic guarantee: any category reifiable within the type system is
 * inherently Small, as its inhabitants are bounded by the translation unit's
 * finite universe of types.
 *
 * @details Defines a single-species category as a small category containing
 * exactly one object type. In this setting, all morphisms are endomorphisms,
 * causing the category to behave as a monoid where composition is the binary
 * operation. This provides a focused foundation for species-specific logic
 * within the C++ type system.
 *
 * @quote
 * "Il linguaggio delle categorie è affettuosamente noto come 'nonsense
 * astratto'. Questo termine è essenzialmente accurato: le categorie si
 * riferiscono al 'nonsense' nel senso che riguardano esclusivamente la
 * 'struttura', e non il 'significato' di ciò che rappresentano." (The language
 * of categories is affectionately known as "abstract nonsense." This term is
 * essentially accurate: categories refer to "nonsense" in the sense that they
 * are all about the "structure," and not about the "meaning," of what they
 * represent.) — Paolo Aluffi, Algebra: Chapter 0
 *
 *
 * @note "With all the variety of mathematical knowledge, we are still clearly
 * conscious of the similarity of the logical devices, the relationship of the
 * ideas in mathematics as a whole and the numerous analogies in its different
 * departments."
 *       -- David Hilbert, Mathematical Problems (1900)
 */

module;

#include <concepts>
#include <functional>
#include <string>

export module dedekind.category:small;

import :morphism;
import :posetal;
import :species;

namespace dedekind::category {

/** @section small__Canonical_Species_Infrastructure */

/** @brief Primary template for canonical operations. */
template <typename T>
struct canonical_op;

/** @brief Specialization for Unsigned: Modulo Addition. */
template <std::unsigned_integral T>
struct canonical_op<T> {
  using type = std::plus<void>;
};

/** @brief Specialization for Signed: Lattice Max. */
template <std::signed_integral T>
struct canonical_op<T> {
  using type = std::ranges::greater;
};

/** @brief Specialization for Floating: Lattice Max. */
template <std::floating_point T>
struct canonical_op<T> {
  using type = std::ranges::greater;
};

/** @brief Specialization for Bools: Boolean Logic. */
template <>
struct canonical_op<bool> {
  using type = std::logical_and<void>;
};

/** @brief Specialization for Strings: Concatenation. */
template <>
struct canonical_op<std::string> {
  using type = std::plus<void>;
};

/** @brief Canonical operation shorthand. */
template <typename T>
using category_op_t = typename canonical_op<T>::type;

/** @section small__Grounding_Truths */

// Since is_associative_v is already defined in :species, we must
// specialize the underlying is_associative trait it likely wraps,
// or provide the specialized bool value directly if permitted.

template <typename T, typename Op>
  requires(std::integral<T> || std::floating_point<T>) &&
              std::same_as<Op, std::ranges::greater>
inline constexpr bool is_associative_v<T, Op> = true;

/**
 * @concept IsSmallCategory
 * @brief A point-free, morphism-only reification of a Category.
 * @details In this structuralist approach, we bypass the traditional
 * object-morphism dichotomy. A category is defined solely by its arrows and
 * their composition laws. Objects are not distinct entities but are identified
 * with their identity morphisms (Id). This "singly-typed" view allows functors
 * and natural transformations to be treated as arrows within higher-dimensional
 * structures.
 *
 * In this codebase we use a further specialization: a category fixes a single
 * `Species` type, and `Cat::Arrow::Domain` is required to coincide with that
 * object label type. This is stricter than the textbook many-object view, and
 * it gives a canonical type-/label-level representative of an object via the
 * identity arrow `Cat::id_c(x)`, allowing object-level reasoning to be
 * re-entered through composition. Recovering the original value-level label
 * `x` from that identity arrow, however, is not required by `IsSmallCategory`;
 * that would need an additional guarantee that identity arrows carry or expose
 * such a witness.
 *
 * @section small__What_Smallness_Pins
 * In CT jargon, @c IsSmallCategory pins exactly the @b smallness reading of
 * "small category" --- the @em cardinality reading: the collection of
 * objects (the value-set of @c Species) and the collection of arrows
 * (the value-set of @c Arrow) are each sets in the metatheory.  This
 * is forced by representing both as C++ types: a C++ type's values
 * form a set (not a proper class), so Ob(𝒞) and the total Hom-collection
 * are sets.
 *
 * @c IsSmallCategory deliberately does @b not pin the second, ontological
 * reading of "small" --- @em concreteness --- under which each object
 * is itself a set (equivalently: there is a faithful forgetful functor
 * @c U @c : @c 𝒞 @c → @c Set).  Whether each object is a set or a tag
 * depends on what @c Species is bound to at the call site
 * (@c Set<T> binds to sets; an enum-style @c Species binds to tags).
 * Concreteness as a structural commitment is layered downstream in
 * @c :concrete --- @c IsConcrete<C> @c = @c IsSmallCategory<C> @c ∧
 * @c IsSpecies<C::Species> --- and further refined in @c :etcs, whose
 * ETCS axiomatization picks out @c Set specifically among concrete
 * categories via the topos structure (well-pointedness, NNO, choice).
 *
 * Other refinements of "category" --- thin, discrete, locally-finite,
 * intensional / extensional, lazy / strict --- are also intentionally
 * out of scope here; they are named (or named-able) at downstream
 * partitions where they have semantic content.  The intensional /
 * extensional split in particular is reified at @c :sets:expressions
 * (intensional, @c TernaryLogic) vs @c :sets:extensional (decidable
 * membership), not at the category layer.
 *
 * @section small__The_Locally_Small_Slot
 * Standard CT distinguishes three slots along the size axis:
 *
 * | Concept                       | Object collection | Hom-sets    |
 * |:------------------------------|:------------------|:------------|
 * | (general) category            | proper class      | proper class|
 * | @b locally @b small category  | proper class      | sets        |
 * | @b small category             | set               | sets        |
 *
 * @c IsSmallCategory pins the third row (small).  The @b locally @b small
 * row above it is named at the value level by the new @c
 * IsLocallySmallCategory concept (defined below): when @c Cat::Arrow is a
 * single C++ type, its value-set is a set in the metatheory, hence the
 * total Hom-collection is a set and so is every @c Hom_Cat(a, b) (a
 * subset).  In the project's value-level encoding, @c IsSmallCategory =
 * @c IsLocallySmallCategory + Ob-is-a-set; small implies locally small,
 * making "small @b and locally small" a structurally equivalent gate to
 * @c IsSmallCategory.
 *
 * What remains deferred is the @em higher-kinded reading of locally-
 * small-not-small --- the textbook home of @c Set itself, @c Cpp,
 * @c Top, @c Alg(Σ), where the object-collection is a proper class
 * and @c Hom<A, B> must be exposed as a type-level slot.  Two reasons
 * to defer:
 *
 *   1. @b No @b current @b consumer.  No site in the project today
 *      requires a category whose object-collection is type-level
 *      (a proper class).  The slot would be speculative API, and the
 *      project's struct-creation posture is "burden of proof on adding
 *      a concept, not on omitting one".
 *   2. @b Implementation @b cost.  A faithful higher-kinded locally-
 *      small concept requires a @c Cat::template Hom<A, @c B> slot,
 *      which is the same template-template territory the codebase
 *      otherwise tries to keep at arm's length (in the @c IsFunctor
 *      Hub/Spoke pattern, etc.).  Paying that complexity without a
 *      load-bearing consumer would be the wrong trade.
 *
 * If a future slice introduces an honest @c Cpp-as-category or
 * @c Alg(Σ)-as-category construction, that's the natural moment to
 * lift @c IsLocallySmallCategory from its value-level form to the
 * higher-kinded slot --- with a real consumer to pressure-test the
 * API shape rather than guessing it ahead of time.
 *
 * Axioms:
 * 1. Existence: For every arrow f, there exist unique identity arrows id_dom(f)
 *    and id_cod(f).
 * 2. Composition: If cod(f) == dom(g), then f >> g exists.
 * 3. Unitary: id_dom(f) >> f = f = f >> id_cod(f).
 */
/**
 * @concept IsLocallySmallCategory
 * @brief Value-level locally-small witness: the Hom-collection of @c Cat is
 *        a set in the metatheory.
 *
 * @details Forced by representing arrows as values of a single C++ type
 * @c Cat::Arrow --- a C++ type's value-set is a set (not a proper class),
 * so the total arrow-collection is a set; for any fixed pair of objects
 * @c (a, b), @c Hom_Cat(a, b) is the subset of @c Cat::Arrow values with
 * @c Dom = a and @c Cod = b, hence itself a set (subset of a set).
 *
 * This is the @em value-level reading of "locally small".  The
 * @em higher-kinded reading --- where objects are a proper class and
 * @c Hom<A, B> is exposed as a type-level slot --- is deliberately not
 * navigated by the project (cf. @c small__The_Locally_Small_Slot below).
 *
 * @section small__LocallySmall_Implies_Small
 * @c IsSmallCategory @c = @c IsLocallySmallCategory @c + Ob-is-a-set, so
 * any @c IsSmallCategory carrier is automatically @c IsLocallySmallCategory;
 * the conjunction "small @b and locally small" is structurally equivalent
 * to @c IsSmallCategory in this encoding.  Paper §2.3 step 1 cites
 * @c IsSmallCategory as the concept that enforces both readings.
 *
 * @note "Concreteness gives locally-smallness for free: a faithful
 *        @c U @c : @c C @c → @c Set forces @c Hom_C(a, b) @c ↪
 *        @c Hom_Set(U(a), U(b)) @c = @c U(b)^U(a), and a subset of a
 *        set is a set."
 *       — Claude (Anthropic AI assistant), conversation log,
 *         PR #639 review (2026-05-10).
 */
export template <typename Cat>
concept IsLocallySmallCategory = requires {
  // The arrow-collection is a single C++ type --- its value-set is a
  // set in the metatheory.
  typename Cat::Arrow;
  // ... and that type satisfies the minimal IsArrow shape, so we're
  // gating on the categorical arrow notion, not any nested `Arrow`
  // typedef.  Rules out types with an unrelated `Arrow` member.
  requires IsArrow<typename Cat::Arrow>;
};

export template <typename Cat>
concept IsSmallCategory = IsLocallySmallCategory<Cat> && requires {
  // The 'label' type for objects in this category
  typename Cat::Species;
  // Per the picking policy in :morphism (#411): use Dom<...> in
  // IsArrow-strict contexts (Cat::Arrow is constrained as IsArrow
  // below).  Self-documents that the Domain comes via the arrow shape.
  requires std::same_as<typename Cat::Species, Dom<typename Cat::Arrow>>;

  // 1. The Type: Capitalized to avoid shadowing the function
  typename Cat::Id;
  requires IsArrow<typename Cat::Id>;
  requires std::convertible_to<typename Cat::Id, typename Cat::Arrow>;

  // 2. The Factory: Map a Species to its Identity Morphism
  requires requires(typename Cat::Species x) {
    { Cat::id_c(x) } -> std::same_as<typename Cat::Id>;
  };

  /**
   * @brief Internal Closure (Endo-composition)
   * A category must be able to compose its own arrows f: A -> B and g: B -> C.
   * This is a strict requirement of the concept.
   */
  requires requires(typename Cat::Arrow f, typename Cat::Arrow g) {
    { f >> g } -> IsArrow;
    // Per the picking policy in :morphism (#411): use Dom / Cod for
    // arrow-shaped composite results.
    requires std::same_as<Dom<decltype(f >> g)>, Dom<typename Cat::Arrow>>;
    requires std::same_as<Cod<decltype(f >> g)>, Cod<typename Cat::Arrow>>;
  };

  /**
   * @note External composition (Exo-composition)
   * While not strictly required by the IsSmallCategory concept, implementations
   * are encouraged to provide templated operator>> overloads to allow
   * composition with any external type satisfying IsArrow, provided
   * the Domain/Codomain plumbing is compatible.
   */
};

/** @section small__Identity_Short_Circuits */

// Law: id_A >> g = g.  Per the picking policy in :morphism (#411): use
// Dom<G> in IsArrow-strict contexts; self-documents that the Domain comes
// via the arrow shape.
export template <typename T, IsArrow G>
  requires std::same_as<T, Dom<G>>
constexpr auto operator>>(Identity<T>, G&& g) {
  return std::forward<G>(g);
}

// Law: f >> id_B = f.  Same picking-policy reasoning: Cod<F> in
// IsArrow-strict contexts.
export template <IsArrow F, typename T>
  requires std::same_as<Cod<F>, T>
constexpr auto operator>>(F&& f, Identity<T>) {
  return std::forward<F>(f);
}

// Law: id_T >> id_T = id_T
export template <typename T>
constexpr auto operator>>(Identity<T> i, Identity<T>) {
  return i;
}

/**
 * @brief An arrow that is just a label.
 */
struct StringArrow {
  std::string label;
  int domain_id;    // The "Object" it starts from
  int codomain_id;  // The "Object" it ends at

  using Domain = int;
  using Codomain = int;

  // Composition is just string concatenation
  friend constexpr StringArrow operator>>(const StringArrow& f,
                                          const StringArrow& g) {
    return {f.label + " then " + g.label, f.domain_id, g.codomain_id};
  }

  // To satisfy IsArrow, it must be "invocable" in some sense
  constexpr int operator()(int x) const { return x; }
};

/**
 * @brief The Category of Labels.
 */
struct StringCategory {
  using Species = int;
  using Arrow = StringArrow;
  using Id = StringArrow;

  static constexpr Id id_c(int x) { return {"id", x, x}; }
};

}  // namespace dedekind::category
