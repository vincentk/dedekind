/**
 * @file dedekind/category/small.cppm
 * @module dedekind.category:small
 * @brief Level 0b: Small Categories (Enumerated Morphism Sets).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
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
 * @section Small: Categories with Set-Sized Morphisms
 * A Small Category is defined by the property that its collections of objects
 * and morphisms are Sets (not Classes). In the context of C++23, "Smallness"
 * is a pragmatic guarantee: any category reifiable within the type system is
 * inherently Small, as its inhabitants are bounded by the translation unit's
 * finite universe of types.
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

/** @section Canonical_Species_Infrastructure */

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

/** @section Grounding_Truths */

// Since is_associative_v is already defined in :species, we must
// specialize the underlying is_associative trait it likely wraps,
// or provide the specialized bool value directly if permitted.

template <typename T, typename Op>
  requires(std::integral<T> || std::floating_point<T>) &&
              std::same_as<Op, std::ranges::greater>
inline constexpr bool is_associative_v<T, Op> = true;

/**
 * @concept IsCategory
 * @brief A point-free, morphism-only reification of a Category.
 * @details In this structuralist approach, we bypass the traditional
 * object-morphism dichotomy. A category is defined solely by its arrows and
 * their composition laws. Objects are not distinct entities but are identified
 * with their identity morphisms (Id). This "singly-typed" view allows functors
 * and natural transformations to be treated as arrows within higher-dimensional
 * structures.
 *
 * Axioms:
 * 1. Existence: For every arrow f, there exist unique identity arrows id_dom(f)
 *    and id_cod(f).
 * 2. Composition: If cod(f) == dom(g), then f >> g exists.
 * 3. Unitary: id_dom(f) >> f = f = f >> id_cod(f).
 */
export template <typename Cat>
concept IsCategory = requires {
  typename Cat::Arrow;

  // The 'label' type for objects in this category
  typename Cat::Species;
  requires std::same_as<typename Cat::Species, typename Cat::Arrow::Domain>;

  // 1. The Type: Capitalized to avoid shadowing the function
  typename Cat::Id;
  requires IsArrow<typename Cat::Id>;
  requires std::convertible_to<typename Cat::Id, typename Cat::Arrow>;

  // 2. The Factory: Map a Species to its Identity Morphism
  requires requires(typename Cat::Species x) {
    { Cat::id_c(x) } -> std::same_as<typename Cat::Id>;
  };

  /** @flavour 1: Endo-composition (The Fortress) */
  // Internal closure: f: A -> B, g: B -> C must stay in the category.
  requires requires(typename Cat::Arrow f, typename Cat::Arrow g) {
    { f >> g } -> std::same_as<typename Cat::Arrow>;
  };

  // Flavor 2: Inbound (Left-Hand Side)
  // We use "IsArrow auto" and a nested "requires" block
  requires requires(typename Cat::Arrow g) {
    []<IsArrow F_in>(F_in&& f, typename Cat::Arrow g_inner) {
      if constexpr (std::same_as<typename F_in::Codomain,
                                 typename Cat::Species>) {
        return f >> g_inner;
      }
    };
  };

  // Flavor 3: Outbound (Right-Hand Side)
  requires requires(typename Cat::Arrow f) {
    []<IsArrow G_out>(typename Cat::Arrow f_inner, G_out&& g) {
      if constexpr (std::same_as<typename G_out::Domain,
                                 typename Cat::Species>) {
        return f_inner >> g;
      }
    };
  };
};

/** @section Identity_Short_Circuits */

// Law: id_A >> g = g
export template <typename T, IsArrow G>
  requires std::same_as<T, typename G::Domain>
constexpr auto operator>>(Identity<T>, G&& g) {
  return std::forward<G>(g);
}

// Law: f >> id_B = f
export template <IsArrow F, typename T>
  requires std::same_as<typename F::Codomain, T>
constexpr auto operator>>(F&& f, Identity<T>) {
  return std::forward<F>(f);
}

// Law: id_T >> id_T = id_T
export template <typename T>
constexpr auto operator>>(Identity<T> i, Identity<T>) {
  return i;
}

}  // namespace dedekind::category
