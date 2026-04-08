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

export module dedekind.category:small;

import :morphism;
import :posetal;
import :species;
import :discrete;

namespace dedekind::category {

/** @brief Discovery engine for a Species' canonical composition rule. */
template <typename T>
struct canonical_op {
  using type = std::multiplies<>;  // The default "Highway"
};

/** @brief Shorthand for the composition rule of a category. */
template <typename T>
using category_op_t = typename canonical_op<T>::type;

/**
 * @concept IsSmallCategory
 * @brief A category where objects and morphisms form sets (decidable
 * collections).
 */
export template <typename T>
concept IsSmallCategory =
    IsPointed<T, category_op_t<T>> && IsAssociative<T, category_op_t<T>>;

/**
 * @section Discrete_Categories
 * A Discrete Category is a Small Category where the only morphisms are
 * identities.
 */
export template <typename T>
concept IsDiscreteCategory =
    IsSmallCategory<T> && (std::same_as<T, One> || std::same_as<T, Zero>);

/** @brief Verification: The Terminal Object is a Discrete Category. */
static_assert(IsDiscreteCategory<One>,
              "Categorical Proof: The Terminal Object (1) must be Discrete.");

}  // namespace dedekind::category
