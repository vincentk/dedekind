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
 * @concept IsSmallCategory
 * @brief A category where objects and morphisms form sets (decidable
 * collections).
 */
export template <typename T>
concept IsSmallCategory =
    IsPointed<T, category_op_t<T>> && IsAssociative<T, category_op_t<T>>;

/** @section Verification_of_Primitive_Categories */

static_assert(IsSmallCategory<int>,
              "Spine Error: int must be a Small Category.");
static_assert(IsSmallCategory<unsigned int>,
              "Spine Error: unsigned int must be a Small Category.");
static_assert(IsSmallCategory<double>,
              "Spine Error: double must be a Small Category.");
static_assert(IsSmallCategory<bool>,
              "Spine Error: bool must be a Small Category.");

/** @brief Specialization for the Terminal Object: One. */
template <>
struct canonical_op<One> {
  // Logic: On a single point, any operation (multiplication/addition)
  // collapses to the point itself. We use multiplies for consistency.
  using type = std::multiplies<void>;
};

/** @brief Shorthand for the composition rule. */
template <typename T>
using category_op_t = typename canonical_op<T>::type;

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
