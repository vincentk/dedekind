/**
 * @file small.cppm
 * @brief Level 0b: Small Categories (Enumerated Morphism Sets).
 *
 * @partition :small
 * @section Small: Categories with Set-Sized Morphisms
 * A Small Category is defined by the property that its collections of objects
 * and morphisms are Sets (not Classes). In the context of C++23, "Smallness"
 * is a pragmatic guarantee: any category reifiable within the type system is
 * inherently Small, as its inhabitants are bounded by the translation unit's
 * finite universe of types.
 *
 * @section Structural_Decidability
 * By formalising Smallness at Level 0, we provide the LLVM backend with a
 * decidable graph of transformations. This allows for symbolic path-tracing
 * and pruning before the maturation of specific algebraic laws (like
 * Associativity) in downstream partitions.
 *
 * Wikipedia: Small category, Discrete category
 */
module;

#include <concepts>
#include <functional>

export module dedekind.category:small;

import :posetal;

namespace dedekind::category {

/**
 * @section Small_Category_Concepts
 * Concepts for categories with enumerable Objects (Obj) and
 * Hom-sets (Hom<A, B>).
 */
export template <typename Cat>
concept IsSmallCategory = requires {
  // 1. Identity: Every object X in the category must have an identity arrow
  typename Cat::Object;
  {
    Cat::identity(std::declval<typename Cat::Object>())
  } -> IsArrow<typename Cat::Object, typename Cat::Object>;

  // 2. Composition: f: A -> B and g: B -> C must compose
  typename Cat::Arrow;
  requires requires(typename Cat::Arrow f, typename Cat::Arrow g) {
    { Cat::compose(g, f) } -> IsArrow;
  };
};

/**
 * @section Discrete_Categories
 * Special cases where the only morphisms are identities.
 */

}  // namespace dedekind::category
