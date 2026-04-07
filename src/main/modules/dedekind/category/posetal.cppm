/**
 * @file posetal.cppm
 * @brief Level 0b: Posetal Categories (Morphisms as Relations).
 *
 * @partition :posetal
 * @section Posetal: Categories as Partially Ordered Sets
 * A Posetal Category is a structure where there is at most one morphism
 * between any two objects. This partition enables the interpretation
 * of "A -> B" as the relation "A ≤ B".
 *
 * Wikipedia: Poset, Preorder (category theory)
 */
module;

#include <concepts>
#include <functional>

export module dedekind.category:posetal;

import :logic;
import :mereology;

namespace dedekind::category {

/**
 * @concept IsPosetal
 * @brief Verified Skeletal Category where morphisms represent orders.
 *
 * A type satisfies IsPosetal if it implements a valid Partial Order
 * (Reflexive, Transitive, Antisymmetric) and returns a truth-value
 * from the Species' Topos (Omega).
 */
export template <typename T, typename Rel>
concept IsPosetal = IsPartRelation<Rel, T> && requires(Rel rel, T a, T b) {
  // The relation must yield a result from the logical classifier
  { rel(a, b) } -> std::same_as<typename Classifier<T>::type>;
};

/**
 * @section Structural_Pruning
 * Utility to verify categorical transitivity at compile-time.
 */
export template <typename T, typename Rel>
  requires IsPosetal<T, Rel>
constexpr auto check_path(T a, T b, T c) {
  if constexpr (Rel{}(a, b) && Rel{}(b, c)) {
    // In a Poset, the path A -> C is a structural certainty.
    static_assert(Rel{}(a, c), "Categorical Transitivity Violation!");
    return true;
  }
  return false;
}

}  // namespace dedekind::category
