/**
 * @concept IsPosetal
 * @brief Represents a Posetal Category (A Category derived from a Partially
 * Ordered Set).
 *
 * @section Categorical_Definition
 * A Posetal Category is a category where for any two objects A and B, there is
 * **at most one** morphism from A to B. In this framework:
 * - Objects are elements of the set.
 * - Morphisms represent the relation (a ≤ b).
 * - Identity morphisms correspond to Reflexivity (a ≤ a).
 * - Composition corresponds to Transitivity (a ≤ b and b ≤ c implies a ≤ c).
 * - Skeletality in the category corresponds to Antisymmetry (a ≤ b and b ≤ a
 * implies a = b).
 *
 * @section Order_Structure
 * This structure corresponds to a **Partially Ordered Set (Poset)**. Unlike a
 * Preorder, a Posetal Category is skeletal, meaning isomorphic objects are
 * identical.
 *
 * @quote
 * "In a sense, the most basic category is a partially ordered set;
 *  the arrows are just the instances of the order relation."
 *  — Saunders Mac Lane, *Categories for the Working Mathematician*
 *
 * @tparam T The type of objects in the poset.
 * @tparam Rel The relation defining the order (the Morphism Generator).
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
 * @brief A Category where morphisms are governed by a specific Logic Species
 * (Ω).
 *
 * This definition reifies the Poset as a skeletal category over a Topos L.
 * By default, it assumes Classical (Boolean) logic, but it can be
 * parameterized to support intuitionistic or fuzzy relations.
 *
 * @tparam T   The Domain (Objects).
 * @tparam Rel The Relation (Morphisms).
 * @tparam L   The Logic Species (The Subobject Classifier).
 */
export template <typename T, typename Rel, typename L = ClassicalLogic>
concept IsPosetal =
    IsPartRelation<T, Rel, typename L::Ω> && requires(Rel rel, T a, T b) {
      // The relation must yield a result from the logical classifier
      { rel(a, b) } -> std::same_as<typename L::Ω>;
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
