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
 * Textbook defaults in this partition:
 * - Relation defaults to `std::less_equal<T>` (the canonical order witness).
 * - Logic defaults to `ClassicalLogic` (Boolean Ω).
 *
 * @quote
 * "In a sense, the most basic category is a partially ordered set;
 *  the arrows are just the instances of the order relation."
 *  — Saunders Mac Lane, *Categories for the Working Mathematician*
 *
 * @see https://en.wikipedia.org/wiki/Partially_ordered_set
 * @see https://en.wikipedia.org/wiki/Preorder
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
export template <typename T, typename Rel = std::less_equal<T>,
                 typename L = ClassicalLogic>
concept IsPosetal =
    IsPartRelation<T, Rel, typename L::Ω> && requires(Rel rel, T a, T b) {
      // The relation must yield a result from the logical classifier
      { rel(a, b) } -> std::same_as<typename L::Ω>;
    };

/**
 * @brief Verify that a two-step path A→B→C exists in the posetal category.
 *
 * @details
 * A morphism A→C in a posetal category exists iff A≤B and B≤C; transitivity
 * (guaranteed by `IsPosetal`) then provides A≤C for free.  The result is
 * expressed in the truth-value codomain Ω of the chosen Logic species L, so
 * this helper works uniformly for Classical, Ternary, or any other pluggable
 * logical universe — no `bool` hard-codes.
 *
 * @tparam T   Object type.
 * @tparam Rel Relation type (must satisfy `IsPosetal<T, Rel, L>`).
 * @tparam L   Logic species providing `AND` and the `Ω` codomain.
 */
export template <typename T, typename Rel = std::less_equal<T>,
                 typename L = ClassicalLogic>
  requires IsPosetal<T, Rel, L>
constexpr typename L::Ω check_path(T a, T b, T c) {
  const auto rel = Rel{};
  // A two-step path exists iff both edges are present.
  // Transitivity (axiom of IsPosetal) guarantees the direct edge A→C.
  return L::AND(rel(a, b), rel(b, c));
}

}  // namespace dedekind::category
