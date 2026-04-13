/**
 * @file dedekind/category/mereology.cppm
 * @partition :mereology
 * @brief Level 0c: Embryonic Mereology (The Language of Parts).
 *
 * Following the formal axiomatization of Stanisław Leśniewski, this partition
 * introduces the "Part-Whole" relation as a foundational structural primitive.
 * In this "embryonic" stage, we define the axioms of a partial order that
 * govern any category with mereological structure.
 *
 * @section Axioms
 * 1. Reflexivity: Everything is a part of itself.
 * 2. Transitivity: A part of a part is a part of the whole.
 * 3. Antisymmetry: Two distinct things cannot be parts of each other.
 */
module;

#include <concepts>
#include <functional>

export module dedekind.category:mereology;

import :species;
import :morphism;

namespace dedekind::category {

/**
 * @concept IsPartRelation
 * @brief Verifies that a binary operator satisfies Leśniewski's mereological
 * axioms.
 *
 * @tparam Op The parthood operator (e.g., a "sub-structure" check).
 * @tparam T The type being reasoned about.
 */
export template <typename Op, typename T, typename Omega = bool>
concept IsPartRelation = requires(Op op, T a, T b, T c) {
  {
    op(a, b)
  } -> std::same_as<Omega>;  // The operator must return a truth value

  // Mereological Axiom 1: Reflexivity (∀x, x ≤ x)
  // Axiom 2: Transitivity (∀x,y,z: x ≤ y ∧ y ≤ z ⇒ x ≤ z)
  // Axiom 3: Antisymmetry (∀x,y: x ≤ y ∧ y ≤ x ⇒ x = y)
  requires is_reflexive_v<Op, T>;
  requires is_transitive_v<Op, T>;
  requires is_antisymmetric_v<Op, T>;
};

/**
 * @concept IsPartOfRelation
 * @brief Minimal core part-whole relation: `part <= whole` yields Omega.
 *
 * Richer mereological structure (species routing, ambient checks, lattice
 * refinements) should be layered by downstream partitions.
 */
export template <typename Part, typename Whole, typename Omega = bool>
concept IsPartOfRelation = requires(const Part& part, const Whole& whole) {
  { part <= whole } -> std::same_as<Omega>;
};

/**
 * @concept IsMereologicalMeetSemilattice
 * @brief Meet-like operation constrained by associativity and idempotence.
 *
 * Textbook note:
 * A meet-semilattice is usually defined as associative + commutative +
 * idempotent. This concept is intentionally weaker for current mereological
 * compatibility and should not be read as the full lattice-theory notion.
 */
export template <typename T, typename Meet>
concept IsMereologicalMeetSemilattice =
  IsAssociative<T, Meet> && IsIdempotent<T, Meet>;

/**
 * @concept IsMereologicalJoinSemilattice
 * @brief Join-like operation constrained by associativity and idempotence.
 *
 * Textbook note:
 * A join-semilattice is usually defined as associative + commutative +
 * idempotent. This concept is intentionally weaker for current mereological
 * compatibility and should not be read as the full lattice-theory notion.
 */
export template <typename T, typename Join>
concept IsMereologicalJoinSemilattice =
  IsAssociative<T, Join> && IsIdempotent<T, Join>;

/**
 * @concept IsMereologicalLatticeOperations
 * @brief Pair of meet/join operations under the mereological semilattice laws.
 *
 * TODO(backlog): Introduce textbook-aligned aliases/refinements once the
 * sets/order/algebra partitions can be migrated without API churn.
 * Candidate naming:
 * - IsJoinSemilattice / IsMeetSemilattice (commutative variants)
 * - IsBand-like operation concepts for non-commutative idempotent operations
 */
export template <typename T, typename Join, typename Meet>
concept IsMereologicalLatticeOperations =
  IsMereologicalJoinSemilattice<T, Join> &&
  IsMereologicalMeetSemilattice<T, Meet>;

/**
 * @brief Skeletal Part-Whole relation.
 * At this embryonic stage, we treat parthood as a formal arrow
 * in a category where objects are wholes and morphisms are inclusion mappings.
 */
export template <typename T>
struct Parthood {
  using species = T;

  static constexpr bool check(const T& part, const T& whole) {
    (void)part;
    (void)whole;
    return true;  // The "Trivial Universe" where everything is part of
                  // everything
  }
};

}  // namespace dedekind::category
