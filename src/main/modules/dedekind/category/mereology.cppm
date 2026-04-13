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
 * @details
 * Textbook term: this is the axiomatic core of a partial-order style
 * parthood relation used in formal mereology.
 *
 * @tparam Op The parthood operator (defaults to `std::less_equal<T>`).
 * @tparam T The type being reasoned about.
 * @tparam Ω The truth-value codomain (default: classical two-valued logic).
 *
 * @note Textbook alignment:
 * - Parthood/partial-order axioms: reflexive, transitive, antisymmetric.
 * - Default comparator `std::less_equal<T>` corresponds to the usual
 *   order-theoretic relation symbol `<=`.
 *
 * @see Structural mereology overview (Stanford Encyclopedia):
 * https://plato.stanford.edu/entries/mereology/
 * @see Lambek & Scott (1988), Introduction to Higher-Order Categorical Logic.
 * @see McLarty (1992), Elementary Categories, Elementary Toposes.
 */
export template <typename T, typename Op = std::less_equal<T>,
                 typename Ω = bool>
concept IsPartRelation = requires(Op op, T a, T b, T c) {
  { op(a, b) } -> std::same_as<Ω>;  // The operator must return a truth value

  // Mereological Axiom 1: Reflexivity (∀x, x ≤ x)
  // Axiom 2: Transitivity (∀x,y,z: x ≤ y ∧ y ≤ z ⇒ x ≤ z)
  // Axiom 3: Antisymmetry (∀x,y: x ≤ y ∧ y ≤ x ⇒ x = y)
  requires is_reflexive_v<T, Op>;
  requires is_transitive_v<T, Op>;
  requires is_antisymmetric_v<T, Op>;
};

/**
 * @concept IsPartOfRelation
 * @brief Minimal core part-whole relation yielding Omega.
 *
 * @details
 * Textbook term: primitive binary parthood predicate (often written
 * `part <= whole` or `part ⊑ whole` depending on formalism).
 *
 * This concept accepts three equivalent encodings used across partitions:
 * - Order-style: `part <= whole`
 * - Predicate-style: `whole(part)`
 * - Indexer-style: `whole[part]`
 *
 * The canonical default in `IsPartRelation` remains order-style (`<=`),
 * while topoi/set layers may present parthood through characteristic
 * predicates (`operator()`) or lookup/index syntax (`operator[]`).
 *
 * Richer mereological structure (species routing, ambient checks, lattice
 * refinements) should be layered by downstream partitions.
 *
 * @see https://en.wikipedia.org/wiki/Mereology
 */
export template <typename Part, typename Whole, typename Ω = bool>
concept IsPartOfRelation = requires(const Part& part, const Whole& whole) {
  requires(
      requires {
        { part <= whole } -> std::same_as<Ω>;
      } ||
      requires {
        { whole(part) } -> std::same_as<Ω>;
      } ||
      requires {
        { whole[part] } -> std::same_as<Ω>;
      });
};

/**
 * @concept IsMereologicalMeetSemilattice
 * @brief Meet-like operation constrained by associativity and idempotence.
 *
 * @note Textbook term:
 * a meet-semilattice is usually the commutative-idempotent-associative
 * operation (meet, `∧`).
 *
 * Textbook note:
 * A meet-semilattice is usually defined as associative + commutative +
 * idempotent. This concept is intentionally weaker for current mereological
 * compatibility and should not be read as the full lattice-theory notion.
 *
 * @see https://en.wikipedia.org/wiki/Semilattice
 */
export template <typename T, typename Meet>
concept IsMereologicalMeetSemilattice =
    IsAssociative<T, Meet> && IsIdempotent<T, Meet>;

/**
 * @concept IsMereologicalJoinSemilattice
 * @brief Join-like operation constrained by associativity and idempotence.
 *
 * @note Textbook term:
 * a join-semilattice is usually the commutative-idempotent-associative
 * operation (join, `∨`).
 *
 * Textbook note:
 * A join-semilattice is usually defined as associative + commutative +
 * idempotent. This concept is intentionally weaker for current mereological
 * compatibility and should not be read as the full lattice-theory notion.
 *
 * @see https://en.wikipedia.org/wiki/Semilattice
 */
export template <typename T, typename Join>
concept IsMereologicalJoinSemilattice =
    IsAssociative<T, Join> && IsIdempotent<T, Join>;

/**
 * @concept IsMereologicalLatticeOperations
 * @brief Pair of meet/join operations under the mereological semilattice laws.
 *
 * @details
 * Textbook term: lattice operations are usually presented as a pair
 * `(join, meet)` satisfying semilattice laws plus absorption. This concept
 * currently captures only the weaker operation-level fragment used in this
 * partition.
 *
 * TODO(backlog): Introduce textbook-aligned aliases/refinements once the
 * sets/order/algebra partitions can be migrated without API churn.
 * Candidate naming:
 * - IsJoinSemilattice / IsMeetSemilattice (commutative variants)
 * - IsBand-like operation concepts for non-commutative idempotent operations
 *
 * @see https://en.wikipedia.org/wiki/Lattice_(order)
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
