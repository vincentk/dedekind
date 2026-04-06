/**
 * @file dedekind.category:mereology
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

export module dedekind.category:mereology;

import :species;
import :logic;

namespace dedekind::category {

/**
 * @concept IsPartRelation
 * @brief Verifies that a binary operator satisfies Leśniewski's mereological axioms.
 * 
 * @tparam Op The parthood operator (e.g., a "sub-structure" check).
 * @tparam T The type being reasoned about.
 */
export template <typename Op, typename T>
concept IsPartRelation = requires(Op op, T a, T b, T c) {
    // Basic requirement: must return a LogicalValue (Classical or Ternary)
    { op(a, b) } -> LogicalValue;

    // Mereological Axiom 1: Reflexivity (∀x, x ≤ x)
    // Axiom 2: Transitivity (∀x,y,z: x ≤ y ∧ y ≤ z ⇒ x ≤ z)
    // Axiom 3: Antisymmetry (∀x,y: x ≤ y ∧ y ≤ x ⇒ x = y)
    requires is_reflexive_v<Op, T>;
    requires is_transitive_v<Op, T>;
    requires is_antisymmetric_v<Op, T>;
};

/**
 * @brief Skeletal Part-Whole relation.
 * At this embryonic stage, we treat parthood as a formal arrow 
 * in a category where objects are wholes and morphisms are inclusion mappings.
 */
export template <typename T>
struct Parthood {
    using species = T;
    
    static constexpr bool check(const T& part, const T& whole) {
        // In the embryo, we provide the signature; 
        // concrete implementations (like Set inclusion) follow in later modules.
        return true; 
    }
};

} // namespace dedekind::category
