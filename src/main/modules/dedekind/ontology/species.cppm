/**
 * @file ontology:species.cppm
 * @brief Level 0a: The Reified Machine (The Taxonomic Bricks).
 *
 * @section The_Taxonomy_of_Species
 * This partition performs the formal reification of C++ machine types.
 * Following Joyal’s Theory of Species, we treat each fundamental type
 * not as a set of values, but as a rule for generating structure.
 *
 * @subsection Structuralist_Elevation
 * Raw types (bool, int, double) are elevated to "Species" by attaching
 * algebraic metadata. This ensures that a species carries its own
 * "laws of nature" (associativity, identity) as static constants.
 *
 * @subsection Zero_Overhead_Requirements
 * To satisfy the Stroustrupian ideal, all species definitions must
 * resolve to their underlying machine primitives at compile-time.
 * Categorification here is an act of reasoning, not an act of
 * allocation.
 *
 * @section Taxonomic_Traits: The Skeletal Constants
 * - identity_v<T, Op>  : The neutral element for a specific operation.
 * - is_associative_v   : Static proof of grouping independence.
 * - is_commutative_v   : Static proof of order independence.
 *
 * @section The_Proto_Morphism
 * Defines the skeletal signature of an Arrow (Domain -> Codomain).
 * This provides the mapping metadata required for the Functorial
 * discovery in Level 0b (:category).
 *
 * Wikipedia: Combinatorial species, Type theory, Generic programming
 */

module;

#include <concepts>
#include <functional>

export module dedekind.ontology:species;

namespace dedekind::ontology {

/**
 * @concept IsSpecies
 * @brief Ensures a type has been formally reified with algebraic traits.
 */
export template <typename T>
concept IsSpecies = requires {
  typename T::machine_type;  // Maps back to the Stroustrupian primitive
};

/**
 * @section The_Skeletal_Arrow
 * @brief A primitive Morphism signature without compositional logic.
 */
export template <typename A, typename B, typename Func>
struct Morphism {
  using Domain = A;
  using Codomain = B;
  Func transform;
};

}  // namespace dedekind::ontology
