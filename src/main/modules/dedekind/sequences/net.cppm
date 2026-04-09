/**
 * @file ontology:sequences.cppm
 * @partition :sequences
 * @brief Level 2.5: The Path (The Morphism of Enumeration).
 *
 * @section Sequences: The Logic of Enumeration
 * This partition defines the "Path" (λn. s_n) as a Morphism from a Domain
 * (Index Set) to a Codomain (Species). This provides the functional basis for
 * Sequence Spaces like ℓ² (Hilbert Spaces), where the Path is an element and
 * the Space is the Set.
 *
 * @details
 * Transitioning from Static Magnitude to Ordered Traversal:
 * - IsSequence: A functional mapping f: D → S. Unlike a Set, it is ordered and
 *   not necessarily idempotent under concatenation.
 * - IsFiniteSequence: A Path over a Domain with Finite Cardinality.
 * - IsCountableSet: A Set (Lattice) that can be projected into a Path via an
 *   as_sequence() morphism (Enumerability).
 * - IsTerminalSet: An Enumerable set with a provable size_t limit.
 */
module;

#include <concepts>
#include <cstddef>

export module dedekind.sequences:net;

import dedekind.category;
import dedekind.sets;
import dedekind.order;

namespace dedekind::sequences {
using namespace dedekind::category;
using namespace dedekind::sets;
using namespace dedekind::order;

/** @concept IsNet: A Morphism from a Directed Set. */
export template <typename N>
concept IsNet = IsArrow<N, typename N::Domain, typename N::Codomain> &&
                IsDirectedSet<typename N::Domain>;

/**
 * @concept IsSequence
 * @brief The fundamental mapping from a Domain Set to a Value Type.
 */
export template <typename Seq>
concept IsSequence = IsNet<Seq> && requires {
  // Refactored: value_type -> Codomain
  typename Seq::Codomain;
  typename Seq::Domain;
} && requires(Seq s) {
  requires IsSet<typename Seq::Domain>;
  { s.cardinality() } -> IsCardinality;
};

/**
 * @concept IsFiniteSequence
 * @brief A sequence bound by a Finite Domain.
 */
export template <typename Seq>
concept IsFiniteSequence = IsSequence<Seq> && requires(Seq s) {
  requires IsFinite<typename Seq::Domain>;
  { s.size() } -> std::convertible_to<std::size_t>;
};

/**
 * @concept IsCountableSet
 * @brief A Set capable of producing a Path (Enumerability).
 */
export template <typename S>
concept IsCountableSet = requires(S s) {
  { s.as_sequence() } -> IsSequence;
};

/**
 * @concept IsTerminalSet
 * @brief An Enumerable Set with a known Terminal Index.
 */
export template <typename S>
concept IsTerminalSet = IsCountableSet<S> && requires(S s) {
  { s.size() } -> std::integral;
};

}  // namespace dedekind::sequences
