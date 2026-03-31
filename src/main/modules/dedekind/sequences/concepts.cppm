/**
 * @file ontology:sequences.cppm
 * @partition :sequences
 * @brief Level 2.5: The Path (The Morphism of Enumeration).
 *
 * @section Sequences: The Logic of Enumeration
 * This partition defines the "Path" (λn. s_n) as a Morphism from a Domain
 * (Index Set) to a Codomain (Species). In the Bourbaki "Mother Structures,"
 * this represents the transition from Static Magnitude (Lattice-based Sets) to
 * Ordered Traversal.
 *
 * @details
 * We distinguish between the Mereological nature of the Domain and the
 * Functional nature of the Sequence:
 * - IsSequence: A mapping from an Index Set (IsSet) to values. Supports
 * infinite domains.
 * - IsFiniteSequence: A sequence whose Domain satisfies IsFinite.
 * - IsCountableSet: A Set that admits an Enumeration (can be projected to a
 * Sequence).
 * - IsTerminalSet: An Enumerable set with a Provable Termination (size).
 *
 * @build_order 5
 * @dependency :mereology
 */
module;

#include <concepts>
#include <cstddef>

export module dedekind.sequences:sequences;

import dedekind.sets;

namespace dedekind::sequences {
using namespace dedekind::sets;

/**
 * @concept IsSequence
 * @brief The fundamental mapping from a Domain Set to a Value Type.
 */
export template <typename Seq>
concept IsSequence = requires {
  typename Seq::value_type;
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
