/**
 * @file ontology:sequences.cppm
 * @partition :sequences
 * @brief Level 2.5: The Path (The Morphism of Enumeration).
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Sequences: The Logic of Enumeration
 * This partition defines the "Path" (λn. s_n) between a Countable Index
 * and a Species. In the Bourbaki "Mother Structures," this represents
 * the transition from the Static Magnitude to the Ordered Traversal.
 *
 * @details
 * Following the Constructivist tradition, we distinguish between:
 * - IsSequence: The functional mapping (f: ℕ → S).
 * - IsComputable: A set whose membership and traversal are Decidable.
 * - IsFiniteSet: A sequence that possesses a Terminal Index.
 *
 * @build_order 5
 * @dependency :mereology, :cardinalities
 *
 * Wikipedia: Sequence, Enumeration, Computable set, Constructivism
 */
module;

#include <concepts>
#include <cstddef>  // Required for size_t

export module dedekind.ontology:sequences;

import :mereology;      // Required for IsSet
import :cardinalities;  // Required for IsFiniteMagnitude/IsCountable

namespace dedekind::ontology {
/**
 * @concept IsSequence
 * @brief A mapping from a Countable Index to a Set.
 */
export template <typename Seq>
concept IsSequence = IsSet<Seq> && requires(Seq s, size_t i) {
  { s.at(i) } -> std::same_as<typename Seq::element_type>;
};

/**
 * @concept IsCountable
 * @brief Can be projected into a Sequence (Enumerability).
 */
export template <typename S>
concept IsCountableSet = requires(S s) {
  { s.as_sequence() } -> IsSequence;
};

/**
 * @concept IsFinite
 * @brief A Sequence that has a terminal index.
 */
export template <typename S>
concept IsFinite = IsCountableSet<S> && requires(S s) {
  { s.size() } -> std::integral;  // The termination proof
};
};  // namespace dedekind::ontology
