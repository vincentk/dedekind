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
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "La duda es uno de los nombres de la inteligencia."
 *       — Jorge Luis Borges (Argentina), Spanish Wikiquote.
 *       [Trans: "Doubt is one of the names of intelligence."]
 */
module;

#include <concepts>
#include <cstddef>
#include <type_traits>

export module dedekind.sequences:net;

import dedekind.category;
import dedekind.sets;
import dedekind.order;

namespace dedekind::sequences {
using namespace dedekind::category;
using namespace dedekind::sets;
using namespace dedekind::order;

/**
 * @concept IsIndexedMorphismFamily
 * @brief A category arrow equipped with explicit index cardinality metadata.
 *
 * @details
 * This concept captures families F: I -> X as first-class morphisms whose
 * index species I and codomain species X are represented by Domain/Codomain,
 * while cardinality metadata remains inspectable at compile time.
 */
export template <typename F>
concept IsIndexedMorphismFamily = IsArrow<F> && requires(const F f) {
  typename F::Domain;
  typename F::Codomain;
  typename F::cardinality_type;
  requires IsCardinality<typename F::cardinality_type>;
  { f.cardinality() } -> std::same_as<typename F::cardinality_type>;
};

/**
 * @concept IsCountablyIndexedFamily
 * @brief An indexed family whose index magnitude is finite or countably
 * infinite.
 */
export template <typename F>
concept IsCountablyIndexedFamily =
    IsIndexedMorphismFamily<F> && IsCountable<typename F::cardinality_type>;

/** @concept IsNet: A Morphism from a Directed Set. */
export template <typename N>
concept IsNet = IsArrow<N> && IsDirectedSet<typename N::Domain>;

/**
 * @concept IsSequence
 * @brief The fundamental mapping from a Domain Set to a Value Type.
 */
export template <typename Seq>
concept IsSequence = IsNet<Seq> && IsCountablyIndexedFamily<Seq> && requires {
  // Refactored: value_type -> Codomain
  typename Seq::Codomain;
  typename Seq::Domain;
} && requires(const Seq s) {
  requires IsSpecies<typename Seq::Domain>;
};

/**
 * @concept IsFiniteSequence
 * @brief A sequence bound by a Finite Domain.
 */
export template <typename Seq>
concept IsFiniteSequence = IsSequence<Seq> && requires(Seq s) {
  typename Seq::cardinality_type;
  requires IsFiniteMagnitude<typename Seq::cardinality_type>;
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

namespace detail {
template <typename T>
struct toy_countable_family {
  using Domain = std::size_t;
  using Codomain = T;
  using cardinality_type = ℵ_0;

  constexpr T operator()(Domain) const { return T{}; }
  static consteval cardinality_type cardinality() { return {}; }
};
}  // namespace detail

/** @section Formal_Verification */
static_assert(IsIndexedMorphismFamily<detail::toy_countable_family<int>>,
              "Countable toy family must satisfy indexed family concept.");

static_assert(IsCountablyIndexedFamily<detail::toy_countable_family<int>>,
              "Countable toy family must satisfy countable-index concept.");

}  // namespace dedekind::sequences
