/**
 * @file dedekind/sequences/net.cppm
 * @partition :sequences
 * @brief The Path (The Morphism of Enumeration).
 *
 * @section net__Sequences
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
 *
 * @section net__IsSequence_vs_stdlib_388
 *
 * @c IsSequence anchors bidirectionally to the C++ standard library
 * via two complementary surfaces.
 *
 * @subsection Iterator_Range_Anchor (available today)
 *
 * @c FinitePath<T> already exposes @c begin() / @c end() returning
 * iterators that satisfy the @c std::input_iterator concept (cf.\
 * @c sequences:path), so @c FinitePath<T> is a
 * @c std::ranges::input_range out of the box.  The reverse direction
 * is provided by the @c from_range factory in @c sequences:path,
 * which lifts any @c std::ranges::input_range into a
 * @c FinitePath<T>.  In symbols:
 *
 *   @c FinitePath<T> @c => @c std::ranges::input_range
 *
 *   @c std::ranges::input_range<R> @c => @c FinitePath<value_t> @c =
 *   @c from_range(r)
 *
 * This is the anchor that makes @c FinitePath<T> values flow into
 * @c std::ranges algorithms such as @c transform and @c for_each,
 * as well as range adaptors such as @c std::views::filter, and
 * into iterator-based reductions such as @c std::accumulate ---
 * without a bespoke adapter.  Other @c IsFiniteSequence carriers
 * acquire the same range surface by additionally exposing
 * @c begin() / @c end() (the concept does not require it;
 * @c FinitePath provides it).
 *
 * @subsection Coroutine_Anchor (C++23, future once libc++ ships @c
 * \<generator\>)
 *
 * The C++23 @c std::generator<T> coroutine (P2502R2,
 * @c \<generator\>) is the natural pull-model dual: a coroutine that
 * yields @c T values in order.  Semantically it is a sequence with
 * domain @f$\mathbb{N}@f$ (or a finite prefix) and codomain @c T,
 * isomorphic to @c IsFiniteSequence / @c IsSequence over @c std::size_t.
 * An adapter once libc++ ships @c \<generator\>:
 *
 * @code
 * template <typename Seq> requires IsFiniteSequence<Seq>
 * std::generator<typename Seq::Codomain> as_std_generator(Seq s) {
 *   for (std::size_t i = 0; i < s.size(); ++i) co_yield s.at(i);
 * }
 * @endcode
 *
 * Documented rather than implemented to avoid a toolchain-version
 * dependency in the main source.  Until libc++ ships it, the
 * iterator/range anchor above covers the same use cases.
 *
 * @subsection On_Pull_vs_Push
 *
 * @c IsSequence is a @b pull model (consumer asks for index @c i,
 * receives @c s.at(i)) and admits a @b directed-set domain via the
 * @c IsNet refinement (cf.\ Munkres / Kelley nets).  Standard
 * iterators and @c std::generator are both @b pull-style too, but
 * with domain implicitly @f$\mathbb{N}@f$.  The library's @c IsNet
 * generalises the standard view to non-totally-ordered index sets;
 * the iterator/range and coroutine anchors specialise to the
 * @f$\mathbb{N}@f$-indexed case.
 *
 * A natural worked example: the primitive-element enumeration
 * @f$\mathbb{F}_q^{\times} = \{\alpha^0, \alpha^1, \ldots,
 * \alpha^{q-2}\}@f$ presents cleanly as both
 * @c std::ranges::input_range (today, via @c FinitePath) and
 * @c std::generator<F> (future, via the adapter sketched above).
 */
export template <typename Seq>
concept IsSequence = IsNet<Seq> && IsCountablyIndexedFamily<Seq> && requires {
  // Refactored: value_type -> Codomain
  typename Seq::Codomain;
  typename Seq::Domain;
} && requires(const Seq s) { requires IsSpecies<typename Seq::Domain>; };

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

/** @section net__Formal_Verification */
static_assert(IsIndexedMorphismFamily<detail::toy_countable_family<int>>,
              "Countable toy family must satisfy indexed family concept.");

static_assert(IsCountablyIndexedFamily<detail::toy_countable_family<int>>,
              "Countable toy family must satisfy countable-index concept.");

}  // namespace dedekind::sequences
