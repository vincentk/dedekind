/**
 * @file dedekind/sequences/net.cppm
 * @partition :sequences
 * @brief The Path (The Morphism of Enumeration).
 *
 * @section net__Sequences
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
  requires dedekind::sets::IsFinite<typename Seq::cardinality_type>;
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

/** @section net__Halfspace_As_Net_Witnesses — Sealing the IS-A relation (#665).
 *
 * A @c Halfspace<T,k,D,S,L> in @c :order:halfspace already exposes
 * @c Domain=T, @c Codomain=typename @c L::Ω, and
 * @c operator()(const @c T&) @c -> @c Codomain in the exact shape
 * @c IsArrow requires.  When @c T satisfies @c IsDirectedSet<T,L> (the
 * standard ordered carriers --- @f$\mathbb{N}@f$, @f$\mathbb{Z}@f$,
 * @f$\mathbb{R}@f$, @c std::size_t), the halfspace satisfies @c IsNet
 * automatically.  Same for @c OrderInterval; @c Singleton inhabits
 * @c IsArrow at the predicate level.
 *
 * Textbook identification (Munkres / Kelley): a halfspace
 * @f$\{x \in T \mid x \bowtie k\}@f$ on a directed carrier is an
 * @b indicator @b net --- predicate-valued on T's directed structure,
 * with the pivot @f$k@f$ as the convergence witness (eventually-True for
 * upward halfspaces with strict direction; eventually-False otherwise).
 * The C++ ontology and the algebraic ontology coincide here.
 *
 * The static_asserts below cost nothing at runtime and add zero surface
 * area to the existing types; they exist so the IS-A relation is visible
 * to downstream concept-binding (@c requires @c IsNet<H> picks up
 * halfspaces uniformly alongside sequences and other nets) and the
 * textbook identity is sealed mechanically, not by prose.  Honest
 * Rejection comes free: a carrier that fails @c IsDirectedSet (e.g.\
 * @f$\mathbb{C}@f$ without a chosen order) fails
 * @c IsNet<Halfspace<ℂ,...>> at instantiation, with the diagnostic
 * naming the missing textbook axiom.
 *
 * @note The companion identification --- @c Halfspace's @e extension as
 *       a typed directed sub-poset of @c T, and its inclusion as a
 *       sub-net of the identity-net on @c T --- requires sub-poset
 *       typing not yet present in the codebase, and is a downstream
 *       Sollbruchstelle for the convergence reification
 *       (@c net @c -> @c L @c ⟺ @c ∃ @c halfspace @c H @c ⊆ @c T
 *       @c such @c that @c f(H) @c ⊆ @c V).  See #602 / #665.
 */

// Halfspace IS-A IsArrow (carrier-independent: int suffices).
static_assert(
    dedekind::category::IsArrow<
        Halfspace<int, 5, Direction::Upward, Strictness::Strict,
                  dedekind::category::ClassicalLogic>>,
    "Halfspace exposes Domain / Codomain / operator() in the IsArrow shape.");

// Halfspace IS-A IsNet on a directed carrier (std::size_t under ≤).
static_assert(
    IsNet<Halfspace<std::size_t, 5, Direction::Upward, Strictness::Strict,
                    dedekind::category::ClassicalLogic>>,
    "On a directed carrier, a Halfspace is structurally an indicator net.");

// Downward direction is structurally symmetric.
static_assert(
    IsNet<Halfspace<std::size_t, 5, Direction::Downward, Strictness::NonStrict,
                    dedekind::category::ClassicalLogic>>,
    "Downward halfspaces inhabit IsNet symmetrically (eventually-False net).");

// OrderInterval inherits the same conformance --- two-sided indicator.
static_assert(
    dedekind::category::IsArrow<
        OrderInterval<int, 3, 7, Strictness::NonStrict, Strictness::NonStrict,
                      dedekind::category::ClassicalLogic>>,
    "OrderInterval exposes the IsArrow shape.");

static_assert(IsNet<OrderInterval<std::size_t, 3, 7, Strictness::NonStrict,
                                  Strictness::NonStrict,
                                  dedekind::category::ClassicalLogic>>,
              "On a directed carrier, OrderInterval is an indicator net "
              "(eventually-False past the upper pivot).");

// Singleton inhabits IsArrow at the predicate level --- the degenerate case.
static_assert(dedekind::category::IsArrow<
                  Singleton<42, dedekind::category::ClassicalLogic>>,
              "Singleton<v> exposes the IsArrow shape "
              "(degenerate indicator: True at a single index).");

}  // namespace dedekind::sequences
