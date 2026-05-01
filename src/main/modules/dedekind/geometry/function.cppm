/**
 * @file dedekind/geometry/function.cppm
 * @partition :function
 * @brief Concept anchoring for function spaces (functions on infinite domains as infinite-dimensional vectors) — #537 slice 1.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section function__Motivation
 *
 * A function @c f: @c D @c → @c T over an infinite domain @c D
 * (typically @c ℕ or @c ℝ) is morally an @b infinite-dimensional
 * @b vector — its components are the values @c f(d) indexed by
 * @c d @c ∈ @c D.  The codebase already has rule-based intensional
 * carriers that play this role at the algebraic / sequence layer:
 *
 *  - @c dedekind::sequences::Path<T, @c Cardinality> — domain
 *    @c std::size_t @c (≅ @c ℕ when @c Cardinality @c = @c ℵ_0),
 *    codomain @c T.
 *  - @c dedekind::linear_algebra::Diagonal<D, @c F>'s rule type @c F
 *    (#534) — itself an @c IsArrow with a scalar codomain, hence a
 *    function-space inhabitant.
 *  - @c dedekind::linear_algebra::OuterProduct<U, @c V>'s factor
 *    types @c U and @c V (#534) — same reading.
 *
 * This partition adds the concept-side anchoring:
 *
 *  - @c IsFunctionSpace<W, @c D, @c T> — concept witnessing that
 *    @c W carries the structural commitments of a function space:
 *    eval @c f(d), pointwise @c +, scalar @c · (vector-space
 *    operations under @c T as the scalar field).
 *  - @c Path<T, @c ℵ_0> registered as
 *    @c IsFunctionSpace<Path<T>, @c std::size_t, @c T> via the
 *    pointwise operators added in @c sequences:path under this issue.
 *
 * @section function__Pairing_With_Inner_And_Outer_Product
 *
 * Function spaces are the natural inhabitant of @c L²-flavoured
 * inner / outer products at infinite dimension.  @c :inner_product
 * already carries @c HasInnerProduct<V, @c F> @c / @c IsInnerProductSpace
 * for finite-dim @c Vector<F, @c N>; the obvious follow-up is an
 * @c L²(ℕ) @c / @c L²(D) carrier whose inner product is a series
 * @c ⟨f, @c g⟩ @c = @c Σ_{d∈D} @c f(d) @c · @c g(d) (subject to
 * summability).  The summability question is deferred to a measure-
 * theoretic follow-up; this partition only scopes the @b vector-space
 * surface (eval, @c +, scalar @c ·) without committing to a measure.
 *
 * @section function__Why_Geometry
 *
 * Three plausible homes were weighed in #537:
 * @c geometry:function (chosen here, named symmetrically with
 * @c :inner_product and @c :outer_product), @c linear_algebra:function
 * (pairs with the @c :diagonal carrier), and @c analysis:function
 * (pairs with future measure / convergence content).  The geometric
 * home wins on partition naming consistency and on the natural pairing
 * with the existing inner / outer-product anchorings.
 *
 * Wikipedia: Function space, Sequence space, L^p space
 *
 * @note "Le but général de l'analyse, c'est de soumettre tous les
 *  phénomènes au calcul, et de réduire ainsi à des problèmes
 *  d'algèbre les questions les plus difficiles de la mécanique et
 *  de l'astronomie."
 *       [Trans: "The general aim of analysis is to subject all
 *       phenomena to calculation, and so reduce the most difficult
 *       questions of mechanics and astronomy to problems of algebra."]
 *       — Joseph Fourier, @em Théorie @em analytique @em de @em la
 *       @em chaleur (1822), Discours préliminaire.  The function-
 *       space anchoring here lifts that Fourier-style "function as
 *       infinite vector" reading into the type system.
 */
module;

#include <concepts>
#include <cstddef>

export module dedekind.geometry:function;

import dedekind.sequences; // Path<T, Cardinality> — the canonical infinite-dim function-space inhabitant
import dedekind.sets; // ℵ_0 — the canonical infinite cardinality

namespace dedekind::geometry {

/** @section function__Concept */

/**
 * @concept IsFunctionSpace
 * @brief @c W is a function space @c T^D — eval @c f(d), pointwise
 *        @c +, and scalar @c · over @c T.
 *
 * @tparam W  Carrier type representing the function space.
 * @tparam D  Domain type (the index / argument type; @c std::size_t
 *            for @c ℕ-indexed function spaces, future
 *            @c IsExtensionalCardinal carriers for richer domains).
 * @tparam T  Codomain / scalar field type.
 *
 * @details The concept value-side requires three operations:
 *
 *  - @b Eval: @c f(d) yields a @c T-convertible value.
 *  - @b Pointwise @c +: @c f @c + @c g closes the carrier.
 *  - @b Scalar @c ·: @c s @c · @c f closes the carrier (left
 *    multiplication; carriers MAY also expose right multiplication
 *    @c f @c · @c s but it is not required by the concept).
 *
 * The pointwise / scalar laws (associativity of @c +, distributivity
 * over @c ·, identity elements) are documented but not encoded in
 * this concept.  Pinning them as @c static_asserts requires
 * value-level witnesses on a constexpr-friendly inhabitant; today
 * @c sequences::Path<T> uses @c std::function and is not constexpr,
 * so the witness story is at the type level (concept inhabitation).
 * Constexpr inhabitants (e.g. @c Diagonal<D, @c F>'s rule type) can
 * pin the laws value-side in a follow-up.
 */
export template <typename W, typename D, typename T>
concept IsFunctionSpace =
    requires(W const& f, W const& g, D const& d, T const& s) {
      /** Eval: a function-space inhabitant is callable on the domain. */
      { f(d) } -> std::convertible_to<T>;
      /** Pointwise addition closes the carrier strictly (no proxy /
       *  promoted return type — same convention as
       *  @c geometry::IsAffine and
       *  @c algebra::HasVectorSpaceOperators). */
      { f + g } -> std::same_as<W>;
      /** Scalar multiplication closes the carrier strictly. */
      { s * f } -> std::same_as<W>;
    };

/** @section function__Witnesses */

// Path<T, ℵ_0> — the canonical ℵ_0-dimensional function-space
// inhabitant.  Witnesses the concept at int / unsigned int (the
// canonical primitive carrier under modular arithmetic).
static_assert(IsFunctionSpace<dedekind::sequences::Path<int>, std::size_t, int>,
              "Path<int> (cardinality ℵ_0) is a function space ℕ → int: "
              "eval / pointwise + / scalar · all close on the carrier.");
static_assert(IsFunctionSpace<dedekind::sequences::Path<unsigned int>,
                              std::size_t, unsigned int>,
              "Path<unsigned int> is a function space ℕ → unsigned int.");

// FinitePath<T> — the finite cardinal sibling.  Same operator surface
// (with min-extent semantics on +); same function-space inhabitation.
static_assert(
    IsFunctionSpace<dedekind::sequences::FinitePath<int>, std::size_t, int>,
    "FinitePath<int> is a function space [0, N) → int — finite-dim "
    "function space, but the same intensional operator surface as Path.");

/** @section function__Followups
 *
 *  Tracked on #537:
 *
 *   - Register @c Diagonal<D, @c F>'s rule type @c F structurally as
 *     a function-space inhabitant once the @c IsArrow surface gets
 *     pointwise @c + / scalar @c ·.
 *   - Extend the concept with @c IsFunctionSpaceVectorSpace adding
 *     the additive identity @c 0_F and laws.
 *   - @c L²(ℕ) carrier with summable inner product
 *     @c ⟨f, @c g⟩ @c = @c Σ @c f(d) @c · @c g(d); deferred until the
 *     measure / summability story arrives in @c analysis.
 */

}  // namespace dedekind::geometry
