/**
 * @file dedekind/algebra/vectorspace.cppm
 * @partition :vectorspace
 * @brief vector-space concepts + concrete witnesses.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section vectorspace__Scope
 * A vector space is a module whose scalar ring is additionally a
 * field.  This partition hosts the concepts and the concrete
 * vector-space witnesses at that strictly higher layer:
 *
 *   - `IsVectorSpace<V, F>`     --- strict axiomatic witness, uses
 *                                   the axiomatic
 *                                   `dedekind::category::IsField`.
 *   - `HasVectorSpaceOperators<V, F>` --- operational counterpart.
 *   - `IsFieldElement<F>`       --- slogan: "a field element is a 1D
 *                                   vector space over itself."  Its
 *                                   operational counterpart is the
 *                                   canonical `HasFieldOperators<F>`
 *                                   from `:field`.
 *   - `SatisfiesVectorSpaceAxioms` --- structural axiom-shape battery.
 *
 * Witnesses:
 *   - @c RealLine as @c HasVectorSpaceOperators (operational);
 *   - @c 𝔽64 (from @c :galois) as @c IsVectorSpace<𝔽64, bool> ---
 *     a 6-D vector space over the prime subfield @f$\mathbb{F}_2@f$;
 *   - @c uint64_t as @c IsVectorSpace<uint64_t, bool, ...> ---
 *     @f$\mathbb{F}_2^{64}@f$, the 64-D vector space over
 *     @f$\mathbb{F}_2@f$.  Not a field: has zero divisors.  Adds no
 *     struct: @c uint64_t already \emph{is} the carrier; XOR is
 *     addition; a @c Bit2U64Action functor is the scalar action.
 *
 * The module-level concepts (semimodule, module, 1D
 * @c OneDimensionalVector, integral-scalar detectors) remain in
 * @c :modules; the operational field-shape witness
 * @c HasFieldOperators is canonical in @c :field; Galois-field
 * concepts and carriers (bool-as-𝔽2, struct 𝔽64) live in
 * @c :galois.  @c :vectorspace imports all three.
 *
 * @note "Die Geometrie ist nicht die Wissenschaft von den Größen,
 *        sondern von der Art und Weise, wie sie zusammenhängen."
 *       [Trans: "Geometry is not the science of magnitudes, but of the
 *        way they hang together."]
 *       — Hermann Graßmann, *Die lineale Ausdehnungslehre, ein neuer
 *         Zweig der Mathematik* (Leipzig, 1844), Einleitung. The
 *         founding text of vector-space theory; Graßmann's relational
 *         framing prefigures the modern axiomatic definition.
 */
module;

#include <cstdint>  // for std::uint64_t (F_2^64 carrier)
#include <functional>  // for std::plus, std::multiplies, std::bit_xor, std::bit_and

export module dedekind.algebra:vectorspace;

import dedekind.category;
import :field;
import :galois;
import :modules;

namespace dedekind::algebra {
using namespace dedekind::category;

/**
 * @concept IsVectorSpace
 * @brief The Gold Standard: A Module where the Scalar is a Field.
 * @tparam V The vector carrier.
 * @tparam F The scalar field carrier.
 * @tparam AddV Additive law on V (defaults to `std::plus<V>`).
 * @tparam AddF Additive law on F (defaults to `std::plus<F>`).
 * @tparam MultF Multiplicative law on F (defaults to `std::multiplies<F>`).
 * @tparam Act External action witness F x V -> V
 * (defaults to `std::multiplies<>`).
 *
 * @details Uses @c dedekind::category::IsField (axiomatic, no
 *          division-operator requirement) so that primitive scalar
 *          carriers like @c bool under @c (std::bit_xor,
 *          std::bit_and) work without needing a division surface.
 *          The operator-level field witness
 *          @c dedekind::algebra::IsField composes additionally with
 *          @c IsDivisionRing; code that needs the division surface
 *          on the scalar side should check that separately.
 */
export template <typename V, typename F, typename AddV = std::plus<V>,
                 typename AddF = std::plus<F>,
                 typename MultF = std::multiplies<F>,
                 typename Act = std::multiplies<>>
concept IsVectorSpace = IsModule<V, F, AddV, AddF, MultF, Act> &&
                        dedekind::category::IsField<F, AddF, MultF>;

/**
 * @concept IsFieldElement
 * @brief A bona fide element of a field is a 1D vector space over itself.
 * @details Tightening `IsScalar` when division is available: F is its own
 *          1D vector space, with F as the scalar field.
 */
export template <typename F>
concept IsFieldElement = IsVectorSpace<F, F>;

/**
 * @concept HasVectorSpaceOperators
 * @brief Pragmatic vector-space check used by first reified vector carriers.
 * @details Uses the operational `HasFieldOperators` witness and does not
 *          depend on the stronger `IsField` proof machinery.
 *
 * @note @b Design @b choice (audit #393): paired with
 *       @c HasFieldOperators in the operational tower, intentionally
 *       distinct from the strict @c category::IsVectorSpace.  The
 *       paired pragmatism admits IEEE-backed carriers (e.g.\ a
 *       @c LinearMap<double, R, C> witness over @c double) that
 *       deliberately do not carry the strict categorical proof.
 *       Where a callsite depends only on the operator surface
 *       compiling (and not on vector-space axioms holding under the
 *       policy in force), see @c dedekind::algebra::HasRingOperators
 *       and siblings.
 */
export template <typename V, typename F, typename Act = std::multiplies<>>
concept HasVectorSpaceOperators =
    HasFieldOperators<F> && HasGroupOperatorsAdd<V> && requires(F a, V v) {
      { Act{}(a, v) } -> std::same_as<V>;
    };

// "A field element is a 1D vector space over itself" — the slogan was
// previously reified as @c IsFieldElementLike<F> and then briefly as a derived
// alias here.  The operational counterpart of @c IsFieldElement collapses to
// the canonical @c HasFieldOperators<F> from @c :field (Stream 3 of #374);
// downstream code consumes @c HasFieldOperators directly.

/**
 * @concept SatisfiesVectorSpaceAxioms
 * @brief Operational witness of the core vector-space axiom signatures.
 */
export template <typename V, typename F, typename AddV = std::plus<V>,
                 typename AddF = std::plus<F>,
                 typename MultF = std::multiplies<F>,
                 typename Act = std::multiplies<>>
concept SatisfiesVectorSpaceAxioms =
    HasVectorSpaceOperators<V, F, Act> && requires(F a, F b, V x, V y) {
      { AddV{}(x, y) } -> std::same_as<V>;
      { Act{}(a, AddV{}(x, y)) } -> std::same_as<V>;
      { AddV{}(Act{}(a, x), Act{}(a, y)) } -> std::same_as<V>;

      { AddF{}(a, b) } -> std::same_as<F>;
      { Act{}(AddF{}(a, b), x) } -> std::same_as<V>;
      { AddV{}(Act{}(a, x), Act{}(b, x)) } -> std::same_as<V>;

      { MultF{}(a, b) } -> std::same_as<F>;
      { Act{}(MultF{}(a, b), x) } -> std::same_as<V>;
      { Act{}(a, Act{}(b, x)) } -> std::same_as<V>;

      // Unit axiom is checked when a multiplicative identity witness exists.
      requires(
          !requires { dedekind::category::identity_v<F, MultF>; } ||
          requires {
            {
              Act{}(dedekind::category::identity_v<F, MultF>, x)
            } -> std::same_as<V>;
          });
    };

/**
 * @struct Bit2U64Action
 * @brief Scalar action @f$\mathbb{F}_2 \times \mathbb{F}_2^{64} \to
 *        \mathbb{F}_2^{64}@f$ on @c (bool, uint64_t).
 *
 * @details The scalar side is @f$\mathbb{F}_2@f$ (witnessed on @c
 * bool under @c (std::bit_xor, std::bit_and); see @c :galois).  The
 * vector side is @c std::uint64_t carrying the 64-D vector space
 * @f$\mathbb{F}_2^{64}@f$ under @c std::bit_xor<uint64_t>.  The
 * action is "multiply every bit of @c v by the scalar bit":
 * @c false·v @c = @c 0, @c true·v @c = @c v, which is bitwise AND
 * between the broadcast scalar and the vector --- implementable as a
 * branch or as @c (s ? ~0ull : 0ull) @c & @c v.
 */
export struct Bit2U64Action {
  constexpr std::uint64_t operator()(bool s, std::uint64_t v) const noexcept {
    return s ? v : std::uint64_t{0};
  }
};

}  // namespace dedekind::algebra

namespace dedekind::algebra {

/** @section vectorspace__Formal_Verification */

// RealLine's operational vector-space witnesses.
static_assert(HasVectorSpaceOperators<RealLine, RealLineScalar>,
              "RealLine should satisfy the baseline 1D vector-space witness.");
static_assert(SatisfiesVectorSpaceAxioms<RealLine, RealLineScalar>,
              "RealLine should satisfy vector-space axiom signatures.");

// 𝔽64 is a 6-D vector space over the prime subfield 𝔽2 (witnessed on
// bool under (XOR, AND); see :galois for the scalar-action operator
// `operator*(bool, 𝔽64)`).  The vector-side additive group uses
// std::plus<𝔽64> (defined on the struct), and the scalar-side field
// uses std::bit_xor<bool> / std::bit_and<bool>.
static_assert(IsVectorSpace<𝔽64, bool, std::plus<𝔽64>, std::bit_xor<bool>,
                            std::bit_and<bool>>,
              "𝔽64 must satisfy IsVectorSpace<𝔽64, bool>: the Galois field "
              "𝔽_{64} = 𝔽_2[x]/(x^6 + x + 1) is a 6-D vector space over 𝔽2.");

// uint64_t under XOR is the 64-D vector space 𝔽_2^{64} over 𝔽2.
// Vector additive group: std::bit_xor<uint64_t>.  Scalar field: bool
// under (std::bit_xor, std::bit_and).  Scalar action: Bit2U64Action.
static_assert(
    IsVectorSpace<std::uint64_t, bool, std::bit_xor<std::uint64_t>,
                  std::bit_xor<bool>, std::bit_and<bool>, Bit2U64Action>,
    "uint64_t under XOR must satisfy IsVectorSpace<uint64_t, bool>: "
    "it is the 64-dimensional vector space 𝔽_2^{64} over 𝔽2.");

}  // namespace dedekind::algebra
