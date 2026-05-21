/**
 * @file dedekind/numbers/mandelbrot.cppm
 * @partition :mandelbrot
 * @brief Core Mandelbrot recurrence helpers over complex carriers.
 *
 * Architecture — three layers of the "intensional first" strategy:
 *
 *   Layer 1 (intensional, Kleene, infinite):
 *     orbit_divergence_path(orbit(c), criterion) : Path<Ternary>
 *     The exact epistemic state at each depth — not fully computable for
 *     in-set points, but expressible as a lazy infinite sequence.
 *
 *   Layer 2 (N-step Kleene approximation, computable):
 *     M_kleene_N(max_iter, criterion)(c) : Ternary
 *       True    = escape witnessed within max_iter steps (c is outside M)
 *       Unknown = no escape yet (open question: c might be in M or escape
 * later) False   = provably bounded (unreachable by finite computation alone)
 *
 *   Layer 3 (Boolean collapse, classical set):
 *     M_N(max_iter, criterion, policy)(c) : Set<..., Bool>
 *     KleenePolicy::Inclusive  =>  Unknown → in M   (outer approx, M_N ⊇
 * M_true) KleenePolicy::Exclusive  =>  Unknown → not in M (inner approx, M_N ⊆
 * M_true; ≡ ∅ under finite computation)
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "Man muss immer umkehren."
 *       ("One must always invert.")
 *       -- Carl Gustav Jacob Jacobi
 */
module;

#include <concepts>
#include <cstddef>
#include <functional>
#include <optional>
#include <type_traits>
#include <utility>

export module dedekind.numbers:mandelbrot;

import dedekind.category;
import dedekind.geometry;
import dedekind.sequences;
import dedekind.sets;
import :complex;

namespace dedekind::numbers {
using namespace dedekind::category;
using namespace dedekind::geometry;
using namespace dedekind::sequences;
using namespace dedekind::sets;

/**
 * @concept IsEscapeCriterion
 * @brief Escape predicate: ComplexType → bool.
 *
 * Returns true when the orbit point has definitively left the bounded region.
 * The Kleene lifting (bool → Ternary) is performed internally by the layer
 * machinery; callers supply a classical Boolean criterion.
 */
template <typename EscapeCriterion, typename ComplexType>
concept IsEscapeCriterion =
    std::copy_constructible<std::decay_t<EscapeCriterion>> &&
    std::invocable<const std::decay_t<EscapeCriterion>&, const ComplexType&> &&
    std::same_as<std::invoke_result_t<const std::decay_t<EscapeCriterion>&,
                                      const ComplexType&>,
                 bool>;

// ─── Escape criterion
// ─────────────────────────────────────────────────────────

/**
 * Classical escape criterion: |z|² > r².
 */
export template <IsComplexScalar R>
constexpr auto euclidean_escape_radius_squared(R escape_radius_squared = R{4}) {
  return outside_closed_euclidean_ball_squared(escape_radius_squared);
}

// ─── Orbit primitives
// ─────────────────────────────────────────────────────────

export template <IsComplexScalar R>
using OrbitPath = Path<Complex<R>>;

export template <IsComplexScalar R>
constexpr auto mandelbrot_step(const Complex<R>& c) {
  return [c](const Complex<R>& z) { return (z * z) + c; };
}

export template <IsComplexScalar R>
constexpr auto mandelbrot_orbit(const Complex<R>& c) {
  const auto zero = partial_identity_v<Complex<R>, PartialAddComplex<R>>;
  return iterate(zero, mandelbrot_step(c));
}

// ─── Layer 1: intensional divergence path
// ─────────────────────────────────────

/**
 * @class DivergencePath
 * @brief The escape signal of a Mandelbrot orbit, tagged as a sequence-shape
 *        witness: a monotone @c {Unknown,True}-valued @c Path<Ternary> that
 *        is @b structurally absorptive for every parameter @c c.
 *
 * @details The divergence path of @c orbit_divergence_path is eventually
 * constant regardless of @c c:
 *
 *   - @b Escaping @c c (outside @f$M@f$): the path is @c Unknown until the
 *     escape index, then @c True forever (@c True is the absorbing element
 *     of Kleene OR — @c §sequences::IsAbsorptiveSequence's "tail factors
 *     through a singleton @f$\{z\}@f$" with @f$z = @c True@f$).
 *   - @b Bounded @c c (inside @f$M@f$): the path is the constant @c Unknown
 *     sequence — eventually-constant from index 0, the degenerate absorptive
 *     case (@f$z = @c Unknown@f$).
 *
 * So the @c IsAbsorptiveSequence shape holds at the @b type level for the
 * carrier as a whole, independently of the (value-level, undecidable)
 * membership question for any individual @c c.  This is the honest leverage
 * the sequence API affords Mandelbrot: per-@c c boundedness
 * (@c IsBoundedSequence ⟺ @f$c \in M@f$) is value-dependent and cannot be
 * type-witnessed, but "the escape indicator is an absorptive sequence" @b is.
 *
 * @see dedekind::sequences::IsAbsorptiveSequence
 */
export template <IsComplexScalar R>
struct DivergencePath : Path<Ternary> {
  constexpr explicit DivergencePath(Path<Ternary> p)
      : Path<Ternary>{std::move(p)} {}
};

/**
 * The divergence spectrum of an orbit: a monotone @c DivergencePath<R>.
 *
 *   orbit_divergence_path(orbit, p)(n)
 *     = True    if ∃ k ≤ n : p(z_k)   (escape witnessed)
 *     = Unknown if ∀ k ≤ n : ¬p(z_k)  (no escape yet)
 *
 * Monotonicity: True is the absorbing element of Kleene OR, so once the
 * path reaches True it stays there.  A monotone {Unknown,True}-valued path
 * is isomorphic to ℕ∞ — its information content is exactly the escape time.
 * The result type @c DivergencePath<R> carries the
 * @c sequences::IsAbsorptiveSequence shape (eventually constant for every
 * @c c — see the class doc), making the eventual-constancy a type-level fact.
 *
 * This is the Layer 1 intensional object; orbit_escape_time() is the
 * efficient materialization.
 */
export template <IsComplexScalar R, typename EscapeCriterion>
  requires IsEscapeCriterion<EscapeCriterion, Complex<R>>
constexpr auto orbit_divergence_path(const OrbitPath<R>& orbit,
                                     EscapeCriterion criterion)
    -> DivergencePath<R> {
  return DivergencePath<R>{scan(
      [criterion](const FinitePath<Complex<R>>& p) -> Ternary {
        return exists(p, classify<Complex<R>>(criterion).χ) ? Ternary::True
                                                            : Ternary::Unknown;
      },
      orbit)};
}

/**
 * The escape time: the first index k ≤ max_iter at which the orbit escapes,
 * or nullopt if no escape is witnessed within the budget.
 *
 *   orbit_escape_time(orbit, n, p) = min { k ≤ n | p(z_k) }  or  nullopt
 *
 * Equivalently:
 *   orbit_divergence_path(orbit, p).at(n) = True
 *     iff orbit_escape_time(orbit, n, p).has_value()
 */
export template <IsComplexScalar R, typename EscapeCriterion>
  requires IsEscapeCriterion<EscapeCriterion, Complex<R>>
constexpr std::optional<std::size_t> orbit_escape_time(
    const OrbitPath<R>& orbit, std::size_t max_iter,
    const EscapeCriterion& criterion) {
  return first_where(orbit, criterion, max_iter);
}

/**
 * @overload with default Euclidean escape radius.
 */
export template <IsComplexScalar R>
constexpr std::optional<std::size_t> orbit_escape_time(
    const OrbitPath<R>& orbit, std::size_t max_iter,
    R escape_radius_squared = R{4}) {
  return orbit_escape_time(
      orbit, max_iter, euclidean_escape_radius_squared(escape_radius_squared));
}

// ─── Collapse policy
// ──────────────────────────────────────────────────────────

/**
 * How Unknown (undecided within budget) maps to Boolean set membership.
 *
 *   Inclusive: Unknown → in M  (outer approximation, M_N ⊇ M_true)
 *   Exclusive: Unknown → not in M  (inner approximation, M_N ⊆ M_true;
 *              with finite computation this is effectively ∅, since no
 *              finite prefix can witness orbit boundedness)
 */
export enum class KleenePolicy { Inclusive, Exclusive };

/**
 * Collapse a Ternary escape signal to Boolean set membership under policy.
 *   True  = escaped → false (not in M)
 *   False = proven bounded → true (in M; unreachable by finite computation)
 *   Unknown → policy-dependent
 */
constexpr bool collapse_ternary(Ternary t, KleenePolicy policy) noexcept {
  return t != Ternary::True &&
         (t == Ternary::False || policy == KleenePolicy::Inclusive);
}

// ─── Layer 2: N-step Kleene membership
// ────────────────────────────────────────

/**
 * Point-level N-step Kleene membership:
 *   M_kleene_N(n, p)(c)
 *     = True    if orbit(c) escapes within n steps under p
 *     = Unknown if no escape witnessed within n steps
 *
 * The epistemic Layer 2 object: captures what we know at depth n, before any
 * Boolean collapse.
 */
export template <IsComplexScalar R, typename EscapeCriterion>
  requires IsEscapeCriterion<EscapeCriterion, Complex<R>>
constexpr auto M_kleene_N(std::size_t max_iter, EscapeCriterion criterion) {
  return [max_iter, criterion](const Complex<R>& c) -> Ternary {
    return orbit_escape_time(mandelbrot_orbit(c), max_iter, criterion)
                   .has_value()
               ? Ternary::True
               : Ternary::Unknown;
  };
}

// ─── Layer 3: Boolean set
// ─────────────────────────────────────────────────────

/**
 * M_N: { c in C | M_kleene_N(n, p)(c) is not True, under policy }.
 *
 *   KleenePolicy::Inclusive (default, outer approximation):
 *     M_N ⊇ M_true — includes all undecided points; the standard rendering.
 *   KleenePolicy::Exclusive (inner approximation):
 *     M_N ⊆ M_true — only confirmed members; ≡ ∅ under finite computation.
 */
export template <IsComplexScalar R, typename EscapeCriterion>
  requires IsEscapeCriterion<EscapeCriterion, Complex<R>>
constexpr auto M_N(std::size_t max_iter, EscapeCriterion criterion,
                   KleenePolicy policy = KleenePolicy::Inclusive) {
  const auto kleene = M_kleene_N<R>(max_iter, criterion);
  auto c = element<Ω<Complex<R>>>;
  return Set{c | classify([kleene, policy](const Complex<R>& x) {
                   return collapse_ternary(kleene(x), policy);
                 }).χ};
}

/**
 * Tower constructor:
 *   n |-> M_N(n, criterion, policy)
 */
export template <IsComplexScalar R, typename EscapeCriterion>
  requires IsEscapeCriterion<EscapeCriterion, Complex<R>>
constexpr auto M_tower(EscapeCriterion criterion,
                       KleenePolicy policy = KleenePolicy::Inclusive) {
  return [criterion, policy](std::size_t n) {
    return M_N<R>(n, criterion, policy);
  };
}

}  // namespace dedekind::numbers

namespace dedekind::sequences {

/** @brief Opt-in: every @c DivergencePath<R> is absorptive (eventually
 *         constant — @c True after escape, or the degenerate constant
 *         @c Unknown when bounded).  The eventual-constancy is structural,
 *         holding for all @c c, so it is a sound type-level registration.
 *         See @c dedekind::numbers::DivergencePath. */
export template <dedekind::numbers::IsComplexScalar R>
inline constexpr bool
    is_absorptive_sequence_v<dedekind::numbers::DivergencePath<R>> = true;

}  // namespace dedekind::sequences

namespace dedekind::numbers {

/** @section mandelbrot__Formal_Verification
 *  Sequence-API leverage pinned at the type level (#719). */

// The raw orbit n ↦ z_n is a bona-fide sequence (ℕ → Complex<R>).
static_assert(dedekind::sequences::IsSequence<OrbitPath<double>>,
              "A Mandelbrot orbit is an IsSequence (ℕ → Complex).");

// The escape indicator is a typed absorptive sequence — eventual-constancy
// for every c, independent of the value-level membership question.
static_assert(
    dedekind::sequences::IsAbsorptiveSequence<DivergencePath<double>>,
    "The Mandelbrot divergence path is an absorptive sequence: eventually "
    "constant (True after escape, else the degenerate constant Unknown).");

}  // namespace dedekind::numbers
