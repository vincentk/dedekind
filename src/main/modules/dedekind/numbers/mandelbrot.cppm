/**
 * @file dedekind/numbers/mandelbrot.cppm
 * @partition :mandelbrot
 * @brief Core Mandelbrot recurrence helpers over complex carriers.
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
 * @brief Escape predicate: ComplexType → L (a LogicalValue).
 *
 * Default L = bool gives the classical criterion.
 * L = Ternary gives the Kleene criterion:
 *   True    = escape witnessed
 *   Unknown = not yet escaped (open question)
 *   False   = provably bounded (unreachable by finite computation alone)
 */
template <typename EscapeCriterion, typename ComplexType, typename L = bool>
concept IsEscapeCriterion =
    std::copy_constructible<std::decay_t<EscapeCriterion>> &&
    std::invocable<const std::decay_t<EscapeCriterion>&, const ComplexType&> &&
    std::same_as<std::invoke_result_t<const std::decay_t<EscapeCriterion>&,
                                      const ComplexType&>,
                 L>;

/**
 * Classical escape criterion: |z|² > r².
 */
export template <IsComplexScalar R>
constexpr auto euclidean_escape_radius_squared(R escape_radius_squared = R{4}) {
  return outside_closed_euclidean_ball_squared(escape_radius_squared);
}

/**
 * Kleene escape criterion: True if |z|² > r², Unknown otherwise.
 *
 * The asymmetry is epistemic: we can witness divergence (True) but
 * cannot prove boundedness from a finite prefix (so never False).
 */
export template <IsComplexScalar R>
constexpr auto kleene_escape_radius_squared(R escape_radius_squared = R{4}) {
  return [escape_radius_squared](const Complex<R>& z) -> Ternary {
    return outside_closed_euclidean_ball_squared(escape_radius_squared)(z)
               ? Ternary::True
               : Ternary::Unknown;
  };
}

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

/**
 * The divergence spectrum of an orbit: a monotone Path<Ternary>.
 *
 *   orbit_divergence_path(orbit, p)(n)
 *     = exists(prefix(orbit, n+1), p)
 *     = True    if ∃ k ≤ n : p(z_k) = True   (escape witnessed)
 *     = Unknown if ∀ k ≤ n : p(z_k) = Unknown (no escape yet)
 *
 * Monotonicity: True is the absorbing element of Kleene OR, so once the
 * path reaches True it stays there.  A monotone {Unknown,True}-valued path
 * is isomorphic to ℕ∞ — its information content is exactly the escape time.
 *
 * This is the intensional form; orbit_escape_time() is the materialization.
 */
export template <IsComplexScalar R, typename EscapeCriterion>
  requires IsEscapeCriterion<EscapeCriterion, Complex<R>>
constexpr auto orbit_divergence_path(const OrbitPath<R>& orbit,
                                     EscapeCriterion criterion)
    -> Path<Ternary> {
  return scan(
      [criterion](const FinitePath<Complex<R>>& p) -> Ternary {
        return exists(p, classify<Complex<R>>(criterion).χ) ? Ternary::True
                                                            : Ternary::Unknown;
      },
      orbit);
}

/**
 * The escape time: the first index k ≤ max_iter at which the orbit escapes,
 * or nullopt if no escape is witnessed within the budget.
 *
 *   orbit_escape_time(orbit, n, p) = min { k ≤ n | p(z_k) }  or  nullopt
 *
 * This is the canonical materialization of the divergence spectrum:
 *   orbit_divergence_path(orbit, kleene(p)).at(n) = True
 *     iff orbit_escape_time(orbit, n, p).has_value()
 *
 * Escape time is strictly more informative than orbit_escapes() — it carries
 * the depth at which divergence was witnessed, enabling escape-time colouring
 * of the tower approximation.
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

/**
 * Lift an orbit-level criterion to a point-level predicate:
 *   c -> criterion(orbit(c)).
 */
export template <IsComplexScalar R, typename OrbitCriterion>
constexpr auto bounded(OrbitCriterion criterion) {
  auto orbit = arrow(mandelbrot_orbit<R>);
  auto classify_orbit = arrow(criterion);
  return orbit >> classify_orbit;
}

/**
 * Subobject view:
 *   M = { c in C | bounded(orbit(c)) }.
 */
export template <IsComplexScalar R, typename BoundedPredicate>
constexpr auto M(BoundedPredicate is_bounded) {
  auto c = var<ComplexesOf<R>>;
  return Set{c % ComplexesOf<R>{} | classify(is_bounded).χ};
}

/**
 * Finite-prefix orbit criterion: true iff orbit does not escape within N steps.
 *   prefix_bounded_N(orbit) ≡ ¬ orbit_escape_time(orbit, N).has_value()
 */
export template <IsComplexScalar R, typename EscapeCriterion>
  requires IsEscapeCriterion<EscapeCriterion, Complex<R>>
constexpr auto prefix_bounded_N(std::size_t max_iter,
                                EscapeCriterion escape_criterion) {
  return [max_iter, escape_criterion](const OrbitPath<R>& orbit) {
    return !orbit_escape_time(orbit, max_iter, escape_criterion).has_value();
  };
}

/**
 * @overload with default Euclidean escape radius.
 */
export template <IsComplexScalar R>
constexpr auto prefix_bounded_N(std::size_t max_iter,
                                R escape_radius_squared = R{4}) {
  return prefix_bounded_N<R>(
      max_iter, euclidean_escape_radius_squared(escape_radius_squared));
}

/**
 * M_N = { c in C | prefix_bounded_N(orbit(c)) }.
 */
export template <IsComplexScalar R>
constexpr auto M_N(std::size_t max_iter, R escape_radius_squared = R{4}) {
  return M<R>(bounded<R>(prefix_bounded_N<R>(max_iter, escape_radius_squared)));
}

/**
 * Tower constructor:
 *   n |-> M_n where each stage is a computable finite-prefix approximation.
 */
export template <IsComplexScalar R>
constexpr auto M_tower(R escape_radius_squared = R{4}) {
  return [escape_radius_squared](std::size_t n) {
    return M_N<R>(n, escape_radius_squared);
  };
}

}  // namespace dedekind::numbers
