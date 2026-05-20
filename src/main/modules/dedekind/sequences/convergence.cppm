/**
 * @file dedekind/sequences/convergence.cppm
 * @partition :convergence
 * @brief Sequence/series convergence predicates and standard test hooks.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "A theorem's elegance is measured by the simplicity of the tests
 * needed to trust it in practice."
 *       -- Olga Taussky-Todd, paraphrase
 */
module;

#include <cmath>
#include <concepts>
#include <cstddef>
#include <vector>

export module dedekind.sequences:convergence;

import :net;  // IsSequence (Form-chain row 4) — #719 Slice 0
import :path;
import :limits;  // limit() — :sequences-side prerequisite for
                 // IsConvergentSequence

namespace dedekind::sequences {

// ===========================================================================
// Sequence-side convergence concepts (#719 Slice 0).
//
// Re-homed from @c :morphologies::archimedean per the textbook
// factoring discussed in the design review for #719: sequence-side
// properties (@c IsCauchySequence, @c IsConvergentSequence) live in
// the @c :sequences partition next to the sequence-trait machinery
// they consume; carrier-side prerequisites stay where they live in
// the wider library — @c IsArchimedean in @c :order::completeness,
// @c IsArchimedeanField / @c IsDedekindCompleteField in
// @c :morphologies::archimedean.  The @c CauchyPath<T> @em carrier
// (a tagged @c Path<T> alias) stays in @c :morphologies::archimedean
// too — it is a concrete derived struct, not a concept.
//
// Form-chain rows 5 and 6 of the sequence categorification (#719).
// Carrier-side prerequisite @c order::IsArchimedean<Codomain<Seq>>
// is the engineer's honesty obligation (the concept body pins only
// the operational shape; metric-completeness lives on the carrier).
// ===========================================================================

/**
 * @concept IsCauchySequence
 * @brief A @c Seq is a @b Cauchy @b sequence: its tail is eventually
 *        arbitrarily small under the metric on the carrier.
 *
 * @details Structurally, the concept pins:
 *          (i)  @c Seq satisfies @c IsSequence,
 *          (ii) @c s.at(0) returns @c Seq::Codomain,
 *          (iii) @c Codomain supports subtraction (@c a @c - @c a is
 *               well-formed), the minimal arithmetic needed to write
 *               down @c |s(m)-s(n)|.
 *
 *          The earlier formulation required @c std::abs(diff) to be
 *          callable, which excluded user-defined carriers that don't
 *          overload @c std::abs.  The current body keeps the
 *          subtraction shape (permissive: user-defined carriers with
 *          @c operator- participate) and lifts the absolute-value
 *          metric reasoning into the engineer's honesty obligation,
 *          alongside the full Cauchy criterion
 *          (∀ε>0 ∃N ∀m,n≥N. |s(m)−s(n)|<ε).
 *
 *          Anchor: Bishop, @em Foundations @em of @em Constructive
 *          @em Analysis §2.10.  Form-chain row 5 of #719.
 */
export template <typename Seq>
concept IsCauchySequence = IsSequence<Seq> && requires(Seq s) {
  { s.at(0) } -> std::same_as<typename Seq::Codomain>;
  { s.at(0) - s.at(0) };
};

/**
 * @concept IsConvergentSequence
 * @brief A Cauchy sequence whose limit can be witnessed via
 *        @c limit(s).
 *
 * @details Strict refinement of @c IsCauchySequence by the existence
 *          of a @c limit(s) witness.  Extracting @c limit(s) from a
 *          @c Cauchy sequence is the LEM-flavoured collapse step
 *          (Specker's recursive counterexample under
 *          @c ConstructiveLogic; classical under @c ClassicalLogic).
 *          The carrier-axis cardinality cut determines whether the
 *          collapse fires honestly — see #719 Slice 5.
 *
 *          Anchor: Bishop §2.18.  Form-chain row 6 of #719.
 */
export template <typename Seq>
concept IsConvergentSequence = IsCauchySequence<Seq> && requires(Seq s) {
  { limit(s) } -> std::same_as<typename Seq::Codomain>;
};

// ===========================================================================
// Sequence-shape concepts (#719 Slice 1).
//
// Boundedness, monotonicity, periodicity, absorptivity, and the
// subsequence relation are @b properties @b of @b the @b value (which
// specific sequence), not of the carrier type — a @c Path<double> can
// be bounded or unbounded, monotone or oscillating.  C++ concepts gate
// on types, so each property is pinned by an opt-in trait the carrier
// (or a tagged carrier wrapper) registers when it @b guarantees the
// property.  Same honesty discipline as @c :morphism's
// @c is_epic_arrow_v and the relation traits in @c :cartesian: the
// concept names the structural role; the witness opts in.
//
// Periodic / absorptive are the dual compression readings surfaced in
// the #719 design review: periodic compresses the index
// (@c ℕ ↠ ℤ/Nℤ); absorptive compresses the codomain (eventually
// constant; @c ℕ → @c {z}).  Both lift the carrier-axis vocabulary
// (@c is_periodic_v / @c is_absorptive_v in @c :species) onto the
// sequence axis.
//
// Anchors: Bishop §2 (bounded / monotone); Bolzano-Weierstrass
// (subsequence, feeds #719 Slice 5).  Form-chain rows 5a–5e of #719.
// ===========================================================================

/** @brief Opt-in: @c Seq is order-bounded (image bounded above and
 *         below).  The bound witnesses are the engineer's obligation. */
export template <typename Seq>
inline constexpr bool is_bounded_sequence_v = false;

/** @concept IsBoundedSequence
 *  @brief A sequence whose image is order-bounded.  Bishop §2. */
export template <typename Seq>
concept IsBoundedSequence = IsSequence<Seq> && is_bounded_sequence_v<Seq>;

/** @brief Opt-in: @c Seq is monotone (@c s(n) ≤ s(n+1) for all n, or
 *         the reverse).  Direction is the witness's concern. */
export template <typename Seq>
inline constexpr bool is_monotone_sequence_v = false;

/** @concept IsMonotoneSequence
 *  @brief A monotone sequence.  Bishop §2; together with
 *         @c IsBoundedSequence drives the monotone-convergence upgrade
 *         (LEM-gated, #719 Slice 5). */
export template <typename Seq>
concept IsMonotoneSequence = IsSequence<Seq> && is_monotone_sequence_v<Seq>;

/** @brief Opt-in: @c Seq is periodic with period @c N
 *         (@c s(n) @c = @c s(n+N) for all n).  Index-side compression:
 *         the sequence factors through @c ℕ/Nℤ.  Sequence-axis analog
 *         of @c :species's @c is_periodic_v<T, Op>. */
export template <typename Seq, std::size_t N>
inline constexpr bool is_periodic_sequence_v = false;

/** @concept IsPeriodicSequence
 *  @brief A period-@c N sequence.  Honest-Rejection foil for the
 *         monotone / Cauchy rows (a non-constant periodic sequence is
 *         bounded but neither monotone nor Cauchy). */
export template <typename Seq, std::size_t N>
concept IsPeriodicSequence = IsSequence<Seq> && is_periodic_sequence_v<Seq, N>;

/** @brief Opt-in: @c Seq is absorptive (eventually constant —
 *         @c ∃N, z. ∀n≥N. s(n) = z).  Codomain-side compression: the
 *         tail factors through a singleton @c {z}.  Dual of
 *         @c is_periodic_sequence_v; sequence-axis analog of
 *         @c :species's @c is_absorptive_v. */
export template <typename Seq>
inline constexpr bool is_absorptive_sequence_v = false;

/** @concept IsAbsorptiveSequence
 *  @brief An eventually-constant sequence.  The simplest convergent
 *         case (limit = the absorbing value); period-1-eventually
 *         dual to @c IsPeriodicSequence. */
export template <typename Seq>
concept IsAbsorptiveSequence = IsSequence<Seq> && is_absorptive_sequence_v<Seq>;

/** @brief Opt-in: @c Sub is a subsequence of @c Sup — there is a
 *         strictly increasing index map @c φ with
 *         @c Sub(n) @c = @c Sup(φ(n)).  The index map is the
 *         engineer's obligation. */
export template <typename Sub, typename Sup>
inline constexpr bool is_subsequence_v = false;

/** @concept IsSubsequence
 *  @brief @c Sub is a subsequence of @c Sup (same Codomain, strictly
 *         increasing index map).  Feeds the Bolzano-Weierstrass row
 *         (#719 Slice 5: every bounded sequence has a convergent
 *         subsequence). */
export template <typename Sub, typename Sup>
concept IsSubsequence =
    IsSequence<Sub> && IsSequence<Sup> &&
    std::same_as<typename Sub::Codomain, typename Sup::Codomain> &&
    is_subsequence_v<Sub, Sup>;

}  // namespace dedekind::sequences

namespace dedekind::sequences {

namespace detail {

constexpr auto natural_indices(std::size_t count) {
  return iterate(std::size_t{0}, [](std::size_t n) { return n + 1; }, count);
}

template <std::floating_point R>
constexpr R euclidean_distance(R a, R b) {
  return std::abs(a - b);
}

template <std::floating_point R>
constexpr R metric_norm(R point, R origin = static_cast<R>(0)) {
  return euclidean_distance(point, origin);
}

template <std::floating_point R>
constexpr bool in_closed_euclidean_ball(R center, R point, R radius) {
  return euclidean_distance(point, center) <= radius;
}

}  // namespace detail

/**
 * @brief Build a geometric-series term path: a_n = r^n.
 */
export template <std::floating_point R>
constexpr Path<R> geometric_series_terms(R ratio) {
  return Path<R>{
      [ratio](std::size_t n) { return std::pow(ratio, static_cast<R>(n)); }};
}

/**
 * @brief Build a p-series term path: a_n = 1/(n+1)^p.
 */
export template <std::floating_point R>
constexpr Path<R> p_series_terms(R p) {
  return Path<R>{[p](std::size_t n) {
    return static_cast<R>(1) / std::pow(static_cast<R>(n + 1), p);
  }};
}

/**
 * @brief Convergence by Cauchy-tail check on sequence values.
 */
export template <std::floating_point R>
constexpr bool converges_sequence_cauchy(const Path<R>& sequence,
                                         R epsilon = static_cast<R>(1e-6),
                                         std::size_t tail_start = 5000,
                                         std::size_t tail_span = 64) {
  const auto tail = prefix(drop(sequence, tail_start), tail_span);
  const auto ixs = detail::natural_indices(tail_span);

  return count_if(ixs, [&](std::size_t i) {
           const R a = tail.at(i);
           const auto js = detail::natural_indices(tail_span - (i + 1));
           return count_if(js, [&](std::size_t j_offset) {
                    const std::size_t j = i + 1 + j_offset;
                    const R b = tail.at(j);
                    // Cauchy criterion: all sampled tail values must lie in
                    // the same closed epsilon-ball under the Euclidean metric.
                    return !detail::in_closed_euclidean_ball(a, b, epsilon);
                  }) == 0;
         }) == tail_span;
}

/**
 * @brief Convergence of a series by Cauchy-tail check on partial sums.
 *
 * @details Optimized to cache partial sums once (avoiding O(n^2)
 * recomputation). Computes running sum up to warmup + tail_span - 1, stores
 * tail partial sums in a vector, then validates Cauchy criterion on the tail.
 */
export template <std::floating_point R>
constexpr bool converges_series_partial_sums(const Path<R>& terms,
                                             R epsilon = static_cast<R>(1e-5),
                                             std::size_t warmup = 4096,
                                             std::size_t tail_span = 128) {
  // Pre-compute partial sums for tail window using a single pass
  std::vector<R> tail_sums(tail_span);
  R running_sum = static_cast<R>(0);

  for (std::size_t n = 0; n <= warmup + tail_span - 1; ++n) {
    running_sum += terms.at(n);
    if (n >= warmup) {
      tail_sums[n - warmup] = running_sum;
    }
  }

  // Check Cauchy criterion on cached tail partial sums as finite-path
  // quantification over index sequences.
  const auto tail = from_range(tail_sums);
  const auto ixs = detail::natural_indices(tail_span);

  return count_if(ixs, [&](std::size_t i) {
           const R si = tail.at(i);
           const auto js = detail::natural_indices(tail_span - (i + 1));
           return count_if(js, [&](std::size_t j_offset) {
                    const std::size_t j = i + 1 + j_offset;
                    const R sj = tail.at(j);
                    // Cauchy-tail validation phrased as epsilon-ball
                    // membership.
                    return !detail::in_closed_euclidean_ball(si, sj, epsilon);
                  }) == 0;
         }) == tail_span;
}

/**
 * @brief Standard ratio-test hook for non-zero terms.
 * @details Returns true when the sampled tail ratio is strictly below 1.
 */
export template <std::floating_point R>
constexpr bool ratio_test_converges(const Path<R>& terms,
                                    std::size_t tail_start = 4000,
                                    std::size_t samples = 128,
                                    R margin = static_cast<R>(1e-3)) {
  R worst_ratio = static_cast<R>(0);
  for (std::size_t i = 0; i < samples; ++i) {
    const R an = detail::metric_norm(terms.at(tail_start + i));
    const R an1 = detail::metric_norm(terms.at(tail_start + i + 1));
    if (an <= static_cast<R>(1e-14)) {
      continue;
    }

    const R ratio = an1 / an;
    if (ratio > worst_ratio) {
      worst_ratio = ratio;
    }
  }

  return worst_ratio < (static_cast<R>(1) - margin);
}

/**
 * @brief Standard root-test hook on absolute term magnitudes.
 * @details Returns true when sampled n-th roots are strictly below 1.
 */
export template <std::floating_point R>
constexpr bool root_test_converges(const Path<R>& terms,
                                   std::size_t tail_start = 4000,
                                   std::size_t samples = 128,
                                   R margin = static_cast<R>(1e-3)) {
  R worst_root = static_cast<R>(0);
  for (std::size_t i = 0; i < samples; ++i) {
    const std::size_t n = tail_start + i + 1;
    const R an = detail::metric_norm(terms.at(n));
    if (an <= static_cast<R>(1e-14)) {
      continue;
    }

    const R root = std::pow(an, static_cast<R>(1) / static_cast<R>(n));
    if (root > worst_root) {
      worst_root = root;
    }
  }

  return worst_root < (static_cast<R>(1) - margin);
}

/**
 * @brief Standard comparison-test hook for non-negative terms.
 */
export template <std::floating_point R>
constexpr bool comparison_test_converges(const Path<R>& candidate,
                                         const Path<R>& upper_bound,
                                         std::size_t check_terms = 6000,
                                         R epsilon = static_cast<R>(1e-5)) {
  const auto ixs = detail::natural_indices(check_terms);
  const bool admissible =
      count_if(ixs, [&](std::size_t n) {
        const R a = candidate.at(n);
        const R b = upper_bound.at(n);
        return (a < static_cast<R>(0) || b < static_cast<R>(0) || a > b);
      }) == 0;

  if (!admissible) {
    return false;
  }

  return converges_series_partial_sums(upper_bound, epsilon);
}

}  // namespace dedekind::sequences
