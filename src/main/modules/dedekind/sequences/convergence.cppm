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

import :path;

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
