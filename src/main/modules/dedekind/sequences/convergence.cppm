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

export module dedekind.sequences:convergence;

import :path;

namespace dedekind::sequences {

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
  for (std::size_t i = 0; i < tail_span; ++i) {
    const R a = sequence.at(tail_start + i);
    for (std::size_t j = i + 1; j < tail_span; ++j) {
      const R b = sequence.at(tail_start + j);
      if (std::abs(a - b) > epsilon) {
        return false;
      }
    }
  }
  return true;
}

/**
 * @brief Convergence of a series by Cauchy-tail check on partial sums.
 */
export template <std::floating_point R>
constexpr bool converges_series_partial_sums(const Path<R>& terms,
                                             R epsilon = static_cast<R>(1e-5),
                                             std::size_t warmup = 4096,
                                             std::size_t tail_span = 128) {
  for (std::size_t i = 0; i < tail_span; ++i) {
    R si = static_cast<R>(0);
    for (std::size_t n = 0; n <= warmup + i; ++n) {
      si += terms.at(n);
    }

    for (std::size_t j = i + 1; j < tail_span; ++j) {
      R sj = static_cast<R>(0);
      for (std::size_t n = 0; n <= warmup + j; ++n) {
        sj += terms.at(n);
      }

      if (std::abs(si - sj) > epsilon) {
        return false;
      }
    }
  }

  return true;
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
    const R an = std::abs(terms.at(tail_start + i));
    const R an1 = std::abs(terms.at(tail_start + i + 1));

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
    const R an = std::abs(terms.at(n));
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
  for (std::size_t n = 0; n < check_terms; ++n) {
    const R a = candidate.at(n);
    const R b = upper_bound.at(n);
    if (a < static_cast<R>(0) || b < static_cast<R>(0) || a > b) {
      return false;
    }
  }

  return converges_series_partial_sums(upper_bound, epsilon);
}

}  // namespace dedekind::sequences
