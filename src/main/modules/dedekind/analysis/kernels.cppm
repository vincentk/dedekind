/**
 * @file dedekind/analysis/kernels.cppm
 * @partition :kernels
 * @brief Level 11.4: Kernel primitives -- Gaussian and reproducing kernels.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Description
 * This partition currently provides a compact kernel toolkit used to anchor
 * analysis terminology in code:
 *
 * - GaussianKernel<T>     -- symmetric radial kernel on a scalar carrier.
 * - ReproducingKernel<S>  -- unary representer view over a domain species.
 * - IsKernel              -- concept witness for binary kernels.
 *
 * @quote
 * "Die Theorie reproduzierender Kerne verbindet Geometrie und Rechnung auf
 *  eine Weise, die den Operator erst wirklich sichtbar macht."
 * ("The theory of reproducing kernels links geometry and computation in a way
 *  that makes the operator truly visible.")
 * -- Stefan Bergman, paraphrase
 */
module;

#include <cmath>
#include <concepts>

export module dedekind.analysis:kernels;

import dedekind.sequences;

namespace dedekind::analysis {

export template <typename T = double>
struct GaussianKernel {
  using value_type = T;
  using Domain = T;
  using Codomain = T;

  T sigma = static_cast<T>(1);

  constexpr T operator()(T a, T b) const noexcept {
    const T diff = a - b;
    return std::exp(-(diff * diff) / (static_cast<T>(2) * sigma * sigma));
  }

  constexpr T at(std::size_t n) const noexcept {
    return (*this)(static_cast<T>(0), static_cast<T>(n));
  }

  // Unary representer view: x |-> K(0, x)
  constexpr T operator()(T x) const noexcept {
    return (*this)(static_cast<T>(0), x);
  }

  constexpr T operator[](T x) const noexcept {
    return (*this)(static_cast<T>(0), x);
  }
};

export template <typename K, typename D, typename T>
concept IsKernel = requires(K k, D a, D b) {
  { k(a, b) } -> std::convertible_to<T>;
  { k(b, a) } -> std::convertible_to<T>;
};

export template <typename DomainSet, typename T = double>
struct ReproducingKernel {
  using value_type = T;
  using Domain = DomainSet;

  DomainSet domain_v{};
  value_type y_point{};

  constexpr T at(const typename DomainSet::Domain& x) const {
    const T d = static_cast<T>(x) - y_point;
    return std::exp(-(d * d));
  }

  constexpr T operator()(const typename DomainSet::Domain& x) const {
    return at(x);
  }

  constexpr T operator[](const typename DomainSet::Domain& x) const {
    return at(x);
  }
};

}  // namespace dedekind::analysis
