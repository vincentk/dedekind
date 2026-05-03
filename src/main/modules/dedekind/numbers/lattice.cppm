/**
 * @file dedekind/numbers/lattice.cppm
 * @partition :lattice
 * @brief Integer / Gaussian lattices in ℝ, ℂ, ℝⁿ, ℂⁿ.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "Wir mussen wissen, wir werden wissen."
 *       ("We must know, we will know.")
 *       -- David Hilbert (1930)
 */

module;

#include <array>
#include <cmath>
#include <cstddef>
#include <limits>
#include <utility>

/**
 * @section numbers_lattice__Description
 * This partition provides the user-facing lattice factory:
 *
 *   auto c = lattice<C>;         // Gaussian-integer lattice in ℂ
 *   auto r = lattice<R>;         // Integer lattice in ℝ
 *   auto x = lattice<R, 3>;      // Integer lattice in ℝ^3
 *   auto y = lattice<C, 3>;      // Gaussian-integer lattice in ℂ^3
 *
 * and bounded variants:
 *
 *   auto grid_c = lattice<C>.bounded(size);
 *   auto grid_r = lattice<R>.bounded(size);
 *
 * `lattice<C>.bounded(n)` yields points x+iy with x,y in {0,...,n-1}.
 * `lattice<R>.bounded(n)` yields points x in {0,...,n-1} embedded in ℝ.
 */

export module dedekind.numbers:lattice;

import dedekind.category;
import dedekind.sets;
import dedekind.geometry;
import :real;
import :complex;

namespace dedekind::numbers {

using namespace dedekind::category;
using namespace dedekind::sets;

namespace detail {
constexpr bool is_integral_coordinate(double x) {
  constexpr double lo = static_cast<double>(std::numeric_limits<int>::min());
  constexpr double hi = static_cast<double>(std::numeric_limits<int>::max());
  if ((x < lo) || (x > hi)) return false;
  return std::trunc(x) == x;
}
}  // namespace detail

/**
 * @brief Primary template for lattice factory values.
 *
 * @tparam AmbientSet A canonical ambient set value (e.g. C, R).
 */
export template <auto AmbientSet, std::size_t N = 1>
struct LatticeFactory;

/**
 * @brief Lattice factory specialization for ℂ.
 *
 * `lattice<C>` denotes the Gaussian-integer lattice ℤ[i] ⊂ ℂ.
 * `lattice<C>(n)` denotes the bounded square lattice
 * {x+iy | x,y in {0,...,n-1}}.
 */
template <>
struct LatticeFactory<C, 1> {
  using Domain = typename ℂ::Domain;
  using Codomain = typename ℂ::Codomain;
  using logic_species = typename ℂ::logic_species;
  using cardinality_type = typename ℂ::cardinality_type;

  constexpr Codomain operator()(const Domain& z) const {
    return detail::is_integral_coordinate(z.real()) &&
                   detail::is_integral_coordinate(z.imag())
               ? logic_species::True
               : logic_species::False;
  }

  constexpr auto bounded(int n) const { return complex_lattice(n); }
};

/**
 * @brief Lattice factory specialization for ℂ^N, N > 1.
 */
template <std::size_t N>
  requires(N > 1)
struct LatticeFactory<C, N> {
  using Domain = std::array<Complex<double>, N>;
  using Codomain = bool;
  using logic_species = ClassicalLogic;
  using cardinality_type = typename ℂ::cardinality_type;

  constexpr Codomain operator()(const Domain& xs) const {
    for (const auto& z : xs) {
      if (!detail::is_integral_coordinate(z.real()) ||
          !detail::is_integral_coordinate(z.imag()))
        return logic_species::False;
    }
    return logic_species::True;
  }

  constexpr auto bounded(int n) const {
    auto pred = [n](const Domain& xs) {
      for (const auto& z : xs) {
        const double re = z.real();
        const double im = z.imag();
        if (!detail::is_integral_coordinate(re) ||
            !detail::is_integral_coordinate(im))
          return false;
        if ((re < 0.0) || (re >= static_cast<double>(n)) || (im < 0.0) ||
            (im >= static_cast<double>(n)))
          return false;
      }
      return true;
    };
    return Set<Domain, ClassicalLogic, decltype(pred)>{pred};
  }
};

/**
 * @brief Lattice factory specialization for ℝ.
 *
 * `lattice<R>` denotes integer points embedded in ℝ.
 * `lattice<R>(n)` denotes the bounded lattice
 * {x in ℝ | x in {0,...,n-1}}.
 */
template <>
struct LatticeFactory<R, 1> {
  using Domain = typename ℝ::Domain;
  using Codomain = typename ℝ::Codomain;
  using logic_species = typename ℝ::logic_species;
  using cardinality_type = typename ℝ::cardinality_type;

  constexpr Codomain operator()(const Domain& x) const {
    return detail::is_integral_coordinate(x.resolve()) ? logic_species::True
                                                       : logic_species::False;
  }

  constexpr auto bounded(int n) const {
    // FIXME(#399 slice 4-6): once ℝ becomes a carrier alias, switch
    // to @c element<Ω<ℝ>>; for now ℝ is still the predicate-set type.
    auto r = element<Ω<Real<double>>>;
    return Set{r | [n](const Real<double>& x) {
      const double v = x.resolve();
      if (!detail::is_integral_coordinate(v)) return false;
      return (v >= 0.0) && (v < static_cast<double>(n));
    }};
  }
};

/**
 * @brief Lattice factory specialization for ℝ^N, N > 1.
 */
template <std::size_t N>
  requires(N > 1)
struct LatticeFactory<R, N> {
  using Domain = std::array<Real<double>, N>;
  using Codomain = bool;
  using logic_species = ClassicalLogic;
  using cardinality_type = typename ℝ::cardinality_type;

  constexpr Codomain operator()(const Domain& xs) const {
    for (const auto& x : xs) {
      if (!detail::is_integral_coordinate(x.resolve()))
        return logic_species::False;
    }
    return logic_species::True;
  }

  constexpr auto bounded(int n) const {
    auto pred = [n](const Domain& xs) {
      for (const auto& x : xs) {
        const double v = x.resolve();
        if (!detail::is_integral_coordinate(v)) return false;
        if ((v < 0.0) || (v >= static_cast<double>(n))) return false;
      }
      return true;
    };
    return Set<Domain, ClassicalLogic, decltype(pred)>{pred};
  }
};

/**
 * @brief First-class lattice value: `lattice<C>` and `lattice<R>`.
 */
export template <auto AmbientSet, std::size_t N = 1>
inline constexpr LatticeFactory<AmbientSet, N> lattice{};

}  // namespace dedekind::numbers
