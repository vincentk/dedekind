/**
 * @file dedekind/numbers/lattice.cppm
 * @partition :lattice
 * @brief Integer lattices in ℝ_d and ℝ_dⁿ.
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
 *   auto r = lattice<ℝ_d>;         // Integer lattice in ℝ
 *   auto x = lattice<ℝ_d, 3>;      // Integer lattice in ℝ^3
 *
 * and the bounded variant:
 *
 *   auto grid_r = lattice<ℝ_d>.bounded(size);
 *
 * `lattice<ℝ_d>.bounded(n)` yields points x in {0,...,n-1} embedded in ℝ.
 */

export module dedekind.numbers:lattice;

import dedekind.category;
import dedekind.sets;
import dedekind.geometry;
import dedekind.morphologies; // 𝕃<double>, ℝ_d's carrier
import :real;

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
 * @tparam AmbientSet A canonical ambient set value (@c ℝ_d).
 */
export template <auto AmbientSet, std::size_t N = 1>
struct LatticeFactory;

/**
 * @brief Lattice factory specialization for ℝ.
 *
 * `lattice<ℝ_d>` denotes integer points embedded in ℝ_d.
 * `lattice<ℝ_d>.bounded(n)` denotes the bounded lattice
 * {x in ℝ | x in {0,...,n-1}}.
 */
template <>
struct LatticeFactory<ℝ_d, 1> {
  using Domain = dedekind::morphologies::𝕃<machine_real_scalar>;
  using Codomain = Boole::Ω;
  using logic_species = Boole;
  using cardinality_type = ℶ_1;

  constexpr Codomain operator()(const Domain& x) const {
    return detail::is_integral_coordinate(x.value()) ? logic_species::True
                                                     : logic_species::False;
  }

  constexpr auto bounded(int n) const {
    // This lattice specialisation computes on the finite doubles @c 𝕃<double>,
    // so it scouts the machine ambient @c ℝ_d --- not the abstract @c ℝ (the
    // coat-hanger over @c QuadraticReal<2>).
    // Runtime bound @c n and an integrality gate: not π-expressible, so a NAMED
    // local predicate over the ℝ_d base (comprehension form).
    const auto in_bounded_grid = [n](const Domain& x) {
      const double v = x.value();
      if (!detail::is_integral_coordinate(v)) return false;
      return (v >= 0.0) && (v < static_cast<double>(n));
    };
    return Comprehension{ℝ_d, in_bounded_grid};
  }
};

/**
 * @brief Lattice factory specialization for ℝ^N, N > 1.
 */
template <std::size_t N>
  requires(N > 1)
struct LatticeFactory<ℝ_d, N> {
  using Domain = std::array<dedekind::morphologies::𝕃<machine_real_scalar>, N>;
  using Codomain = bool;
  using logic_species = Boole;
  using cardinality_type = ℶ_1;

  constexpr Codomain operator()(const Domain& xs) const {
    for (const auto& x : xs) {
      if (!detail::is_integral_coordinate(x.value()))
        return logic_species::False;
    }
    return logic_species::True;
  }

  constexpr auto bounded(int n) const {
    auto pred = [n](const Domain& xs) {
      for (const auto& x : xs) {
        const double v = x.value();
        if (!detail::is_integral_coordinate(v)) return false;
        if ((v < 0.0) || (v >= static_cast<double>(n))) return false;
      }
      return true;
    };
    return Comprehension<𝔸<Domain, Boole>, decltype(pred)>{pred};
  }
};

/**
 * @brief First-class lattice value: `lattice<ℝ_d>`.
 */
export template <auto AmbientSet, std::size_t N = 1>
inline constexpr LatticeFactory<AmbientSet, N> lattice{};

}  // namespace dedekind::numbers
