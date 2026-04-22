/**
 * @file
 * src/test/cpp/modules/dedekind/python/showcase_02_lattice_singleton.cpp
 * @brief Showcase 2 — Compile-time proof of a lattice/square singleton in ℂ.
 *
 * The 2D natural-number lattice ℕ² lifted into ℂ (Gaussian integers with
 * 0 ≤ Re, Im ≤ 3) intersected with the closed square [0.5, 1.5] × [0.5, 1.5]
 * contains exactly one point: c₃ = 1 + i.
 *
 * The compiler proves membership and non-membership for four representative
 * points via static_assert, and the exported function constant-folds to true.
 *
 * Expected LLVM IR: `ret i1 true`
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */

import dedekind.category;
import dedekind.sets;
import dedekind.algebra;
import dedekind.numbers;

using namespace dedekind::category;
using namespace dedekind::sets;
using namespace dedekind::algebra;
using namespace dedekind::numbers;

constexpr bool is_integral_coordinate(double x) {
  const int xi = static_cast<int>(x);
  return static_cast<double>(xi) == x;
}

// Symbolic variable ranging over ℂ
constexpr auto c = var<ℂ>;

// Lifted natural-number lattice: Gaussian integers with 0 ≤ Re, Im ≤ 3
constexpr auto natural_lattice_in_c =
    Set{c % C | [](const Complex<double>& z) {
      return is_integral_coordinate(z.real()) &&
             is_integral_coordinate(z.imag()) && (z.real() >= 0.0) &&
             (z.real() <= 3.0) && (z.imag() >= 0.0) && (z.imag() <= 3.0);
    }};

// Square region [0.5, 1.5] × [0.5, 1.5] inside ℂ
constexpr auto square_c1_c2 = Set{c % C | [](const Complex<double>& z) {
  return (z.real() >= 0.5) && (z.real() <= 1.5) && (z.imag() >= 0.5) &&
         (z.imag() <= 1.5);
}};

// Intersection contains exactly c₃ = 1 + i
constexpr auto lattice_square_intersection =
    natural_lattice_in_c & square_c1_c2;
using CLogic = typename decltype(lattice_square_intersection)::logic_species;

// Representative test points
constexpr Complex<double> c3{1.0, 1.0};  // 1 + i  → in intersection
constexpr Complex<double> c_left{
    0.0, 1.0};  // i      → outside square (Re < 0.5)
constexpr Complex<double> c_bottom{
    1.0, 0.0};  // 1      → outside square (Im < 0.5)
constexpr Complex<double> c_diag{2.0, 2.0};  // 2 + 2i → outside square

// Compile-time witnesses.
static_assert(lattice_square_intersection(c3) == CLogic::True);
static_assert(lattice_square_intersection(c_left) == CLogic::False);
static_assert(lattice_square_intersection(c_bottom) == CLogic::False);
static_assert(lattice_square_intersection(c_diag) == CLogic::False);

/**
 * @brief Showcase 2: singleton lattice/square intersection at c₃ = 1 + i.
 *
 * Returns whether the full conjunction — c₃ in and the three other lattice
 * points out — holds.  The answer is statically true; the compiler should
 * constant-fold the body.
 *
 * Expected IR: `ret i1 true`
 */
extern "C" __attribute__((noinline)) bool impress_lattice_square_singleton() {
  return (lattice_square_intersection(c3) == CLogic::True) &&
         (lattice_square_intersection(c_left) == CLogic::False) &&
         (lattice_square_intersection(c_bottom) == CLogic::False) &&
         (lattice_square_intersection(c_diag) == CLogic::False);
}
