/**
 * @file
 * src/test/cpp/modules/dedekind/python/showcase_02_lattice_singleton.cpp
 * @brief Showcase 2 — Compile-time proof of a lattice/square singleton in ℝ².
 *
 * The natural-number lattice {0,…,3}² inside ℝ_d × ℝ_d intersected with the
 * closed square [0.5, 1.5] × [0.5, 1.5] contains exactly one point: (1, 1).
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
import dedekind.order;

using namespace dedekind::category;
using namespace dedekind::sets;
using namespace dedekind::algebra;
using namespace dedekind::numbers;
using namespace dedekind::order;

// The carrier is ℝ_d × ℝ_d, pairs of finite doubles 𝕃<double>.  The square is
// point-free.  The lattice keeps one named predicate: integrality has no
// point-free spelling.
constexpr auto R2 = ℝ_d * ℝ_d;
using R2Point = typename decltype(R2)::Domain;

constexpr auto on_small_natural_grid = [](const R2Point& p) {
  const double x = p.first.value();
  const double y = p.second.value();
  // Bounds first: the int casts below are only defined inside [0, 3].
  return x >= 0.0 && x <= 3.0 && y >= 0.0 && y <= 3.0 &&
         static_cast<double>(static_cast<int>(x)) == x &&
         static_cast<double>(static_cast<int>(y)) == y;
};
constexpr auto natural_lattice = Comprehension{R2, on_small_natural_grid};

constexpr auto unit_square = R2 | (π1 >= bound<0.5> && π1 <= bound<1.5> &&
                                   π2 >= bound<0.5> && π2 <= bound<1.5>);

// Intersection contains exactly (1, 1)
constexpr auto lattice_square_intersection = natural_lattice & unit_square;
using CLogic = typename decltype(lattice_square_intersection)::logic_species;

// Representative test points
constexpr R2Point c3{1.0, 1.0};        // (1, 1) → in intersection
constexpr R2Point c_left{0.0, 1.0};    // (0, 1) → outside square (x < 0.5)
constexpr R2Point c_bottom{1.0, 0.0};  // (1, 0) → outside square (y < 0.5)
constexpr R2Point c_diag{2.0, 2.0};    // (2, 2) → outside square

// Compile-time witnesses.
static_assert(lattice_square_intersection(c3) == CLogic::True);
static_assert(lattice_square_intersection(c_left) == CLogic::False);
static_assert(lattice_square_intersection(c_bottom) == CLogic::False);
static_assert(lattice_square_intersection(c_diag) == CLogic::False);

/**
 * @brief Showcase 2: singleton lattice/square intersection at (1, 1).
 *
 * Returns whether the full conjunction — c₃ in and the three other lattice
 * points out — holds.  The answer is statically true; the compiler should
 * constant-fold the body.
 *
 * Expected IR: `ret i1 true`
 */
extern "C" __attribute__((noinline)) bool witness_lattice_square_singleton() {
  return (lattice_square_intersection(c3) == CLogic::True) &&
         (lattice_square_intersection(c_left) == CLogic::False) &&
         (lattice_square_intersection(c_bottom) == CLogic::False) &&
         (lattice_square_intersection(c_diag) == CLogic::False);
}
