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

using R2 = QuadraticReal<2>;  // the exact real carrier ℝ = ℚ(√2)
using Q = Rational<>;         // for the exact rational thresholds ½, 1½

// A coordinate is a "small natural" iff it is one of 0,1,2,3 — on the EXACT
// carrier the "integral ∧ 0 ≤ · ≤ 3" test IS membership in {0,1,2,3}.
constexpr bool is_small_natural(const R2& t) {
  return t == R2{} || t == R2{1} || t == R2{2} || t == R2{3};
}

// Post-HSP retarget: ℂ is the coat-hanger Ω<Complex<QuadraticReal<2>>, ...>, so
// this showcase proves the singleton over EXACT ℚ(√2) arithmetic.  The scout
// stays element<ℂ> (now a Complex<QuadraticReal<2>> scout).
constexpr auto c = element<ℂ>;

// Lifted natural-number lattice: Gaussian integers with 0 ≤ Re, Im ≤ 3.
constexpr auto natural_lattice_in_c = Set{c | [](const Complex<R2>& z) {
  return is_small_natural(z.real()) && is_small_natural(z.imag());
}};

// Square region [½, 1½] × [½, 1½] inside ℂ.
constexpr auto square_c1_c2 = Set{c | [](const Complex<R2>& z) {
  return (z.real() >= R2{Q{1, 2}}) && (z.real() <= R2{Q{3, 2}}) &&
         (z.imag() >= R2{Q{1, 2}}) && (z.imag() <= R2{Q{3, 2}});
}};

// Intersection contains exactly c₃ = 1 + i
constexpr auto lattice_square_intersection =
    natural_lattice_in_c & square_c1_c2;
using CLogic = typename decltype(lattice_square_intersection)::logic_species;

// Representative test points
constexpr Complex<R2> c3{R2{1}, R2{1}};     // 1 + i  → in intersection
constexpr Complex<R2> c_left{R2{}, R2{1}};  // i      → outside square (Re < ½)
constexpr Complex<R2> c_bottom{R2{1},
                               R2{}};        // 1      → outside square (Im < ½)
constexpr Complex<R2> c_diag{R2{2}, R2{2}};  // 2 + 2i → outside square

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
extern "C" __attribute__((noinline)) bool witness_lattice_square_singleton() {
  return (lattice_square_intersection(c3) == CLogic::True) &&
         (lattice_square_intersection(c_left) == CLogic::False) &&
         (lattice_square_intersection(c_bottom) == CLogic::False) &&
         (lattice_square_intersection(c_diag) == CLogic::False);
}
