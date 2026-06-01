/**
 * @file
 * src/test/cpp/modules/dedekind/python/showcase_12_lp_unimodular_fast_path.cpp
 * @brief Showcase 12 — Compile-time LP via the bit-ops fast path: optimum
 *        as a typed constant on a signed-unimodular axis-aligned pack.
 *
 *   maximize x + y
 *   subject to  x  ≤ 1    (U1)
 *               y  ≤ 1    (U2)
 *              -x  ≤ 0    (U3:  x ≥ 0)
 *                -y ≤ 0   (U4:  y ≥ 0)
 *   ⇒ optimum = Vec2<Rat, 1, 1>, objective value = 2.
 *
 * @section Description
 * The pack @c {U1, U2, U3, U4} is both signed-unimodular and axis-aligned
 * (every halfspace has @c (coeff_x, coeff_y) ∈ {−1, 0, +1}² with one
 * coefficient zero), so the NTTP entry @c maximize<>() dispatches to
 * @c detail::maximize_impl_axis_aligned —  the bit-ops fast path —
 * rather than the generic Cramer fold used by @c showcase_09.  The
 * compile-time fold reduces the whole reduction to a typed constant
 * @c Vec2<Rat, Rat{1}, Rat{1}>; the witness functions below should
 * compile to @c ret @c i64 @c 1 at @c -O2.
 *
 * Companion: @c showcase_12b (runtime call site of the same fast-path
 * kernel), which exhibits the bit-ops IR without compile-time folding.
 *
 * Triptych position: middle panel of the §5 triptych (fast path);
 * @c showcase_09 holds the generic Cramer panel.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */

#include <concepts>
#include <cstdint>

import dedekind.linear_algebra;
import dedekind.numbers;
import dedekind.optimization;
import dedekind.sets;

using namespace dedekind::linear_algebra;
using namespace dedekind::numbers;
using namespace dedekind::optimization;
using dedekind::sets::SignedExtensionalCardinal;

using Rat = Rational<SignedExtensionalCardinal<>>;

// The unit-square pack: signed-unimodular + axis-aligned.
using U1 = Halfspace2D<Rat, Rat{1}, Rat{0}, Rat{1}>;   //  x ≤ 1
using U2 = Halfspace2D<Rat, Rat{0}, Rat{1}, Rat{1}>;   //  y ≤ 1
using U3 = Halfspace2D<Rat, Rat{-1}, Rat{0}, Rat{0}>;  //  x ≥ 0
using U4 = Halfspace2D<Rat, Rat{0}, Rat{-1}, Rat{0}>;  //  y ≥ 0

// Construct G ⊆ F as the meet of halfspace Sets, U : F → ℚ as the
// linear functional, and apply argmax(G, U) — the canonical Set DSL
// combinator.
constexpr auto G = halfspace_set(U1{}) & halfspace_set(U2{}) &
                   halfspace_set(U3{}) & halfspace_set(U4{});
constexpr LinearFunctional<Rat, Rat{1}, Rat{1}> Uf{};
using OptimumSet = std::remove_cvref_t<decltype(argmax(G, Uf))>;
using OptimumPred =
    std::remove_cvref_t<decltype(std::declval<OptimumSet>().predicate())>;

// Type-level witnesses: the fast-path optimum Set's predicate IS the
// Singleton2DPredicate carrying (1, 1).
static_assert(
    std::same_as<OptimumPred, Singleton2DPredicate<Rat, Rat{1}, Rat{1}>>);
static_assert(OptimumPred::coord_x == Rat{1});
static_assert(OptimumPred::coord_y == Rat{1});

/**
 * @brief Showcase 12a: the fast-path optimum's x-coordinate as a
 *        compile-time literal in the emitted IR.
 *
 * Expected IR: @c ret @c i64 @c 1 — the bit-ops fast-path reduction
 * folds entirely at translation time when the inputs are constexpr.
 */
extern "C" __attribute__((noinline)) int64_t witness_lp_fast_optimum_x() {
  return static_cast<int64_t>(OptimumPred::coord_x.num());
}

/** @brief Showcase 12b: the fast-path optimum's y-coordinate.
 *
 * Expected IR: @c ret @c i64 @c 1.
 */
extern "C" __attribute__((noinline)) int64_t witness_lp_fast_optimum_y() {
  return static_cast<int64_t>(OptimumPred::coord_y.num());
}
