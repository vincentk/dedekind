/**
 * @file
 * src/test/cpp/modules/dedekind/python/showcase_01_diagonal_contradiction.cpp
 * @brief Showcase 1 — Compile-time proof of an empty diagonal cut in R².
 *
 * On the diagonal x = y in R², the strip predicate (x > 5) ∧ (y < 3) is
 * contradictory: any point on the diagonal has x = y, so x > 5 forces y > 5,
 * which violates y < 3.  The intersection is provably empty, and the
 * exported function compiles to a single constant-false return.
 *
 * Expected LLVM IR: `ret i1 false`
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

// Ambient product set ℝ × ℝ and its symbolic scout.
constexpr auto R2 = R * R;
constexpr auto xy = var<decltype(R2)>;
using R2Point = typename decltype(R2)::Domain;

// Diagonal: { (x, y) ∈ ℝ² | x = y }
constexpr auto diagonal =
    Set{xy % R2 | [](R2Point p) { return p.first == p.second; }};

// Strip: { (x, y) ∈ ℝ² | x > 5 ∧ y < 3 }
constexpr auto strip = Set{
    xy % R2 | [](R2Point p) { return (p.first > 5.0) && (p.second < 3.0); }};

// Intersection is empty: no point lies on the diagonal AND in the strip.
constexpr auto empty_diagonal_cut = diagonal & strip;
using R2Logic = typename decltype(empty_diagonal_cut)::logic_species;

// Compile-time witnesses confirming emptiness at two representative points.
static_assert(empty_diagonal_cut(R2Point{6.0, 6.0}) == R2Logic::False);
static_assert(empty_diagonal_cut(R2Point{2.0, 2.0}) == R2Logic::False);

/**
 * @brief Showcase 1: contradiction on the diagonal in R².
 *
 * Returns whether the representative point (6, 6) — on the diagonal, inside
 * the strip by x > 5 but not by y < 3 — is a member.  The answer is
 * statically false; the compiler should constant-fold the body.
 *
 * Expected IR: `ret i1 false`
 */
extern "C" __attribute__((noinline)) bool impress_empty_diagonal_cut() {
  return empty_diagonal_cut(R2Point{6.0, 6.0}) == R2Logic::True;
}
