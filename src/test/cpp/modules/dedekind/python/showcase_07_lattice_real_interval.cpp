/**
 * @file
 * src/test/cpp/modules/dedekind/python/showcase_07_lattice_real_interval.cpp
 * @brief Showcase 7 — Integer lattice ∩ real-valued interval.
 *
 *   ℤ  ∩  { x ∈ ℝ | -21.0 < x ≤ 21.0 }   =   (integer lattice ∩ (-21, 21])
 *                                         =   {-20, -19, …, 21}   (|·| = 42)
 *
 * The carrier is ℤ but the bounds are real-valued NTTPs. Different types on
 * pivot and carrier exercise the `auto Pivot` / `convertible_to` machinery
 * the DSL was extended with, and the cardinality is still computed at
 * compile time from the bounds and strictness pair.
 *
 * Expected LLVM IR: `ret i1 true` — inhabitant 0 is a member.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */

#include <concepts>

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

// Integer-valued variable, real-valued bounds.  This showcase
// deliberately uses the machine-int carrier rather than the canonical
// exact ℤ carrier (@c SignedExtensionalCardinal<>): on machine int,
// the int↔double standard arithmetic promotion is what lets the
// real-valued bound compare with the integer variable.  The exact
// carrier intentionally does not silently narrow to double; real-bound
// support against @c SignedExtensionalCardinal<> needs a
// non-narrowing SEC<>↔real comparison arrow (deferred follow-up to
// #399 slice 3 / #551).
//
// @c IntsOnInt is the int-Domain universal predicate, defined locally
// because the canonical @c IntegersOf<> now carries @c Domain @c =
// @c SignedExtensionalCardinal<> (the exact ℤ carrier).  The redesign
// in #551 retires this kind of one-off naming under @c Ω<int>.
struct IntsOnInt {
  using Domain = int;
  constexpr bool operator()(int) const { return true; }
};
constexpr IntsOnInt Z_int{};
constexpr auto n = var<int>;

constexpr auto above = Set{n % Z_int | (n > bound<-21.0>)};
constexpr auto at_most = Set{n % Z_int | (n <= bound<21.0>)};

// Meet: integer lattice inside a real interval.
constexpr auto lattice_cut = above & at_most;
using Iv = std::decay_t<decltype(lattice_cut)>;

// Type-level bounds and strictness reported back from the DSL — with the
// real-valued pivots preserved exactly.
static_assert(Iv::lower_pivot == -21.0);
static_assert(Iv::upper_pivot == 21.0);
static_assert(Iv::lower_strictness == Strictness::Strict);
static_assert(Iv::upper_strictness == Strictness::NonStrict);

// Cardinality falls out of the compile-time formula, independently of the
// pivot carrier type (here `double`), by leaning on the integer carrier `T`.
static_assert(lattice_cut.size() == 42u);

// Computability classification matches showcase_06: finite, decidable,
// but not compile-time-enumerable (the 42 elements aren't NTTPs).
static_assert(HasDecidableMembership<decltype(lattice_cut)>);
static_assert(IsFiniteSet<decltype(lattice_cut)>);
static_assert(!IsCompileTimeEnumerable<decltype(lattice_cut)>);

/**
 * @brief Showcase 7: integer lattice ∩ real interval, 42 elements.
 *
 * Expected IR: `ret i1 true` — inhabitant 0 is a member.
 */
extern "C" __attribute__((noinline)) bool witness_lattice_real_interval() {
  using Logic = typename decltype(lattice_cut)::logic_species;
  return lattice_cut(0) == Logic::True;
}
