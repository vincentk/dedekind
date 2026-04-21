/**
 * @file src/test/cpp/modules/dedekind/python/pruning_noop_vs_runtime_fixture.cpp
 * @brief Compile-time pruning witness via the dedekind.sets intensional Set DSL.
 *
 * The library under test is dedekind.sets; in particular Set::operator& and
 * Set::operator| which compose predicates transparently so that the compiler
 * back-end can perform constant-folding across set operations.
 *
 * Two exported functions contrast the two paths:
 *
 *  1. pruning_compile_time_noop: both operands carry compile-time-constant
 *     predicates.  The intersection {false} ∩ {true} ≡ ∅ is visible to the
 *     optimiser, which collapses the body to a single `ret false`.
 *
 *  2. pruning_runtime_guard: the second operand arrives as a runtime function
 *     pointer (e.g. a Python-side callback).  The optimiser cannot eliminate
 *     the branch; the generated IR retains an indirect call.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */

import dedekind.category;
import dedekind.sets;
import dedekind.algebra;

using namespace dedekind::category;
using namespace dedekind::sets;
using namespace dedekind::algebra;

// Symbolic variable ranging over 𝔹 = Ω<bool, ClassicalLogic, Finite>
inline constexpr auto b = var<𝔹>;

// { x ∈ 𝔹 | x == false }  =  the singleton {false} ⊂ 𝔹
inline constexpr auto b_false = Set{b % B | (b == false)};

// { x ∈ 𝔹 | x == true }   =  the singleton {true}  ⊂ 𝔹
inline constexpr auto b_true  = Set{b % B | (b == true)};

// {false} and {true} partition 𝔹: their intersection is ∅ ...
static_assert(Ø<bool, ClassicalLogic>{} == (b_false & b_true));
static_assert((b_false & b_true)(false) == false);
static_assert((b_false & b_true)(true)  == false);

// ... and their union covers 𝔹 entirely.
static_assert(B == (b_false | b_true));
static_assert((b_false | b_true)(false) == true);
static_assert((b_false | b_true)(true)  == true);

/**
 * @brief Compile-time pruned path.
 *
 * Both b_false and b_true have statically-known predicates.  operator&
 * composes them inline; the optimiser reduces (x==false)&&(x==true) to the
 * constant false.
 *
 * Expected IR: a single `ret i1 false`.
 */
extern "C" __attribute__((noinline)) bool
pruning_compile_time_noop(bool x) {
  return (b_false & b_true)(x);
}

/**
 * @brief Runtime path -- optimiser cannot prune.
 *
 * The second predicate is a function pointer supplied at call-time
 * (e.g. via the Python bindings).  The optimiser must retain the call.
 *
 * Expected IR: an indirect call through the function pointer.
 */
extern "C" __attribute__((noinline)) bool
pruning_runtime_guard(bool x, bool (*runtime_pred)(bool)) {
  const auto dynamic =
      Set<bool, ClassicalLogic, bool (*)(bool)>{runtime_pred};
  return (b_false & dynamic)(x);
}
