/** @file dedekind/algebra/universal_test.cpp
 *
 * Read-side anchor for the universal-algebra (A, F) meta-pattern
 * introduced in @c dedekind.algebra:universal.  Mirrors the partition's
 * own @c static_assert witnesses into Catch2 @c STATIC_CHECK so the
 * universal-algebra story shows up in the test report alongside the
 * specific algebraic concepts (@c f2_test, @c boolean_test,
 * @c ring_test, ...).
 *
 * The exercises here are deliberately at the @b closure tier:
 *   - @c IsAlgebra<T, Ops...> requires only that T is regular and each
 *     Op closes on T.  Axioms (associativity, commutativity, etc.)
 *     live in specific refinements --- those are exercised in the
 *     existing per-concept tests.
 *
 * Idiomatic patterns demonstrated here:
 *   1. Closure-tier witness on canonical primitive carriers.
 *   2. @b Same @b underlying @b set, @b two @b distinct (A, F) tuples ---
 *      bool realises both a Boolean lattice (∨, ∧) and the Galois
 *      field 𝔽₂ (⊕, ∧); same carrier, different operation tuples,
 *      different concept witnesses.  This is the textbook
 *      Burris--Sankappanavar §I.1 reading made type-checked.
 *   3. The (A, F) pattern survives composition: @c IsAlgebra fires on
 *      composite carriers (e.g.\ @c Rational<...>) the same way it
 *      fires on primitives.
 *
 * @see dedekind/algebra/universal.cppm — the partition-side definition.
 * @see dedekind/algebra/f2_test.cpp — value-level Cayley-table
 *      exercise of bool-as-𝔽₂; complementary to case 2 below.
 * @see Burris & Sankappanavar 1981, A Course in Universal Algebra,
 *      Springer GTM 78, §I.1.
 */

#include <catch2/catch_test_macros.hpp>
#include <functional>

import dedekind.algebra;

using dedekind::algebra::IsAlgebra;
using dedekind::algebra::IsBinaryOpOn;
using dedekind::algebra::IsUnaryOpOn;

TEST_CASE(
    "Universal algebra: closure-tier witness on canonical primitive carriers",
    "[algebra][universal][closure][primitive]") {
  // int with (+, *) closes the (A, F) pattern.  No axioms claimed here ---
  // signed-overflow UB lives one tier deeper in the IsRing rejection.
  STATIC_CHECK(IsAlgebra<int, std::plus<int>, std::multiplies<int>>);

  // unsigned int with (+, *) closes (A, F) under modular wrap.
  STATIC_CHECK(IsAlgebra<unsigned int, std::plus<unsigned int>,
                         std::multiplies<unsigned int>>);

  // Helper concepts (operation-shape predicates) fire on the building blocks.
  STATIC_CHECK(IsBinaryOpOn<int, std::plus<int>>);
  STATIC_CHECK(IsBinaryOpOn<int, std::multiplies<int>>);
  STATIC_CHECK(IsUnaryOpOn<int, std::negate<int>>);
}

TEST_CASE("Universal algebra: same underlying set, two distinct (A, F) tuples",
          "[algebra][universal][carrier][bool]") {
  // The underlying set is bool in both cases; the difference is the
  // operation tuple F.  Burris-Sankappanavar §I.1 names this kind of
  // pair an "algebra (A, F)", with A the carrier and F the family of
  // operations.  Same A, different F → different algebras.

  // Reading 1: bool as the Boolean lattice under (∨, ∧).
  STATIC_CHECK(IsAlgebra<bool, std::logical_or<bool>, std::logical_and<bool>>);

  // Reading 2: bool as the Galois field 𝔽₂ under (⊕, ∧).
  STATIC_CHECK(IsAlgebra<bool, std::bit_xor<bool>, std::bit_and<bool>>);

  // Both fire on the same underlying set; the operation tuple is the
  // disambiguator.  See dedekind/algebra/f2_test.cpp for the value-
  // level Cayley-table exercise of the second reading.
}

// Note: a composite-carrier exhibit (e.g.\ @c IsAlgebra<Rational<...>,
// +, *>) belongs in a downstream test file because @c dedekind.numbers
// sits below @c dedekind.algebra in the module DAG.  See
// @c linear_algebra/embeddings_test.cpp for the downstream lattice-
// corners exhibit pinning Rational, Complex, and Dual carriers
// against the (A, F) pattern.
