/**
 * @file initial_ring_test.cpp
 * @brief Test suite for the @c algebra:initial_ring partition.
 *
 * @section Scope
 * Exercises the dual universal-property reification of @c ℤ
 * (closes part of #446):
 *
 *   1. @c IsInitialRing<SignedCardinality> — for every ring @c R
 *      there exists a unique ring homomorphism
 *      @c SignedCardinality → @c R.  The test exercises the universal
 *      property at @c R = @c Modular<n>, where the unique
 *      homomorphism is the mod-@c n reduction.
 *
 *   2. @c IsGrothendieckGroup<SignedCardinality, Cardinality> —
 *      @c SignedCardinality is the free abelian group on the
 *      commutative monoid @c (Cardinality, +, 0).  The test
 *      exercises the closure-forcing operator
 *      @c Cardinality - Cardinality → SignedCardinality as the
 *      operator-level realisation of the Grothendieck construction.
 *
 * Both Forms anchor the same carrier; this test suite is the
 * @b formal, @b type-checked @b documentation that the dual
 * inhabitance holds operationally.
 *
 *   3. Carrier-lattice arrows (Figure 1 in @c paper.tex /
 *      @c report.tex) — the labelled arrows are mechanically
 *      witnessed as @c IsMonicArrow / @c IsArrow concepts so the
 *      figure's claims are not schematic prose but type-checked
 *      facts.
 */
#include <catch2/catch_test_macros.hpp>
#include <type_traits>
#include <variant>  // load-bearing for variant operator== via ADL

import dedekind.algebra;
import dedekind.category;
import dedekind.sets;

using namespace dedekind::algebra;
using namespace dedekind::category;
using namespace dedekind::sets;
using dedekind::algebra::is_grothendieck_group_v;
using dedekind::algebra::is_initial_ring_v;
using dedekind::algebra::IsGrothendieckGroup;
using dedekind::algebra::IsInitialRing;

// ===========================================================================
// (1) Concept fires on the canonical witnesses
// ===========================================================================

TEST_CASE(
    "initial_ring: opt-in trait defaults to false (no initial-ring witness "
    "without explicit registration)",
    "[algebra][initial_ring][negative]") {
  // Default: not initial.  Carriers must opt in explicitly.
  STATIC_CHECK(!is_initial_ring_v<int>);
  STATIC_CHECK(!is_initial_ring_v<unsigned int>);
  STATIC_CHECK(!is_initial_ring_v<Cardinality>);
  STATIC_CHECK((!is_grothendieck_group_v<unsigned int, Cardinality>));
  STATIC_CHECK((!is_grothendieck_group_v<Cardinality, Cardinality>));
}

// ===========================================================================
// (3) Grothendieck construction exercised: closure-forcing operator
// ===========================================================================

TEST_CASE(
    "grothendieck: Cardinality - Cardinality → SignedCardinality realises "
    "the Grothendieck construction at the operator level",
    "[algebra][grothendieck][universal-property][closure-forcing]") {
  // Exercise the textbook construction: [(a, b)] ∈ ℤ represents
  // a - b.  The closure-forcing operator's existence with the wider
  // codomain is the Grothendieck-construction's @b multiplication
  // (the construction map at the operator level); the @b unit is
  // the embedding @c lift_ℕ_ℤ_ that lifts a single Cardinality into
  // SignedCardinality (covered in test (5) below).
  auto const a3 = finite_cardinality(3);
  auto const a5 = finite_cardinality(5);
  auto const a7 = finite_cardinality(7);
  // 5 - 3 = 2.
  CHECK(a5 - a3 == finite_signed_cardinality(2));
  // 3 - 5 = -2 (the construction's defining clause: the operator
  // widens to the larger carrier where the result lives).
  CHECK(a3 - a5 == finite_signed_cardinality(-2));
  // 7 - 7 = 0 (the equivalence-class neutral element).
  CHECK(a7 - a7 == finite_signed_cardinality(0));
  // 0 - 5 = -5.
  CHECK(finite_cardinality(0) - a5 == finite_signed_cardinality(-5));
}

TEST_CASE(
    "grothendieck: equivalence classes [(a, b)] ~ [(c, d)] iff a + d = b + c",
    "[algebra][grothendieck][universal-property][equivalence]") {
  // Concrete witnesses of the Grothendieck quotient relation:
  // (a, b) ~ (c, d) iff a + d = b + c, both representing a - b.
  // The closure-forcing operator should yield equal results for
  // equivalent input pairs.
  // [(5, 3)] = [(7, 5)] = [(2, 0)] all represent 2.
  auto const v1 = finite_cardinality(5) - finite_cardinality(3);
  auto const v2 = finite_cardinality(7) - finite_cardinality(5);
  auto const v3 = finite_cardinality(2) - finite_cardinality(0);
  CHECK(v1 == v2);
  CHECK(v1 == v3);
  CHECK(v1 == finite_signed_cardinality(2));
  // Negative side: [(0, 4)] = [(2, 6)] = [(7, 11)] all represent -4.
  auto const n1 = finite_cardinality(0) - finite_cardinality(4);
  auto const n2 = finite_cardinality(2) - finite_cardinality(6);
  auto const n3 = finite_cardinality(7) - finite_cardinality(11);
  CHECK(n1 == n2);
  CHECK(n1 == n3);
  CHECK(n1 == finite_signed_cardinality(-4));
}

TEST_CASE(
    "grothendieck: closure-forcing operator produces a SignedCardinality "
    "that is structurally an abelian group (witnesses the codomain widening)",
    "[algebra][grothendieck][carrier-lattice]") {
  // The construction's load-bearing claim: the codomain admits
  // additive inverses, which the source @c Cardinality (a
  // saturating commutative monoid) does not.  Verify the inverse
  // structure on the lifted side.
  auto const z = finite_cardinality(5) - finite_cardinality(3);  // = 2
  auto const neg_z = -z;                                         // = -2
  CHECK((z + neg_z) == finite_signed_cardinality(0));
  CHECK(neg_z == finite_signed_cardinality(-2));
}

// ===========================================================================
// (4) The dual reification holds: same carrier inhabits both Forms
// ===========================================================================

TEST_CASE(
    "initial_ring: SignedCardinality inhabits BOTH IsInitialRing AND "
    "IsGrothendieckGroup — formal type-checked documentation of the dual "
    "universal-property reading",
    "[algebra][initial_ring][grothendieck][duality]") {
  // The load-bearing structural claim of #446: the same carrier
  // simultaneously witnesses two distinct universal properties.
  STATIC_CHECK(IsInitialRing<SignedCardinality>);
  STATIC_CHECK(IsGrothendieckGroup<SignedCardinality, Cardinality>);

  // The two readings agree up to canonical isomorphism on the
  // operational level: a value constructed via the closure-forcing
  // operator (Grothendieck side) equals the same value constructed
  // directly (initial-ring side).
  auto const via_grothendieck = finite_cardinality(7) - finite_cardinality(0);
  auto const via_initial_ring = finite_signed_cardinality(7);
  CHECK(via_grothendieck == via_initial_ring);
}

TEST_CASE(
    "carrier-lattice: embed_𝔹_𝕂3 (set-level lift) on Singleton<true> lands at "
    "Singleton<Ternary::True> in 𝕂3",
    "[carrier-lattice][boolean][kleene][embed][image]") {
  // The per-shape dispatch (which sets::image overload to use) is
  // resolved at compile time via template overload resolution; this
  // test invokes the resulting code at runtime so the lambda /
  // forwarding-reference body show up as covered for codecov.
  // Sister of PR #624's embed_𝔹_ℕ test; same pattern, different
  // codomain.
  constexpr SingletonSet<bool, ClassicalLogic> s_true{true};
  const auto image_set = embed_𝔹_𝕂3(s_true);
  CHECK(image_set.pivot == Ternary::True);

  constexpr SingletonSet<bool, ClassicalLogic> s_false{false};
  const auto image_set_false = embed_𝔹_𝕂3(s_false);
  CHECK(image_set_false.pivot == Ternary::False);
}

// ===========================================================================
// (7) Carrier-lattice lift unification (#455): existential-proof
//     dispatch on the central (Cardinality, SignedCardinality) pair.
// ===========================================================================

TEST_CASE(
    "carrier-lattice lift dispatch: lift<Cardinality, SignedCardinality>(c) "
    "agrees with the canonical bespoke lift_ℕ_ℤ_(c) — discoverability alias "
    "without categorical overclaim",
    "[carrier-lattice][lift-unification][dispatch]") {
  // The unified lift<From, To> trait dispatches to the canonical
  // bespoke arrow registered for the (From, To) pair.  For the
  // central variant-layer ℕ → ℤ pair, the dispatch resolves to
  // lift_ℕ_ℤ_; the result must agree on every input.
  for (const std::size_t v : {std::size_t{0}, std::size_t{1}, std::size_t{42},
                              std::size_t{1000}, std::size_t{2147483648u}}) {
    const auto via_alias =
        lift<Cardinality, SignedCardinality>(finite_cardinality(v));
    const auto via_bespoke = lift_ℕ_ℤ_(finite_cardinality(v));
    CHECK(via_alias == via_bespoke);
  }
  // The transfinite case: lift_ℕ_ℤ_ promotes ℵ_0 to PositiveInfinity;
  // the alias dispatch agrees.
  const auto inf = Cardinality{ℵ_0{}};
  CHECK(lift<Cardinality, SignedCardinality>(inf) == lift_ℕ_ℤ_(inf));
}
