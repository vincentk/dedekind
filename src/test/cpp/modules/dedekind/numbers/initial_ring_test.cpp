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
 */
#include <catch2/catch_test_macros.hpp>
#include <variant>  // load-bearing for variant operator== via ADL

import dedekind.algebra;
import dedekind.category;
import dedekind.morphologies;
import dedekind.numbers;
import dedekind.sets;

using namespace dedekind::category;
using namespace dedekind::numbers;
using namespace dedekind::sets;
using dedekind::algebra::is_grothendieck_group_v;
using dedekind::algebra::is_initial_ring_v;
using dedekind::algebra::IsGrothendieckGroup;
using dedekind::algebra::IsInitialRing;
using dedekind::morphologies::Modular;

// ===========================================================================
// (1) Concept fires on the canonical witnesses
// ===========================================================================

TEST_CASE("initial_ring: IsInitialRing<SignedCardinality> fires",
          "[algebra][initial_ring][witness]") {
  STATIC_CHECK(IsInitialRing<SignedCardinality>);
  STATIC_CHECK(is_initial_ring_v<SignedCardinality>);
}

TEST_CASE(
    "initial_ring: IsGrothendieckGroup<SignedCardinality, Cardinality> "
    "fires",
    "[algebra][grothendieck][witness]") {
  STATIC_CHECK(IsGrothendieckGroup<SignedCardinality, Cardinality>);
  STATIC_CHECK((is_grothendieck_group_v<SignedCardinality, Cardinality>));
}

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
// (2) Universal property exercised: χ_{Modular<n>} as mod-n reduction
// ===========================================================================
//
// The unique ring homomorphism @c χ_R : @c SignedCardinality → @c R
// for @c R = @c Modular<n> is the mod-@c n reduction.  We exercise
// this operationally — the test suite pins what the universal-
// property witness's @b operational behaviour must be on a concrete
// target ring.

namespace {
// Helper: mod-n reduction of a finite SignedCardinality value.
// Implements χ_{Modular<n>} for the finite fragment.
template <auto N>
constexpr Modular<N> chi_to_modular(SignedCardinality const& z) noexcept {
  // For finite values, extract magnitude with sign and reduce.
  // The universal property's content (uniqueness) is the engineer's
  // honesty obligation; this implementation realises the unique
  // homomorphism on the finite fragment.
  if (auto const* sec = std::get_if<SignedExtensionalCardinal<>>(&z)) {
    using ML = typename Modular<N>::machine_type;
    auto const mag = static_cast<ML>(sec->magnitude.limbs[0]);
    auto const reduced_mag = mag % static_cast<ML>(N);
    if (sec->negative && reduced_mag != 0) {
      return Modular<N>{static_cast<ML>(N) - reduced_mag};
    }
    return Modular<N>{reduced_mag};
  }
  // Sentinels (±ℵ_0, NaZ): not part of the textbook ℤ; the
  // universal property does not extend.  Return zero as a
  // placeholder; this case is out of scope for the test.
  return Modular<N>{0};
}
}  // namespace

TEST_CASE(
    "initial_ring: χ_{Modular<5>} is the unique ring homomorphism — sends "
    "1 to 1, preserves +, *, zero",
    "[algebra][initial_ring][modular][universal-property]") {
  // Sends 1_ℤ to 1_M (the load-bearing universal-property clause).
  CHECK(chi_to_modular<5>(finite_signed_cardinality(1)).value == 1);
  CHECK(chi_to_modular<5>(finite_signed_cardinality(0)).value == 0);
  // Mod-5 reduction on positive finite values.
  CHECK(chi_to_modular<5>(finite_signed_cardinality(7)).value == 2);
  CHECK(chi_to_modular<5>(finite_signed_cardinality(13)).value == 3);
  CHECK(chi_to_modular<5>(finite_signed_cardinality(25)).value == 0);
  // Mod-5 reduction on negative values (sign-aware: -3 ≡ 2 (mod 5)).
  CHECK(chi_to_modular<5>(finite_signed_cardinality(-1)).value == 4);
  CHECK(chi_to_modular<5>(finite_signed_cardinality(-3)).value == 2);
  CHECK(chi_to_modular<5>(finite_signed_cardinality(-5)).value == 0);
  CHECK(chi_to_modular<5>(finite_signed_cardinality(-7)).value == 3);
}

TEST_CASE(
    "initial_ring: χ_{Modular<n>} is a ring homomorphism — preserves + and "
    "* on representative values",
    "[algebra][initial_ring][modular][ring-homomorphism]") {
  auto const z2 = finite_signed_cardinality(2);
  auto const z3 = finite_signed_cardinality(3);
  auto const z5 = finite_signed_cardinality(5);
  // χ(a + b) = χ(a) + χ(b)
  auto const sum_via_z = chi_to_modular<7>(z2 + z3);
  auto const sum_via_m = chi_to_modular<7>(z2) + chi_to_modular<7>(z3);
  CHECK(sum_via_z.value == sum_via_m.value);
  // χ(a * b) = χ(a) * χ(b)
  auto const prod_via_z = chi_to_modular<7>(z2 * z5);
  auto const prod_via_m = chi_to_modular<7>(z2) * chi_to_modular<7>(z5);
  CHECK(prod_via_z.value == prod_via_m.value);
  // Wrap example: χ(8) = χ(1) since 8 ≡ 1 (mod 7).
  CHECK(chi_to_modular<7>(finite_signed_cardinality(8)).value == 1);
}

// ===========================================================================
// (3) Grothendieck construction exercised: closure-forcing operator
// ===========================================================================

TEST_CASE(
    "grothendieck: Cardinality - Cardinality → SignedCardinality realises "
    "the Grothendieck construction at the operator level",
    "[algebra][grothendieck][universal-property][closure-forcing]") {
  // The operator's existence with the wider codomain @b is the
  // Grothendieck-construction unit at the operator level.  Exercise
  // the textbook construction: [(a, b)] ∈ ℤ represents a - b.
  // Verify the operator yields the corresponding SignedCardinality.
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
  // χ_{Modular<5>} of either yields the same residue.
  CHECK(chi_to_modular<5>(via_grothendieck).value ==
        chi_to_modular<5>(via_initial_ring).value);
}
