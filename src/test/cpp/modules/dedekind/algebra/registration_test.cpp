/** @file dedekind/algebra/registration_test.cpp
 *
 * Tests for the @c algebra:registration machinery (issue #382):
 * @c FieldRegistration / @c GaloisFieldRegistration CRTP-ish helpers
 * plus the @c SpeciesTraits-based discovery for the free-standing
 * trait machinery in @c dedekind.category.
 *
 * These tests focus on the \emph{mechanism} --- the per-trait
 * discovery paths --- using the library's only current
 * Registration-retargeted carrier, @c 𝔽64.  Axiom-level and
 * operator-level coverage of @c 𝔽64 itself lives in
 * @c galois_test.cpp; here we only assert the narrow structural
 * claim that registering via @c GaloisFieldRegistration causes
 * the matching traits to fire through the library's concept
 * machinery, so a carrier author does not need to write each
 * trait specialisation by hand.
 */

#include <catch2/catch_test_macros.hpp>
#include <cstdint>
#include <functional>

import dedekind.algebra;
import dedekind.category;

using dedekind::algebra::galois_order_v;
using dedekind::algebra::is_galois_field_v;
using dedekind::algebra::𝔽64;

namespace {

// Shorthand for 𝔽64's canonical ops.
using Add64 = std::plus<𝔽64>;
using Mult64 = std::multiplies<𝔽64>;

}  // namespace

TEST_CASE(
    "registration — SpeciesTraits-based discovery lifts struct traits for 𝔽64",
    "[algebra][registration]") {
  using dedekind::category::identity_v;
  using dedekind::category::is_associative_v;
  using dedekind::category::is_commutative_v;
  using dedekind::category::is_invertible_v;
  using dedekind::category::is_periodic_v;

  // Associativity + commutativity (both ops).
  STATIC_CHECK(is_associative_v<𝔽64, Add64>);
  STATIC_CHECK(is_associative_v<𝔽64, Mult64>);
  STATIC_CHECK(is_commutative_v<𝔽64, Add64>);
  STATIC_CHECK(is_commutative_v<𝔽64, Mult64>);

  // Totality via periodicity (both ops).
  STATIC_CHECK(is_periodic_v<𝔽64, Add64>);
  STATIC_CHECK(is_periodic_v<𝔽64, Mult64>);

  // Invertibility (via inverse_trait partial spec in :registration).
  STATIC_CHECK(is_invertible_v<𝔽64, Add64>);
  STATIC_CHECK(is_invertible_v<𝔽64, Mult64>);

  // Identities from the registration helper, not a per-carrier
  // identity_trait specialisation.
  STATIC_CHECK(identity_v<𝔽64, Add64> == 𝔽64{});
  STATIC_CHECK(identity_v<𝔽64, Mult64> == 𝔽64{std::uint8_t{1}});
}

TEST_CASE(
    "registration — GaloisFieldRegistration also lifts the Galois opt-ins",
    "[algebra][registration]") {
  // `is_galois_field_v` and `galois_order_v` are struct-backed in
  // `:galois` so the SpeciesTraits-based discovery is cross-module
  // robust.  The carrier registers the claim through
  // `GaloisFieldRegistration<𝔽64, …, 64>` --- no per-carrier
  // explicit spec needed.
  STATIC_CHECK(is_galois_field_v<𝔽64, Add64, Mult64>);
  STATIC_CHECK(galois_order_v<𝔽64, Add64, Mult64> == 64);

  // A negative control: bool-as-𝔽2 is registered manually (via
  // struct specialisation of `is_galois_field` / `galois_order`
  // rather than via a Registration helper), so its traits should
  // still hold.  This confirms the two registration paths --- helper
  // and explicit --- coexist.
  STATIC_CHECK(is_galois_field_v<bool, std::bit_xor<bool>, std::bit_and<bool>>);
  STATIC_CHECK(galois_order_v<bool, std::bit_xor<bool>, std::bit_and<bool>> ==
               2);
}

TEST_CASE(
    "registration — discovery does not fire for carriers without the helper",
    "[algebra][registration]") {
  // The discovery partial specs are marker-gated on
  // `dedekind_registration_tag`, so carriers whose SpeciesTraits
  // does not inherit from a Registration helper are unaffected.
  //
  // `int` has no Galois-field claim registered.  Confirm the
  // registration discovery doesn't accidentally lift it into a
  // Galois field.
  STATIC_CHECK(!is_galois_field_v<int, std::plus<int>, std::multiplies<int>>);
  STATIC_CHECK(galois_order_v<int, std::plus<int>, std::multiplies<int>> == 0);
}

// The Modular<N> regression test (verifying that hand-written
// `SpeciesTraits<Modular<N>>` member templates aren't broadened by
// the marker-gated discovery) lives in
// `morphologies/modular_test.cpp` --- Modular relocated to
// `morphologies:cyclic`, which the algebra test layer cannot
// import (morphologies is downstream of algebra).
