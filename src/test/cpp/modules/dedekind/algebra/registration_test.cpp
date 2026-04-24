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

TEST_CASE(
    "registration — Modular<N>'s hand-written SpeciesTraits is not broadened",
    "[algebra][registration]") {
  // Regression check.  `SpeciesTraits<Modular<N>>` in
  // `category:species` has Op-agnostic member templates
  // (`template <typename Op> static constexpr bool is_associative_v
  // = true;`) with pre-Registration semantics.  Without marker
  // gating, the `:registration` discovery would pick these up and
  // claim `is_associative<Modular<N>, Op>` = true for \emph{any}
  // Op (e.g.\ `std::modulus` --- which isn't associative).  The
  // marker gate prevents that: Modular<N>'s SpeciesTraits does not
  // carry the `dedekind_registration_tag`, so the discovery
  // specs don't apply, and Modular<N>'s free-standing
  // specialisations (for the specific `std::plus<Modular<N>>` /
  // `std::multiplies<Modular<N>>` Ops it genuinely supports)
  // continue to govern.
  using M = dedekind::category::Modular<256>;

  // Positive: the hand-written specialisations for the canonical
  // (+, *) ops still hold (untouched by the discovery machinery).
  STATIC_CHECK(dedekind::category::is_associative_v<M, std::plus<M>>);
  STATIC_CHECK(dedekind::category::is_associative_v<M, std::multiplies<M>>);

  // Negative: the discovery did NOT accidentally claim Modular<N>
  // is a Galois field under (+, *).  Modular<N> is a commutative
  // ring but not a field for generic N (only N = prime gives a
  // field), so `is_galois_field_v` must remain false.
  STATIC_CHECK(!is_galois_field_v<M, std::plus<M>, std::multiplies<M>>);
}
