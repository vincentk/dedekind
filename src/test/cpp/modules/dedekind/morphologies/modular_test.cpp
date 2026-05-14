/** @file dedekind/morphologies/modular_test.cpp
 *
 * Tests for @c Modular<N> after its relocation from
 * @c dedekind.category:species (and the transient move
 * to @c dedekind.morphologies:cyclic).
 *
 * Exercises the dual nature of the carrier: it satisfies the
 * operational @c morphologies::IsCyclic shape concept (via
 * @c Domain / @c generator() / @c successor() member API) AND the
 * categorical @c category::IsCyclicGroup<T, std::plus<T>> axiom-
 * bearing concept.  These two concepts are kept complementary in
 * the project's strict / operational vocabulary pattern, and
 * @c Modular<N> is the canonical carrier that bridges them.
 */

#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <functional>
#include <type_traits>
#include <variant>  // SignedCardinality / Cardinality reach equality via ADL.

import dedekind.algebra;
import dedekind.category;
import dedekind.morphologies;
import dedekind.sets;

using dedekind::algebra::is_galois_field_v;
using dedekind::algebra::IsInitialRing;
using dedekind::category::cyclic_order_v;
using dedekind::category::IsCyclicGroup;
using dedekind::morphologies::IsCyclic;
using dedekind::morphologies::Modular;
using dedekind::sets::Cardinality;
using dedekind::sets::finite_cardinality;
using dedekind::sets::finite_signed_cardinality;
using dedekind::sets::SignedCardinality;
using dedekind::sets::SignedExtensionalCardinal;

TEST_CASE("Modular<N> — operational IsCyclic shape (morphologies)",
          "[morphologies][modular][cyclic]") {
  using M256 = Modular<256>;
  using M17 = Modular<17>;

  // Duck-typed: Domain alias, generator(), successor() all present.
  STATIC_CHECK(IsCyclic<M256>);
  STATIC_CHECK(IsCyclic<M17>);

  // Generator is 1.
  STATIC_CHECK(M256::generator() == M256{1});
  STATIC_CHECK(M17::generator() == M17{1});

  // Successor walks the chain modulo N.
  STATIC_CHECK(M17::successor(M17{16}) == M17{0});  // 16 + 1 ≡ 0 (mod 17)
  STATIC_CHECK(M17::successor(M17{0}) == M17{1});
}

TEST_CASE("Modular<N> — axiomatic IsCyclicGroup (category)",
          "[morphologies][modular][cyclic-group]") {
  using M256 = Modular<256>;
  using M17 = Modular<17>;

  STATIC_CHECK(IsCyclicGroup<M256, std::plus<M256>>);
  STATIC_CHECK(cyclic_order_v<M256, std::plus<M256>> == 256);

  STATIC_CHECK(IsCyclicGroup<M17, std::plus<M17>>);
  STATIC_CHECK(cyclic_order_v<M17, std::plus<M17>> == 17);
}

TEST_CASE("Modular<N> — arithmetic round-trip", "[morphologies][modular]") {
  using M = Modular<7>;
  constexpr M a{3};
  constexpr M b{5};
  STATIC_CHECK((a + b) == M{1});  // 3 + 5 ≡ 1 (mod 7)
  STATIC_CHECK((a * b) == M{1});  // 3 * 5 ≡ 1 (mod 7)
}

TEST_CASE(
    "Modular<N> — hand-written SpeciesTraits not broadened by Registration",
    "[morphologies][modular][registration]") {
  // Regression for #382/#385.  `SpeciesTraits<Modular<N>>` in
  // `morphologies:cyclic` has Op-agnostic member templates
  // (`template <typename Op> static constexpr bool is_associative_v
  // = true;`) with pre-Registration semantics.  Without marker
  // gating in `algebra:registration`, the discovery would pick these
  // up and claim `is_associative<Modular<N>, Op>` = true for any Op
  // (e.g.\ `std::modulus` --- which isn't associative).  The marker
  // gate prevents that: `SpeciesTraits<Modular<N>>` does not carry
  // the `dedekind_registration_tag`, so the discovery specs don't
  // apply.  Modular<N>'s free-standing specialisations (for the
  // specific `std::plus<Modular<N>>` / `std::multiplies<Modular<N>>`
  // Ops it genuinely supports) continue to govern.
  using M = Modular<256>;

  // Positive: the hand-written specialisations for the canonical
  // (+, *) ops still hold.
  STATIC_CHECK(dedekind::category::is_associative_v<M, std::plus<M>>);
  STATIC_CHECK(dedekind::category::is_associative_v<M, std::multiplies<M>>);

  // Negative: the discovery did NOT accidentally claim Modular<N>
  // is a Galois field under (+, *).  Modular<256> is a commutative
  // ring but not a field (256 = 2^8 isn't a prime, and even prime-
  // power orders need polynomial-quotient construction, not
  // integer modular arithmetic).
  STATIC_CHECK(!is_galois_field_v<M, std::plus<M>, std::multiplies<M>>);
}

TEST_CASE("Modular<N> — Peano successor / generator coherence (#388)",
          "[morphologies][modular][peano][successor][generator]") {
  // The carrier-level T::successor(x) member API and the algebraic
  // Peano successor S(x) = x + 1 must agree pointwise.  This is
  // pinned at the type level in cyclic.cppm; the runtime walk here
  // exercises the same coherence at every index, including the
  // wrap-around at N-1 → 0 that the algebraic identity needs to
  // respect.
  using M7 = Modular<7>;

  // Successor walks the chain back to its starting point in exactly
  // N steps (this IS the cyclicity claim).
  M7 cursor = M7::generator();  // = M7{1}
  CHECK(cursor == M7{1});
  for (int i = 0; i < 7; ++i) {
    cursor = M7::successor(cursor);
  }
  // After 7 successor applications starting from 1, we should be
  // back at 1: 1 → 2 → 3 → 4 → 5 → 6 → 0 → 1.
  CHECK(cursor == M7{1});

  // Pointwise: T::successor(x) == x + 1 across every residue.
  for (int k = 0; k < 7; ++k) {
    const M7 x{k};
    CHECK(M7::successor(x) == (x + M7{1}));
  }

  // Generator + successor enumerate every residue (the orbit
  // covers Z/7Z, since 1 is a generator of Z/7Z under +).
  bool seen[7] = {false};
  M7 walker = M7{0};
  for (int i = 0; i < 7; ++i) {
    seen[walker.value] = true;
    walker = M7::successor(walker);
  }
  for (int i = 0; i < 7; ++i) CHECK(seen[i]);
}

// ===========================================================================
// (4) Universal property of ℤ: @c χ_{Modular<n>} as the unique ring
//     homomorphism @c SignedCardinality → @c Modular<n> (closes part
//     of #446).  Exercises @c IsInitialRing<SignedCardinality>
//     operationally at a concrete target ring.
// ===========================================================================

namespace {
// Helper: mod-n reduction of a finite SignedCardinality value.
// Implements χ_{Modular<n>} for the finite fragment.  Lives in this
// test file because it composes algebra (@c IsInitialRing) with
// morphologies (@c Modular<N>) — the universal-property exercise
// straddles the two partitions.
template <auto N>
constexpr Modular<N> χ_to_modular(SignedCardinality const& z) noexcept {
  if (auto const* sec = std::get_if<SignedExtensionalCardinal<>>(&z)) {
    using ML = typename Modular<N>::machine_type;
    auto const limb = sec->magnitude.limbs[0];
    using LimbType = std::remove_cv_t<std::remove_reference_t<decltype(limb)>>;
    auto const reduced_in_limb = limb % static_cast<LimbType>(N);
    auto const reduced_mag = static_cast<ML>(reduced_in_limb);
    if (sec->negative && reduced_mag != 0) {
      return Modular<N>{static_cast<ML>(N) - reduced_mag};
    }
    return Modular<N>{reduced_mag};
  }
  // Sentinels (±ℵ_0, NaZ): not part of the textbook ℤ; the universal
  // property does not extend.  Return zero as a placeholder; this
  // case is out of scope for the test.
  return Modular<N>{0};
}
}  // namespace

TEST_CASE(
    "initial_ring: χ_{Modular<5>} is the unique ring homomorphism — sends "
    "1 to 1, preserves +, *, zero",
    "[algebra][initial_ring][modular][universal-property]") {
  STATIC_CHECK(IsInitialRing<SignedCardinality>);
  // Sends 1_ℤ to 1_M (the load-bearing universal-property clause).
  CHECK(χ_to_modular<5>(finite_signed_cardinality(1)).value == 1);
  CHECK(χ_to_modular<5>(finite_signed_cardinality(0)).value == 0);
  // Mod-5 reduction on positive finite values.
  CHECK(χ_to_modular<5>(finite_signed_cardinality(7)).value == 2);
  CHECK(χ_to_modular<5>(finite_signed_cardinality(13)).value == 3);
  CHECK(χ_to_modular<5>(finite_signed_cardinality(25)).value == 0);
  // Mod-5 reduction on negative values (sign-aware: -3 ≡ 2 (mod 5)).
  CHECK(χ_to_modular<5>(finite_signed_cardinality(-1)).value == 4);
  CHECK(χ_to_modular<5>(finite_signed_cardinality(-3)).value == 2);
  CHECK(χ_to_modular<5>(finite_signed_cardinality(-5)).value == 0);
  CHECK(χ_to_modular<5>(finite_signed_cardinality(-7)).value == 3);
}

TEST_CASE(
    "initial_ring: χ_{Modular<n>} is a ring homomorphism — preserves + and "
    "* on representative values",
    "[algebra][initial_ring][modular][ring-homomorphism]") {
  auto const z2 = finite_signed_cardinality(2);
  auto const z3 = finite_signed_cardinality(3);
  auto const z5 = finite_signed_cardinality(5);
  // χ(a + b) = χ(a) + χ(b)
  auto const sum_via_z = χ_to_modular<7>(z2 + z3);
  auto const sum_via_m = χ_to_modular<7>(z2) + χ_to_modular<7>(z3);
  CHECK(sum_via_z.value == sum_via_m.value);
  // χ(a * b) = χ(a) * χ(b)
  auto const prod_via_z = χ_to_modular<7>(z2 * z5);
  auto const prod_via_m = χ_to_modular<7>(z2) * χ_to_modular<7>(z5);
  CHECK(prod_via_z.value == prod_via_m.value);
  // Wrap example: χ(8) = χ(1) since 8 ≡ 1 (mod 7).
  CHECK(χ_to_modular<7>(finite_signed_cardinality(8)).value == 1);
}
