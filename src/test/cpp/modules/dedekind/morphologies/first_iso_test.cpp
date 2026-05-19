/** @file dedekind/morphologies/first_iso_test.cpp
 *
 * Unit coverage for the First-Isomorphism-Theorem typed witness at the
 * canonical parity-homomorphism case (#718 Slice 4, paper-§3 crown).
 *
 * The theorem is the @b reusable @c WitnessesFirstIso<F, Connector>
 * concept in @c :category::image.  The witness at @c mod_2 is the
 * canonical first instance — any future First-Iso instance follows
 * the same three-line pattern:
 *
 *   template <> struct kernel_quotient<F> { using Class = …; };
 *   template <> struct image_carrier <F> { using type  = …; };
 *   static_assert(WitnessesFirstIso<F, connector>);
 *
 * Coverage targets:
 *  - The @c WitnessesFirstIso concept body (compile-time, via
 *    @c STATIC_CHECK and the inline @c static_assert in the partition).
 *  - The @c mod_2_arrow / @c connector / @c connector_inv operator
 *    bodies (runtime, via @c CHECK).
 *  - Round-trip identity (runtime).
 *  - Negative gate: a candidate iso with mismatching Domain or
 *    Codomain honestly fails @c WitnessesFirstIso (compile-time).
 */

#include <catch2/catch_test_macros.hpp>

import dedekind.category;
import dedekind.morphologies;

using namespace dedekind::morphologies;
using namespace dedekind::morphologies::first_iso_mod_2;
using dedekind::category::WitnessesFirstIso;

TEST_CASE("morphologies:first_iso — WitnessesFirstIso<mod_2_arrow, connector> "
          "(the reusable First-Iso crown at mod_2)",
          "[morphologies][cyclic][first-iso][crown]") {
  /** @brief The typed claim: @c connector witnesses the First Iso
   *         Theorem at @c mod_2.  The static_assert in :morphologies
   *         already pins it at compile time; this STATIC_CHECK is the
   *         runtime test harness mirror. */
  STATIC_CHECK(WitnessesFirstIso<mod_2_arrow, connector>);
  // The reverse iso is also a valid witness when the roles of quotient
  // and image are swapped (a true iso is bidirectional).  But our trait
  // registry only declares the textbook (Modular<2>, bool) pairing in
  // the (Domain, Codomain) order, so connector_inv would need a separate
  // mod_2_arrow-inverse witness to satisfy WitnessesFirstIso<…, _inv>.
  // Pin the asymmetry as documentation:
  STATIC_CHECK_FALSE(WitnessesFirstIso<mod_2_arrow, connector_inv>);
}

TEST_CASE("morphologies:first_iso — mod_2 parity homomorphism",
          "[morphologies][cyclic][first-iso][mod_2]") {
  /** @brief The parity homomorphism @c mod_2 sends evens to @c false
   *         and odds to @c true.  Surjective onto @c bool. */
  mod_2_arrow f{};
  CHECK_FALSE(f(0));
  CHECK_FALSE(f(2));
  CHECK_FALSE(f(-4));
  CHECK_FALSE(f(100));
  CHECK(f(1));
  CHECK(f(3));
  CHECK(f(-5));
  CHECK(f(101));
}

TEST_CASE(
    "morphologies:first_iso — connector / connector_inv runtime exercise",
    "[morphologies][cyclic][first-iso][connector]") {
  /** @brief Runtime exercise of the iso bodies + the @c inverse()
   *         free-function hooks (codecov coverage). */
  connector forward{};
  connector_inv reverse{};

  // Forward direction: Modular<2> → bool
  CHECK_FALSE(forward(Modular<2>{0}));
  CHECK(forward(Modular<2>{1}));

  // Reverse direction: bool → Modular<2>
  CHECK(reverse(false) == Modular<2>{0});
  CHECK(reverse(true) == Modular<2>{1});

  // inverse() free-function hooks
  auto inv_f = inverse(forward);
  auto inv_r = inverse(reverse);
  CHECK(inv_f(false) == Modular<2>{0});
  CHECK(inv_f(true) == Modular<2>{1});
  CHECK_FALSE(inv_r(Modular<2>{0}));
  CHECK(inv_r(Modular<2>{1}));
}

TEST_CASE("morphologies:first_iso — round-trip is identity in both directions",
          "[morphologies][cyclic][first-iso][round-trip]") {
  /** @brief Iso law: forward ∘ reverse = id_bool;  reverse ∘ forward
   *         = id_{Modular<2>}.  WitnessesFirstIso pins the type-level
   *         shape; this runtime test pins the actual round-trip
   *         identity (the engineer's honesty obligation). */
  connector forward{};
  connector_inv reverse{};

  CHECK(reverse(forward(Modular<2>{0})) == Modular<2>{0});
  CHECK(reverse(forward(Modular<2>{1})) == Modular<2>{1});
  CHECK(forward(reverse(false)) == false);
  CHECK(forward(reverse(true)) == true);
}

namespace _first_iso_negative {

/** @brief Stand-in arrow with NO kernel_quotient / image_carrier
 *         registrations.  @c WitnessesFirstIso must reject it. */
struct unregistered_arrow {
  using Domain = int;
  using Codomain = int;
  constexpr int operator()(int x) const noexcept { return x; }
};

/** @brief A candidate "iso" with mismatching Domain (int instead of
 *         Modular<2>).  Even with @c IsIsomorphism, @c WitnessesFirstIso
 *         must reject because the textbook claim
 *         @c kernel_quotient<mod_2_arrow>::Class = @c Modular<2>
 *         doesn't match. */
struct misdomained_connector {
  using Domain = int;  // WRONG — should be Modular<2>
  using Codomain = bool;
  constexpr bool operator()(int x) const noexcept { return x != 0; }
};
struct misdomained_connector_inv {
  using Domain = bool;
  using Codomain = int;
  constexpr int operator()(bool b) const noexcept { return b ? 1 : 0; }
};
constexpr misdomained_connector_inv inverse(misdomained_connector) noexcept {
  return {};
}
constexpr misdomained_connector inverse(misdomained_connector_inv) noexcept {
  return {};
}

}  // namespace _first_iso_negative

TEST_CASE(
    "morphologies:first_iso — negative gates: unregistered F + mismatched "
    "connector Domain",
    "[morphologies][cyclic][first-iso][negative]") {
  /** @brief Two negative cases that pin the load-bearing parts of
   *         @c WitnessesFirstIso:
   *
   *  1. Unregistered F: no @c kernel_quotient<F>::Class registration ⇒
   *     the typename-requires clause fails ⇒ honest reject.
   *
   *  2. Mis-Domained connector: even with all opt-ins registered for
   *     @c mod_2_arrow and a valid @c IsIsomorphism, a connector whose
   *     @c Domain isn't @c Modular<2> fails the @c same_as gate
   *     ⇒ honest reject. */
  STATIC_CHECK_FALSE(WitnessesFirstIso<_first_iso_negative::unregistered_arrow,
                                       connector>);
  STATIC_CHECK_FALSE(
      WitnessesFirstIso<mod_2_arrow,
                        _first_iso_negative::misdomained_connector>);
}
