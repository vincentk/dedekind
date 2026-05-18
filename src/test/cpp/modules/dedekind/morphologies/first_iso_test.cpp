/** @file dedekind/morphologies/first_iso_test.cpp
 *
 * Unit coverage for the First-Isomorphism-Theorem typed witness at the
 * canonical parity-homomorphism case (#718 Slice 4, paper-§3 crown).
 *
 * The theorem (Burris-Sankappanavar §II.6 / Birkhoff & Mac Lane
 * "Algebra" §III) — A/ker(f) ≅ im(f) for any homomorphism f — is
 * pinned at compile time in @c morphologies/cyclic.cppm via
 * @c static_assert(IsIsomorphism<first_iso_mod_2::connector>).  This
 * file adds runtime exercises to keep codecov honest about the
 * connector function bodies.
 *
 * Coverage targets:
 *  - @c mod_2_arrow::operator()  (parity homomorphism)
 *  - @c connector::operator()  (forward: Modular<2> → bool)
 *  - @c connector_inv::operator()  (reverse: bool → Modular<2>)
 *  - @c inverse() free functions in both directions
 *  - Round-trip witnesses (composition is identity in both directions)
 */

#include <catch2/catch_test_macros.hpp>

import dedekind.category;
import dedekind.morphologies;

using namespace dedekind::morphologies;
using namespace dedekind::morphologies::first_iso_mod_2;

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

TEST_CASE("morphologies:first_iso — Modular<2> ≅ bool (the First-Iso crown)",
          "[morphologies][cyclic][first-iso][crown]") {
  /** @brief The First-Iso connector @c Modular<2> @c → @c bool sends
   *         @c [0] to @c false and @c [1] to @c true.  Inverse goes
   *         the other way.  Together they witness the iso predicted
   *         by the First-Iso theorem at the parity-homomorphism case. */
  connector forward{};
  connector_inv reverse{};

  // Forward direction: Modular<2> → bool
  CHECK_FALSE(forward(Modular<2>{0}));
  CHECK(forward(Modular<2>{1}));

  // Reverse direction: bool → Modular<2>
  CHECK(reverse(false) == Modular<2>{0});
  CHECK(reverse(true) == Modular<2>{1});

  // inverse() free-function hooks (used by IsIsomorphism).
  auto inv_f = inverse(forward);
  auto inv_r = inverse(reverse);
  CHECK(inv_f(false) == Modular<2>{0});
  CHECK(inv_f(true) == Modular<2>{1});
  CHECK_FALSE(inv_r(Modular<2>{0}));
  CHECK(inv_r(Modular<2>{1}));
}

TEST_CASE("morphologies:first_iso — round-trip is identity in both directions",
          "[morphologies][cyclic][first-iso][round-trip]") {
  /** @brief Iso law check: forward ∘ reverse = id_bool, and
   *         reverse ∘ forward = id_{Modular<2>}.  The two laws
   *         together imply the iso is genuine; the static_assert
   *         IsIsomorphism only requires the inverse() hook to exist
   *         with the right Domain/Codomain types, so this runtime
   *         check is the engineer's honesty obligation for the
   *         actual round-trip identity. */
  connector forward{};
  connector_inv reverse{};

  // Modular<2> → bool → Modular<2>
  CHECK(reverse(forward(Modular<2>{0})) == Modular<2>{0});
  CHECK(reverse(forward(Modular<2>{1})) == Modular<2>{1});

  // bool → Modular<2> → bool
  CHECK(forward(reverse(false)) == false);
  CHECK(forward(reverse(true)) == true);
}
