/** @file dedekind/morphologies/orbit_bridge_test.cpp
 *
 * Unit coverage for the orbit bridge (#719 Slice 1b): carrier-axis
 * periodicity (@c is_periodic_v in @c :species) propagates to
 * sequence-axis periodicity (@c is_periodic_sequence_v in
 * @c :sequences::convergence) via the @c OrbitSequence<T, Op>
 * construction, with the period read off @c cyclic_order_v.
 *
 * This makes the two @c is_periodic notions provably one: a periodic
 * carrier's orbit sequence is a periodic sequence with the same
 * period.  Witnessed at the canonical cyclic ring @c Modular<6>
 * (cyclic order 6 under @c +).
 *
 * Test lives in the morphologies test dir because it needs both
 * @c :sequences (for OrbitSequence / is_periodic_sequence_v) and
 * @c :morphologies (for Modular<N> and its cyclic_order_v
 * specialisation).
 */

#include <catch2/catch_test_macros.hpp>

#include <functional>

import dedekind.category;
import dedekind.morphologies;
import dedekind.sequences;

using namespace dedekind::sequences;
using dedekind::category::cyclic_order_v;
using dedekind::category::is_periodic_v;
using dedekind::morphologies::Modular;

using Z6 = Modular<6>;
using Plus6 = std::plus<Z6>;
using Orbit6 = OrbitSequence<Z6, Plus6>;

TEST_CASE(
    "morphologies:orbit-bridge — Modular<6> is a periodic carrier of order 6",
    "[morphologies][orbit][periodic][carrier]") {
  /** @brief The carrier-axis premise: Modular<6> is periodic under +
   *         with cyclic order 6.  These are the inputs the orbit
   *         bridge consumes. */
  STATIC_CHECK(is_periodic_v<Z6, Plus6>);
  STATIC_CHECK(cyclic_order_v<Z6, Plus6> == 6);
}

TEST_CASE(
    "morphologies:orbit-bridge — the orbit of Modular<6> is a period-6 sequence",
    "[morphologies][orbit][periodic][bridge][crown]") {
  /** @brief The bridge: OrbitSequence<Modular<6>, +> is a period-6
   *         sequence.  is_periodic_v(carrier) ⇒ is_periodic_sequence_v
   *         (orbit), with the period read off cyclic_order_v.  The two
   *         is_periodic notions are now provably one. */
  STATIC_CHECK(IsSequence<Orbit6>);
  STATIC_CHECK(IsPeriodicSequence<Orbit6, 6>);

  // The period is exactly the cyclic order — wrong periods reject.
  STATIC_CHECK_FALSE(IsPeriodicSequence<Orbit6, 5>);
  STATIC_CHECK_FALSE(IsPeriodicSequence<Orbit6, 0>);  // degenerate guard
}

TEST_CASE(
    "morphologies:orbit-bridge — runtime: the orbit cycles with period 6",
    "[morphologies][orbit][runtime]") {
  /** @brief Runtime exercise of the OrbitSequence body — the orbit
   *         values cycle with period 6.  Confirms the type-level
   *         periodicity claim is backed by the actual orbit. */
  Orbit6 orbit{Z6{1}, Plus6{}};

  // The orbit values repeat every 6 steps (the carrier's cyclic order).
  for (std::size_t n = 0; n < 6; ++n) {
    CHECK(orbit.at(n) == orbit.at(n + 6));
    CHECK(orbit.at(n) == orbit.at(n + 12));
  }
}
