/** @file dedekind/sequences/order_convergence_concepts_test.cpp
 *
 * Unit coverage for the order-convergence concept (#719 Slice 3):
 * @c IsOrderConvergent — lattice-theoretic (metric-free) convergence,
 * @c liminf = limsup.
 *
 * The concept gates on three things: @c IsSequence, the carrier being an
 * order @b lattice (both @c order::IsOrderMeetSemilattice and
 * @c order::IsOrderJoinSemilattice — so liminf/limsup are definable),
 * and the opt-in @c is_order_convergent_v trait (the engineer's honesty
 * obligation for completeness + the liminf=limsup equality).
 *
 * Coverage:
 *  - Fires on a tagged lattice-carrier witness that opts in.
 *  - Honestly rejects a plain (non-opted-in) lattice sequence.
 *  - Honestly rejects a non-lattice carrier (std::complex has no
 *    meet/join) EVEN WITH the opt-in — the :order::lattice gate is
 *    load-bearing, not advisory.
 */

#include <catch2/catch_test_macros.hpp>
#include <complex>
#include <cstddef>

import dedekind.sequences;
import dedekind.order;

using namespace dedekind::sequences;

namespace order_conv_witnesses {

/** @brief Tagged toy path over @c int (an order lattice under min/max);
 *         the opt-in below marks it as order-convergent. */
struct convergent_int_path : Path<int> {
  using Path<int>::Path;
};

/** @brief Tagged toy path over @c std::complex<double> — NOT an order
 *         lattice (no operator< ⇒ no min/max meet/join).  Opts in below
 *         to prove the carrier gate overrides the trait. */
struct tagged_complex_path : Path<std::complex<double>> {
  using Path<std::complex<double>>::Path;
};

}  // namespace order_conv_witnesses

namespace dedekind::sequences {
template <>
inline constexpr bool
    is_order_convergent_v<order_conv_witnesses::convergent_int_path> = true;
template <>
inline constexpr bool
    is_order_convergent_v<order_conv_witnesses::tagged_complex_path> = true;
}  // namespace dedekind::sequences

TEST_CASE(
    "sequences:order-convergence — IsOrderConvergent fires on the tagged "
    "lattice witness",
    "[sequences][convergence][order]") {
  STATIC_CHECK(IsOrderConvergent<order_conv_witnesses::convergent_int_path>);
  // No opt-in ⇒ honest reject, even though int is a lattice carrier.
  STATIC_CHECK_FALSE(IsOrderConvergent<Path<int>>);
}

TEST_CASE(
    "sequences:order-convergence — the :order::lattice gate is "
    "load-bearing (non-lattice carrier rejected despite opt-in)",
    "[sequences][convergence][order][honest-rejection]") {
  // std::complex<double> has no meet/join, so liminf/limsup are not even
  // definable — IsOrderConvergent rejects regardless of the trait opt-in.
  STATIC_CHECK_FALSE(
      dedekind::order::IsOrderMeetSemilattice<std::complex<double>>);
  STATIC_CHECK_FALSE(
      IsOrderConvergent<order_conv_witnesses::tagged_complex_path>);
}

TEST_CASE(
    "sequences:order-convergence — int is the lattice carrier the "
    "witness relies on",
    "[sequences][convergence][order][lattice]") {
  STATIC_CHECK(dedekind::order::IsOrderMeetSemilattice<int>);
  STATIC_CHECK(dedekind::order::IsOrderJoinSemilattice<int>);
}
