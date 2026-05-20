/** @file dedekind/morphologies/cauchy_sequence_concepts_test.cpp
 *
 * Unit coverage for the re-homed sequence-side convergence concepts
 * (#719 Slice 0): @c IsCauchySequence and @c IsConvergentSequence
 * now live in @c :sequences::convergence under the textbook
 * factoring (sequence-side concepts in the sequence partition;
 * carrier-side prerequisites in @c :order::completeness for
 * @c IsArchimedean and @c :morphologies::archimedean for the field
 * extensions).
 *
 * Coverage targets:
 *  - @c IsCauchySequence fires on @c CauchyPath<double>.
 *  - @c IsCauchySequence fires on a generic @c Path<double> (the
 *    @c std::abs structural shape is well-formed on @c double).
 *  - Honest-Rejection on @c Path<int>: @c IsConvergentSequence rejects
 *    because no @c limit() overload exists for @c int (the
 *    @c :sequences::limits @c static_assert pins @c !HasLimit<int>).
 *
 * Note: a positive @c IsConvergentSequence<Path<double>> case is
 * deliberately omitted from this slice — Slice 0 is a pure re-home
 * (no semantic change to the concepts); the positive convergent
 * witness is exercised downstream in #719 Slice 5 (logical-collapse
 * upgrades + Bolzano-Weierstrass crown), where the @c limit()
 * resolution paths and the @c ClassicalLogic / @c Ternary gating
 * are set up coherently.
 */

#include <catch2/catch_test_macros.hpp>

import dedekind.morphologies;
import dedekind.sequences;

using namespace dedekind::sequences;
using dedekind::morphologies::CauchyPath;

TEST_CASE(
    "sequences:convergence — IsCauchySequence fires on CauchyPath<double>",
    "[sequences][convergence][cauchy]") {
  /** @brief The canonical Cauchy-tagged path inhabits
   *         @c IsCauchySequence in its new home. */
  STATIC_CHECK(IsCauchySequence<CauchyPath<double>>);
}

TEST_CASE(
    "sequences:convergence — Path<double> satisfies IsCauchySequence "
    "(structural shape: std::abs(diff) is callable on double)",
    "[sequences][convergence][cauchy][positive]") {
  /** @brief @c Path<double> witnesses @c IsCauchySequence: the
   *         structural shape (@c std::abs(s.at(0) - s.at(0))) is
   *         well-formed on @c double.  Metric-completeness is the
   *         engineer's honesty obligation on the carrier side. */
  STATIC_CHECK(IsCauchySequence<Path<double>>);
}

TEST_CASE(
    "sequences:convergence — Honest-Rejection: Path<int> doesn't have a "
    "limit() overload; IsConvergentSequence honestly rejects",
    "[sequences][convergence][honest-rejection]") {
  /** @brief @c Path<int> can satisfy @c IsCauchySequence (the requires
   *         clause asks only that @c std::abs(s.at(0) - s.at(0)) be
   *         well-formed, and @c int supports that), but
   *         @c IsConvergentSequence requires a @c limit() overload.
   *         No such overload exists for @c int (the
   *         @c :sequences::limits @c static_assert pins
   *         @c !HasLimit<int>), so @c IsConvergentSequence honestly
   *         rejects. */
  STATIC_CHECK_FALSE(IsConvergentSequence<Path<int>>);
}
