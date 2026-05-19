/** @file dedekind/sequences/cauchy_sequence_concepts_test.cpp
 *
 * Unit coverage for the re-homed sequence-side convergence concepts
 * (#719 Slice 0): @c IsCauchySequence and @c IsConvergentSequence
 * now live in @c :sequences::convergence under the textbook
 * factoring (sequence-side concepts in the sequence partition;
 * carrier-side prerequisites in @c :morphologies::archimedean).
 *
 * Coverage targets:
 *  - @c IsCauchySequence fires on the canonical @c CauchyPath<double>.
 *  - @c IsConvergentSequence fires on the same carrier (Cauchy +
 *    @c limit() machinery).
 *  - Honest-Rejection on a generic @c Path<int> (no @c std::abs
 *    semantics needed for the type-level check, but @c limit() is
 *    not registered for @c int).
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
