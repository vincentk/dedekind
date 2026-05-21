/** @file dedekind/sequences/series_concepts_test.cpp
 *
 * Unit coverage for the series concepts (#719 Slice 4): @c IsSeries,
 * @c IsSummable, @c IsAbsolutelyConvergent, and the @c partial_sums
 * primitive.
 *
 * A series Σaₙ is a sequence carrying the additive partial-sum
 * structure; its convergence is the convergence of the partial-sum
 * sequence Sₙ = Σ_{k≤n} aₖ.  The concepts gate on the additive shape of
 * the carrier (load-bearing) plus opt-in traits (the engineer's honesty
 * obligation for actual convergence + carrier completeness).
 *
 * Coverage:
 *  - IsSeries fires on an additive carrier (double), rejects a
 *    non-additive carrier.
 *  - partial_sums materialises Sₙ correctly (geometric series).
 *  - IsSummable fires on a tagged opt-in witness; rejects without opt-in.
 *  - IsAbsolutelyConvergent refines IsSummable: a witness opting into
 *    BOTH traits fires; a witness opting only into absolute-convergence
 *    (not summable) is honestly rejected — the refinement is load-bearing.
 */

#include <catch2/catch_test_macros.hpp>
#include <cstddef>

import dedekind.sequences;

using namespace dedekind::sequences;

namespace series_witnesses {

struct no_add {};  // no operator+ : not a series carrier

/** @brief Tagged term-path opting into summability only. */
struct summable_path : Path<double> {
  using Path<double>::Path;
};
/** @brief Tagged term-path opting into BOTH summable + abs-convergent. */
struct abs_convergent_path : Path<double> {
  using Path<double>::Path;
};
/** @brief Tagged term-path opting into abs-convergent ONLY (no summable):
 *         must be rejected by IsAbsolutelyConvergent (refines IsSummable). */
struct abs_only_path : Path<double> {
  using Path<double>::Path;
};

}  // namespace series_witnesses

namespace dedekind::sequences {
template <>
inline constexpr bool is_summable_v<series_witnesses::summable_path> = true;

template <>
inline constexpr bool is_summable_v<series_witnesses::abs_convergent_path> =
    true;
template <>
inline constexpr bool
    is_absolutely_convergent_v<series_witnesses::abs_convergent_path> = true;

template <>
inline constexpr bool
    is_absolutely_convergent_v<series_witnesses::abs_only_path> = true;
}  // namespace dedekind::sequences

TEST_CASE("sequences:series — IsSeries gates on the additive carrier shape",
          "[sequences][series]") {
  STATIC_CHECK(IsSeries<Path<double>>);
  STATIC_CHECK(IsSeries<Path<int>>);
  // A carrier with no operator+ cannot host a series.
  STATIC_CHECK_FALSE(IsSeries<Path<series_witnesses::no_add>>);
}

TEST_CASE("sequences:series — partial_sums materialises Sₙ = Σ_{k≤n} aₖ",
          "[sequences][series][partial-sums]") {
  // Geometric series with ratio 1/2: aₙ = (1/2)ⁿ, Sₙ = 2 - (1/2)ⁿ.
  const auto terms = geometric_series_terms<double>(0.5);
  const auto sums = partial_sums(terms);
  STATIC_CHECK(IsSequence<decltype(sums)>);
  REQUIRE(sums.at(0) == 1.0);    // S₀ = 1
  REQUIRE(sums.at(1) == 1.5);    // S₁ = 1 + 1/2
  REQUIRE(sums.at(2) == 1.75);   // S₂ = 1 + 1/2 + 1/4
  REQUIRE(sums.at(3) == 1.875);  // S₃
}

TEST_CASE("sequences:series — IsSummable fires on the tagged opt-in witness",
          "[sequences][series][summable]") {
  STATIC_CHECK(IsSummable<series_witnesses::summable_path>);
  // No opt-in ⇒ honest reject (a plain double series is not claimed
  // summable just because the carrier is additive).
  STATIC_CHECK_FALSE(IsSummable<Path<double>>);
}

TEST_CASE("sequences:series — IsAbsolutelyConvergent refines IsSummable",
          "[sequences][series][absolute]") {
  // Opts into both traits ⇒ fires.
  STATIC_CHECK(IsAbsolutelyConvergent<series_witnesses::abs_convergent_path>);
  // Opts into absolute-convergence ONLY (not summable) ⇒ rejected: the
  // refinement IsAbsolutelyConvergent ⊑ IsSummable is load-bearing
  // (absolute convergence implies convergence — the witness must carry
  // both, encoding the Banach-space implication as the obligation).
  STATIC_CHECK_FALSE(IsAbsolutelyConvergent<series_witnesses::abs_only_path>);
  STATIC_CHECK_FALSE(IsSummable<series_witnesses::abs_only_path>);
}
