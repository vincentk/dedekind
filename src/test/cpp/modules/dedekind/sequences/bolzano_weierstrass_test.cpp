/** @file dedekind/sequences/bolzano_weierstrass_test.cpp
 *
 * The crown of the sequences categorification (#719 Slice 5): the
 * logical-collapse upgrade @c IsClassicallyConvergent and the
 * Bolzano–Weierstrass witness @c WitnessesBolzanoWeierstrass, exhibited
 * as an @b existence demonstration with the LEM gate visible at the type
 * level.
 *
 * The Cauchy⇒convergent collapse, and hence Bolzano–Weierstrass
 * ("every bounded sequence has a convergent subsequence"), is classically
 * true but constructively blocked (Specker).  The carrier-axis
 * cardinality cut (NaturalLogic) decides the regime:
 *
 *   - countable carrier (cardinality_type = ℵ_0) → ClassicalLogic → the
 *     collapse fires, BW witness holds;
 *   - Ternary carrier (a non-cardinality'd primitive like double — the
 *     float↔ℝ gap) → TernaryLogic → the collapse is honestly rejected,
 *     BW witness fails.
 *
 * This is an existence demonstration, NOT a guarantee: it exhibits one
 * (bounded Sup, convergent Sub) pair that witnesses BW classically, and
 * the dual Ternary pair that is honestly rejected.
 */

#include <catch2/catch_test_macros.hpp>
#include <type_traits>

import dedekind.sequences;
import dedekind.sets;
import dedekind.category;

using namespace dedekind::sequences;

namespace bw_witnesses {

/** @brief A countable real-ish carrier: cardinality_type = ℵ_0 places it
 *         in the ClassicalLogic regime; operator- gives it the Cauchy
 *         subtraction shape. */
struct CountableReal {
  using cardinality_type = dedekind::sets::ℵ_0;
  double v = 0.0;
  friend constexpr CountableReal operator-(CountableReal a, CountableReal b) {
    return CountableReal{a.v - b.v};
  }
};

// --- Classical side: countable carrier ---
struct bw_super : Path<CountableReal> {
  using Path<CountableReal>::Path;
};
struct bw_sub : Path<CountableReal> {
  using Path<CountableReal>::Path;
};

// --- Ternary side: double (no cardinality_type ⇒ TernaryLogic) ---
struct bw_super_ternary : Path<double> {
  using Path<double>::Path;
};
struct bw_sub_ternary : Path<double> {
  using Path<double>::Path;
};

}  // namespace bw_witnesses

namespace dedekind::sequences {
// Sup is order-bounded on both sides…
template <>
inline constexpr bool is_bounded_sequence_v<bw_witnesses::bw_super> = true;
template <>
inline constexpr bool is_bounded_sequence_v<bw_witnesses::bw_super_ternary> =
    true;
// …and Sub is a subsequence of Sup on both sides.
template <>
inline constexpr bool
    is_subsequence_v<bw_witnesses::bw_sub, bw_witnesses::bw_super> = true;
template <>
inline constexpr bool is_subsequence_v<bw_witnesses::bw_sub_ternary,
                                       bw_witnesses::bw_super_ternary> = true;
}  // namespace dedekind::sequences

TEST_CASE(
    "sequences:collapse — convergence_logic reads the carrier regime off "
    "the cardinality cut",
    "[sequences][convergence][collapse]") {
  STATIC_CHECK(
      std::is_same_v<convergence_logic<Path<bw_witnesses::CountableReal>>,
                     dedekind::category::ClassicalLogic>);
  // double has no cardinality_type ⇒ NaturalLogic defaults to Ternary.
  STATIC_CHECK(std::is_same_v<convergence_logic<Path<double>>,
                              dedekind::category::TernaryLogic>);
}

TEST_CASE(
    "sequences:collapse — IsClassicallyConvergent fires only in the "
    "classical regime (Specker honest-rejection under Ternary)",
    "[sequences][convergence][collapse]") {
  // Countable carrier ⇒ classical regime ⇒ the Cauchy⇒convergent collapse
  // fires.
  STATIC_CHECK(IsClassicallyConvergent<Path<bw_witnesses::CountableReal>>);
  // double is Cauchy-shaped but sits in the Ternary regime, so the
  // collapse is honestly rejected even though the subtraction shape holds.
  STATIC_CHECK(IsCauchySequence<Path<double>>);
  STATIC_CHECK_FALSE(IsClassicallyConvergent<Path<double>>);
}

TEST_CASE("sequences:crown — Bolzano–Weierstrass witness holds classically",
          "[sequences][convergence][bolzano-weierstrass]") {
  // Bounded Sup + subsequence Sub + Sub classically convergent ⇒ the
  // (Sub, Sup) pair witnesses Bolzano–Weierstrass.  Existence
  // demonstration over a countable carrier.
  STATIC_CHECK(WitnessesBolzanoWeierstrass<bw_witnesses::bw_sub,
                                           bw_witnesses::bw_super>);
}

TEST_CASE(
    "sequences:crown — Bolzano–Weierstrass is honestly rejected under "
    "the Ternary regime (the LEM gate is load-bearing)",
    "[sequences][convergence][bolzano-weierstrass][honest-rejection]") {
  // Same shape (bounded Sup, subsequence Sub) but over a Ternary carrier:
  // the convergent-subsequence collapse is Specker-blocked, so the witness
  // fails — BW is exhibited as a logical-collapse theorem, not a guarantee.
  STATIC_CHECK(IsBoundedSequence<bw_witnesses::bw_super_ternary>);
  STATIC_CHECK(IsSubsequence<bw_witnesses::bw_sub_ternary,
                             bw_witnesses::bw_super_ternary>);
  STATIC_CHECK_FALSE(
      WitnessesBolzanoWeierstrass<bw_witnesses::bw_sub_ternary,
                                  bw_witnesses::bw_super_ternary>);
}

TEST_CASE("sequences:crown — the boundedness gate is load-bearing",
          "[sequences][convergence][bolzano-weierstrass]") {
  // A classically-convergent subsequence of an UNbounded super does not
  // witness BW: with no is_bounded_sequence_v opt-in on Path<CountableReal>,
  // the Sup leg fails.  (bw_sub is a registered subsequence of bw_super,
  // not of a bare Path, so we check the bounded gate directly.)
  STATIC_CHECK_FALSE(IsBoundedSequence<Path<bw_witnesses::CountableReal>>);
  STATIC_CHECK(IsClassicallyConvergent<bw_witnesses::bw_sub>);
}
