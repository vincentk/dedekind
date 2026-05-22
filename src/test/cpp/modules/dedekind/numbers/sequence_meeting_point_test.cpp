/** @file dedekind/numbers/sequence_meeting_point_test.cpp
 *
 * The sequence categorification meeting-point test (#719 Slice 6 — final
 * slice of the sequence Form-chain epic, closing #719).  Mirrors #718's
 * quotient meeting-point: a family of canonical sequence carriers each
 * exercises the rows it honestly supports, jointly populating the
 * Form-chain from @c IsSequence through the Bolzano–Weierstrass crown.
 *
 * The canonical carriers:
 *
 *   1. @c Path<double>                      — the workhorse analytic
 *      sequence: IsSequence, IsCauchySequence, IsSeries, IsStreamComonad.
 *      Sits in the Ternary regime (float↔ℝ gap) ⇒ IsClassicallyConvergent
 *      honestly rejects.
 *   2. Mandelbrot orbit + @c DivergencePath — the dynamical-systems leg:
 *      @c mandelbrot_orbit(c) is an IsSequence; its escape-indicator
 *      @c DivergencePath is an IsAbsorptiveSequence (eventually constant).
 *   3. @c OrbitSequence<Modular<6>, +>      — the group-orbit / periodic
 *      leg: the orbit bridge lifts the carrier's cyclic order 6 to
 *      IsPeriodicSequence<…, 6>.
 *   4. a countable carrier (cardinality_type = ℵ_0) — the classical
 *      logical-collapse crown: IsClassicallyConvergent fires and a
 *      (bounded Sup, subsequence Sub) pair witnesses Bolzano–Weierstrass.
 *   5. @c ExactReal (ℝ)                     — the honest-rejection foil:
 *      Cauchy-shaped but in the Ternary / continuum regime, so the
 *      Cauchy⇒convergent collapse (and hence BW) is Specker-blocked —
 *      exactly the carrier on which BW is classically famous.
 *
 * No carrier fires every row; the family jointly populates the chain. A
 * regression in any concept body or trait registration fails one leg here.
 */

#include <catch2/catch_test_macros.hpp>
#include <functional>
#include <type_traits>

import dedekind.sequences;
import dedekind.morphologies;
import dedekind.numbers;
import dedekind.category;
import dedekind.sets;

using namespace dedekind::sequences;
using dedekind::morphologies::Modular;
using dedekind::numbers::Complex;
using dedekind::numbers::DivergencePath;
using dedekind::numbers::ExactReal;
using dedekind::numbers::mandelbrot_orbit;

namespace seq_meet {

/** @brief A countable real-ish carrier (cardinality_type = ℵ_0 ⇒
 *         ClassicalLogic regime; operator- gives the Cauchy shape). */
struct CountableReal {
  using cardinality_type = dedekind::sets::ℵ_0;
  double v = 0.0;
  friend constexpr CountableReal operator-(CountableReal a, CountableReal b) {
    return CountableReal{a.v - b.v};
  }
};

// Bolzano–Weierstrass witness pair over the countable carrier.
struct bw_super : Path<CountableReal> {
  using Path<CountableReal>::Path;
};
struct bw_sub : Path<CountableReal> {
  using Path<CountableReal>::Path;
};

}  // namespace seq_meet

namespace dedekind::sequences {
template <>
inline constexpr bool is_bounded_sequence_v<seq_meet::bw_super> = true;
template <>
inline constexpr bool is_subsequence_v<seq_meet::bw_sub, seq_meet::bw_super> =
    true;
}  // namespace dedekind::sequences

TEST_CASE("sequence meeting-point — Path<double> fires the analytic rows",
          "[sequences][meeting-point][analytic]") {
  /** @brief The workhorse: IsSequence (row 4), IsCauchySequence (row 5),
   *         IsSeries (row 6), IsStreamComonad (row 7).  Ternary regime ⇒
   *         IsClassicallyConvergent honestly rejects. */
  STATIC_CHECK(IsSequence<Path<double>>);
  STATIC_CHECK(IsCauchySequence<Path<double>>);
  STATIC_CHECK(IsSeries<Path<double>>);
  STATIC_CHECK(IsStreamComonad<Path<double>>);
  STATIC_CHECK_FALSE(IsClassicallyConvergent<Path<double>>);  // float↔ℝ gap
}

TEST_CASE(
    "sequence meeting-point — Mandelbrot orbit + DivergencePath "
    "(dynamical-systems leg)",
    "[sequences][meeting-point][mandelbrot]") {
  const auto orbit = mandelbrot_orbit(Complex<double>{0.0, 0.0});
  STATIC_CHECK(IsSequence<std::remove_cvref_t<decltype(orbit)>>);
  // The escape indicator is eventually constant ⇒ absorptive (row 5).
  STATIC_CHECK(IsAbsorptiveSequence<DivergencePath<double>>);
}

TEST_CASE(
    "sequence meeting-point — OrbitSequence<Modular<6>, +> (orbit bridge)",
    "[sequences][meeting-point][orbit][periodic]") {
  /** @brief The group-orbit leg: the orbit bridge lifts Modular<6>'s
   *         cyclic order 6 to IsPeriodicSequence<…, 6> (row 5). */
  using Z6 = Modular<6>;
  STATIC_CHECK(IsPeriodicSequence<OrbitSequence<Z6, std::plus<Z6>>, 6>);
}

TEST_CASE(
    "sequence meeting-point — countable carrier fires the classical "
    "collapse + Bolzano–Weierstrass crown",
    "[sequences][meeting-point][crown]") {
  /** @brief The crown (row 7): a countable carrier sits in the
   *         ClassicalLogic regime, so IsClassicallyConvergent fires and a
   *         (bounded Sup, subsequence Sub) pair witnesses
   *         Bolzano–Weierstrass. */
  STATIC_CHECK(std::is_same_v<convergence_logic<Path<seq_meet::CountableReal>>,
                              dedekind::category::ClassicalLogic>);
  STATIC_CHECK(IsClassicallyConvergent<Path<seq_meet::CountableReal>>);
  STATIC_CHECK(
      WitnessesBolzanoWeierstrass<seq_meet::bw_sub, seq_meet::bw_super>);
}

TEST_CASE("sequence meeting-point — ExactReal (ℝ) is the honest-rejection foil",
          "[sequences][meeting-point][continuum][honest-rejection]") {
  /** @brief ℝ is Cauchy-shaped but sits in the Ternary / continuum
   *         regime, so the Cauchy⇒convergent collapse is Specker-blocked —
   *         BW is classically famous on ℝ yet constructively rejected. */
  using R = ExactReal<>;
  STATIC_CHECK(IsCauchySequence<Path<R>>);
  STATIC_CHECK(std::is_same_v<convergence_logic<Path<R>>,
                              dedekind::category::TernaryLogic>);
  STATIC_CHECK_FALSE(IsClassicallyConvergent<Path<R>>);
}

TEST_CASE(
    "sequence meeting-point — the carrier family jointly populates the "
    "sequence Form-chain (crown)",
    "[sequences][meeting-point][form-chain][crown]") {
  /** @brief The meeting-point crown: the canonical sequence carriers
   *         exercising the Form-chain together.  No carrier fires every
   *         row, but the family jointly populates IsSequence,
   *         IsCauchySequence, the shape concepts (absorptive / periodic),
   *         IsSeries, IsStreamComonad, the classical collapse, and the
   *         Bolzano–Weierstrass crown — with ℝ as the honest-rejection
   *         foil that keeps the LEM gate load-bearing. */
  using Z6 = Modular<6>;
  using R = ExactReal<>;

  // Row 4 (IsSequence) — analytic leg
  STATIC_CHECK(IsSequence<Path<double>>);
  // Row 5 (absorptive) — Mandelbrot leg
  STATIC_CHECK(IsAbsorptiveSequence<DivergencePath<double>>);
  // Row 5 (periodic) — orbit-bridge leg
  STATIC_CHECK(IsPeriodicSequence<OrbitSequence<Z6, std::plus<Z6>>, 6>);
  // Row 6 (series) — analytic leg
  STATIC_CHECK(IsSeries<Path<double>>);
  // Row 7 (stream comonad) — analytic leg
  STATIC_CHECK(IsStreamComonad<Path<double>>);
  // Row 7 (classical collapse + BW) — countable leg
  STATIC_CHECK(IsClassicallyConvergent<Path<seq_meet::CountableReal>>);
  STATIC_CHECK(
      WitnessesBolzanoWeierstrass<seq_meet::bw_sub, seq_meet::bw_super>);
  // Honest-rejection foil — continuum leg
  STATIC_CHECK_FALSE(IsClassicallyConvergent<Path<R>>);
}
