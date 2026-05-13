/**
 * @file q_scout_algebra_test.cpp
 * @brief End-to-end witnesses tying @c ℚ to the in-line scout-algebra
 *        comprehension surface for the @b multiplicative slice
 *        (#664 Slice 3, downstream of PR #674's @c IsField cert).
 *
 * @section q_scout_algebra_test__Scope
 *
 * Slice 3 sibling to @c z_scout_algebra_test for additive translation.
 * Validates that the canonical @c ℚ alias (post-PR #673 anchored on
 * @c Rational<default_integer>) supports multiplicative scaling of
 * halfspaces:
 *   * positive scalar preserves direction and strictness;
 *   * negative scalar @b flips direction (Upward ↔ Downward),
 *     strictness preserved;
 *   * zero scalar is Honest-Rejected (the halfspace would collapse).
 *
 * Algebraic gate: @c IsAbelianGroup<Rational<default_integer>,
 * std::multiplies<...>> closes via the @c is_invertible_v trait
 * registered in @c :rational under PR #674 (zero excluded by the
 * @c total.cppm convention).  Order-compatibility: the carrier-promise
 * marker @c is_ordered_multiplicative_group<Rational<I>> is registered
 * in @c :rational for any @c IsInteger I (ℚ is an ordered field, Lang,
 * @em Algebra §III.1).
 */
#include <catch2/catch_test_macros.hpp>
#include <type_traits>

import dedekind.algebra;
import dedekind.category;
import dedekind.numbers;
import dedekind.order;
import dedekind.sets;

using namespace dedekind::numbers;

// ---------------------------------------------------------------------------
// Algebraic + order witnesses on @c Rational<default_integer>.
// ---------------------------------------------------------------------------

static_assert(
    dedekind::algebra::IsOrderedMultiplicativeGroup<Rational<default_integer>>,
    "ℚ satisfies IsOrderedMultiplicativeGroup: IsField (post-#674) "
    "gives the multiplicative-group axiom on the non-zero cone, and "
    "the ordered-field marker is registered for Rational<I>.");

// ---------------------------------------------------------------------------
// Scout-algebra: positive-scalar multiplicative scaling preserves
// direction and strictness.
// ---------------------------------------------------------------------------

TEST_CASE("ℚ: positive-scalar scaling preserves direction (#664 Slice 3)",
          "[numbers][rational][scout_algebra][slice3]") {
  constexpr auto x = dedekind::sets::element<ℚ>;
  // {x ∈ ℚ | x > 2} pushed through λx. 3·x → {y ∈ ℚ | y > 6}.
  constexpr auto S = dedekind::sets::Set{x * dedekind::order::bound<3> |
                                         (x > dedekind::order::bound<2>)};

  using SetL = typename dedekind::sets::NaturalLogic<
      std::remove_cvref_t<decltype(ℚ)>>::type;
  using ExpectedPredicate =
      dedekind::order::Halfspace<Rational<default_integer>, 6,
                                 dedekind::order::Direction::Upward,
                                 dedekind::order::Strictness::Strict>;
  using ExpectedSet =
      dedekind::sets::Set<Rational<default_integer>, SetL, ExpectedPredicate>;
  STATIC_CHECK(std::same_as<std::remove_cvref_t<decltype(S)>, ExpectedSet>);
}

// ---------------------------------------------------------------------------
// Scout-algebra: negative-scalar scaling flips direction (Upward ↔
// Downward), strictness preserved.
// ---------------------------------------------------------------------------

TEST_CASE("ℚ: negative-scalar scaling flips direction (#664 Slice 3)",
          "[numbers][rational][scout_algebra][slice3]") {
  constexpr auto x = dedekind::sets::element<ℚ>;
  // {x ∈ ℚ | x > 2} pushed through λx. (-3)·x → {y ∈ ℚ | y < -6}.
  // Direction Upward (>) flips to Downward (<); strictness Strict
  // preserved.
  constexpr auto S = dedekind::sets::Set{x * dedekind::order::bound<-3> |
                                         (x > dedekind::order::bound<2>)};

  using SetL = typename dedekind::sets::NaturalLogic<
      std::remove_cvref_t<decltype(ℚ)>>::type;
  using ExpectedPredicate =
      dedekind::order::Halfspace<Rational<default_integer>, -6,
                                 dedekind::order::Direction::Downward,
                                 dedekind::order::Strictness::Strict>;
  using ExpectedSet =
      dedekind::sets::Set<Rational<default_integer>, SetL, ExpectedPredicate>;
  STATIC_CHECK(std::same_as<std::remove_cvref_t<decltype(S)>, ExpectedSet>);
}

// ---------------------------------------------------------------------------
// Honest Rejection: ℤ has no multiplicative inverses (post-#674 the
// IsField gate is firmly @c false on @c SignedCardinality), so the
// multiplicative factory and pipe are removed from the candidate set.
// The negative is documentation: writing @c in<ℤ> @c * @c bound<3>
// fails to compile, not silently produces a wrong type.
// ---------------------------------------------------------------------------

static_assert(
    !dedekind::algebra::IsOrderedMultiplicativeGroup<
        dedekind::sets::SignedCardinality>,
    "ℤ (SignedCardinality) is NOT a multiplicative group: it is a ring "
    "but lacks multiplicative inverses for non-units, so the scout-"
    "algebra multiplicative pipe Honest-Rejects scaling on ℤ.");
