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
 * Algebraic gate: @c IsOrderedMultiplicativeGroup<T> composes
 * upstream concepts directly --- @c algebra::IsField<T> (closes on
 * @c Rational<default_integer> via the @c is_invertible_v trait
 * registered in @c :rational under PR #674; zero excluded by the
 * @c total.cppm convention) and @c order::IsTotallyOrdered<T>
 * (closes via @c Rational 's @c <=> returning @c std::strong_ordering).
 * No carrier-promise marker is needed on the multiplicative side
 * (asymmetric with the additive sibling, which keeps
 * @c is_translation_invariant_ordered to admit the variant ℤ proxy
 * @c SignedCardinality).
 */
#include <catch2/catch_test_macros.hpp>
#include <type_traits>

import dedekind.algebra;
import dedekind.category;
import dedekind.numbers;
import dedekind.order;
import dedekind.sets;

using namespace dedekind::numbers;

// Algebraic witnesses moved upstream per the static_assert-in-main
// pattern:
//   * @c IsOrderedMultiplicativeGroup<Rational<default_integer>> ---
//     pinned at @c rational.cppm.
//   * @c !IsOrderedMultiplicativeGroup<SignedCardinality> ---
//     pinned at @c integer.cppm (alongside @c IsInitialRing and
//     @c IsGrothendieckGroup, completing the algebraic identity of ℤ).
// The TEST_CASEs below exercise the actual pipe behaviour (positive
// scaling, direction-flip on negative scalar, @c k_E = 0 rejection).

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
// Scout-algebra: affine composition `(m * x) + b` (#664 Slice 4).
// The canonical affine form: inner multiplicative scale, outer
// additive shift.  Halfspace transport applies the inner pipe first,
// then the outer pipe on the result.
// ---------------------------------------------------------------------------

TEST_CASE(
    "ℚ: affine composition (m * x) + b transports correctly (#664 Slice 4)",
    "[numbers][rational][scout_algebra][slice4]") {
  constexpr auto x = dedekind::sets::element<ℚ>;
  // {x ∈ ℚ | x > 5} pushed through λx. 2·x + 1.
  //   Inner *2:  pivot 5 → 10, direction Upward preserved.
  //   Outer +1:  pivot 10 → 11, direction Upward preserved.
  // Final: {y ∈ ℚ | y > 11}.
  constexpr auto S = dedekind::sets::Set{x * dedekind::order::bound<2> +
                                             dedekind::order::bound<1> |
                                         (x > dedekind::order::bound<5>)};

  using SetL = typename dedekind::sets::NaturalLogic<
      std::remove_cvref_t<decltype(ℚ)>>::type;
  using ExpectedPredicate =
      dedekind::order::Halfspace<Rational<default_integer>, 11,
                                 dedekind::order::Direction::Upward,
                                 dedekind::order::Strictness::Strict>;
  using ExpectedSet =
      dedekind::sets::Set<Rational<default_integer>, SetL, ExpectedPredicate>;
  STATIC_CHECK(std::same_as<std::remove_cvref_t<decltype(S)>, ExpectedSet>);
}

TEST_CASE(
    "ℚ: affine composition (x + b) * m transports correctly (#664 Slice 4)",
    "[numbers][rational][scout_algebra][slice4]") {
  constexpr auto x = dedekind::sets::element<ℚ>;
  // {x ∈ ℚ | x > 5} pushed through λx. 2·(x + 3) = 2x + 6.
  //   Inner +3:  pivot 5 → 8, direction Upward preserved.
  //   Outer *2:  pivot 8 → 16, direction Upward preserved.
  // Final: {y ∈ ℚ | y > 16}.
  constexpr auto S = dedekind::sets::Set{
      (x + dedekind::order::bound<3>)*dedekind::order::bound<2> |
      (x > dedekind::order::bound<5>)};

  using SetL = typename dedekind::sets::NaturalLogic<
      std::remove_cvref_t<decltype(ℚ)>>::type;
  using ExpectedPredicate =
      dedekind::order::Halfspace<Rational<default_integer>, 16,
                                 dedekind::order::Direction::Upward,
                                 dedekind::order::Strictness::Strict>;
  using ExpectedSet =
      dedekind::sets::Set<Rational<default_integer>, SetL, ExpectedPredicate>;
  STATIC_CHECK(std::same_as<std::remove_cvref_t<decltype(S)>, ExpectedSet>);
}

TEST_CASE(
    "ℚ: 3-layer composition ((x + a) * b) + c transports correctly "
    "(#664 Slice 4)",
    "[numbers][rational][scout_algebra][slice4]") {
  constexpr auto x = dedekind::sets::element<ℚ>;
  // {x ∈ ℚ | x > 5} pushed through λx. ((x + 1) * 2) + 3 = 2x + 5.
  //   Innermost +1: pivot 5 → 6.
  //   Middle *2:   pivot 6 → 12.
  //   Outermost +3: pivot 12 → 15.
  // The recursive composition pipe handles depth >2 automatically ---
  // each layer's pipe calls Inner{} | hs which recurses down to the
  // BoundScout base case.
  constexpr auto S = dedekind::sets::Set{
      ((x + dedekind::order::bound<1>)*dedekind::order::bound<
          2>)+dedekind::order::bound<3> |
      (x > dedekind::order::bound<5>)};

  using SetL = typename dedekind::sets::NaturalLogic<
      std::remove_cvref_t<decltype(ℚ)>>::type;
  using ExpectedPredicate =
      dedekind::order::Halfspace<Rational<default_integer>, 15,
                                 dedekind::order::Direction::Upward,
                                 dedekind::order::Strictness::Strict>;
  using ExpectedSet =
      dedekind::sets::Set<Rational<default_integer>, SetL, ExpectedPredicate>;
  STATIC_CHECK(std::same_as<std::remove_cvref_t<decltype(S)>, ExpectedSet>);
}

TEST_CASE(
    "ℚ: affine composition with negative outer scaling flips direction "
    "(#664 Slice 4)",
    "[numbers][rational][scout_algebra][slice4]") {
  constexpr auto x = dedekind::sets::element<ℚ>;
  // {x ∈ ℚ | x > 5} pushed through λx. (-2)·(x + 3) = -2x - 6.
  //   Inner +3:  pivot 5 → 8, direction Upward.
  //   Outer *(-2): pivot 8 → -16, direction FLIPPED to Downward.
  // Final: {y ∈ ℚ | y < -16}.
  constexpr auto S = dedekind::sets::Set{
      (x + dedekind::order::bound<3>)*dedekind::order::bound<-2> |
      (x > dedekind::order::bound<5>)};

  using SetL = typename dedekind::sets::NaturalLogic<
      std::remove_cvref_t<decltype(ℚ)>>::type;
  using ExpectedPredicate =
      dedekind::order::Halfspace<Rational<default_integer>, -16,
                                 dedekind::order::Direction::Downward,
                                 dedekind::order::Strictness::Strict>;
  using ExpectedSet =
      dedekind::sets::Set<Rational<default_integer>, SetL, ExpectedPredicate>;
  STATIC_CHECK(std::same_as<std::remove_cvref_t<decltype(S)>, ExpectedSet>);
}

// ---------------------------------------------------------------------------
// Honest Rejection: @c k_E = 0 scaling.  Scaling by zero collapses the
// scout function to the constant map @c x @c ↦ @c 0; the image is the
// singleton @c {0} (or @c ∅), not a halfspace, so the pivot-transport
// pattern doesn't apply.  The pipe's @c Element != zero gate refuses
// the comprehension at compile time.  Wrapped in a concept so the
// rejection is observed via concept-satisfaction rather than as a
// hard error from operator-overload resolution at namespace scope.
// ---------------------------------------------------------------------------

template <typename Scout, typename HS>
concept HasScalingPipe = requires(const Scout& s, const HS& h) { s | h; };

static_assert(
    !HasScalingPipe<
        decltype(dedekind::sets::element<ℚ> * dedekind::order::bound<0>),
        dedekind::order::Halfspace<Rational<default_integer>, 2,
                                   dedekind::order::Direction::Upward,
                                   dedekind::order::Strictness::Strict>>,
    "k_E = 0 scaling is Honest-Rejected: scaling by zero is not a "
    "halfspace-pivot transport (the image is the singleton {0} or ∅, "
    "not a halfspace).");
