/** @file dedekind/category/boolean_lattice_test.cpp
 *
 * Unit coverage for @c :category::lattice::IsBooleanLatticeCategory
 * (row 7 of the lattice Form-chain, #698 Slice 7).
 *
 * The Boolean lattice refinement layers two terms onto the Heyting
 * structure:
 *
 *   - @c IsInvolutiveEndofunctor<Not, T> — structural (Slice 5).
 *   - @c is_complement_v<Not, T, Rel, Join, Meet> — opt-in semantic
 *     gate for the complement laws (@c x @c ∧ @c ¬x @c = @c ⊥,
 *     @c x @c ∨ @c ¬x @c = @c ⊤), which are value-level and cannot
 *     be discharged at type level.
 *
 * Canonical witness: @c bool under @c std::less_equal with
 * @c std::logical_not<bool>.  Integer carriers under
 * @c std::less_equal with @c std::bit_not are deliberately @b
 * excluded — the bitwise Boolean algebra under a bit-subset relation
 * is tracked under #710 as a separate canonical witness.
 */

#include <algorithm>  // std::ranges::min / max — Join / Meet template args
#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <functional>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE("category:boolean — bool is the canonical 2-element Boolean lattice",
          "[category][lattice][boolean][bool][canonical]") {
  /** @brief @c bool under @c (std::less_equal, max, min, std::logical_not)
   *         participates in @c IsBooleanLatticeCategory — both structural
   *         legs (Heyting + involutiveness) fire, and the canonical
   *         specialisation of @c is_complement_v registers the semantic
   *         gate. */
  STATIC_CHECK(IsBooleanLatticeCategory<bool>);

  /** @brief Value-level witness of the complement laws.  These are not
   *         enforced by the concept body (they're value-level) but the
   *         opt-in registration commits the carrier to upholding them. */
  constexpr std::logical_not<bool> not_op{};
  STATIC_CHECK(std::ranges::min(true, not_op(true)) == false);  // x ∧ ¬x = ⊥
  STATIC_CHECK(std::ranges::max(true, not_op(true)) == true);   // x ∨ ¬x = ⊤
  STATIC_CHECK(std::ranges::min(false, not_op(false)) == false);
  STATIC_CHECK(std::ranges::max(false, not_op(false)) == true);
}

TEST_CASE("category:boolean — is_complement_v is opt-in (no default true)",
          "[category][lattice][boolean][negative][opt-in]") {
  /** @brief The @c is_complement_v trait follows the @c is_involutive_v
   *         opt-in pattern: absence of a specialisation means @c false,
   *         so non-registered pairings fail closed. */
  STATIC_CHECK(
      is_complement_v<std::logical_not<bool>, bool, std::less_equal<bool>,
                      decltype(std::ranges::max), decltype(std::ranges::min)>);

  /** @brief @c int under @c std::less_equal with @c std::bit_not is
   *         @b not a Boolean lattice — the bitwise complement doesn't
   *         match the order-theoretic meet/join.  Witness:
   *         @c min(5, ~5) = @c min(5, -6) = @c -6, not @c INT_MIN. */
  STATIC_CHECK_FALSE(
      is_complement_v<std::bit_not<int>, int, std::less_equal<int>,
                      decltype(std::ranges::max), decltype(std::ranges::min)>);
  STATIC_CHECK_FALSE(
      IsBooleanLatticeCategory<int, std::less_equal<int>,
                               decltype(std::ranges::max),
                               decltype(std::ranges::min), std::bit_not<int>>);
}

TEST_CASE(
    "category:boolean — Form-chain faithful inclusions hold through row 7",
    "[category][lattice][boolean][faithful][form-chain]") {
  /** @brief Every Boolean lattice category is a Heyting lattice, a
   *         bounded lattice, a lattice, filtered, posetal, and thin —
   *         all encoded definitionally through @c IsBooleanLatticeCategory's
   *         signature chain. */
  STATIC_CHECK(IsThinCategory<bool>);
  STATIC_CHECK(IsPosetal<bool>);
  STATIC_CHECK(IsFilteredCategory<bool>);
  STATIC_CHECK(IsLatticeCategory<bool>);
  STATIC_CHECK(IsBoundedLatticeCategory<bool>);
  STATIC_CHECK(IsHeytingLatticeCategory<bool>);
  STATIC_CHECK(IsBooleanLatticeCategory<bool>);
}

TEST_CASE("category:boolean — int participates in rows 1–6 but NOT row 7",
          "[category][lattice][boolean][negative][int][honest-rejection]") {
  /** @brief Honest Rejection at row 7: @c int under
   *         @c std::less_equal IS a totally-ordered Heyting algebra
   *         (a chain — every chain is Heyting), but a chain is Boolean
   *         only if it has exactly two elements (i.e. only @c bool).
   *         @c int participates in rows 1–6 but stops at row 7 under
   *         every available @c Not pairing the opt-in trait hasn't
   *         registered. */
  STATIC_CHECK(IsThinCategory<int>);
  STATIC_CHECK(IsPosetal<int>);
  STATIC_CHECK(IsFilteredCategory<int>);
  STATIC_CHECK(IsLatticeCategory<int>);
  STATIC_CHECK(IsBoundedLatticeCategory<int>);
  STATIC_CHECK(IsHeytingLatticeCategory<int>);

  /** @brief Default @c Not @c = @c std::logical_not<int> — invocable
   *         and structurally returns @c bool (convertible to @c int),
   *         but @c is_involutive_v is @c false (no opt-in), so
   *         @c IsInvolutiveEndofunctor fails. */
  STATIC_CHECK_FALSE(IsBooleanLatticeCategory<int>);

  /** @brief Explicit @c Not @c = @c std::bit_not<int> — involutive, but
   *         the complement laws fail under the order-theoretic
   *         meet/join, so @c is_complement_v is not registered. */
  STATIC_CHECK_FALSE(
      IsBooleanLatticeCategory<int, std::less_equal<int>,
                               decltype(std::ranges::max),
                               decltype(std::ranges::min), std::bit_not<int>>);
}
