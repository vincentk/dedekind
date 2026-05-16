/** @file dedekind/category/filtered_test.cpp
 *
 * Unit coverage for @c :category::filtered (the lattice-Form-chain row 3,
 * #698 Slice 2).
 *
 * @c IsFilteredCategory<T, Rel, L> reifies "thin category whose every
 * finite subset has an upper bound" as thin + directed.  Faithful
 * inclusion @c IsFilteredCategory ⊊ @c IsThinCategory encoded in the
 * signature per the project's "faithful specialization in the type
 * signature from day one" posture (#698).
 *
 * Witnesses below pin the canonical entry points and the parallel-to-
 * @c :posetal status (filtered does @b not require antisymmetry).
 */

#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <functional>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE(
    "category:filtered — Bool is the canonical 2-element filtered category",
    "[category][filtered][bool][canonical]") {
  /** @brief @c bool with @c std::less_equal is filtered: @c true is the
   *         upper bound of every pair.  Together with reflexivity +
   *         transitivity (already established in @c :thin) this makes
   *         @c bool the smallest non-trivial filtered category. */
  STATIC_CHECK(IsFilteredCategory<bool>);
  STATIC_CHECK(IsFilteredCategory<bool, std::less_equal<bool>, ClassicalLogic>);
}

TEST_CASE("category:filtered — totally ordered integral carriers are filtered",
          "[category][filtered][numeric]") {
  /** @brief Totally ordered carriers under @c std::less_equal are
   *         trivially directed: @c max(a, b) is the upper bound of
   *         every pair, and the @c :species specialisation of
   *         @c is_directed_v on @c std::totally_ordered captures this. */
  STATIC_CHECK(IsFilteredCategory<int>);
  STATIC_CHECK(IsFilteredCategory<unsigned>);
  STATIC_CHECK(IsFilteredCategory<std::size_t>);
}

TEST_CASE(
    "category:filtered — IsFilteredCategory faithfully includes IsThinCategory",
    "[category][filtered][thin][faithful]") {
  /** @brief Every filtered category is thin (faithful inclusion encoded
   *         in @c IsFilteredCategory's signature). */
  STATIC_CHECK(IsThinCategory<bool>);
  STATIC_CHECK(IsFilteredCategory<bool>);

  STATIC_CHECK(IsThinCategory<int>);
  STATIC_CHECK(IsFilteredCategory<int>);
}

TEST_CASE("category:filtered — filtered is parallel to posetal, not downstream",
          "[category][filtered][posetal][parallel]") {
  /** @brief Architectural note: filtered does @b not require
   *         antisymmetry, so @c IsFilteredCategory is @b parallel to
   *         @c IsPosetal in the Form-chain, not a refinement.
   *
   *  For the canonical witnesses (@c bool, @c int) both happen to hold
   *  because @c std::less_equal is antisymmetric on these carriers,
   *  but the concept itself does not depend on antisymmetry — a
   *  preorder with two distinct mutually-@c ≤ elements would satisfy
   *  filtered (with itself / either as upper bound) but @b not posetal. */
  STATIC_CHECK(IsFilteredCategory<bool>);
  STATIC_CHECK(IsPosetal<bool>);  // bool happens to be antisymmetric

  STATIC_CHECK(IsFilteredCategory<int>);
  STATIC_CHECK(IsPosetal<int>);  // int happens to be antisymmetric
}
