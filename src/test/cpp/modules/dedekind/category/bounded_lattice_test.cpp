/** @file dedekind/category/bounded_lattice_test.cpp
 *
 * Unit coverage for @c :category::lattice::IsBoundedLatticeCategory
 * (row 5 of the lattice Form-chain, #698 Slice 4).
 *
 * Faithful inclusion encoded in the signature:
 *   IsBoundedLatticeCategory ⊊ IsLatticeCategory ⊊ IsFilteredCategory
 *                              ⊊ IsThinCategory
 *
 * The bounded refinement requires lattice-internal initial / terminal
 * elements via the @c HasLatticeBottom / @c HasLatticeTop traits in
 * @c :species.  Distinct from @c :limit's strict @c IsInitialObject
 * (which pins @c std::nullptr_t globally) — these are per-(T, Rel)
 * lattice-internal bottom / top.
 */

#include <catch2/catch_test_macros.hpp>
#include <climits>
#include <concepts>
#include <functional>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE("category:bounded-lattice — bool is the canonical 2-element bounded lattice",
          "[category][lattice][bounded][bool][canonical]") {
  /** @brief @c bool under @c std::less_equal has bottom = @c false,
   *         top = @c true — the canonical 2-element bounded lattice
   *         (also the subobject classifier Ω in Set). */
  STATIC_CHECK(IsBoundedLatticeCategory<bool>);
  STATIC_CHECK(HasLatticeBottom<bool, std::less_equal<bool>>);
  STATIC_CHECK(HasLatticeTop<bool, std::less_equal<bool>>);
  STATIC_CHECK(lattice_bottom_v<bool, std::less_equal<bool>> == false);
  STATIC_CHECK(lattice_top_v<bool, std::less_equal<bool>> == true);
}

TEST_CASE("category:bounded-lattice — integral carriers are bounded via numeric_limits",
          "[category][lattice][bounded][numeric]") {
  /** @brief Integral carriers under @c std::less_equal are bounded:
   *         bottom = @c numeric_limits<T>::min(), top = max(). */
  STATIC_CHECK(IsBoundedLatticeCategory<int>);
  STATIC_CHECK(IsBoundedLatticeCategory<unsigned>);
  STATIC_CHECK(IsBoundedLatticeCategory<std::size_t>);

  STATIC_CHECK(lattice_bottom_v<int, std::less_equal<int>> == INT_MIN);
  STATIC_CHECK(lattice_top_v<int, std::less_equal<int>> == INT_MAX);

  STATIC_CHECK(lattice_bottom_v<unsigned, std::less_equal<unsigned>> == 0u);
  STATIC_CHECK(lattice_top_v<unsigned, std::less_equal<unsigned>> == UINT_MAX);
}

TEST_CASE("category:bounded-lattice — Form-chain faithful inclusions hold",
          "[category][lattice][bounded][faithful][form-chain]") {
  /** @brief Every bounded lattice category is a lattice, posetal,
   *         filtered, and thin — all encoded definitionally in
   *         @c IsBoundedLatticeCategory's signature chain. */
  STATIC_CHECK(IsThinCategory<bool>);
  STATIC_CHECK(IsPosetal<bool>);
  STATIC_CHECK(IsFilteredCategory<bool>);
  STATIC_CHECK(IsLatticeCategory<bool>);
  STATIC_CHECK(IsBoundedLatticeCategory<bool>);

  STATIC_CHECK(IsThinCategory<int>);
  STATIC_CHECK(IsPosetal<int>);
  STATIC_CHECK(IsFilteredCategory<int>);
  STATIC_CHECK(IsLatticeCategory<int>);
  STATIC_CHECK(IsBoundedLatticeCategory<int>);
}
