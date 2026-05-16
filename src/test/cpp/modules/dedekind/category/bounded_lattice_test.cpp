/** @file dedekind/category/bounded_lattice_test.cpp
 *
 * Unit coverage for @c :category::lattice::IsBoundedLatticeCategory
 * (row 5 of the lattice Form-chain, #698 Slice 4).
 *
 * Faithful inclusion encoded in the signature:
 *   IsBoundedLatticeCategory ⊊ IsLatticeCategory ⊊ IsFilteredCategory
 *                              ⊊ IsThinCategory
 *
 * The bounded refinement requires structural witness types
 * @c LatticeBottom<T, Rel> and @c LatticeTop<T, Rel> declared in
 * @c :lattice — each carries the @c is_initial_object_tag / @c
 * is_terminal_object_tag typedef so the relaxed @c :limit::
 * IsInitialObject / @c IsTerminalObject (tag-discovery branch) fires.
 * No parallel concept surface; the universal-property reading is
 * carried by the existing @c :limit categorical concepts.
 */

#include <catch2/catch_test_macros.hpp>
#include <climits>
#include <concepts>
#include <functional>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE(
    "category:bounded-lattice — bool is the canonical 2-element bounded lattice",
    "[category][lattice][bounded][bool][canonical]") {
  /** @brief @c bool under @c std::less_equal has bottom = @c false,
   *         top = @c true — the canonical 2-element bounded lattice
   *         (also the subobject classifier Ω in Set). */
  STATIC_CHECK(IsBoundedLatticeCategory<bool>);
  STATIC_CHECK(IsInitialObject<LatticeBottom<bool, std::less_equal<bool>>>);
  STATIC_CHECK(IsTerminalObject<LatticeTop<bool, std::less_equal<bool>>>);
  STATIC_CHECK(LatticeBottom<bool, std::less_equal<bool>>::value == false);
  STATIC_CHECK(LatticeTop<bool, std::less_equal<bool>>::value == true);
}

TEST_CASE(
    "category:bounded-lattice — integral carriers are bounded via numeric_limits",
    "[category][lattice][bounded][numeric]") {
  /** @brief Integral carriers under @c std::less_equal are bounded:
   *         bottom = @c numeric_limits<T>::min(), top = max(). */
  STATIC_CHECK(IsBoundedLatticeCategory<int>);
  STATIC_CHECK(IsBoundedLatticeCategory<unsigned>);
  STATIC_CHECK(IsBoundedLatticeCategory<std::size_t>);

  STATIC_CHECK(LatticeBottom<int, std::less_equal<int>>::value == INT_MIN);
  STATIC_CHECK(LatticeTop<int, std::less_equal<int>>::value == INT_MAX);

  STATIC_CHECK(LatticeBottom<unsigned, std::less_equal<unsigned>>::value == 0u);
  STATIC_CHECK(LatticeTop<unsigned, std::less_equal<unsigned>>::value == UINT_MAX);
}

TEST_CASE("category:bounded-lattice — relaxed :limit concepts still fire on global Zero/One",
          "[category][lattice][bounded][limit][relaxed]") {
  /** @brief The relaxation to @c :limit::IsInitialObject /
   *         @c IsTerminalObject preserves the existing strict-global
   *         branch — @c Zero and @c One still satisfy.  Verifies that
   *         the existing ETCS / topoi consumers continue to fire. */
  STATIC_CHECK(IsInitialObject<Zero>);
  STATIC_CHECK(IsTerminalObject<One>);
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
