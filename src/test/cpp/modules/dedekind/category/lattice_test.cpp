/** @file dedekind/category/lattice_test.cpp
 *
 * Unit coverage for @c :category::lattice (the lattice-Form-chain row 4,
 * #698 Slice 3).
 *
 * @c IsLatticeCategory<T, Rel, Join, Meet, L> is the Form-witness that
 * binds the top-down universal-property reading (thin + antisymmetric +
 * filtered + cofiltered) to the bottom-up algebraic reading
 * (@c IsOrderLatticeOperations: commutative semilattices with absorption).
 *
 * Faithful inclusions encoded in the signature:
 *   IsLatticeCategory ⊊ IsFilteredCategory ⊊ IsThinCategory
 *   IsLatticeCategory ⊊ IsPosetal           ⊊ IsThinCategory
 */

#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <functional>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE("category:lattice — Bool is the canonical 2-element lattice category",
          "[category][lattice][bool][canonical]") {
  /** @brief @c bool with @c std::less_equal, @c std::ranges::max,
   *         @c std::ranges::min is the smallest non-trivial lattice
   *         category — also the canonical Boolean algebra (the
   *         categorical witness of that Boolean structure lands in a
   *         future slice; #698). */
  STATIC_CHECK(IsLatticeCategory<bool>);
}

TEST_CASE("category:lattice — totally ordered integral carriers are lattices",
          "[category][lattice][numeric]") {
  /** @brief Totally ordered carriers under @c std::less_equal are
   *         trivially both directed (max is the upper bound) and
   *         codirected (min is the lower bound), and the lattice
   *         operations are @c std::ranges::max / @c std::ranges::min. */
  STATIC_CHECK(IsLatticeCategory<int>);
  STATIC_CHECK(IsLatticeCategory<unsigned>);
  STATIC_CHECK(IsLatticeCategory<std::size_t>);
}

TEST_CASE(
    "category:lattice — IsLatticeCategory faithfully includes the Form-chain "
    "rows above",
    "[category][lattice][faithful][form-chain]") {
  /** @brief Every lattice category is filtered, posetal, and thin —
   *         encoded in @c IsLatticeCategory's signature definitionally. */
  STATIC_CHECK(IsThinCategory<bool>);
  STATIC_CHECK(IsPosetal<bool>);
  STATIC_CHECK(IsFilteredCategory<bool>);
  STATIC_CHECK(IsLatticeCategory<bool>);

  STATIC_CHECK(IsThinCategory<int>);
  STATIC_CHECK(IsPosetal<int>);
  STATIC_CHECK(IsFilteredCategory<int>);
  STATIC_CHECK(IsLatticeCategory<int>);
}
