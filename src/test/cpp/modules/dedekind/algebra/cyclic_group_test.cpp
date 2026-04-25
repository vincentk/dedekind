/** @file dedekind/algebra/cyclic_group_test.cpp
 *
 * Tests for @c IsCyclicGroup (issue #378) on carriers visible at the
 * algebra test layer.  The concept is defined in
 * @c dedekind.category:total; @c Modular<N> lives in
 * @c morphologies:cyclic and is exercised in
 * @c morphologies/modular_test.cpp instead.
 */

#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <functional>
#include <limits>

import dedekind.algebra;
import dedekind.category;
import dedekind.sets;

using dedekind::category::cyclic_order_v;
using dedekind::category::is_cyclic_group_v;
using dedekind::category::IsCyclicGroup;

TEST_CASE("IsCyclicGroup — primitive unsigned integrals",
          "[category][cyclic-group][unsigned]") {
  // unsigned int / unsigned long / size_t all wrap modulo 2^N where
  // N is the bit width.  The exact bit width is platform-dependent
  // (ILP32 vs LP64 vs LLP64), so tests phrase expectations in terms
  // of `std::numeric_limits<T>::digits` rather than baking in a
  // specific N.
  STATIC_CHECK(IsCyclicGroup<unsigned int, std::plus<unsigned int>>);
  STATIC_CHECK(IsCyclicGroup<unsigned long, std::plus<unsigned long>>);
  STATIC_CHECK(IsCyclicGroup<std::size_t, std::plus<std::size_t>>);

  // For unsigned types whose bit width is strictly less than
  // std::size_t's, the cyclic-group order 2^N is representable as
  // std::size_t and the trait returns it.
  if constexpr (std::numeric_limits<unsigned int>::digits <
                std::numeric_limits<std::size_t>::digits) {
    STATIC_CHECK(cyclic_order_v<unsigned int, std::plus<unsigned int>> ==
                 (std::size_t{1} << std::numeric_limits<unsigned int>::digits));
  }

  // For std::size_t itself --- and any type at least as wide as
  // std::size_t --- the order 2^N overflows std::size_t and the
  // trait returns the sentinel 0 ("finite but not representable").
  STATIC_CHECK(cyclic_order_v<std::size_t, std::plus<std::size_t>> == 0);
}

TEST_CASE("IsCyclicGroup — ExtensionalCardinal and signed counterpart",
          "[algebra][cyclic-group][cardinal]") {
  using C1 = dedekind::sets::ExtensionalCardinal<>;
  using Z1 = dedekind::sets::SignedExtensionalCardinal<>;

  STATIC_CHECK(IsCyclicGroup<C1, std::plus<C1>>);
  STATIC_CHECK(IsCyclicGroup<Z1, std::plus<Z1>>);
}

TEST_CASE("IsCyclicGroup — bool under bit_xor is the additive 𝔽2 group",
          "[algebra][cyclic-group][f2]") {
  // 𝔽2's additive group is cyclic of order 2.
  STATIC_CHECK(IsCyclicGroup<bool, std::bit_xor<bool>>);
  STATIC_CHECK(cyclic_order_v<bool, std::bit_xor<bool>> == 2);
}

TEST_CASE("IsCyclicGroup — 𝔽64 multiplicative group is cyclic of order 63",
          "[algebra][cyclic-group][f64]") {
  using dedekind::algebra::𝔽64;
  // The multiplicative group of any finite field is cyclic;
  // for 𝔽_{q} it has order q - 1.
  STATIC_CHECK(IsCyclicGroup<𝔽64, std::multiplies<𝔽64>>);
  STATIC_CHECK(cyclic_order_v<𝔽64, std::multiplies<𝔽64>> == 63);

  // The *additive* group of 𝔽64 is NOT cyclic --- it's elementary
  // abelian (Z/2)^6, requiring 6 generators.
  STATIC_CHECK_FALSE(is_cyclic_group_v<𝔽64, std::plus<𝔽64>>);
}

TEST_CASE("IsCyclicGroup — rejects carriers without the opt-in",
          "[category][cyclic-group][negative]") {
  // signed int is not registered as cyclic (it isn't: + is partial
  // due to UB on overflow, and the concept requires IsAbelianGroup
  // which also fails).
  STATIC_CHECK_FALSE(IsCyclicGroup<int, std::plus<int>>);

  // bool under std::plus isn't an abelian group either (the library
  // models bool's group structure via std::bit_xor, not std::plus).
  STATIC_CHECK_FALSE(is_cyclic_group_v<bool, std::plus<bool>>);
}
