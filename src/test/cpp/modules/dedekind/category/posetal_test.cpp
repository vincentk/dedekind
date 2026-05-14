/** @file test/cpp/modules/dedekind/category/posetal_test.cpp */
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <functional>  // std::less_equal (default order relation for the
                       // IsMonotone family below)

import dedekind.category;

using namespace dedekind::category;

TEST_CASE("Posetal: textbook default relation", "[category][posetal][order]") {
  SECTION("Default witness is <= over classical Omega") {
    STATIC_CHECK(IsPosetal<int>);
  }

  SECTION("Path composition follows transitivity") {
    CHECK(check_path<int, std::less_equal<int>>(1, 2, 3));
    CHECK_FALSE(check_path<int, std::less_equal<int>>(3, 2, 1));
  }

  SECTION("Semilattice and lattice laws for min/max are validated in tests") {
    const int a = 7;
    const int b = 3;
    const int c = 9;

    const auto meet = std::ranges::min;
    const auto join = std::ranges::max;

    CHECK(meet(a, b) == meet(b, a));
    CHECK(join(a, b) == join(b, a));

    CHECK(meet(meet(a, b), c) == meet(a, meet(b, c)));
    CHECK(join(join(a, b), c) == join(a, join(b, c)));

    CHECK(meet(a, a) == a);
    CHECK(join(a, a) == a);

    CHECK(join(a, meet(a, b)) == a);
    CHECK(meet(a, join(a, b)) == a);

    CHECK(join(a, meet(b, c)) == meet(join(a, b), join(a, c)));
    CHECK(meet(a, join(b, c)) == join(meet(a, b), meet(a, c)));
  }

  SECTION(
      "Certified semilattice and lattice concepts verify commutativity and "
      "associativity") {
    STATIC_CHECK(
        IsCertifiedOrderMeetSemilattice<int, decltype(std::ranges::min)>);
    STATIC_CHECK(
        IsCertifiedOrderJoinSemilattice<int, decltype(std::ranges::max)>);
    STATIC_CHECK(
        IsCertifiedOrderLatticeOperations<int, decltype(std::ranges::max),
                                          decltype(std::ranges::min)>);

    const int x = 5;
    const int y = 12;
    const int z = 3;

    const auto meet = std::ranges::min;
    const auto join = std::ranges::max;

    STATIC_CHECK(IsCertifiedOrderMeetSemilattice<int, decltype(meet)>);
    STATIC_CHECK(IsCertifiedOrderJoinSemilattice<int, decltype(join)>);
    STATIC_CHECK(IsOrderLatticeOperations<int, decltype(join), decltype(meet)>);

    // Verify certified properties are correct for int
    CHECK(meet(x, y) == std::min(x, y));
    CHECK(join(x, y) == std::max(x, y));

    CHECK(meet(x, meet(y, z)) == meet(meet(x, y), z));
    CHECK(join(x, join(y, z)) == join(join(x, y), z));

    CHECK(join(x, meet(x, y)) == x);
    CHECK(meet(x, join(x, y)) == x);

    CHECK(join(x, meet(y, z)) == meet(join(x, y), join(x, z)));
    CHECK(meet(x, join(y, z)) == join(meet(x, y), meet(x, z)));
  }
}

// ===========================================================================
// Order-aware morphism vocabulary (#664 acceptance criteria).
//
// Confirms the new concepts behave as advertised:
//   * default trait state is @c false (Honest stance: no claim is the safe
//     claim);
//   * @c Identity registers monotone (and therefore order-iso, since it is
//     also bijective via @c :morphism);
//   * the synonym pair @c IsBijective / @c IsBijectiveArrow agrees with
//     the new order-iso concepts on the Identity arrow.
// Per-arrow witnesses on the @c embed_* family are exercised in their
// home partitions' static_asserts (e.g.\ @c morphologies/uint.cppm and
// @c sint.cppm; @c sets/cardinality.cppm) so the test surface here stays
// upstream-of-numbers and the @c :category test layer doesn't reach
// across the module DAG.
// ===========================================================================

TEST_CASE("Posetal: order-aware morphism vocabulary defaults (#664)",
          "[category][posetal][morphism][monotone][negative]") {
  SECTION("is_monotone_v / is_antimonotone_v default to false") {
    STATIC_CHECK(!is_monotone_v<int, std::less_equal<>>);
    STATIC_CHECK(!is_antimonotone_v<int, std::less_equal<>>);
  }

  SECTION("IsMonotone / IsAntiMonotone gate on IsArrow + the trait") {
    // Non-arrow types refuse the concept even if the trait were set —
    // the @c IsArrow gate is structural.
    STATIC_CHECK(!IsMonotone<int>);
    STATIC_CHECK(!IsAntiMonotone<int>);
  }
}

TEST_CASE("Posetal: Identity is the canonical monotone witness (#664)",
          "[category][posetal][morphism][monotone][identity]") {
  SECTION("Identity is monotone under the default <= relation") {
    STATIC_CHECK(IsMonotone<Identity<int>>);
    STATIC_CHECK(IsMonotone<Identity<int>, std::less_equal<>>);
  }

  SECTION("Identity is bijective via :morphism") {
    // Sanity: the existing @c IsBijective registration on Identity is
    // load-bearing for the IsOrderIsomorphism composition below.
    STATIC_CHECK(IsBijectiveArrow<Identity<int>>);
    STATIC_CHECK(IsBijective<Identity<int>>);
  }

  SECTION("Identity is therefore an order-isomorphism (composed concept)") {
    // The composition IsBijectiveArrow && IsMonotone fires for free once
    // both ingredients hold — no separate is_order_iso_v opt-in needed.
    STATIC_CHECK(IsOrderIsomorphism<Identity<int>>);
  }

  SECTION("Identity is NOT anti-monotone (it preserves order, not reverses)") {
    STATIC_CHECK(!IsAntiMonotone<Identity<int>>);
    STATIC_CHECK(!IsOrderAntiIsomorphism<Identity<int>>);
  }
}
