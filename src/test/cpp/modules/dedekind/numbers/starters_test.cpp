#include <catch2/catch_test_macros.hpp>

import dedekind.category;
import dedekind.numbers;
import dedekind.sets;

using namespace dedekind::category;
using namespace dedekind::numbers;
using namespace dedekind::sets;

TEST_CASE("Numbers: starter universes construct from ambient values",
          "[numbers][starter][sets]") {
  // Post-#895 the universe value @b is the ambient; wrap it directly:
  //   constexpr auto naturals = Set{ℕ};      // universal Set over ℕ-carrier
  //   static_assert(naturals(NatExpr{7u}));  // value-level membership query
  // (ℕ = @c 𝔸<NatExpr>; the retired @c element / @c % scout is gone.)

  constexpr auto naturals = ℕ;
  static_assert(naturals(NatExpr<>{7u}));
  static_assert(naturals(NatExpr<>{0u}));
  // Direct ambient-call route: ℕ.contains(value) (or equivalently
  // 𝔸<NatExpr>.contains(value), since ℕ = 𝔸<NatExpr>) returns
  // L::True for every NatExpr (the universal-set semantics).
  static_assert(𝔸<NatExpr<>>{}.contains(NatExpr<>{7u}));
  static_assert(𝔸<NatExpr<>>{}.contains(NatExpr<>{0u}));

  constexpr auto integers = ℤ;
  static_assert(integers(IntExpr<>{-7}));

  // 𝔻 starter-universe construction moved to
  // src/test/cpp/modules/dedekind/analysis/dual_test.cpp at PR #513
  // (Dual<F> relocated from :numbers to :analysis).
}

TEST_CASE("Numbers: starter universes satisfy lattice identities",
          "[numbers][starter][algebra]") {
  {
    const auto U = ℕ;
    const auto O = !U;
    CHECK((U | O)(NatExpr<>{7u}));
    CHECK_FALSE((U & O)(NatExpr<>{7u}));
    CHECK((U | O)(NatExpr<>{0u}));
    CHECK_FALSE((U & O)(NatExpr<>{0u}));
  }

  {
    const auto U = ℤ;
    const auto O = !U;
    CHECK((U | O)(IntExpr<>{4}));
    CHECK_FALSE((U & O)(IntExpr<>{4}));
  }

  // 𝔻 / D / Dual lattice-identity check moved to
  // src/test/cpp/modules/dedekind/analysis/dual_test.cpp at PR #513
  // (Dual<F> relocated from :numbers to :analysis).
}
