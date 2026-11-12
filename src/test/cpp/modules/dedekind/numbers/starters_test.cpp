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
  //   static_assert(naturals(Numeral{7u}));  // value-level membership query
  // (ℕ = @c 𝔸<Numeral>; the retired @c element / @c % scout is gone.)

  constexpr auto naturals = ℕ;
  static_assert(naturals(Numeral{7u}));
  static_assert(naturals(Numeral{0u}));
  // Direct ambient-call route: ℕ.contains(value) (or equivalently
  // 𝔸<Numeral>.contains(value), since ℕ = 𝔸<Numeral>) returns
  // L::True for every Numeral (the universal-set semantics).
  static_assert(𝔸<Numeral>{}.contains(Numeral{7u}));
  static_assert(𝔸<Numeral>{}.contains(Numeral{0u}));

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
    CHECK((U | O)(Numeral{7u}));
    CHECK_FALSE((U & O)(Numeral{7u}));
    CHECK((U | O)(Numeral{0u}));
    CHECK_FALSE((U & O)(Numeral{0u}));
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
