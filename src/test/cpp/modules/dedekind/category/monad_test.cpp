#include <catch2/catch_test_macros.hpp>
#include <concepts>

import dedekind.category;

using namespace dedekind::category;

namespace {

using IntCat = DiscreteCategory<int>;
using IdF = identity_functor<IntCat>;

struct IdentityUnit {
  auto operator()(int) const { return id<int>(); }
};

struct IdentityJoin {
  auto operator()(int) const { return id<int>(); }
};

struct IdentityCounit {
  auto operator()(int) const { return id<int>(); }
};

struct IdentityDuplicate {
  auto operator()(int) const { return id<int>(); }
};

}  // namespace

TEST_CASE("Category: Monad Pipeline Operators",
          "[category][monad][operator-shift]") {
  SECTION("η_tag lifts through the identity monad") {
    CHECK((42 >> η_tag<IdentityUnit>{}) == 42);
    CHECK((-7 >> η_tag<IdentityUnit>{}) == -7);
  }

  SECTION("μ_tag joins through the identity monad") {
    CHECK((42 >> μ_tag<IdF, IdentityJoin>{}) == 42);
    CHECK((-7 >> μ_tag<IdF, IdentityJoin>{}) == -7);
  }
}
