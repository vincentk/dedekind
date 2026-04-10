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

static_assert(IsNaturalTransformation<IdentityUnit, identity_functor<IntCat>,
                                      IdF>);
static_assert(
    IsNaturalTransformation<IdentityJoin, composite_functor<IdF, IdF>, IdF>);
static_assert(
    IsNaturalTransformation<IdentityCounit, IdF, identity_functor<IntCat>>);
static_assert(IsNaturalTransformation<IdentityDuplicate, IdF,
                                      composite_functor<IdF, IdF>>);

}  // namespace

TEST_CASE("Category: Monad and Comonad Concepts", "[category][monad]") {
  STATIC_CHECK(IsMonad<IdF, IdentityUnit, IdentityJoin>);
  STATIC_CHECK(IsComonad<IdF, IdentityCounit, IdentityDuplicate>);
}

TEST_CASE("Category: Monad Pipeline Operators", "[category][monad][operator-shift]") {
  SECTION("η_tag lifts through the identity monad") {
    CHECK((42 >> η_tag<IdentityUnit>{}) == 42);
    CHECK((-7 >> η_tag<IdentityUnit>{}) == -7);
  }

  SECTION("μ_tag joins through the identity monad") {
    CHECK((42 >> μ_tag<IdentityJoin>{}) == 42);
    CHECK((-7 >> μ_tag<IdentityJoin>{}) == -7);
  }
}
