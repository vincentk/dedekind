#include <catch2/catch_test_macros.hpp>
#include <concepts>

import dedekind.category;

using namespace dedekind::category;

namespace {

using IntCat = DiscreteCategory<int>;
using IdF = identity_functor<IntCat>;

struct IdentityComponent {
  auto operator()(int) const { return id<int>(); }
};

static_assert(IsNaturalTransformation<IdentityComponent, IdF, IdF>);

}  // namespace

TEST_CASE("Category: Natural Transformation Concepts", "[category][natural]") {
  STATIC_CHECK(IsNaturalTransformation<identity_transformation<IdF>, IdF, IdF>);
  STATIC_CHECK(IsNaturalTransformation<IdentityComponent, IdF, IdF>);
}

TEST_CASE("Category: Natural Transformation Runtime Witnesses",
          "[category][natural]") {
  SECTION("Identity transformation returns the target identity component") {
    identity_transformation<IdF> alpha{IdF{}};
    auto component = alpha(4);

    CHECK(component(42) == 42);
    CHECK(component(4) == 4);
  }
}