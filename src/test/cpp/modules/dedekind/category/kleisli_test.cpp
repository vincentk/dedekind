#include <catch2/catch_test_macros.hpp>
#include <concepts>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE("Category: Kleisli Extension Concepts", "[category][kleisli]") {
  STATIC_CHECK(IsKleisliExtension<Box, int, int>);
  STATIC_CHECK(IsCoKleisliExtension<Box, int, int>);
  STATIC_CHECK(IsFrobenius<Box, int, int>);
}

TEST_CASE("Category: Box Bind and Extend", "[category][kleisli]") {
  Box<int> value{21};

  SECTION("Bind (>>=) chains in-context computations") {
    auto doubled = value >>= [](int x) { return Box<int>{x * 2}; };
    CHECK(doubled == Box<int>{42});
  }

  SECTION("Extend (<<=) samples context to build new context") {
    auto summarized = value <<= [](Box<int> b) { return b.value + 1; };
    CHECK(summarized == Box<int>{22});
  }
}

TEST_CASE("Category: Kleisli fmap Bridge", "[category][kleisli][fmap]") {
  auto plus_one = arrow([](int x) { return x + 1; });
  auto lifted = fmap<Box>(plus_one);

  STATIC_CHECK(IsArrow<decltype(lifted)>);
  CHECK(lifted(Box<int>{1}) == Box<int>{2});
  CHECK(lifted(Box<int>{-3}) == Box<int>{-2});
}

TEST_CASE("Category: Co-Kleisli Duplicate Tag", "[category][kleisli]") {
  Box<int> value{5};

  auto duplicated = value << ε_tag<Box>{};

  STATIC_CHECK(std::same_as<decltype(duplicated), Box<Box<int>>>);
  CHECK(duplicated == Box<Box<int>>{Box<int>{5}});
}
