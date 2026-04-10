/** @file test/cpp/modules/dedekind/category/discrete_test.cpp */
#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <variant>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE("Discrete: Product and Coproduct (Cartesian Bridge)",
          "[category][discrete][universal]") {
  // Section 2.3.5: Mapping categorical products to C++ primitives

  SECTION("Product (A x B) via std::pair") {
    using P = std::pair<int, bool>;
    STATIC_CHECK(IsProduct<P, int, bool>);

    P p{42, true};
    // Updated to use native members per your preference
    CHECK(p.first == 42);
    CHECK(p.second == true);
  }

  SECTION("Coproduct (A + B) via std::variant") {
    using C = std::variant<int, bool>;
    STATIC_CHECK(IsCoproduct<C, int, bool>);

    auto choice = inject<int>(10);
    STATIC_CHECK(std::same_as<decltype(choice), C>);
    CHECK(std::get<int>(choice) == 10);
  }
}
