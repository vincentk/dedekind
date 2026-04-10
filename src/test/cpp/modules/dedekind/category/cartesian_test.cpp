/** @file src/test/cpp/modules/dedekind/category/cartesian_test.cpp */
#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <utility>
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
    STATIC_CHECK(IsCoproduct<std::variant<int, bool>, int, bool>);

    auto choice_1 = ι_1<int, bool>(10);
    STATIC_CHECK(std::same_as<decltype(choice_1), std::variant<int, bool>>);
    CHECK(std::get<int>(choice_1) == 10);

    auto choice_2 = ι_1<int, int>(10);
    STATIC_CHECK(std::same_as<decltype(choice_2), std::variant<int, int>>);
    CHECK(std::get<0>(choice_2) == 10);

    auto choice_3 = ι_2<int, bool>(true);
    STATIC_CHECK(std::same_as<decltype(choice_3), std::variant<int, bool>>);
    CHECK(std::get<bool>(choice_3) == true);

    auto choice_4 = ι_2<bool, bool>(true);
    STATIC_CHECK(std::same_as<decltype(choice_4), std::variant<bool, bool>>);
    CHECK(std::get<1>(choice_4) == true);
  }
}

TEST_CASE("Cartesian: Labeled Coproduct and Uncurry",
          "[category][cartesian][labeled]") {
  SECTION("Coproduct Mediation (Case Analysis)") {
    auto f =
        Morphism<int, double>([](int i) { return static_cast<double>(i); });
    auto g = Morphism<bool, double>([](bool b) { return b ? 1.0 : 0.0; });

    auto cases = mediate_coproduct(f, g);

    using CaseType = decltype(cases);
    STATIC_CHECK(IsArrow<CaseType, std::variant<int, bool>, double>);

    CHECK(cases(std::variant<int, bool>{42}) == 42.0);
    CHECK(cases(std::variant<int, bool>{true}) == 1.0);
  }

  SECTION("Uncurry Adjunction Identity") {
    // A curried arrow: int -> (int -> int)
    auto curried_val = Morphism<int, std::move_only_function<int(int)>>(
        [](int x) { return [x](int y) { return x * y; }; });

    auto uncurried = uncurry(curried_val);

    using UncurryType = decltype(uncurried);
    STATIC_CHECK(IsArrow<UncurryType, std::pair<int, int>, int>);

    CHECK(uncurried({6, 7}) == 42);
  }
}
