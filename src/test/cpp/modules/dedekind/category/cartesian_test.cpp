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
    // Let arrow() deduce Morphism<int, double> and Morphism<bool, double>
    auto f = arrow([](int i) { return static_cast<double>(i); });
    auto g = arrow([](bool b) { return b ? 1.0 : 0.0; });

    auto cases = mediate_coproduct(f, g);

    // IsArrow now uses your inferred labels
    STATIC_CHECK(IsArrow<decltype(cases)>);
    using Var = std::variant<int, bool>;

    CHECK(cases(Var{42}) == 42.0);
    CHECK(cases(Var{true}) == 1.0);
  }

  SECTION("Uncurry Adjunction Identity") {
    // Instead of move_only_function, we return a nested arrow
    // This allows uncurry to deduce its labels from the nested Morphism
    auto curried_val =
        arrow([](int x) { return arrow([x](int y) { return x * y; }); });

    auto uncurried = uncurry(curried_val);

    STATIC_CHECK(IsArrow<decltype(uncurried)>);
    // Domain should be inferred as std::pair<int, int>
    CHECK(uncurried({6, 7}) == 42);
  }
}

TEST_CASE("CCC: CurryPlus", "[category][cartesian]") {
  // Use the simplest arrow inference
  auto plus_morphism =
      arrow([](std::pair<int, int> p) { return p.first + p.second; });

  auto curried_plus = curry(plus_morphism);

  // curried_plus(5) returns a labeled arrow, so it is callable
  auto add_five = curried_plus(5);

  CHECK(add_five(10) == 15);
  CHECK(curried_plus(10)(20) == 30);
}

TEST_CASE("Cartesian: Product Mediation", "[category][cartesian][labeled]") {
  SECTION("Product Mediation (Pairing)") {
    // 1. Define two morphisms f: X -> A and g: X -> B
    auto f = arrow([](int i) { return i * 2.0; });  // int -> double
    auto g = arrow([](int i) { return i > 0; });    // int -> bool

    // 2. Mediate them into a product arrow <f, g>: int -> (double x bool)
    auto pairing = mediate_product(f, g);

    // 3. Verify it satisfies the skeletal IsArrow concept
    STATIC_CHECK(IsArrow<decltype(pairing)>);

    // 4. Verify Domain and Codomain labels are correct
    using ProductType = std::pair<double, bool>;
    STATIC_CHECK(std::same_as<Dom<decltype(pairing)>, int>);
    STATIC_CHECK(std::same_as<Cod<decltype(pairing)>, ProductType>);

    // 5. Verify the action: <f, g>(x) == (f(x), g(x))
    auto result = pairing(5);
    CHECK(result.first == 10.0);
    CHECK(result.second == true);

    auto result_zero = pairing(-1);
    CHECK(result_zero.first == -2.0);
    CHECK(result_zero.second == false);
  }
}
