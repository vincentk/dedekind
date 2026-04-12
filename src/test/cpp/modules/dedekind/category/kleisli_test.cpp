#include <catch2/catch_test_macros.hpp>
#include <concepts>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE("Category: Kleisli Extension Concepts", "[category][kleisli]") {
  STATIC_CHECK(IsKleisliExtension<Box, int, int>);
  STATIC_CHECK(IsCoKleisliExtension<Box, int, int>);
  STATIC_CHECK(IsFrobenius<Box, int, int>);
}

TEST_CASE("Category: Box Frobenius structure",
          "[category][kleisli][frobenius]") {
  Box<int> value{9};

  SECTION("Unit followed by counit returns the original value") {
    auto boxed = unit_witness<Box, int>{}(value.value);
    auto raw = counit_witness<Box, int>{}(boxed);
    CHECK(raw == value.value);
  }

  SECTION("Kleisli right unit law: κ(ma, η) = ma") {
    auto result = κ(value, unit_witness<Box, int>{});
    CHECK(result == value);
  }

  SECTION("Co-Kleisli right counit law: σ(wa, ε) = wa") {
    auto result = σ(value, counit_witness<Box, int>{});
    CHECK(result == value);
  }

  SECTION("Bind then extract agrees with direct function result") {
    auto f = [](int x) { return Box<int>{x * 3}; };
    auto via_bind = κ(value, f);
    CHECK(counit_witness<Box, int>{}(via_bind) ==
          counit_witness<Box, int>{}(f(value.value)));
  }

  SECTION("Extend then extract agrees with direct co-Kleisli result") {
    auto g = [](Box<int> b) { return b.value + 4; };
    auto via_extend = σ(value, g);
    CHECK(counit_witness<Box, int>{}(via_extend) == g(value));
  }
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

TEST_CASE("Category: Kleisli κ (kappa) extension",
          "[category][kleisli][kappa]") {
  Box<int> value{10};

  SECTION("κ applies Kleisli arrow to boxed value") {
    auto result = κ(value, [](int x) { return Box<int>{x * 2}; });
    CHECK(result == Box<int>{20});
  }

  SECTION("κ chains multiple Kleisli extensions") {
    auto step1 = κ(value, [](int x) { return Box<int>{x + 5}; });
    auto step2 = κ(step1, [](int x) { return Box<int>{x * 2}; });
    CHECK(step2 == Box<int>{30});
  }
}

TEST_CASE("Category: Co-Kleisli σ (sigma) extend",
          "[category][kleisli][sigma]") {
  Box<int> value{7};

  SECTION("σ applies co-Kleisli arrow to comonadic value") {
    auto result = σ(value, [](Box<int> b) { return b.value + 3; });
    CHECK(result == Box<int>{10});
  }

  SECTION("σ chains through δ duplication") {
    auto extracted = σ(value, [](Box<int> b) { return b.value * 2; });
    CHECK(extracted == Box<int>{14});
  }
}

TEST_CASE("Category: Kleisli bind operator>>=", "[category][kleisli][bind]") {
  Box<int> value{5};

  SECTION("Monadic bind with >>= operator") {
    auto result = value >>= [](int x) { return Box<int>{x + 10}; };
    CHECK(result == Box<int>{15});
  }

  SECTION("Chaining multiple binds") {
    auto result = (value >>= [](int x) { return Box<int>{x * 2}; }) >>=
        [](int x) { return Box<int>{x + 1}; };
    CHECK(result == Box<int>{11});
  }

  SECTION("Bind preserves container structure") {
    Box<int> initial{3};
    auto final = initial >>= [](int x) { return Box<int>{x * x}; };
    CHECK(final.value == 9);
  }
}

TEST_CASE("Category: Fish alias operator>> for bind",
          "[category][kleisli][bind][fish-alias]") {
  Box<int> value{5};

  SECTION("Value-level fish aliases bind") {
    auto result = value >> [](int x) { return Box<int>{x + 10}; };
    CHECK(result == Box<int>{15});
  }

  SECTION("Fish alias supports left-associative bind chains") {
    auto result = value >> [](int x) { return Box<int>{x * 2}; } >>
                  [](int x) { return Box<int>{x + 1}; };
    CHECK(result == Box<int>{11});
  }
}

TEST_CASE("Category: Co-Kleisli cobind operator<<=",
          "[category][kleisli][cobind]") {
  Box<int> value{8};

  SECTION("Comonadic extend with <<= operator") {
    auto result = value <<= [](Box<int> b) { return b.value - 2; };
    CHECK(result == Box<int>{6});
  }

  SECTION("Chaining multiple coextends") {
    auto result = (value <<= [](Box<int> b) { return b.value / 2; }) <<=
        [](Box<int> b) { return b.value + 3; };
    CHECK(result == Box<int>{7});
  }
}

TEST_CASE("Category: Fish alias operator<< for cobind",
          "[category][kleisli][cobind][fish-alias]") {
  Box<int> value{8};

  SECTION("Value-level upstream fish aliases cobind") {
    auto result = value << [](Box<int> b) { return b.value - 2; };
    CHECK(result == Box<int>{6});
  }

  SECTION("Fish alias supports left-associative cobind chains") {
    auto result = value << [](Box<int> b) { return b.value / 2; }
                        << [](Box<int> b) { return b.value + 3; };
    CHECK(result == Box<int>{7});
  }
}

TEST_CASE("Category: Named Kleisli aliases", "[category][kleisli][aliases]") {
  SECTION("bind aliases κ for Box") {
    Box<int> value{6};
    auto result = bind(value, [](int x) { return Box<int>{x + 4}; });
    CHECK(result == Box<int>{10});
  }

  SECTION("extend aliases σ for Box") {
    Box<int> value{6};
    auto result = extend(value, [](Box<int> b) { return b.value * 3; });
    CHECK(result == Box<int>{18});
  }

  SECTION("bind aliases κ for Maybe/optional") {
    std::optional<int> present{5};
    std::optional<int> empty{std::nullopt};

    auto plus_two = [](int x) { return std::optional<int>{x + 2}; };
    CHECK(bind(present, plus_two) == std::optional<int>{7});
    CHECK(bind(empty, plus_two) == std::nullopt);
  }
}
