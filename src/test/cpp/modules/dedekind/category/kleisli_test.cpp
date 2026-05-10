#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <optional>
#include <tuple>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE("Category: Kleisli Extension Concepts", "[category][kleisli]") {
  // Maybe is a monad (Kleisli extension only); std::tuple<T> is the
  // bona-fide Frobenius carrier (both Kleisli and co-Kleisli, no
  // Some-fragment concession).
  STATIC_CHECK(IsKleisliExtension<maybe_functor<int>, int, int>);
  STATIC_CHECK(IsKleisliExtension<tuple_functor<int>, int, int>);
  STATIC_CHECK(IsCoKleisliExtension<tuple_functor<int>, int, int>);
  STATIC_CHECK(IsFrobenius<tuple_functor<int>, int, int>);
}

TEST_CASE("Category: Tuple Frobenius structure",
          "[category][kleisli][frobenius]") {
  std::tuple<int> value{9};

  SECTION("Unit followed by counit returns the original value") {
    auto wrapped = unit_witness<tuple_functor<int>, int>{}(std::get<0>(value));
    auto raw = counit_witness<tuple_functor<int>, int>{}(wrapped);
    CHECK(raw == std::get<0>(value));
  }

  SECTION("Kleisli right unit law: κ(ma, η) = ma") {
    auto result = κ(value, unit_witness<tuple_functor<int>, int>{});
    CHECK(result == value);
  }

  SECTION("Co-Kleisli right counit law: σ(wa, ε) = wa") {
    auto result = σ(value, counit_witness<tuple_functor<int>, int>{});
    CHECK(result == value);
  }

  SECTION("Bind then extract agrees with direct function result") {
    auto f = [](int x) { return std::tuple<int>{x * 3}; };
    auto via_bind = κ(value, f);
    CHECK(counit_witness<tuple_functor<int>, int>{}(via_bind) ==
          counit_witness<tuple_functor<int>, int>{}(f(std::get<0>(value))));
  }

  SECTION("Extend then extract agrees with direct co-Kleisli result") {
    auto g = [](std::tuple<int> b) { return std::get<0>(b) + 4; };
    auto via_extend = σ(value, g);
    CHECK(counit_witness<tuple_functor<int>, int>{}(via_extend) == g(value));
  }
}

TEST_CASE("Category: Tuple Bind and Extend", "[category][kleisli]") {
  std::tuple<int> value{21};

  SECTION("Bind (>>=) chains in-context computations") {
    auto doubled = value >>= [](int x) { return std::tuple<int>{x * 2}; };
    CHECK(doubled == std::tuple<int>{42});
  }

  SECTION("Extend (<<=) samples context to build new context") {
    auto summarized = value <<=
        [](std::tuple<int> b) { return std::get<0>(b) + 1; };
    CHECK(summarized == std::tuple<int>{22});
  }
}

TEST_CASE("Category: Maybe Bind", "[category][kleisli]") {
  Maybe<int> value{21};

  SECTION("Bind (>>=) chains in-context computations") {
    auto doubled = value >>= [](int x) { return Maybe<int>{x * 2}; };
    CHECK(doubled == Maybe<int>{42});
  }
}

TEST_CASE("Category: Kleisli κ (kappa) extension",
          "[category][kleisli][kappa]") {
  Maybe<int> value{10};

  SECTION("κ applies Kleisli arrow to monadic value") {
    auto result = κ(value, [](int x) { return Maybe<int>{x * 2}; });
    CHECK(result == Maybe<int>{20});
  }

  SECTION("κ chains multiple Kleisli extensions") {
    auto step1 = κ(value, [](int x) { return Maybe<int>{x + 5}; });
    auto step2 = κ(step1, [](int x) { return Maybe<int>{x * 2}; });
    CHECK(step2 == Maybe<int>{30});
  }
}

TEST_CASE("Category: Co-Kleisli σ (sigma) extend",
          "[category][kleisli][sigma]") {
  std::tuple<int> value{7};

  SECTION("σ applies co-Kleisli arrow to comonadic value") {
    auto result =
        σ(value, [](std::tuple<int> b) { return std::get<0>(b) + 3; });
    CHECK(result == std::tuple<int>{10});
  }

  SECTION("σ chains through δ duplication") {
    auto extracted =
        σ(value, [](std::tuple<int> b) { return std::get<0>(b) * 2; });
    CHECK(extracted == std::tuple<int>{14});
  }
}

TEST_CASE("Category: Kleisli bind operator>>=", "[category][kleisli][bind]") {
  Maybe<int> value{5};

  SECTION("Monadic bind with >>= operator") {
    auto result = value >>= [](int x) { return Maybe<int>{x + 10}; };
    CHECK(result == Maybe<int>{15});
  }

  SECTION("Chaining multiple binds") {
    auto result = (value >>= [](int x) { return Maybe<int>{x * 2}; }) >>=
        [](int x) { return Maybe<int>{x + 1}; };
    CHECK(result == Maybe<int>{11});
  }

  SECTION("Bind preserves container structure") {
    Maybe<int> initial{3};
    auto final = initial >>= [](int x) { return Maybe<int>{x * x}; };
    CHECK(*final == 9);
  }

  SECTION("Bind on nullopt propagates the empty case") {
    Maybe<int> empty{std::nullopt};
    auto result = empty >>= [](int x) { return Maybe<int>{x + 10}; };
    CHECK(result == std::nullopt);
  }
}

TEST_CASE("Category: Fish alias operator>> for bind",
          "[category][kleisli][bind][fish-alias]") {
  Maybe<int> value{5};

  SECTION("Value-level fish aliases bind") {
    auto result = value >> [](int x) { return Maybe<int>{x + 10}; };
    CHECK(result == Maybe<int>{15});
  }

  SECTION("Fish alias supports left-associative bind chains") {
    auto result = value >> [](int x) { return Maybe<int>{x * 2}; } >>
                  [](int x) { return Maybe<int>{x + 1}; };
    CHECK(result == Maybe<int>{11});
  }
}

TEST_CASE("Category: Co-Kleisli cobind operator<<=",
          "[category][kleisli][cobind]") {
  std::tuple<int> value{8};

  SECTION("Comonadic extend with <<= operator") {
    auto result = value <<=
        [](std::tuple<int> b) { return std::get<0>(b) - 2; };
    CHECK(result == std::tuple<int>{6});
  }

  SECTION("Chaining multiple coextends") {
    auto result =
        (value <<= [](std::tuple<int> b) { return std::get<0>(b) / 2; }) <<=
        [](std::tuple<int> b) { return std::get<0>(b) + 3; };
    CHECK(result == std::tuple<int>{7});
  }
}

TEST_CASE("Category: Fish alias operator<< for cobind",
          "[category][kleisli][cobind][fish-alias]") {
  std::tuple<int> value{8};

  SECTION("Value-level upstream fish aliases cobind") {
    auto result = value << [](std::tuple<int> b) { return std::get<0>(b) - 2; };
    CHECK(result == std::tuple<int>{6});
  }

  SECTION("Fish alias supports left-associative cobind chains") {
    auto result = value << [](std::tuple<int> b) { return std::get<0>(b) / 2; }
                        << [](std::tuple<int> b) { return std::get<0>(b) + 3; };
    CHECK(result == std::tuple<int>{7});
  }
}

TEST_CASE("Category: Named Kleisli aliases", "[category][kleisli][aliases]") {
  SECTION("bind aliases κ for Maybe") {
    Maybe<int> value{6};
    auto result = bind(value, [](int x) { return Maybe<int>{x + 4}; });
    CHECK(result == Maybe<int>{10});
  }

  SECTION("extend aliases σ for std::tuple") {
    std::tuple<int> value{6};
    auto result =
        extend(value, [](std::tuple<int> b) { return std::get<0>(b) * 3; });
    CHECK(result == std::tuple<int>{18});
  }

  SECTION("bind aliases κ for Maybe/optional") {
    std::optional<int> present{5};
    std::optional<int> empty{std::nullopt};

    auto plus_two = [](int x) { return std::optional<int>{x + 2}; };
    CHECK(bind(present, plus_two) == std::optional<int>{7});
    CHECK(bind(empty, plus_two) == std::nullopt);
  }

  SECTION("bind with maybe_hub uses default join/phi route") {
    std::optional<int> present{5};
    std::optional<int> empty{std::nullopt};

    auto plus_two = [](int x) { return std::optional<int>{x + 2}; };
    CHECK(bind(maybe_hub, present, plus_two) == std::optional<int>{7});
    CHECK(bind(maybe_hub, empty, plus_two) == std::nullopt);
  }

  SECTION("bind and extend with tuple_hub use default Frobenius routes") {
    std::tuple<int> value{6};

    auto bound =
        bind(tuple_hub, value, [](int x) { return std::tuple<int>{x + 4}; });
    CHECK(bound == std::tuple<int>{10});

    auto extended = extend(
        tuple_hub, value, [](std::tuple<int> b) { return std::get<0>(b) * 3; });
    CHECK(extended == std::tuple<int>{18});
  }
}

TEST_CASE(
    "Category: Fish-operator concept tier (#450) — operator-shape "
    "concepts for arrow compose / Kleisli bind / comonadic extend",
    "[category][fish-operators][concept-shape]") {
  // The fish-operator concept tier classifies types by which fish-family
  // operator surfaces they support.  These are operator-shape concepts:
  // they pin syntactic availability, not the equational laws (which are
  // pinned by IsArrow / IsMonad / IsComonad in their own partitions).
  using MaybeBind = decltype([](int x) { return Maybe<int>{x + 1}; });

  SECTION("HasKleisliBindOperators: Maybe<int> + Kleisli arrow fires") {
    STATIC_CHECK(HasKleisliBindOperators<Maybe<int>, MaybeBind>);
  }

  SECTION(
      "HasComonadicExtendOperators: std::tuple<int> + co-Kleisli "
      "arrow fires") {
    using TupleCoBind =
        decltype([](std::tuple<int> b) { return std::get<0>(b) * 2; });
    STATIC_CHECK(HasComonadicExtendOperators<std::tuple<int>, TupleCoBind>);
  }

  SECTION(
      "HasKleisliBindOperators negative: bare int + bind-arrow does NOT "
      "fire (no Kleisli operator surface on a non-monadic carrier)") {
    STATIC_CHECK(!HasKleisliBindOperators<int, MaybeBind>);
  }
}
