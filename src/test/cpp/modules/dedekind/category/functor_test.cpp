#include <catch2/catch_test_macros.hpp>
#include <concepts>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE("Category: Functor Concepts", "[category][functor]") {
  using IntCat = DiscreteCategory<int>;
  auto plus_one = arrow([](int x) { return x + 1; });

  STATIC_CHECK(IsFunctor<identity_functor<IntCat>>);
  STATIC_CHECK(IsEndofunctor<identity_functor<IntCat>>);
  STATIC_CHECK(IsFunctor<maybe_functor<int>>);
  STATIC_CHECK(IsFunctor<trace_functor<int>>);
  STATIC_CHECK(IsSpokeArrow<decltype(plus_one)>);
}

TEST_CASE("Category: Functor hub action", "[category][functor][hub-action]") {
  SECTION("Identity functor preserves arrow behavior") {
    identity_functor<DiscreteCategory<int>> idf;
    auto plus_one = arrow([](int x) { return x + 1; });

    auto lifted = idf.φ(plus_one);

    STATIC_CHECK(IsArrow<decltype(lifted)>);
    CHECK(lifted(41) == 42);
    CHECK(lifted(-2) == -1);
  }

  SECTION("Maybe functor short-circuits empty inputs") {
    maybe_functor<int> maybef;
    auto plus_one = arrow([](int x) { return x + 1; });

    auto lifted = maybef.φ(plus_one);

    STATIC_CHECK(IsArrow<decltype(lifted)>);
    CHECK(lifted(std::optional<int>{41}) == std::optional<int>{42});
    CHECK(lifted(std::optional<int>{}) == std::nullopt);
  }

  SECTION("Trace functor lifts into StringCategory") {
    trace_functor<int> tracef;
    auto plus_one = arrow([](int x) { return x + 1; });

    auto lifted = tracef.φ(plus_one);

    STATIC_CHECK(IsArrow<decltype(lifted)>);
    CHECK(lifted.label == "lifted");
    CHECK(lifted.domain_id == 0);
    CHECK(lifted.codomain_id == 0);
  }
}

TEST_CASE("Category: Functor composition", "[category][functor][composition]") {
  using IntCat = DiscreteCategory<int>;
  using IdF = identity_functor<IntCat>;

  auto plus_one = arrow([](int x) { return x + 1; });
  composite_functor<IdF, IdF> composed{};

  SECTION("Composed functor keeps category handles") {
    static_assert(IsFunctor<decltype(composed)>);
    static_assert(std::same_as<typename decltype(composed)::Σ_cat, IntCat>);
    static_assert(std::same_as<typename decltype(composed)::Τ_cat, IntCat>);
    SUCCEED();
  }

  SECTION("Composed object mapping matches direct application") {
    auto mapped = composed(IntCat{});

    STATIC_CHECK(std::same_as<decltype(mapped), IntCat>);
    SUCCEED();
  }

  SECTION("Composed action matches direct application") {
    auto via_composed = composed.φ(plus_one);
    auto via_direct = IdF{}.φ(plus_one);

    CHECK(via_composed(9) == via_direct(9));
    CHECK(via_composed(-1) == via_direct(-1));
  }

  SECTION("Fish composition instantiates to a functor") {
    auto downstream = IdF{} >> IdF{};
    auto upstream = IdF{} << IdF{};

    STATIC_CHECK(IsFunctor<decltype(downstream)>);
    STATIC_CHECK(IsFunctor<decltype(upstream)>);
    CHECK(downstream.φ(plus_one)(9) == 10);
    CHECK(upstream.φ(plus_one)(9) == 10);
  }
}

TEST_CASE("Category: functor composition type proof",
          "[category][functor][composition-proof]") {
  using IntCat = DiscreteCategory<int>;
  using IdF = identity_functor<IntCat>;

  using Arrow = typename IdF::Σ_cat::Arrow;
  using Path1 = decltype(std::declval<IdF const&>().φ(std::declval<Arrow>() >>
                                                      std::declval<Arrow>()));
  using Path2 = decltype(std::declval<IdF const&>().φ(std::declval<Arrow>()) >>
                         std::declval<IdF const&>().φ(std::declval<Arrow>()));

  STATIC_CHECK(std::same_as<Path1, Path2>);
  SUCCEED();
}

TEST_CASE("Category: Endofunctor algebras and fixed points",
          "[category][functor][algebra]") {
  using IntCat = DiscreteCategory<int>;
  using IdF = identity_functor<IntCat>;

  auto successor = arrow([](int x) { return x + 1; });
  auto halve = arrow([](int x) { return x / 2; });

  STATIC_CHECK(IsFAlgebra<int, decltype(successor), IdF>);
  STATIC_CHECK(IsFCoalgebra<int, decltype(halve), IdF>);

  // IsAdjunction tightened (#434) to require IsNaturalTransformation
  // unit/counit per Mac Lane / Milewski; the trivial @c IdF ⊣ @c IdF
  // self-adjunction's unit and counit are both the @c
  // identity_transformation on @c IdF.
  using IdT = identity_transformation<IdF>;
  STATIC_CHECK(IsAdjunction<IdF, IdF, IdT, IdT>);

  auto algebra = make_f_algebra(IdF{}, successor);
  auto coalgebra = make_f_coalgebra(IdF{}, halve);
  auto adjunction = make_adjunction(IdF{}, IdF{}, IdT{}, IdT{});

  CHECK(algebra(41) == 42);
  CHECK(coalgebra(42) == 21);
  // Unit / counit of @c IdF ⊣ @c IdF as natural transformations on
  // @c IntCat: at object @c c they return @c id_c, the identity
  // arrow on @c c.  Witnessed structurally — calling the components
  // is enough; the typing derivation discharges the rest.
  STATIC_CHECK(IsArrow<decltype(adjunction.unit(7))>);
  STATIC_CHECK(IsArrow<decltype(adjunction.counit(9))>);

  CHECK(least_fixpoint(0, [](int x) { return x < 5 ? x + 1 : x; }) == 5);

  // Cover the max_iterations-exhausted fallback path in fixed_point.
  CHECK(fixed_point(
            0, [](int x) { return x + 1; }, std::equal_to<int>{}, 3) == 3);
}
