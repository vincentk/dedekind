/** @file src/test/cpp/modules/dedekind/category/pullback_test.cpp */
#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <utility>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE("Pullback: IsEqualizer Concept", "[category][pullback][equalizer]") {
  SECTION("Subobject of a product satisfies IsEqualizer for a parallel pair") {
    using X = int;
    using Z = int;
    using Π = std::pair<X, X>;

    // Two parallel morphisms h, k : Π ⟶ Z
    auto h = arrow<Π, Z>([](const Π& p) { return p.first; });
    auto k = arrow<Π, Z>([](const Π& p) { return p.second; });

    // Build the pullback P of (id, id) over Z = int
    auto f = arrow<X, Z>([](X x) { return x; });
    auto g = arrow<X, Z>([](X y) { return y; });

    auto P = pullback<ClassicalLogic, Π>(f, g);

    // P must satisfy IsEqualizer w.r.t. the parallel pair (h, k)
    STATIC_CHECK(IsEqualizer<decltype(P), decltype(h), decltype(k)>);
  }
}

TEST_CASE("Pullback: IsPullback Concept", "[category][pullback]") {
  SECTION("Fiber product of (f, g : int ⟶ int) satisfies IsPullback") {
    using X = int;
    using Y = int;
    using Z = int;
    using Π = std::pair<X, Y>;

    auto f = arrow<X, Z>([](X x) { return x; });
    auto g = arrow<Y, Z>([](Y y) { return y; });

    auto P = pullback<ClassicalLogic, Π>(f, g);

    STATIC_CHECK(IsPullback<decltype(P), decltype(f), decltype(g)>);
  }
}

TEST_CASE("Pullback: Factory Runtime Behavior", "[category][pullback]") {
  using X = int;
  using Y = int;
  using Z = int;
  using Π = std::pair<X, Y>;

  // f(x) = x,  g(y) = y  =>  pullback selects pairs where x == y
  auto f = arrow<X, Z>([](X x) { return x; });
  auto g = arrow<Y, Z>([](Y y) { return y; });

  auto P = pullback<ClassicalLogic, Π>(f, g);

  SECTION("χ is true for pairs where f(x) == g(y)") {
    CHECK(P.χ({3, 3}) == true);
    CHECK(P.χ({0, 0}) == true);
    CHECK(P.χ({-5, -5}) == true);
  }

  SECTION("χ is false for pairs where f(x) != g(y)") {
    CHECK(P.χ({1, 2}) == false);
    CHECK(P.χ({0, 1}) == false);
    CHECK(P.χ({-1, 1}) == false);
  }

  SECTION("Projections π₁ and π₂ recover the original components") {
    using Member = typename decltype(P)::Member;
    Member m{{4, 7}};
    CHECK(P.π1(m) == 4);
    CHECK(P.π2(m) == 7);
  }

  SECTION("Inclusion ι embeds a Member into the ambient product space") {
    using Member = typename decltype(P)::Member;
    Member m{{3, 3}};
    Π ambient = P.ι(m);
    CHECK(ambient.first == 3);
    CHECK(ambient.second == 3);
  }
}

TEST_CASE("Pullback: Non-identity morphisms", "[category][pullback]") {
  using X = int;
  using Y = int;
  using Z = int;
  using Π = std::pair<X, Y>;

  // f(x) = 2*x,  g(y) = y  =>  pullback selects pairs where 2x == y
  auto f = arrow<X, Z>([](X x) { return x * 2; });
  auto g = arrow<Y, Z>([](Y y) { return y; });

  auto P = pullback<ClassicalLogic, Π>(f, g);

  SECTION("χ is true when f(x) == g(y), i.e., 2x == y") {
    CHECK(P.χ({1, 2}) == true);
    CHECK(P.χ({3, 6}) == true);
    CHECK(P.χ({0, 0}) == true);
  }

  SECTION("χ is false when f(x) != g(y)") {
    CHECK(P.χ({1, 1}) == false);
    CHECK(P.χ({2, 3}) == false);
  }
}

TEST_CASE("Pullback: TernaryLogic classifier", "[category][pullback][ternary]") {
  using X = int;
  using Y = int;
  using Z = int;
  using Π = std::pair<X, Y>;

  // f(x) = x,  g(y) = y  =>  pullback selects pairs where x == y
  auto f = arrow<X, Z>([](X x) { return x; });
  auto g = arrow<Y, Z>([](Y y) { return y; });

  auto P = pullback<TernaryLogic, Π>(f, g);

  SECTION("χ returns Ternary::True when f(x) == g(y)") {
    CHECK(P.χ({5, 5}) == Ternary::True);
    CHECK(P.χ({0, 0}) == Ternary::True);
  }

  SECTION("χ returns Ternary::False when f(x) != g(y)") {
    CHECK(P.χ({1, 2}) == Ternary::False);
    CHECK(P.χ({-1, 1}) == Ternary::False);
  }
}
