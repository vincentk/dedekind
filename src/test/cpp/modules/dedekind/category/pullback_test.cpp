/** @file src/test/cpp/modules/dedekind/category/pullback_test.cpp */
#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <utility>

import dedekind.category;

using namespace dedekind::category;

namespace {
// Two subobject inclusions into the ambient ℤ, presented as genuinely monic
// (injective), TOTAL finite embeddings --- no arithmetic, so neither int's
// signed-overflow UB (2*a) nor size_t's modular non-injectivity (2*a mod 2^N
// collides) applies.  Each is a two-point window of a subgroup: 2ℤ restricted
// to {0, 2} and 3ℤ restricted to {0, 3}, indexed by @c bool (false ↦ 0,
// true ↦ the nonzero representative).  They share exactly the point 0, so the
// pullback of the two inclusions is that intersection {0} --- the least shared
// multiple (6ℤ) meeting the window.  Named functors, not lambdas, so the
// inclusion is nameable.
struct IncludeTwoZ {
  using Domain = bool;
  using Codomain = int;
  constexpr int operator()(bool a) const { return a ? 2 : 0; }
};
struct IncludeThreeZ {
  using Domain = bool;
  using Codomain = int;
  constexpr int operator()(bool b) const { return b ? 3 : 0; }
};
}  // namespace

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

TEST_CASE("Pullback: TernaryLogic classifier",
          "[category][pullback][ternary]") {
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

// #881 (category → sets alignment): the intersection of two subobjects IS the
// pullback of their inclusions (the fiber product over the ambient).  Here the
// two-point windows 2ℤ ⊇ {0,2} ↪ ℤ ↩ {0,3} ⊆ 3ℤ pull back to their intersection
// {0}: χ fires on a pair (a, b) exactly when the images agree, and the images
// (0/2 and 0/3) agree only at 0 --- the least shared multiple (6ℤ) meeting the
// window.  This is the categorical anchor for set intersection; the sets-level
// meet (structured_and, χ_{A∩B} = χ_A ∧ χ_B) is the classifier presentation of
// this same pullback.  bool domains make the inclusions genuinely monic +
// total.
TEST_CASE("Pullback: intersection of subobjects is the pullback of inclusions",
          "[category][pullback][intersection]") {
  using Π = std::pair<bool, bool>;
  auto iota2 = arrow<bool, int>(IncludeTwoZ{});    // {0,2} ↪ ℤ
  auto iota3 = arrow<bool, int>(IncludeThreeZ{});  // {0,3} ↪ ℤ
  auto P = pullback<ClassicalLogic, Π>(iota2, iota3);

  STATIC_CHECK(IsPullback<decltype(P), decltype(iota2), decltype(iota3)>);

  SECTION("χ fires exactly on the shared element (0)") {
    CHECK(P.χ({false, false}));       // 0 = 0 : the shared point
    CHECK_FALSE(P.χ({true, true}));   // 2 ≠ 3
    CHECK_FALSE(P.χ({true, false}));  // 2 ≠ 0
    CHECK_FALSE(P.χ({false, true}));  // 0 ≠ 3
  }
  SECTION("the pullback projections recover the two witnesses") {
    typename decltype(P)::Member m{{false, false}};
    CHECK(P.π1(m) == false);
    CHECK(P.π2(m) == false);
  }
}
