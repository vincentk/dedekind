#include <catch2/catch_test_macros.hpp>
#include <set>
#include <unordered_set>
#include <vector>

import dedekind.category; // ambient_set / IsSet — for the lift-then-certify nugget
import dedekind.sets;

using namespace dedekind::sets;

TEST_CASE("Sets: std container bridges materialise ExtensionalSet",
          "[sets][extensional]") {
  SECTION("Set-like concept recognizes supported std containers") {
    static_assert(StdSetLike<std::set<int>>);
    static_assert(StdSetLike<std::unordered_set<int>>);
    static_assert(!StdSetLike<std::vector<int>>);
  }

  SECTION("Round trip through std::set preserves extensional equality") {
    const std::set<int> ordered{1, 3, 7};

    const auto ext = from_std(ordered);
    REQUIRE(ext.size() == 3u);
    REQUIRE(ext.contains(1));
    REQUIRE(ext.contains(3));
    REQUIRE(ext.contains(7));

    const auto back = to_std<std::set<int>>(ext);
    REQUIRE(back == ordered);

    const auto ext_round_trip = from_std(back);
    REQUIRE(ext_round_trip == ext);
  }

  SECTION("Round trip through std::unordered_set preserves membership") {
    const std::unordered_set<int> values{11, 13, 17};

    const auto ext = from_std(values);
    REQUIRE(ext.size() == 3u);
    REQUIRE(ext.contains(11));
    REQUIRE(ext.contains(13));
    REQUIRE(ext.contains(17));

    const auto back = to_std<std::unordered_set<int>>(ext);
    REQUIRE(back == values);

    const auto ext_round_trip = from_std(back);
    REQUIRE(ext_round_trip == ext);
  }

  SECTION("ExtensionalSet<bool> materialises 𝔹 as a listed 2-element set") {
    // Element-level witness for the type-level breadcrumb in
    // numbers/boolean.cppm:(1b).  std::unordered_set is not
    // constexpr-initializable with non-empty contents in C++23, so the
    // listed-form ExtensionalSet<bool>{false, true} witness lives here
    // at runtime rather than at namespace scope.
    const auto B = from_std(std::unordered_set<bool>{false, true});
    REQUIRE(B.size() == 2u);
    REQUIRE(B.contains(false));
    REQUIRE(B.contains(true));
  }
}

TEST_CASE("Sets: image / filter on std-container carriers (#602, trivial)",
          "[sets][extensional][image][filter]") {
  // Operationally trivial helpers on the canonical extensional carrier.
  // The categorical / pure framework lives in :category; these are
  // engineering convenience for downstream callers holding std containers.

  SECTION("image(double, std::unordered_set<int>) lvalue: {1,2,3} ↦ {2,4,6}") {
    const std::unordered_set<int> S{1, 2, 3};
    const auto T = image([](const int& x) { return 2 * x; }, S);
    STATIC_CHECK(std::same_as<decltype(T), const std::unordered_set<int>>);
    CHECK(T.size() == 3u);
    CHECK(T.contains(2));
    CHECK(T.contains(4));
    CHECK(T.contains(6));
  }

  SECTION("image rvalue, type-changing transform: int ↦ bool via parity") {
    const auto T = image([](const int& x) { return x % 2 == 0; },
                         std::unordered_set<int>{0, 1, 2, 3, 4});
    STATIC_CHECK(std::same_as<decltype(T), const std::unordered_set<bool>>);
    CHECK(T.size() == 2u);
    CHECK(T.contains(true));
    CHECK(T.contains(false));
  }

  SECTION("image(square, std::set<int>) preserves ordered family + collapses") {
    const std::set<int> S{-2, -1, 0, 1, 2};
    const auto T = image([](const int& x) { return x * x; }, S);
    STATIC_CHECK(std::same_as<decltype(T), const std::set<int>>);
    CHECK(T.size() == 3u);
    CHECK(T.contains(0));
    CHECK(T.contains(1));
    CHECK(T.contains(4));
  }

  SECTION("filter(even, std::unordered_set<int>): {1..10} ↦ {2,4,6,8,10}") {
    const std::unordered_set<int> S{1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    const auto T = filter([](const int& x) { return x % 2 == 0; }, S);
    STATIC_CHECK(std::same_as<decltype(T), const std::unordered_set<int>>);
    CHECK(T.size() == 5u);
    CHECK(T.contains(2));
    CHECK(T.contains(10));
    CHECK_FALSE(T.contains(1));
  }

  SECTION("filter(positive, std::set<int>) preserves ordered family") {
    const std::set<int> S{-2, -1, 0, 1, 2};
    const auto T = filter([](const int& x) { return x > 0; }, S);
    STATIC_CHECK(std::same_as<decltype(T), const std::set<int>>);
    CHECK(T.size() == 2u);
    CHECK(T.contains(1));
    CHECK(T.contains(2));
  }

  SECTION("filter on tautology / contradiction: identity / empty") {
    const std::unordered_set<int> S{2, 3, 5, 7, 11};
    CHECK(filter([](const int&) { return true; }, S) == S);
    CHECK(filter([](const int&) { return false; }, S).empty());
  }

  SECTION(
      "Lift-then-certify: image / filter result composes with ambient_set") {
    const std::unordered_set<int> S{1, 2, 3, 4, 5};
    const auto evens = filter([](const int& x) { return x % 2 == 0; }, S);
    const auto evens_isset = dedekind::category::ambient_set(evens);
    STATIC_CHECK(dedekind::category::IsSet<decltype(evens_isset)>);
    CHECK(evens_isset.χ(2));
    CHECK(evens_isset.χ(4));
    CHECK_FALSE(evens_isset.χ(3));
  }
}

TEST_CASE(
    "Dedekind Sets: image(F, ExtensionalSet<T, L>) — #602 Layer 2 / Case B",
    "[sets][image][extensional][enumerated][layer2][602]") {
  // Enumeration-decidable image: no invertibility requirement on F.
  // Decidability comes from the source's extensionality --- iterate,
  // fmap, collect.  The result is an ExtensionalSet<U, L>, preserving
  // the source's logic species.

  SECTION("Endomorphic image preserves Classical logic species") {
    ExtensionalSet<int, dedekind::category::ClassicalLogic> S;
    S.elements = {1, 2, 3, 4, 5};

    auto img = image([](const int& x) { return x * 2; }, S);

    STATIC_CHECK(std::same_as<typename decltype(img)::logic_species,
                              dedekind::category::ClassicalLogic>);
    STATIC_CHECK(std::same_as<typename decltype(img)::Domain, int>);
    CHECK(img.size() == 5u);
    CHECK(img.contains(2));
    CHECK(img.contains(10));
    CHECK_FALSE(img.contains(3));  // Odd, not in 2*{1..5}.
  }

  SECTION("Cross-carrier image (T → U) lands in ExtensionalSet<U, L>") {
    ExtensionalSet<int, dedekind::category::ClassicalLogic> S;
    S.elements = {-1, 0, 1};

    // Map to bool: is the int non-negative?
    auto img = image([](const int& x) { return x >= 0; }, S);

    STATIC_CHECK(std::same_as<typename decltype(img)::Domain, bool>);
    STATIC_CHECK(std::same_as<typename decltype(img)::logic_species,
                              dedekind::category::ClassicalLogic>);
    // {-1, 0, 1} → {false, true} after image.
    CHECK(img.size() == 2u);
    CHECK(img.contains(true));
    CHECK(img.contains(false));
  }

  SECTION(
      "Non-injective F collapses duplicates (decidability via set semantics)") {
    ExtensionalSet<int, dedekind::category::ClassicalLogic> S;
    S.elements = {-2, -1, 0, 1, 2};

    // x ↦ |x| collapses {-2,2} → 2 and {-1,1} → 1.
    auto img = image([](const int& x) { return x < 0 ? -x : x; }, S);

    CHECK(img.size() == 3u);  // {0, 1, 2}
    CHECK(img.contains(0));
    CHECK(img.contains(1));
    CHECK(img.contains(2));
    CHECK_FALSE(img.contains(-1));
  }
}
