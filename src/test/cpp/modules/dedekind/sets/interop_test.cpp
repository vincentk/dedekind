#include <catch2/catch_test_macros.hpp>
#include <set>
#include <unordered_set>
#include <vector>

import dedekind.sets;

using namespace dedekind::sets;
using namespace dedekind::interop;

TEST_CASE("Sets: std container interop bridges explicit extensional sets",
          "[sets][interop]") {
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
}
