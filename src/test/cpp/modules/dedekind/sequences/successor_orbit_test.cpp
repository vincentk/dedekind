/**
 * @file dedekind/sequences/successor_orbit_test.cpp
 * @brief The unfold lives in @c sequences: @c SuccessorOrbit<N> is
 *        @c iterate(a, Successor<N>{}), the NNO's universal morphism as a
 *        sequence; @c iterate samples it; its finite prefix is the interval
 *        read as @c iota_view (#1001, slice B).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <ranges>

import dedekind.category;
import dedekind.order;
import dedekind.sequences;
import dedekind.sets;

using namespace dedekind::category;
using namespace dedekind::order;
using namespace dedekind::sequences;
using namespace dedekind::sets;

TEST_CASE("sequences — the successor orbit is the one unfold, three spellings",
          "[sequences][nno][orbit][unfold]") {
  const SuccessorOrbit<int> orbit{3};
  const auto stream = iterate(3, Successor<int>{});
  for (std::size_t n = 0; n < 6; ++n) {
    CHECK(orbit.at(n) == 3 + static_cast<int>(n));
    CHECK(orbit.at(n) == stream.at(n));
    CHECK(orbit.at(n) == dedekind::category::iterate(Successor<int>{}, 3, n));
  }
}

TEST_CASE(
    "sequences — the interval is the orbit's finite prefix, which is the "
    "iota view",
    "[sequences][nno][orbit][interval][iota]") {
  constexpr int a = 3, b = 8;
  const auto prefix_ab =
      prefix(SuccessorOrbit<int>{a}, static_cast<std::size_t>(b - a));
  STATIC_CHECK(IsFiniteSequence<decltype(prefix_ab)>);
  CHECK(std::ranges::equal(as_range(prefix_ab), std::views::iota(a, b)));
  CHECK(std::ranges::equal(
      as_range(prefix_ab),
      to_iota_view(Interval<int>::bounded(a, Strictness::NonStrict, b,
                                          Strictness::Strict))));
  // The empty interval is the empty prefix.
  CHECK(prefix(SuccessorOrbit<int>{a}, 0).size() == 0u);
}

TEST_CASE("sequences — the orbit's shape is the carrier's posture at the bound",
          "[sequences][nno][orbit][saturation]") {
  // K₃: ⊥ → U → ⊤ → ⊤ → …, eventually constant.
  const SuccessorOrbit<Ternary> k3{Ternary::False};
  CHECK(k3.at(0) == Ternary::False);
  CHECK(k3.at(1) == Ternary::Unknown);
  CHECK(k3.at(2) == Ternary::True);
  CHECK(k3.at(7) == Ternary::True);
  // ℕ: the orbit walks, with no top to absorb it.
  const SuccessorOrbit<NatExpr<>> nat{NatExpr<>{40u}};
  CHECK(nat.at(2) == NatExpr<>{42u});
}

TEST_CASE(
    "sequences — the orbit on ℕ's and ℤ's terms adds the offset, at any "
    "capacity",
    "[sequences][nno][orbit][terms]") {
  // A seed at its bound: 63 nodes, the sum 1 + 1 doubled five times.
  NatExpr<> seed = NatExpr<>::atom(1);
  for (int i = 0; i < 5; ++i) seed = seed + seed;
  REQUIRE(seed.size() == 63u);
  const SuccessorOrbit<NatExpr<>> at_bound{seed};
  CHECK(at_bound.at(0) == seed);
  CHECK(at_bound.at(3) == NatExpr<>{35u});
  // A seed whose canonical form fills the capacity: 2^1280, 21 limbs, 63
  // nodes.  Its successor's form fits too, though seed + 1 as a term does
  // not: the offset is added in a scratch.
  const NatExpr<> full = power_of_two<NatSort>(1280);
  REQUIRE(full.size() == 63u);
  const SuccessorOrbit<NatExpr<>> filled{full};
  CHECK(filled.at(1) == successor(full));
  CHECK(filled.at(1) > full);
  CHECK(filled.at(1).size() == 63u);
  // The capacity is kept: ℕ at 128, and ℤ, a large offset in one addition.
  const SuccessorOrbit<NatExpr<128>> wide{NatExpr<128>{5u}};
  CHECK(wide.at(1000000) == NatExpr<128>{1000005u});
  constexpr std::size_t big = std::size_t{1} << 62;
  const SuccessorOrbit<IntExpr<>> integers{IntExpr<>{-5}};
  CHECK(integers.at(big) == IntExpr<>{-5} + IntExpr<>::atom(big));
  CHECK(integers.at(big).size() <= 3u);
}
