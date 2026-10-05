/**
 * @file dedekind/sequences/successor_orbit_test.cpp
 * @brief The unfold lives in @c sequences: @c SuccessorOrbit<N> is
 *        @c iterate(a, Successor<N>{}), the NNO's universal morphism as a
 *        sequence; @c nno_iterate samples it; its finite prefix is the interval
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
    CHECK(orbit.at(n) == nno_iterate(3, Successor<int>{}, n));
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
      to_iota_view(
          make_interval<Strictness::NonStrict, Strictness::Strict>(a, b))));
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
  // ℕ's proxy: the finite fragment walks, the top absorbs.
  const SuccessorOrbit<Cardinality> nat{finite_cardinality(40)};
  CHECK(nat.at(2) == finite_cardinality(42));
  const SuccessorOrbit<Cardinality> top{Cardinality{ℵ_0{}}};
  CHECK(top.at(5) == Cardinality{ℵ_0{}});
}
