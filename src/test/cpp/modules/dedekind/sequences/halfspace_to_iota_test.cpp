/** @file dedekind/sequences/halfspace_to_iota_test.cpp
 *
 * Unit coverage for the typed→runtime half of the halfspace ↔ iota_view
 * isomorphism (#703 Slice 1): @c to_iota_view, the adapter from
 * @c order::OrderInterval (a compile-time typed-Δ⁰₁ predicate) to
 * @c std::ranges::iota_view (its range view).
 *
 * Coverage:
 *  - The four (lower, upper) strictness combinations normalise to
 *    iota_view's canonical [start, bound) shape with the correct bounds.
 *  - The image iota_view's elements all satisfy the source OrderInterval
 *    predicate (the iso's defining property — value-level agreement).
 *  - Cardinalities agree: OrderInterval::size() == iota_view's element count.
 *  - The image flows into the library's IsFiniteSequence concept via the
 *    existing from_range adapter — the bridge plugs into the sequence layer.
 */

#include <catch2/catch_test_macros.hpp>
#include <ranges>
#include <type_traits>

import dedekind.sequences;
import dedekind.order;
import dedekind.category;

using namespace dedekind::sequences;
using dedekind::order::OrderInterval;
using dedekind::order::Strictness;

namespace {

template <typename Iv>
constexpr std::size_t iv_size(const Iv& iv) {
  // iota_view<T,T> with integral T has a size() member.
  return static_cast<std::size_t>(iv.size());
}

}  // namespace

TEST_CASE(
    "ranges:halfspace→iota — [Lo, Hi) (lower NonStrict, upper Strict): "
    "the canonical iota_view shape",
    "[ranges][halfspace][iota]") {
  // OrderInterval<int, 3, 8, NonStrict, Strict> = {x : 3 ≤ x < 8} = [3, 8).
  using OI =
      OrderInterval<int, 3, 8, Strictness::NonStrict, Strictness::Strict>;
  constexpr OI predicate{};
  const auto iv = to_iota_view(predicate);

  STATIC_CHECK(std::is_same_v<std::remove_cvref_t<decltype(iv)>,
                              std::ranges::iota_view<int, int>>);
  REQUIRE(*iv.begin() == 3);
  REQUIRE(iv_size(iv) == 5u);
  REQUIRE(iv_size(iv) == predicate.size());
  // Every element of the iota_view satisfies the source predicate.
  for (const int x : iv) {
    REQUIRE(predicate(x));
  }
}

TEST_CASE(
    "ranges:halfspace→iota — strictness combinations normalise to "
    "[start, bound)",
    "[ranges][halfspace][iota][strictness]") {
  // (Strict, NonStrict): {x : Lo < x ≤ Hi} = [Lo+1, Hi+1) = [4, 9) for
  // Lo=3,Hi=8
  using OI_SN =
      OrderInterval<int, 3, 8, Strictness::Strict, Strictness::NonStrict>;
  const auto iv_sn = to_iota_view(OI_SN{});
  REQUIRE(*iv_sn.begin() == 4);
  REQUIRE(iv_size(iv_sn) == 5u);

  // (Strict, Strict): {x : Lo < x < Hi} = [Lo+1, Hi) = [4, 8)
  using OI_SS =
      OrderInterval<int, 3, 8, Strictness::Strict, Strictness::Strict>;
  const auto iv_ss = to_iota_view(OI_SS{});
  REQUIRE(*iv_ss.begin() == 4);
  REQUIRE(iv_size(iv_ss) == 4u);

  // (NonStrict, NonStrict): {x : Lo ≤ x ≤ Hi} = [Lo, Hi+1) = [3, 9)
  using OI_NN =
      OrderInterval<int, 3, 8, Strictness::NonStrict, Strictness::NonStrict>;
  const auto iv_nn = to_iota_view(OI_NN{});
  REQUIRE(*iv_nn.begin() == 3);
  REQUIRE(iv_size(iv_nn) == 6u);

  // Cardinality agreement on each shape:
  REQUIRE(iv_size(iv_sn) == OI_SN{}.size());
  REQUIRE(iv_size(iv_ss) == OI_SS{}.size());
  REQUIRE(iv_size(iv_nn) == OI_NN{}.size());
}

TEST_CASE(
    "ranges:halfspace→iota — empty interval round-trips to an empty "
    "iota_view",
    "[ranges][halfspace][iota][empty]") {
  // Empty under (Strict, Strict): {x : 5 < x < 5} = ∅
  using OI_empty =
      OrderInterval<int, 5, 5, Strictness::Strict, Strictness::Strict>;
  const auto iv = to_iota_view(OI_empty{});
  REQUIRE(iv_size(iv) == 0u);
  REQUIRE(iv_size(iv) == OI_empty{}.size());
}

TEST_CASE(
    "ranges:halfspace→iota — unsigned carrier: pivots cross types, and the "
    "empty case does not wrap",
    "[ranges][halfspace][iota][unsigned]") {
  // Pivots are int (3, 7); carrier is std::size_t — exercises the
  // auto-NTTP / convertible-to-T pivot deduction.
  using OI_us = OrderInterval<std::size_t, 3, 7, Strictness::NonStrict,
                              Strictness::Strict>;
  const auto iv = to_iota_view(OI_us{});
  STATIC_CHECK(
      std::is_same_v<std::remove_cvref_t<decltype(iv)>,
                     std::ranges::iota_view<std::size_t, std::size_t>>);
  REQUIRE(iv_size(iv) == 4u);  // {3,4,5,6}

  // Empty after strictness normalisation on an unsigned carrier: the clamp
  // must produce an empty iota_view, not an underflowed (size_t)-1.
  using OI_us_empty =
      OrderInterval<std::size_t, 5, 5, Strictness::Strict, Strictness::Strict>;
  const auto iv_empty = to_iota_view(OI_us_empty{});
  REQUIRE(iv_size(iv_empty) == 0u);
}

TEST_CASE(
    "ranges:halfspace→iota — the image plugs into IsFiniteSequence via "
    "from_range",
    "[ranges][halfspace][iota][sequence-bridge]") {
  // The whole point of routing through iota_view: the library's sequence
  // layer already lifts ranges via from_range, so to_iota_view gets us
  // straight into IsFiniteSequence territory.
  using OI =
      OrderInterval<int, 0, 4, Strictness::NonStrict, Strictness::Strict>;
  const auto fp = from_range(to_iota_view(OI{}));
  STATIC_CHECK(IsFiniteSequence<std::remove_cvref_t<decltype(fp)>>);
  REQUIRE(fp.size() == 4u);
  REQUIRE(fp.at(0) == 0);
  REQUIRE(fp.at(3) == 3);
}
