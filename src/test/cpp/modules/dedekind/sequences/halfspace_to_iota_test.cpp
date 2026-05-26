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

#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <iterator>
#include <ranges>
#include <type_traits>
#include <vector>

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

TEST_CASE(
    "ranges:iota→halfspace — from_iota_view rebuilds OI from a matching "
    "iota_view, rejects a mismatched one (#703 Slice 2)",
    "[ranges][halfspace][iota][inverse]") {
  using OI =
      OrderInterval<int, 3, 8, Strictness::NonStrict, Strictness::Strict>;
  // Round-trip on a matching iota_view: from_iota_view sees the [3, 8)
  // bounds and rebuilds OI{}.
  const auto matched = from_iota_view<OI>(to_iota_view(OI{}));
  REQUIRE(matched.has_value());

  // Honest-Rejection: an iota_view with the wrong bounds yields nullopt —
  // the iso is value-level, so only the SPECIFIC iota_view value
  // to_iota_view(OI{}) corresponds to OI{}.
  const auto wrong_bounds = from_iota_view<OI>(std::ranges::views::iota(0, 5));
  REQUIRE_FALSE(wrong_bounds.has_value());

  // Even a partial mismatch (correct start, wrong bound) is rejected.
  const auto partial = from_iota_view<OI>(std::ranges::views::iota(3, 9));
  REQUIRE_FALSE(partial.has_value());
}

TEST_CASE("ranges:halfspace ↔ iota — the bridge respects meet (#703 Slice 3a)",
          "[ranges][halfspace][iota][meet]") {
  // The OrderInterval ∧ on the carrier composes with to_iota_view: the
  // image's bounds are exactly the set-intersection bounds.
  using A = OrderInterval<int, 2, 8, Strictness::NonStrict, Strictness::Strict>;
  using B =
      OrderInterval<int, 5, 10, Strictness::NonStrict, Strictness::Strict>;
  const auto iv_meet = to_iota_view(dedekind::order::structured_and(A{}, B{}));
  // Size-check before dereferencing — guards against the structured_and
  // result silently regressing to empty.
  REQUIRE(iv_size(iv_meet) == 3u);  // {5, 6, 7}
  REQUIRE(*iv_meet.begin() == 5);

  // And the iota_view of the meet is the set-intersection of the iota_views
  // of A and B — a value-level lattice-homomorphism check.
  std::vector<int> via_meet(iv_meet.begin(), iv_meet.end());
  std::vector<int> via_intersection;
  std::ranges::set_intersection(to_iota_view(A{}), to_iota_view(B{}),
                                std::back_inserter(via_intersection));
  REQUIRE(via_meet == via_intersection);

  // Strictest-wins at a tied boundary: [3, 8) ∧ [3, 8] both with NonStrict
  // lower at 3 ⇒ the meet has lower NonStrict.  Upper Strict beats
  // NonStrict at the same Hi.
  using L = OrderInterval<int, 3, 8, Strictness::NonStrict, Strictness::Strict>;
  using R =
      OrderInterval<int, 3, 8, Strictness::NonStrict, Strictness::NonStrict>;
  const auto iv_tied = to_iota_view(dedekind::order::structured_and(L{}, R{}));
  REQUIRE(iv_size(iv_tied) == 5u);  // [3, 8) wins over [3, 8]
  REQUIRE(*iv_tied.begin() == 3);
}

TEST_CASE("ranges:halfspace ↔ iota — disjoint meet produces an empty iota_view",
          "[ranges][halfspace][iota][meet][empty]") {
  using D1 =
      OrderInterval<int, 0, 3, Strictness::NonStrict, Strictness::Strict>;
  using D2 =
      OrderInterval<int, 5, 10, Strictness::NonStrict, Strictness::Strict>;
  const auto iv_disjoint =
      to_iota_view(dedekind::order::structured_and(D1{}, D2{}));
  REQUIRE(iv_size(iv_disjoint) == 0u);
}

TEST_CASE(
    "ranges:iota→halfspace — round-trip across the four strictness "
    "combinations and across signed/unsigned carriers",
    "[ranges][halfspace][iota][inverse][round-trip]") {
  using OI_SN =
      OrderInterval<int, 3, 8, Strictness::Strict, Strictness::NonStrict>;
  using OI_SS =
      OrderInterval<int, 3, 8, Strictness::Strict, Strictness::Strict>;
  using OI_NN =
      OrderInterval<int, 3, 8, Strictness::NonStrict, Strictness::NonStrict>;
  using OI_us = OrderInterval<std::size_t, 3, 7, Strictness::NonStrict,
                              Strictness::Strict>;

  REQUIRE(from_iota_view<OI_SN>(to_iota_view(OI_SN{})).has_value());
  REQUIRE(from_iota_view<OI_SS>(to_iota_view(OI_SS{})).has_value());
  REQUIRE(from_iota_view<OI_NN>(to_iota_view(OI_NN{})).has_value());
  REQUIRE(from_iota_view<OI_us>(to_iota_view(OI_us{})).has_value());

  // Empty interval round-trips to nullopt? No — to_iota_view produces an
  // empty iota_view at the clamped bounds; from_iota_view should accept it
  // since the bounds match the clamped construction.
  using OI_empty =
      OrderInterval<int, 5, 5, Strictness::Strict, Strictness::Strict>;
  REQUIRE(from_iota_view<OI_empty>(to_iota_view(OI_empty{})).has_value());
}
