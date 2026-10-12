/** @file dedekind/sequences/halfspace_to_iota_test.cpp
 *
 * Unit coverage for the halfspace ↔ iota_view isomorphism (#703): @c
 * to_iota_view, the adapter from an interval --- the meet of two halfspaces,
 * its endpoints values --- to @c std::ranges::iota_view (its range view), and
 * @c from_iota_view, its total inverse.
 *
 * Coverage:
 *  - The four (lower, upper) strictness combinations normalise to
 *    iota_view's canonical [start, bound) shape with the correct bounds.
 *  - The image iota_view's elements all satisfy the source interval predicate
 *    (the iso's defining property — value-level agreement).
 *  - The image flows into the library's IsFiniteSequence concept via the
 *    existing from_range adapter — the bridge plugs into the sequence layer.
 *  - from_iota_view ∘ to_iota_view is the identity (equality of intervals is
 *    extensional, so an empty interval comes back as the empty interval).
 */

#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <climits>
#include <cstddef>
#include <iterator>
#include <ranges>
#include <type_traits>
#include <vector>

import dedekind.sequences;
import dedekind.order;
import dedekind.category;

using namespace dedekind::sequences;
using dedekind::order::Interval;
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
  // {x : 3 ≤ x < 8} = [3, 8).
  constexpr auto predicate =
      Interval<int>::bounded(3, Strictness::NonStrict, 8, Strictness::Strict);
  const auto iv = to_iota_view(predicate);

  STATIC_CHECK(std::is_same_v<std::remove_cvref_t<decltype(iv)>,
                              std::ranges::iota_view<int, int>>);
  REQUIRE(*iv.begin() == 3);
  REQUIRE(iv_size(iv) == 5u);
  // Every element of the iota_view satisfies the source predicate.
  for (const int x : iv) {
    REQUIRE(predicate(x));
  }
}

TEST_CASE(
    "ranges:halfspace→iota — strictness combinations normalise to "
    "[start, bound)",
    "[ranges][halfspace][iota][strictness]") {
  // (Strict, NonStrict): {x : 3 < x ≤ 8} = [4, 9)
  constexpr auto oi_sn =
      Interval<int>::bounded(3, Strictness::Strict, 8, Strictness::NonStrict);
  const auto iv_sn = to_iota_view(oi_sn);
  REQUIRE(*iv_sn.begin() == 4);
  REQUIRE(iv_size(iv_sn) == 5u);

  // (Strict, Strict): {x : 3 < x < 8} = [4, 8)
  constexpr auto oi_ss =
      Interval<int>::bounded(3, Strictness::Strict, 8, Strictness::Strict);
  const auto iv_ss = to_iota_view(oi_ss);
  REQUIRE(*iv_ss.begin() == 4);
  REQUIRE(iv_size(iv_ss) == 4u);

  // (NonStrict, NonStrict): {x : 3 ≤ x ≤ 8} = [3, 9)
  constexpr auto oi_nn = Interval<int>::bounded(3, Strictness::NonStrict, 8,
                                                Strictness::NonStrict);
  const auto iv_nn = to_iota_view(oi_nn);
  REQUIRE(*iv_nn.begin() == 3);
  REQUIRE(iv_size(iv_nn) == 6u);
}

TEST_CASE(
    "ranges:halfspace→iota — empty interval round-trips to an empty "
    "iota_view",
    "[ranges][halfspace][iota][empty]") {
  // Empty under (Strict, Strict): {x : 5 < x < 5} = ∅
  constexpr auto oi_empty =
      Interval<int>::bounded(5, Strictness::Strict, 5, Strictness::Strict);
  const auto iv = to_iota_view(oi_empty);
  REQUIRE(iv_size(iv) == 0u);
}

TEST_CASE(
    "ranges:halfspace→iota — unsigned carrier, and the empty case does not "
    "wrap",
    "[ranges][halfspace][iota][unsigned]") {
  // Carrier std::size_t: the endpoints are values of the carrier type.
  constexpr auto oi_us =
      Interval<std::size_t>::bounded(std::size_t{3}, Strictness::NonStrict,
                                     std::size_t{7}, Strictness::Strict);
  const auto iv = to_iota_view(oi_us);
  STATIC_CHECK(
      std::is_same_v<std::remove_cvref_t<decltype(iv)>,
                     std::ranges::iota_view<std::size_t, std::size_t>>);
  REQUIRE(iv_size(iv) == 4u);  // {3,4,5,6}

  // Empty after strictness normalisation on an unsigned carrier: the clamp
  // must produce an empty iota_view, not an underflowed (size_t)-1.
  constexpr auto oi_us_empty = Interval<std::size_t>::bounded(
      std::size_t{5}, Strictness::Strict, std::size_t{5}, Strictness::Strict);
  const auto iv_empty = to_iota_view(oi_us_empty);
  REQUIRE(iv_size(iv_empty) == 0u);
}

TEST_CASE(
    "ranges:halfspace→iota — the image plugs into IsFiniteSequence via "
    "from_range",
    "[ranges][halfspace][iota][sequence-bridge]") {
  // The whole point of routing through iota_view: the library's sequence
  // layer already lifts ranges via from_range, so to_iota_view gets us
  // straight into IsFiniteSequence territory.
  constexpr auto oi =
      Interval<int>::bounded(0, Strictness::NonStrict, 4, Strictness::Strict);
  const auto fp = from_range(to_iota_view(oi));
  STATIC_CHECK(IsFiniteSequence<std::remove_cvref_t<decltype(fp)>>);
  REQUIRE(fp.size() == 4u);
  REQUIRE(fp.at(0) == 0);
  REQUIRE(fp.at(3) == 3);
}

TEST_CASE(
    "ranges:iota→halfspace — from_iota_view is the total inverse: every "
    "iota_view denotes an interval (#703 Slice 2)",
    "[ranges][halfspace][iota][inverse]") {
  // Round-trip: the interval comes back unchanged.
  constexpr auto oi =
      Interval<int>::bounded(3, Strictness::NonStrict, 8, Strictness::Strict);
  const auto back = from_iota_view<Strictness::NonStrict, Strictness::Strict>(
      to_iota_view(oi));
  REQUIRE(back == oi);

  // With the endpoints as values the inverse simply constructs: any iota_view
  // names the interval it denotes (the NTTP form could only verify a view
  // against a target type, and had to reject a mismatch).
  const auto other = from_iota_view<Strictness::NonStrict, Strictness::Strict>(
      std::ranges::views::iota(0, 5));
  REQUIRE(other == Interval<int>::bounded(0, Strictness::NonStrict, 5,
                                          Strictness::Strict));
  const auto wider = from_iota_view<Strictness::NonStrict, Strictness::Strict>(
      std::ranges::views::iota(3, 9));
  REQUIRE(wider == Interval<int>::bounded(3, Strictness::NonStrict, 9,
                                          Strictness::Strict));
  // The strictness pair asked for decides how the offsets are undone.
  const auto strict_both =
      from_iota_view<Strictness::Strict, Strictness::Strict>(
          std::ranges::views::iota(4, 8));  // [4, 8) = {x : 3 < x < 8}
  REQUIRE(strict_both ==
          Interval<int>::bounded(3, Strictness::Strict, 8, Strictness::Strict));
}

TEST_CASE("ranges:halfspace ↔ iota — the bridge respects meet (#703 Slice 3a)",
          "[ranges][halfspace][iota][meet]") {
  // The interval ∧ composes with to_iota_view: the image's bounds are exactly
  // the set-intersection bounds.  The meet is the reduced VALUE (a Interval),
  // which to_iota_view reads directly.
  constexpr auto A =
      Interval<int>::bounded(2, Strictness::NonStrict, 8, Strictness::Strict);
  constexpr auto B =
      Interval<int>::bounded(5, Strictness::NonStrict, 10, Strictness::Strict);
  const auto iv_meet = to_iota_view(dedekind::order::structured_and(A, B));
  // Size-check before dereferencing — guards against the structured_and
  // result silently regressing to empty.
  REQUIRE(iv_size(iv_meet) == 3u);  // {5, 6, 7}
  REQUIRE(*iv_meet.begin() == 5);

  // And the iota_view of the meet is the set-intersection of the iota_views
  // of A and B — a value-level lattice-homomorphism check.
  std::vector<int> via_meet(iv_meet.begin(), iv_meet.end());
  std::vector<int> via_intersection;
  std::ranges::set_intersection(to_iota_view(A), to_iota_view(B),
                                std::back_inserter(via_intersection));
  REQUIRE(via_meet == via_intersection);

  // Strictest-wins at a tied boundary: [3, 8) ∧ [3, 8] both with NonStrict
  // lower at 3 ⇒ the meet has lower NonStrict.  Upper Strict beats
  // NonStrict at the same Hi.
  constexpr auto L =
      Interval<int>::bounded(3, Strictness::NonStrict, 8, Strictness::Strict);
  constexpr auto R = Interval<int>::bounded(3, Strictness::NonStrict, 8,
                                            Strictness::NonStrict);
  const auto iv_tied = to_iota_view(dedekind::order::structured_and(L, R));
  REQUIRE(iv_size(iv_tied) == 5u);  // [3, 8) wins over [3, 8]
  REQUIRE(*iv_tied.begin() == 3);
}

TEST_CASE("ranges:halfspace ↔ iota — disjoint meet produces an empty iota_view",
          "[ranges][halfspace][iota][meet][empty]") {
  constexpr auto D1 =
      Interval<int>::bounded(0, Strictness::NonStrict, 3, Strictness::Strict);
  constexpr auto D2 =
      Interval<int>::bounded(5, Strictness::NonStrict, 10, Strictness::Strict);
  const auto iv_disjoint =
      to_iota_view(dedekind::order::structured_and(D1, D2));
  REQUIRE(iv_size(iv_disjoint) == 0u);
}

TEST_CASE("ranges:iota — meet-semilattice law witnesses (#703 Slice 3b)",
          "[ranges][iota][meet-semilattice]") {
  // IotaIntersection: the meet operator on iota_view values.  The lattice
  // laws (associative + commutative + idempotent) are registered as traits
  // and pinned by static_assert in ranges.cppm; here we exhibit them on
  // concrete value-level samples so the trait registrations agree with
  // the actual operator behaviour.
  constexpr IotaIntersection meet{};
  const auto a = std::ranges::views::iota(2, 8);
  const auto b = std::ranges::views::iota(5, 10);
  const auto c = std::ranges::views::iota(3, 9);

  // Idempotent: a ∧ a == a.
  const auto a_meet_a = meet(a, a);
  REQUIRE(iv_size(a_meet_a) == 6u);
  REQUIRE(*a_meet_a.begin() == 2);

  // Commutative: a ∧ b == b ∧ a (both = [5, 8)).
  const auto ab = meet(a, b);
  const auto ba = meet(b, a);
  REQUIRE(iv_size(ab) == iv_size(ba));
  REQUIRE(iv_size(ab) == 3u);
  REQUIRE(*ab.begin() == 5);
  REQUIRE(*ba.begin() == 5);

  // Associative: (a ∧ b) ∧ c == a ∧ (b ∧ c).
  const auto abc_left = meet(meet(a, b), c);
  const auto abc_right = meet(a, meet(b, c));
  REQUIRE(iv_size(abc_left) == iv_size(abc_right));
  REQUIRE(*abc_left.begin() == *abc_right.begin());

  // Disjoint ⇒ empty (the codomain-closure that lets the trait
  // registration stand uniformly).
  const auto d = std::ranges::views::iota(20, 30);
  const auto ad = meet(a, d);
  REQUIRE(iv_size(ad) == 0u);
}

TEST_CASE("ranges:iota — meet handles huge signed ranges (overflow regression)",
          "[ranges][iota][meet-semilattice][overflow]") {
  // Regression for the signed-overflow bug where computing the bound via
  // start + (T)size() narrowed a huge size_t to T and tripped UB.  The
  // fix reads the bound directly from iota_view's end iterator — no size
  // arithmetic — so a huge range like [INT_MIN, INT_MAX) intersected with
  // a small range stays the small range.
  constexpr IotaIntersection meet{};
  const auto huge = std::ranges::views::iota(INT_MIN, INT_MAX);
  const auto small_range = std::ranges::views::iota(3, 8);
  const auto m1 = meet(huge, small_range);
  REQUIRE(iv_size(m1) == 5u);
  REQUIRE(*m1.begin() == 3);
  // Commute the operands too, since meet is commutative.
  const auto m2 = meet(small_range, huge);
  REQUIRE(iv_size(m2) == 5u);
  REQUIRE(*m2.begin() == 3);
}

TEST_CASE(
    "ranges:iota→halfspace — round-trip across the four strictness "
    "combinations and across signed/unsigned carriers",
    "[ranges][halfspace][iota][inverse][round-trip]") {
  constexpr auto oi_sn =
      Interval<int>::bounded(3, Strictness::Strict, 8, Strictness::NonStrict);
  constexpr auto oi_ss =
      Interval<int>::bounded(3, Strictness::Strict, 8, Strictness::Strict);
  constexpr auto oi_nn = Interval<int>::bounded(3, Strictness::NonStrict, 8,
                                                Strictness::NonStrict);
  constexpr auto oi_us =
      Interval<std::size_t>::bounded(std::size_t{3}, Strictness::NonStrict,
                                     std::size_t{7}, Strictness::Strict);

  const auto back_sn =
      from_iota_view<Strictness::Strict, Strictness::NonStrict>(
          to_iota_view(oi_sn));
  REQUIRE(back_sn == oi_sn);
  const auto back_ss = from_iota_view<Strictness::Strict, Strictness::Strict>(
      to_iota_view(oi_ss));
  REQUIRE(back_ss == oi_ss);
  const auto back_nn =
      from_iota_view<Strictness::NonStrict, Strictness::NonStrict>(
          to_iota_view(oi_nn));
  REQUIRE(back_nn == oi_nn);
  const auto back_us =
      from_iota_view<Strictness::NonStrict, Strictness::Strict>(
          to_iota_view(oi_us));
  REQUIRE(back_us == oi_us);

  // An empty interval round-trips to the empty interval.
  constexpr auto oi_empty =
      Interval<int>::bounded(5, Strictness::Strict, 5, Strictness::Strict);
  const auto back_empty =
      from_iota_view<Strictness::Strict, Strictness::Strict>(
          to_iota_view(oi_empty));
  REQUIRE(back_empty == Interval<int>::empty());
}
