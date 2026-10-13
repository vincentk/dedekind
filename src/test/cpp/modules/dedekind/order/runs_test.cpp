/** @file dedekind/order/runs_test.cpp
 *
 * The normal form of the sets of a chain built from intervals: a set term is
 * kept as written, a constant like any other value, and its normal form is a
 * lazy step source read by the query that needs it --- every spelling of one
 * set reads the same steps.  A set valued in K₃ is a step function into K₃; a
 * step at the carrier's end is kept while equality reads the end; a stored
 * run list is the materialisation, made on request.
 */

#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <limits>
#include <ranges>

import dedekind.category;
import dedekind.sets;
import dedekind.order;

using namespace dedekind::category;
using namespace dedekind::sets;
using namespace dedekind::order;

namespace {
using I = Interval<int>;
constexpr auto closed = Strictness::NonStrict;
constexpr auto strict = Strictness::Strict;
constexpr I span(int lo, int hi) { return I::bounded(lo, closed, hi, closed); }

// [0, 10] without 5, five ways: terms, stored as constants.
constexpr auto a = span(0, 10) & ~I::point(5);
constexpr auto b = span(0, 4) | span(6, 10);
constexpr auto c = ~(~span(0, 10) | I::point(5));  // De Morgan
constexpr auto d =
    span(6, 10) | (span(0, 10) & I::half(5, Direction::Downward, strict));
constexpr auto e = (span(0, 10) & ~I::point(5)) | (span(0, 10) & ~I::point(5));

// Kleene's join, as a named function for a merge.
struct KleeneOr {
  constexpr Ternary operator()(Ternary x, Ternary y) const {
    return Kleene::OR(x, y);
  }
};
}  // namespace

TEST_CASE("order:runs — one set, one normal form, however it is spelled",
          "[order][runs][normal-form]") {
  STATIC_CHECK(a == b);
  STATIC_CHECK(a == c);
  STATIC_CHECK(a == d);
  STATIC_CHECK(a == e);
  STATIC_CHECK(std::ranges::distance(intervals(steps(a))) == 2);
  STATIC_CHECK((a & ~a) == Ø<int>{});
  STATIC_CHECK((a | ~a) == 𝔸<int>{});
  STATIC_CHECK((a | span(4, 6)) == span(0, 10));
  CHECK(a(4));
  CHECK_FALSE(a(5));
  CHECK(value_at(steps(a), 6));
  CHECK_FALSE(value_at(steps(a), 11));
  for (const auto& r : runs(steps(a))) CHECK(r.hi - r.lo == 4);
}

TEST_CASE(
    "order:runs — the normal form is read on demand, materialised on "
    "request",
    "[order][runs][materialise]") {
  // The term stays as written; its normal form is a lazy source.
  STATIC_CHECK(IsStepSource<decltype(steps(a))>);
  // A stored run list is the materialisation: its breakpoints, each the value
  // from there on, and a source like any other.
  const Runs<int> stored(steps(a));
  CHECK(stored.breakpoints().size() == 4);  // in at 0, out at 5, in, out
  CHECK(stored(4));
  CHECK_FALSE(stored(5));
  CHECK(stored == a);
  CHECK(Runs<int>(steps(~stored)) == ~a);
}

TEST_CASE("order:runs — a set valued in K₃ is a step function into K₃",
          "[order][runs][kleene]") {
  using IK = Interval<int, Kleene>;
  // Unknown on [0, 9], true on [3, 5]: three values along the line.
  const auto maybe = SpanSteps<int, Kleene>{
      IK::bounded(0, closed, 9, closed).span(), Ternary::Unknown};
  const auto surely = SpanSteps<int, Kleene>{
      IK::bounded(3, closed, 5, closed).span(), Ternary::True};
  const auto both =
      MergeSteps<SpanSteps<int, Kleene>, SpanSteps<int, Kleene>, KleeneOr>{
          maybe, surely, {}};
  CHECK(value_at(both, -1) == Ternary::False);
  CHECK(value_at(both, 1) == Ternary::Unknown);
  CHECK(value_at(both, 4) == Ternary::True);
  CHECK(value_at(both, 8) == Ternary::Unknown);
  // Equality is a K₃ verdict, ⋀ (a ⇔ b): false where they disagree, and only
  // to degree U where either is U --- even a set against itself.
  const auto none = SpanSteps<int, Kleene>{std::nullopt};
  CHECK(equal(both, none) == Ternary::False);
  CHECK(equal(maybe, none) == Ternary::Unknown);
  CHECK(equal(both, both) == Ternary::Unknown);
  CHECK(exists(both) == Ternary::True);
  CHECK(forall(both) == Ternary::False);
}

TEST_CASE("order:runs — a step at the carrier's end is kept, equality reads it",
          "[order][runs][ends]") {
  using W = std::numeric_limits<int>;
  // {x ≤ max} keeps its step (a window reading stays bounded above) ...
  constexpr auto at_most_max = I::half(W::max(), Direction::Downward, closed);
  CHECK(Runs<int>(steps(at_most_max)).breakpoints().size() == 1);
  // ... and is the whole carrier as a set of it, while {x > max} is empty.
  STATIC_CHECK(at_most_max == 𝔸<int>{});
  STATIC_CHECK(I::half(W::max(), Direction::Upward, strict) == Ø<int>{});
  // On a discrete chain a strict bound is closed: (0, 3) = [1, 2].
  STATIC_CHECK(I::bounded(0, strict, 3, strict) == span(1, 2));
}
