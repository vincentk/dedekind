/** @file dedekind/order/runs_test.cpp
 *
 * The normal form of the sets of a chain built from intervals: every spelling
 * of one set reaches one run list; a term built from the lattice nodes stays as
 * written until normalised; a set valued in K₃ is a run list of K₃ values; and
 * a step at the carrier's end is kept while equality reads the end.
 */

#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <limits>
#include <type_traits>

import dedekind.category;
import dedekind.sets;
import dedekind.order;

using namespace dedekind::category;
using namespace dedekind::sets;
using namespace dedekind::order;

namespace {
using I = Interval<int>;
using R = Runs<int>;
constexpr auto closed = Strictness::NonStrict;
constexpr auto strict = Strictness::Strict;
I span(int lo, int hi) { return I::bounded(lo, closed, hi, closed); }
}  // namespace

TEST_CASE("order:runs — one set, one run list, however it is spelled",
          "[order][runs][normal-form]") {
  // [0, 10] without 5, five ways.
  const R a = span(0, 10) & ~I::point(5);
  const R b = span(0, 4) | span(6, 10);
  const R c = ~(~span(0, 10) | I::point(5));  // De Morgan
  const R d =
      span(6, 10) | (span(0, 10) & I::half(5, Direction::Downward, strict));
  const R e = (span(0, 10) & ~I::point(5)) | (span(0, 10) & ~I::point(5));
  for (const R& x : {b, c, d, e}) {
    CHECK(x.first() == a.first());
    CHECK(x.steps() == a.steps());
    CHECK(x == a);
  }
  CHECK(intervals(a).size() == 2);
  CHECK(a(4));
  CHECK_FALSE(a(5));
  CHECK(a(6));
  CHECK_FALSE(a(11));
  // The algebra's laws hold on the normal form.
  CHECK((a & ~a) == Ø<int>{});
  CHECK((a | ~a) == 𝔸<int>{});
  CHECK((~~a).steps() == a.steps());
  CHECK(((a | span(4, 6)) == span(0, 10)));
}

TEST_CASE("order:runs — normalisation is an explicit step",
          "[order][runs][normalise]") {
  // A term built from the lattice nodes is kept as written and evaluated
  // pointwise; normalise folds it through the one reducer into a run list.
  const auto raw = Meet<I, Not<I>>{span(0, 10), Not<I>{I::point(5)}};
  CHECK(raw(4));
  CHECK_FALSE(raw(5));
  const auto nf = normalise(raw);
  STATIC_CHECK(std::same_as<std::remove_cvref_t<decltype(nf)>, R>);
  CHECK(nf == (span(0, 4) | span(6, 10)));
  // The operators normalise as they go.
  STATIC_CHECK(
      std::same_as<std::remove_cvref_t<decltype(span(0, 10) & ~I::point(5))>,
                   R>);
}

TEST_CASE("order:runs — a set valued in K₃ is a run list of K₃ values",
          "[order][runs][kleene]") {
  using K = Runs<int, Kleene>;
  using IK = Interval<int, Kleene>;
  // Unknown on [0, 9], true on [3, 5]: three values along the line.
  const K maybe = K::of(IK::bounded(0, closed, 9, closed), Ternary::Unknown);
  const K surely = K::of(IK::bounded(3, closed, 5, closed));
  const K both = maybe | surely;
  CHECK(both(-1) == Ternary::False);
  CHECK(both(1) == Ternary::Unknown);
  CHECK(both(4) == Ternary::True);
  CHECK(both(8) == Ternary::Unknown);
  CHECK((~both)(1) == Ternary::Unknown);
  CHECK((~both)(4) == Ternary::False);
  // Equality is a K₃ verdict, ⋀ (a ⇔ b): false where they disagree, and only
  // to degree U where either is U --- even a set against itself.
  CHECK((both == K::empty()) == Ternary::False);
  CHECK((maybe == K::empty()) == Ternary::Unknown);
  CHECK((both == both) == Ternary::Unknown);
}

TEST_CASE("order:runs — a step at the carrier's end is kept, equality reads it",
          "[order][runs][ends]") {
  using W = std::numeric_limits<int>;
  // {x ≤ max} keeps its step (a window reading stays bounded above) ...
  const R at_most_max = I::half(W::max(), Direction::Downward, closed);
  CHECK(at_most_max.steps().size() == 1);
  // ... and is the whole carrier as a set of it, while {x > max} is empty.
  CHECK(at_most_max == 𝔸<int>{});
  CHECK(R::of(I::half(W::max(), Direction::Upward, strict)) == Ø<int>{});
  // On a discrete chain a strict bound is closed: (0, 3) = [1, 2].
  CHECK(R::of(I::bounded(0, strict, 3, strict)).steps() ==
        R::of(span(1, 2)).steps());
}
