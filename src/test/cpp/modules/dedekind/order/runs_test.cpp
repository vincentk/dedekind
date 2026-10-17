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

TEST_CASE("order:runs: one set, one normal form, however it is spelled",
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
    "order:runs: the normal form is read on demand, materialised on "
    "request",
    "[order][runs][materialise]") {
  // The term stays as written; its normal form is a lazy source.
  STATIC_CHECK(IsStepSource<decltype(steps(a))>);
  // A stored run list is the materialisation: its breakpoints, each the value
  // from there on, and a source like any other.
  const auto stored = ext(a);
  CHECK(stored.breakpoints().size() == 4);  // in at 0, out at 5, in, out
  CHECK(stored(4));
  CHECK_FALSE(stored(5));
  CHECK(stored == a);
  CHECK(ext(~stored) == ~a);
}

TEST_CASE("order:runs: the two points of 𝔹 partition it",
          "[order][runs][boolean]") {
  // Spelled in the set grammar; equality is the run lists'.
  constexpr auto b_false = 𝔹 & (π == false);
  constexpr auto b_true = 𝔹 & (π == true);
  STATIC_CHECK(Ø<bool>{} == (b_false & b_true));
  STATIC_CHECK(𝔸<bool>{} == (b_false | b_true));
}

TEST_CASE(
    "order:runs: a predicate built at run time in the order fragment "
    "has the run lists' normal form",
    "[order][runs][runtime]") {
  using T = Term<int>;
  using Q = Pred<int>;
  const Q gt2 = Q::cmp(Rel::Gt, T::proj(0), T::constant(2));
  const Q lt5 = Q::cmp(Rel::Lt, T::proj(0), T::constant(5));
  // {x > 2} ∩ {x < 5} = [3, 4], one normal form with the typed term.
  const auto window = 𝔸<int>{} & Q::conj(gt2, lt5);
  CHECK(window == span(3, 4));
  CHECK(ext(window).breakpoints().size() == 2);
  CHECK((𝔸<int>{} & Q::disj(lt5, gt2)) == 𝔸<int>{});
  // k ⋈ π is read as its converse; != is two runs.
  CHECK((𝔸<int>{} & Q::cmp(Rel::Lt, T::constant(5), T::proj(0))) ==
        I::half(5, Direction::Upward, strict));
  CHECK(std::ranges::distance(runs(steps(
            𝔸<int>{} & Q::cmp(Rel::Ne, T::proj(0), T::constant(5))))) == 2);
  // Outside the fragment, no normal form yet: refused when read.
  CHECK_THROWS_AS(
      ext(𝔸<int>{} & Q::cmp(Rel::Eq,
                            T::bin(Arith::Mod, T::proj(0), T::constant(2)),
                            T::constant(0))),
      not_admitted);
}

TEST_CASE("order:runs: a run list on a word translates, wrapping at the end",
          "[order][runs][translate]") {
  using W = unsigned;
  using T = Term<int>;
  using Q = Pred<int>;
  constexpr W max = std::numeric_limits<W>::max();
  const auto gt5 = ext(𝔸<W>{} & Q::cmp(Rel::Gt, T::proj(0), T::constant(5)));
  // x ↦ x + 1 sends {x > 5} to {x > 6} ∪ {0}: the word's end wraps to 0.
  const auto image = translate(gt5, W{1});
  CHECK(image(0));
  CHECK_FALSE(image(6));
  CHECK(image(7));
  CHECK(image ==
        (𝔸<W>{} & Q::disj(Q::cmp(Rel::Gt, T::proj(0), T::constant(6)),
                          Q::cmp(Rel::Eq, T::proj(0), T::constant(0)))));
  // A bounded set shifts cleanly; translating back is the identity.
  const auto bounded =
      ext(𝔸<W>{} & Q::conj(Q::cmp(Rel::Ge, T::proj(0), T::constant(3)),
                           Q::cmp(Rel::Lt, T::proj(0), T::constant(8))));
  CHECK(translate(bounded, W{1}) ==
        (𝔸<W>{} & Q::conj(Q::cmp(Rel::Ge, T::proj(0), T::constant(4)),
                          Q::cmp(Rel::Lt, T::proj(0), T::constant(9)))));
  CHECK(translate(translate(bounded, W{1}), max) == bounded);
  // (π − 1) > 5 is the translate of π > 5 by 1: the same set as image.
  CHECK(
      (𝔸<W>{} & Q::cmp(Rel::Gt, T::bin(Arith::Sub, T::proj(0), T::constant(1)),
                       T::constant(5))) == image);
  CHECK(translate(ext(𝔸<W>{}), W{7}) == 𝔸<W>{});
  CHECK(translate(ext(Ø<W>{}), W{7}) == Ø<W>{});
}

TEST_CASE("order:runs: a set valued in K₃ is a step function into K₃",
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

TEST_CASE("order:runs: a step at the carrier's end is kept, equality reads it",
          "[order][runs][ends]") {
  using W = std::numeric_limits<int>;
  // {x ≤ max} keeps its step (a window reading stays bounded above) ...
  constexpr auto at_most_max = I::half(W::max(), Direction::Downward, closed);
  CHECK(ext(at_most_max).breakpoints().size() == 1);
  // ... and is the whole carrier as a set of it, while {x > max} is empty.
  STATIC_CHECK(at_most_max == 𝔸<int>{});
  STATIC_CHECK(I::half(W::max(), Direction::Upward, strict) == Ø<int>{});
  // On a discrete chain a strict bound is closed: (0, 3) = [1, 2].
  STATIC_CHECK(I::bounded(0, strict, 3, strict) == span(1, 2));
}

TEST_CASE(
    "order:runs: a connective forgets the order, grouping and repetition of "
    "its operands (#884)",
    "[order][runs][connective]") {
  using Pr = Pred<unsigned>;
  using Tm = Term<unsigned>;
  const Pr lt5 = Pr::cmp(Rel::Lt, Tm::proj(0), Tm::constant(5u));
  const Pr ne2 = Pr::cmp(Rel::Ne, Tm::proj(0), Tm::constant(2u));
  const Pr sq = Pr::cmp(Rel::Eq, Tm::bin(Arith::Mul, Tm::proj(0), Tm::proj(0)),
                        Tm::constant(4u));
  // One form for either order, a repeated operand kept once, the unit dropped.
  const Pr both =
      connective<unsigned>(Pr::Kind::And, {sq, lt5, ne2, lt5, Pr::top()});
  CHECK(same_shape(both, connective<unsigned>(Pr::Kind::And, {ne2, sq, lt5})));
  // Sorted: a projection's comparisons first (by relation: < before !=),
  // then an operation's.
  CHECK(same_shape(both, Pr::conj(Pr::conj(lt5, ne2), sq)));
  // The absorber absorbs; no operands left is the unit.
  CHECK(same_shape(connective<unsigned>(Pr::Kind::Or, {lt5, Pr::top()}),
                   Pr::top()));
  CHECK(same_shape(connective<unsigned>(Pr::Kind::And, {Pr::bottom(), lt5}),
                   Pr::bottom()));
  CHECK(same_shape(connective<unsigned>(Pr::Kind::Or, {}), Pr::bottom()));
  // The order is total and structural.
  CHECK(structural_order(lt5, ne2) < 0);
  CHECK(structural_order(ne2, sq) < 0);
  CHECK(structural_order(sq, sq) == 0);
}
