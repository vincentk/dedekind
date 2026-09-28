/** @file dedekind/order/interval_reducer_test.cpp
 *
 * An interval IS the meet of two opposing halfspaces: the reducer's crossing
 * @c Meet node, an @c IsProduct over @c Halfspace under the @c MakeMeet
 * pairing. No separate interval encoding is needed --- the two bounding
 * halfspaces are the data, membership is their conjunction, and the interval
 * flows through the value-first term reducer like any other lattice term.
 *
 * The exhibit: a @b semantically @b empty interval (pivots that admit no
 * member) reduces to the empty set.  That is a value-determined collapse the
 * type-level reducer cannot see --- two intervals of one type differ only in
 * their pivots
 * --- so it needs the value leg of the injected leaf-combine (@c SetCombine),
 * which hands the two halfspaces to the carrier's @c structured_and by ADL.
 */

#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <type_traits>

import dedekind.category;
import dedekind.sets;
import dedekind.order;

using namespace dedekind::category;
using namespace dedekind::sets;
using namespace dedekind::order;

namespace {
using Up = Halfspace<int, Direction::Upward, Strictness::Strict>;      // {x>lo}
using Down = Halfspace<int, Direction::Downward, Strictness::Strict>;  // {x<hi}
using Interval = Meet<Up, Down>;  // (lo, hi): the crossing meet IS the interval
}  // namespace

// The interval is the categorical product of its two bounding halfspaces under
// the meet pairing: π_1 / π_2 recover the halfspaces and MakeMeet builds it.
static_assert(IsProduct<Interval, Up, Down, MakeMeet>,
              "an interval is an IsProduct over Halfspace under MakeMeet.");

TEST_CASE(
    "order:interval — Meet<Halfspace↑, Halfspace↓> flows through the value "
    "reducer",
    "[order][halfspace][reducer][value-first]") {
  SECTION("a semantically empty interval (5,5) reduces to the empty set") {
    const auto iv = MakeMeet{}(Up{5}, Down{5});  // {x>5} ∩ {x<5} = ∅
    const auto r = subobject_reduce<Boole, SetCombine>(iv);
    CHECK(r.kind == SetKind::Empty);
    CHECK(!static_cast<bool>(r(5)));
    CHECK(!static_cast<bool>(r(6)));
  }

  SECTION("an inverted interval (7,3) reduces to the empty set") {
    const auto r =
        subobject_reduce<Boole, SetCombine>(MakeMeet{}(Up{7}, Down{3}));
    CHECK(r.kind == SetKind::Empty);
  }

  SECTION("a one-point interval (3,5) reduces to the point {4}") {
    const auto r =
        subobject_reduce<Boole, SetCombine>(MakeMeet{}(Up{3}, Down{5}));
    CHECK(r.kind == SetKind::Singleton);
    CHECK(r.lo == 4);
    CHECK(static_cast<bool>(r(4)));
    CHECK(!static_cast<bool>(r(3)));
  }

  SECTION("a wide interval (-21,21) stays an interval with its bounds") {
    const auto r =
        subobject_reduce<Boole, SetCombine>(MakeMeet{}(Up{-21}, Down{21}));
    CHECK(r.kind == SetKind::Interval);
    CHECK(r.lo == -21);
    CHECK(r.hi == 21);
    CHECK(static_cast<bool>(r(0)));
    CHECK(!static_cast<bool>(r(21)));
  }

  SECTION("dual-phase: the empty collapse folds in constant evaluation") {
    constexpr auto r =
        subobject_reduce<Boole, SetCombine>(MakeMeet{}(Up{5}, Down{5}));
    STATIC_REQUIRE(r.kind == SetKind::Empty);
  }

  SECTION("value leaves: a Meet of two SetVal halfspaces collapses the same") {
    // The leaf type the Python surface builds.  The value leg dispatches on
    // structured_and by ADL, so the same law fires on value leaves as on bare
    // halfspaces: a contradicting pair reduces to the empty set, not to an
    // interval that is merely logically empty.
    using V = SetVal<int, Boole>;
    const auto empty = subobject_reduce<Boole, SetCombine>(
        Meet<V, V>{V::half(5, Direction::Upward, Strictness::Strict),
                   V::half(5, Direction::Downward, Strictness::Strict)});
    CHECK(empty.kind == SetKind::Empty);
    const auto point = subobject_reduce<Boole, SetCombine>(
        Meet<V, V>{V::half(3, Direction::Upward, Strictness::Strict),
                   V::half(5, Direction::Downward, Strictness::Strict)});
    CHECK(point.kind == SetKind::Singleton);
    CHECK(point.lo == 4);
  }

  SECTION("ℕ: the point collapse gates on the NNO, not on std::integral") {
    // The carrier is the Cardinality proxy for ℕ --- not a machine integer.
    // The one-point collapse needs successor / predecessor, which are an axiom
    // of the CATEGORY (the NNO) that the proxy witnesses, so (3,5) on ℕ folds
    // to {4} exactly as it does on int, and (5,5) to the empty set.
    using NUp = Halfspace<Cardinality, Direction::Upward, Strictness::Strict>;
    using NDown =
        Halfspace<Cardinality, Direction::Downward, Strictness::Strict>;
    const auto point = subobject_reduce<Boole, SetCombine>(
        MakeMeet{}(NUp{finite_cardinality(3)}, NDown{finite_cardinality(5)}));
    CHECK(point.kind == SetKind::Singleton);
    CHECK(point.lo == finite_cardinality(4));
    const auto empty = subobject_reduce<Boole, SetCombine>(
        MakeMeet{}(NUp{finite_cardinality(5)}, NDown{finite_cardinality(5)}));
    CHECK(empty.kind == SetKind::Empty);
  }

  SECTION("without the value leg the reducer keeps the Meet (type fallback)") {
    // The default policy has no value leg, so the type-level normal form ---
    // the irreducible Meet of two distinct leaves --- is reconstructed as-is.
    // It still decides membership pointwise; it just cannot SEE that it is
    // empty.
    const auto iv = MakeMeet{}(Up{5}, Down{5});
    const auto r = subobject_reduce<Boole>(iv);
    STATIC_REQUIRE(std::same_as<std::remove_cvref_t<decltype(r)>, Interval>);
    CHECK(!static_cast<bool>(r(5)));
  }
}
