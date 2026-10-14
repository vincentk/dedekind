/** @file dedekind/order/halfspace_test.cpp
 *
 * Unit coverage for the order reading of the term grammar: an order atom
 * `π ⋈ c` on a chain binds to an `Interval<T, L>` value, intervals meet,
 * join and complement, and the point `Singleton<T, L>{v}`.
 */

#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <limits>
#include <type_traits>
#include <utility>

import dedekind.category;
import dedekind.sets;
import dedekind.order;
import dedekind.relational; // RelAnd / RelOr (the structured meet/join result)

using namespace dedekind::category;
using namespace dedekind::sets;
using namespace dedekind::order;

namespace {
using I = Interval<int>;
constexpr auto strict = Strictness::Strict;
constexpr auto closed = Strictness::NonStrict;
constexpr I up(int p, Strictness s) { return I::half(p, Direction::Upward, s); }
constexpr I down(int p, Strictness s) {
  return I::half(p, Direction::Downward, s);
}
// A function-pointer predicate (not a class functor): a Set over one must still
// combine through the free set operators (exercised via a Meet below).
constexpr bool is_pos(int x) { return x > 0; }
}  // namespace

TEST_CASE("order:halfspace — an order atom binds to the ray it names",
          "[order][halfspace]") {
  constexpr auto gt = 𝔸<int>{} | (π > 5);
  constexpr auto ge = 𝔸<int>{} | (π >= 5);
  constexpr auto lt = 𝔸<int>{} | (π < 5);
  constexpr auto le = 𝔸<int>{} | (π <= 5);
  STATIC_CHECK(gt == up(5, strict));
  STATIC_CHECK(ge == up(5, closed));
  STATIC_CHECK(lt == down(5, strict));
  STATIC_CHECK(le == down(5, closed));
  CHECK(gt(6));
  CHECK_FALSE(gt(5));  // boundary excluded
  CHECK(ge(5));        // boundary included
  CHECK(lt(4));
  CHECK_FALSE(lt(5));
  CHECK(le(5));
  CHECK_FALSE(le(6));
}

TEST_CASE("order:halfspace — the union of two intervals",
          "[order][halfspace][union]") {
  // One operand covering the other: the union is the cover, read off the
  // normal form of the Join node.
  STATIC_CHECK((up(5, strict) | up(7, strict)) == up(5, strict));
  STATIC_CHECK((down(5, strict) | down(3, strict)) == down(5, strict));
  // Overlapping opposite rays: the union is the Join node, deciding by
  // membership, and it covers the line.
  constexpr auto u = up(3, closed) | down(5, closed);
  STATIC_CHECK(static_cast<bool>(u(0)));
  STATIC_CHECK(static_cast<bool>(u(4)));
  STATIC_CHECK(static_cast<bool>(u(100)));
}

TEST_CASE("order:halfspace — the complement of an interval",
          "[order][halfspace][complement]") {
  // A ray's complement is the opposite ray; a bounded interval's is the two
  // rays around it, a Join of two intervals.
  constexpr auto gaps = ~I::bounded(2, closed, 5, closed);
  CHECK(static_cast<bool>(gaps(1)));
  CHECK_FALSE(static_cast<bool>(gaps(3)));
  CHECK(static_cast<bool>(gaps(6)));
  STATIC_CHECK((I::bounded(2, closed, 5, closed) & gaps) == Ø<int>{});
  STATIC_CHECK((up(5, strict) & ~up(5, strict)) == Ø<int>{});
}

TEST_CASE("order:halfspace — excluded middle B ∪ ¬B covers the plane (#365)",
          "[order][halfspace][set][complement]") {
  // A set unioned with its complement covers the ambient, the dual of the
  // contradiction B ∩ ¬B = Ø; decided by membership.
  constexpr auto B = ℕ * ℕ | π_1 > 5;
  constexpr auto cover = B | ~B;
  CHECK(static_cast<bool>(
      cover(std::pair{finite_cardinality(6), finite_cardinality(0)})));
  CHECK(static_cast<bool>(
      cover(std::pair{finite_cardinality(0), finite_cardinality(0)})));
}

TEST_CASE("order:halfspace — covering XOR stays an IsSet (#864 CP review)",
          "[order][halfspace][set][xor]") {
  // {x > 10} △ {x < 100}: in exactly one, {x ≤ 10} ∪ {x ≥ 100}.
  constexpr auto a = up(10, strict);
  constexpr auto b = down(100, strict);
  STATIC_CHECK(IsSetObject<decltype(a ^ b)>);
  CHECK((a ^ b)(5));         // in b, not a
  CHECK((a ^ b)(200));       // in a, not b
  CHECK_FALSE((a ^ b)(50));  // in both → excluded
}

TEST_CASE(
    "order:halfspace: the irreducible Meet / Join fallbacks are directly "
    "covered (#365/#892)",
    "[order][halfspace][set][predicate]") {
  SECTION(
      "function-pointer predicate combines via the free operator& (an "
      "irreducible Meet node, itself a set object)") {
    constexpr Comprehension<𝔸<int, Boole>, bool (*)(int)> pos{&is_pos};
    constexpr auto cap = down(10, strict);  // x < 10
    using M = std::decay_t<decltype(pos & cap)>;
    STATIC_CHECK(
        std::same_as<M, Meet<Comprehension<𝔸<int, Boole>, bool (*)(int)>, I>>);
    CHECK((pos & cap)(5));         // 0 < 5 < 10
    CHECK_FALSE((pos & cap)(-1));  // not > 0
    CHECK_FALSE((pos & cap)(20));  // not < 10
  }

  SECTION(
      "predicate-level && / || build the marker-preserving RelAnd / RelOr") {
    // Two projection predicates: structured_and / structured_or (order)
    // dispatch the generic operator&& / operator|| to RelAnd / RelOr, which
    // stay IsRelPredicate (so the result can feed the 𝔸<pair> | relpred
    // comprehension) --- unlike the generic AndPredicate / OrPredicate, which
    // would drop the marker (#824).
    constexpr auto p = π_1 > 5;
    constexpr auto q = π_2 > 3;
    using AndP = std::decay_t<decltype(p && q)>;
    using OrP = std::decay_t<decltype(p || q)>;
    STATIC_CHECK(
        std::same_as<AndP,
                     dedekind::relational::RelAnd<std::decay_t<decltype(p)>,
                                                  std::decay_t<decltype(q)>>>);
    STATIC_CHECK(
        std::same_as<OrP,
                     dedekind::relational::RelOr<std::decay_t<decltype(p)>,
                                                 std::decay_t<decltype(q)>>>);
    STATIC_CHECK(IsRelPredicate<AndP>);  // the marker is preserved through &&
    STATIC_CHECK(IsRelPredicate<OrP>);   // ... and through ||
    CHECK((p || q)(std::pair{finite_cardinality(6), finite_cardinality(0)}));
    CHECK_FALSE(
        (p && q)(std::pair{finite_cardinality(6), finite_cardinality(0)}));
    CHECK((p && q)(std::pair{finite_cardinality(6), finite_cardinality(4)}));
  }
}

TEST_CASE("order:halfspace — Singleton identity and cross-L equality",
          "[order][halfspace][singleton]") {
  constexpr Singleton<int> s_classical{4};
  constexpr Singleton<int, Kleene> s_ternary{4};

  SECTION("Membership at the inhabitant") {
    STATIC_CHECK(s_classical(4) == Boole::True);
    STATIC_CHECK(s_classical(5) == Boole::False);
  }

  SECTION("Cross-L equality: Singleton<V> is Singleton<V> regardless of L") {
    STATIC_CHECK(s_classical == s_ternary);
  }

  SECTION("Size is 1") { STATIC_CHECK(s_classical.size() == 1u); }
}

TEST_CASE("order:halfspace — interval membership matches its sides",
          "[order][halfspace][order_interval]") {
  constexpr auto iv = I::bounded(1, strict, 4, closed);  // (1, 4]
  STATIC_CHECK(!static_cast<bool>(iv(1)));  // strict lower: 1 excluded
  STATIC_CHECK(static_cast<bool>(iv(2)));
  STATIC_CHECK(static_cast<bool>(iv(4)));  // non-strict upper: 4 included
  STATIC_CHECK(!static_cast<bool>(iv(5)));
}

TEST_CASE("order:halfspace — Singleton satisfies the consolidated tiers",
          "[order][halfspace][singleton][computability]") {
  // Sets-level concepts (HasDecidableMembership in :sets:computability,
  // IsExtensional in :sets:cardinality post-2026-05-09 consolidation),
  // instantiated on order-level types. The sets test target cannot
  // import dedekind.order, so these downstream conformance checks live
  // here.
  STATIC_CHECK(HasDecidableMembership<Singleton<int>>);
  STATIC_CHECK(IsExtensional<Singleton<int>>);

  SECTION("Kleene variant: extensional but not decidable") {
    STATIC_CHECK_FALSE(HasDecidableMembership<Singleton<int, Kleene>>);
    STATIC_CHECK(IsExtensional<Singleton<int, Kleene>>);
  }
}

TEST_CASE("order:halfspace: point-free ℕ|pred is carrier-axis decidable (#848)",
          "[order][halfspace][computability][point-free]") {
  // The point-free comprehension ℕ | (χ > 5) reduces to a bare
  // Halfspace in the universe's species, Boole: decidable membership.  Pairs
  // the module-level static_assert witness with a Codecov-visible runtime
  // membership exercise.
  constexpr auto point_free = ℕ | (π > 5);
  STATIC_CHECK(HasDecidableMembership<decltype(point_free)>);

  // Runtime membership on {x ∈ ℕ | x > 5}: 6 ∈, 5 ∉ --- exercises operator()
  // for coverage (static_asserts are invisible to Codecov).
  CHECK(static_cast<bool>(point_free(finite_cardinality(6))));
  CHECK_FALSE(static_cast<bool>(point_free(finite_cardinality(5))));
}

TEST_CASE(
    "order:halfspace — the product of two intervals is the generic "
    "cartesian product (a box)",
    "[order][halfspace][product]") {
  constexpr auto a = I::bounded(0, strict, 5, strict);
  constexpr auto b = I::bounded(0, strict, 3, strict);
  // a = {1,2,3,4}, b = {1,2}
  constexpr auto box = a * b;
  STATIC_CHECK(IsSetObject<decltype(box)>);
  // FIXME(#975): the box's finite cardinality (|a|·|b| = 4·2) is a property of
  // the normal form (runs per coordinate), not of a bespoke product type; the
  // generic product's cardinality_type is the carrier axis until then.

  SECTION("2D membership is the conjunction of factor memberships") {
    using Logic = typename decltype(box)::logic_species;

    STATIC_CHECK(box(std::pair{2, 2}) == Logic::True);
    STATIC_CHECK(box(std::pair{0, 2}) == Logic::False);  // 0 ∉ a
    STATIC_CHECK(box(std::pair{2, 0}) == Logic::False);  // 0 ∉ b
    STATIC_CHECK(box(std::pair{0, 0}) == Logic::False);
  }
}

// Runtime coverage for the projection-arithmetic functional graphs (the
// static_asserts in halfspace.cppm are invisible to coverage).  volatile
// coords force the predicate bodies to actually execute at run time.
TEST_CASE("order:halfspace — projection-arithmetic functional graphs (runtime)",
          "[order][relation][projection-arithmetic]") {
  volatile std::size_t a = 4, m = 20;
  const auto A = finite_cardinality(std::size_t(a));  // 4
  const auto M = finite_cardinality(std::size_t(m));  // 20

  const auto succ = ℕ * ℕ | π_1 + 1 == π_2;  // b = a + 1
  CHECK(succ(std::pair{A, finite_cardinality(5)}));
  CHECK_FALSE(succ(std::pair{A, finite_cardinality(6)}));

  const auto dbl = ℕ * ℕ | π_1 * 2 == π_2;  // b = 2a
  CHECK(dbl(std::pair{finite_cardinality(3), finite_cardinality(6)}));
  CHECK_FALSE(dbl(std::pair{finite_cardinality(3), finite_cardinality(7)}));

  const auto res = ℕ * ℕ | π_1 % 17 == π_2;  // b = a % 17
  CHECK(res(std::pair{M, finite_cardinality(3)}));
  CHECK_FALSE(res(std::pair{M, finite_cardinality(4)}));
}

TEST_CASE("order:halfspace — structural subset ⊆ and derived >=,<,> (#831)",
          "[order][halfspace][subset]") {
  constexpr auto gt5 = up(5, strict);
  constexpr auto gt3 = up(3, strict);
  constexpr auto ge5 = up(5, closed);
  constexpr auto lt3 = down(3, strict);
  constexpr auto lt5 = down(5, strict);

  SECTION("subset via the lattice identity A ⊆ B ⟺ A ∩ B = A") {
    static_assert(bool(gt5 <= gt3), "{x>5} ⊆ {x>3}");
    static_assert(!bool(gt3 <= gt5), "{x>3} ⊄ {x>5}");
    static_assert(bool(ge5 <= gt3), "{x≥5} ⊆ {x>3}");
    static_assert(bool(gt5 <= ge5), "{x>5} ⊆ {x≥5}");
    static_assert(!bool(ge5 <= gt5), "{x≥5} ⊄ {x>5} (5 ∈ LHS, ∉ RHS)");
    static_assert(bool(lt3 <= lt5), "{x<3} ⊆ {x<5}");
    static_assert(!bool(lt5 <= lt3), "{x<5} ⊄ {x<3}");
    // Opposite directions now decide (#832): the meet is empty, and
    // Ø / EmptyPredicate == Halfspace is a theorem — a Halfspace is a proper
    // cut by construction, so it is never empty.
    static_assert(!bool(gt5 <= lt3), "{x>5} ⊄ {x<3} (disjoint, meet ∅)");
    static_assert(!bool(lt3 <= gt5), "{x<3} ⊄ {x>5}");
    static_assert(!bool(ge5 <= lt5), "{x≥5} ⊄ {x<5} (complement pair, meet Ø)");
    CHECK(bool(gt5 <= gt3));
    CHECK_FALSE(bool(gt3 <= gt5));
    CHECK(bool(lt3 <= lt5));
    CHECK_FALSE(bool(gt5 <= lt3));
  }

  SECTION("empty ⊆ anything; anything ⊆ universe") {
    static_assert(bool(Ø<int>{} <= gt5), "∅ ⊆ {x>5}");
    static_assert(bool(gt5 <= 𝔸<int>{}), "{x>5} ⊆ ℤ");
    CHECK(bool(Ø<int>{} <= gt5));
    CHECK(bool(gt5 <= 𝔸<int>{}));
  }

  SECTION("singleton ⊆ via membership") {
    constexpr Singleton<int, Boole> s5{5};
    static_assert(bool(s5 <= ge5), "{5} ⊆ {x≥5}");
    static_assert(!bool(s5 <= gt5), "{5} ⊄ {x>5}");
    CHECK(bool(s5 <= ge5));
    CHECK_FALSE(bool(s5 <= gt5));
  }

  SECTION("derived >=, <, > from the primitive <= and ==") {
    static_assert(bool(gt3 >= gt5), "{x>3} ⊇ {x>5}");
    static_assert(bool(gt5 < gt3), "{x>5} ⊊ {x>3}");
    static_assert(!bool(gt5 < gt5), "not a proper subset of itself");
    static_assert(bool(gt3 > gt5), "{x>3} ⊋ {x>5}");
    static_assert(!bool(gt5 > gt3), "{x>5} ⊉̸ {x>3} strictly");
    CHECK(bool(gt3 >= gt5));
    CHECK(bool(gt5 < gt3));
    CHECK_FALSE(bool(gt5 < gt5));
    CHECK(bool(gt3 > gt5));
  }

  SECTION("interval subset; derived ⊇ rides it") {
    constexpr auto i25 = I::bounded(2, closed, 5, closed);
    constexpr auto i16 = I::bounded(1, closed, 6, closed);
    static_assert(bool(i25 <= i16), "[2,5] ⊆ [1,6]");
    static_assert(!bool(i16 <= i25), "[1,6] ⊄ [2,5]");
    static_assert(bool(i16 >= i25), "[1,6] ⊇ [2,5] (derived, rides <=)");
    CHECK(bool(i25 <= i16));
    CHECK_FALSE(bool(i16 <= i25));
  }

  SECTION("empty intervals: ∅ ⊆ X, and the collapse is sign-safe") {
    constexpr auto empty = I::bounded(5, strict, 5, strict);
    constexpr auto i01 = I::bounded(0, closed, 1, closed);
    static_assert(empty == Ø<int>{});
    static_assert(bool(empty <= i01), "∅ ⊆ [0,1] despite disjoint endpoints");
    static_assert(
        Interval<unsigned>::bounded(5u, strict, 3u, strict) == Ø<unsigned>{},
        "(5u,3u) is empty, not a wrapped unsigned span");
    static_assert(I::bounded(5, strict, 6, strict) == Ø<int>{},
                  "(5,6) has no integer strictly between");
    static_assert(
        !(I::bounded(std::numeric_limits<int>::min(), closed,
                     std::numeric_limits<int>::max(), closed) == Ø<int>{}),
        "[INT_MIN,INT_MAX] is non-empty");
    CHECK(bool(empty <= i01));
  }

  SECTION("discrete intervals compare by their closed bounds (#835)") {
    // (1,4) and [2,3] both denote {2,3} over int.
    constexpr auto open14 = I::bounded(1, strict, 4, strict);
    constexpr auto clos23 = I::bounded(2, closed, 3, closed);
    static_assert(open14 == clos23);
    static_assert(bool(open14 <= clos23) && bool(clos23 <= open14));
    CHECK(open14 == clos23);
  }
}

TEST_CASE("order:halfspace — a ray at the carrier's end is a value fact",
          "[order][halfspace][boundary]") {
  // The pivot is a value: {x ≥ 0} on ℕ is all of ℕ, {x > INT_MAX} is empty,
  // and on ℤ (no floor) {z < 0} is neither.
  STATIC_CHECK((ℕ | (π >= 0)) == ℕ);
  STATIC_CHECK((𝔸<int>{} | (π > std::numeric_limits<int>::max())) == Ø<int>{});
  STATIC_CHECK(!((𝔸<SignedCardinality>{} | (π < 0)) == Ø<SignedCardinality>{}));
  STATIC_CHECK(
      !((𝔸<SignedCardinality>{} | (π >= 0)) == 𝔸<SignedCardinality>{}));
}

TEST_CASE(
    "order:halfspace — the point-free former threads the universe's species "
    "into the halfspace",
    "[order][halfspace][species]") {
  constexpr auto hs = 𝔸<int, Kleene>{} | (π > 5);  // {x > 5}, L = Kleene
  STATIC_CHECK(std::same_as<typename decltype(hs)::logic_species, Kleene>);
  STATIC_CHECK(
      std::same_as<typename decltype(hs)::Codomain, typename Kleene::Ω>);
  STATIC_CHECK(IsLSet<decltype(hs)> && !IsSet<decltype(hs)>);  // Ω ≠ 𝔹
  STATIC_CHECK_FALSE(HasDecidableMembership<decltype(hs)>);
  CHECK(hs(6) == Kleene::True);
  CHECK(hs(5) == Kleene::False);
}

// Witness: an Interval lives on a REGISTERED chain (IsTotallyOrdered), not
// merely a std::totally_ordered one.  The probe is a concept because an
// unsatisfied class-template constraint is a hard error outside a template.
template <typename T>
concept IntervalOver = requires { typename Interval<T>; };

TEST_CASE("order:halfspace — the carrier gate is the registered chain",
          "[order][halfspace][witness]") {
  STATIC_CHECK(IntervalOver<int>);
  // ℤ: a chain under the posture that ±ℵ_0 / NaZ are the memory boundary.
  STATIC_CHECK(IntervalOver<SignedCardinality>);
  // A raw float has no certified order (NaN breaks reflexivity).
  STATIC_CHECK_FALSE(IntervalOver<double>);
}

// The power set 𝔓 (#830) is exercised in order/powerset_test.cpp.
