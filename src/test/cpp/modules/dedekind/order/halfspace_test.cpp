/** @file dedekind/order/halfspace_test.cpp
 *
 * Unit coverage for the value-carrying halfspace DSL: `Halfspace<T, D, S, L>`
 * with its pivot as a value, `Singleton<T, L>{v}`, `Interval<T, SL, SU, L>`
 * (= `Meet<Halfspace↑, Halfspace↓>`, built by `make_interval`), and the
 * `structured_and` overloads that fold them value-first (`reduce_meet` /
 * `SetVal`).
 *
 * Each SECTION exercises one structural branch independently of the Set
 * wrapper; end-to-end Set-level behaviour is covered by the IR showcases.
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

TEST_CASE("order:halfspace — Halfspace operator() on integral carrier",
          "[order][halfspace]") {
  SECTION("Upward, strict: n > 5") {
    constexpr Halfspace<int, Direction::Upward, Strictness::Strict> h{5};
    using Logic = typename decltype(h)::logic_species;

    STATIC_CHECK(h(6) == Logic::True);
    STATIC_CHECK(h(5) == Logic::False);  // boundary excluded
    STATIC_CHECK(h(4) == Logic::False);
  }

  SECTION("Upward, non-strict: n >= 5") {
    constexpr Halfspace<int, Direction::Upward, Strictness::NonStrict> h{5};
    using Logic = typename decltype(h)::logic_species;

    STATIC_CHECK(h(5) == Logic::True);  // boundary included
    STATIC_CHECK(h(6) == Logic::True);
    STATIC_CHECK(h(4) == Logic::False);
  }

  SECTION("Downward, strict: n < 5") {
    constexpr Halfspace<int, Direction::Downward, Strictness::Strict> h{5};
    using Logic = typename decltype(h)::logic_species;

    STATIC_CHECK(h(4) == Logic::True);
    STATIC_CHECK(h(5) == Logic::False);
  }

  SECTION("Downward, non-strict: n <= 5") {
    constexpr Halfspace<int, Direction::Downward, Strictness::NonStrict> h{5};
    using Logic = typename decltype(h)::logic_species;

    STATIC_CHECK(h(5) == Logic::True);
    STATIC_CHECK(h(4) == Logic::True);
    STATIC_CHECK(h(6) == Logic::False);
  }
}

TEST_CASE("order:halfspace — Variable DSL constructs Halfspace from bound<V>",
          "[order][halfspace][dsl]") {
  // ℕ rather than ℤ so the order-test target stays upstream of numbers: ℤ
  // lives in `dedekind.numbers`, which is downstream of `dedekind.order`
  // in the build DAG.  Post-#559 ℕ is the universe value 𝔸<Cardinality>;
  // the underlying carrier is Cardinality (the variant ℕ-proxy from
  // #402), so the test exercises `Halfspace<Cardinality, ...>`
  // instantiations.
  // The DSL binds the pivot-less Halfspace TYPE; the pivot 7 rides in the
  // instance (checked via .pivot).
  SECTION("> constructs Upward/Strict") {
    constexpr auto h = ℕ | (χ > fix(7_c));
    using H = std::decay_t<decltype(h)>;
    STATIC_CHECK(
        std::same_as<
            H, Halfspace<Cardinality, Direction::Upward, Strictness::Strict>>);
    STATIC_CHECK(h.pivot == 7);
  }

  SECTION(">= constructs Upward/NonStrict") {
    constexpr auto h = ℕ | (χ >= fix(7_c));
    using H = std::decay_t<decltype(h)>;
    STATIC_CHECK(std::same_as<H, Halfspace<Cardinality, Direction::Upward,
                                           Strictness::NonStrict>>);
    STATIC_CHECK(h.pivot == 7);
  }

  SECTION("< constructs Downward/Strict") {
    constexpr auto h = ℕ | (χ < fix(7_c));
    using H = std::decay_t<decltype(h)>;
    STATIC_CHECK(std::same_as<H, Halfspace<Cardinality, Direction::Downward,
                                           Strictness::Strict>>);
    STATIC_CHECK(h.pivot == 7);
  }

  SECTION("<= constructs Downward/NonStrict") {
    constexpr auto h = ℕ | (χ <= fix(7_c));
    using H = std::decay_t<decltype(h)>;
    STATIC_CHECK(std::same_as<H, Halfspace<Cardinality, Direction::Downward,
                                           Strictness::NonStrict>>);
    STATIC_CHECK(h.pivot == 7);
  }
  // Note (post-#409 review): the DSL constraint also rejects negative
  // signed pivots on unsigned carriers (e.g. `ℕ | (χ > fix(-1_c))` does not
  // compile, where previously int→unsigned conversion would
  // wrap -1 to UINT_MAX silently).  The regression is exercised
  // implicitly: dropping the constraint would not break any existing
  // test, but enabling it does not break any either, so the rejection
  // is observable only via the constraint-level diagnostic at any
  // would-be call site.  A direct `static_assert(!requires { ... })`
  // witness is not stable here because the generic relational
  // operators in `:expressions` propagate substitution failure as a
  // hard error inside the requires-clause; tightening those is
  // tracked under the bare-`Domain` audit (#411).
}

namespace {
// A Boole halfspace over int, abbreviated for the union/meet tests.  The alias
// fixes direction/strictness; the pivot rides in the instance (HS<D, S>{piv}).
template <Direction D, Strictness S>
using HS = Halfspace<int, D, S, Boole>;
// A function-pointer predicate (not a class functor): a Set over one must still
// combine through the free set operators (exercised via a Meet below).
constexpr bool is_pos(int x) { return x > 0; }
}  // namespace

TEST_CASE("order:halfspace — structured_or joins halfspaces (#365)",
          "[order][halfspace][structured_or]") {
  // A same-direction union collapses to a value SetVal (the weaker/wider bound
  // wins).  A crossing union has no SetVal kind on a runtime pivot, so it is
  // the honest point-wise set (decided by membership).
  SECTION("Upward ∪ Upward: the weaker (wider) pivot wins") {
    constexpr auto r =
        structured_or(HS<Direction::Upward, Strictness::Strict>{5},
                      HS<Direction::Upward, Strictness::Strict>{7});
    STATIC_CHECK(r.kind == SetKind::Halfspace);
    STATIC_CHECK(r.lo == 5);
    STATIC_CHECK(r.dir == Direction::Upward);
  }

  SECTION("Downward ∪ Downward: the wider (larger) pivot wins") {
    constexpr auto r =
        structured_or(HS<Direction::Downward, Strictness::Strict>{5},
                      HS<Direction::Downward, Strictness::Strict>{3});
    STATIC_CHECK(r.kind == SetKind::Halfspace);
    STATIC_CHECK(r.lo == 5);
    STATIC_CHECK(r.dir == Direction::Downward);
  }

  SECTION("Covering opposing (x≥3 ∪ x≤5 overlap [3,5]) covers ℤ") {
    // The opposing-cover-to-universe structural collapse is gone (a value pivot
    // cannot dispatch cover-vs-gap); the union is the point-wise set that still
    // covers every element.
    constexpr auto u = HS<Direction::Upward, Strictness::NonStrict>{3} |
                       HS<Direction::Downward, Strictness::NonStrict>{5};
    STATIC_CHECK(static_cast<bool>(u(0)));
    STATIC_CHECK(static_cast<bool>(u(4)));
    STATIC_CHECK(static_cast<bool>(u(100)));
  }
}

TEST_CASE("order:halfspace — excluded middle B ∪ ¬B covers the plane (#365)",
          "[order][halfspace][set][complement]") {
  // The user's law: a set unioned with its complement covers the ambient, the
  // dual of the contradiction B ∩ ¬B = Ø.  A value pivot cannot dispatch the
  // cover structurally, so the union is the honest point-wise set; it still
  // DECIDES membership --- every pair lands in B or its complement.
  constexpr auto B = ℕ * ℕ | π1 > fix(5_c);
  constexpr auto cover = B | ~B;
  CHECK(static_cast<bool>(
      cover(std::pair{finite_cardinality(6), finite_cardinality(0)})));
  CHECK(static_cast<bool>(
      cover(std::pair{finite_cardinality(0), finite_cardinality(0)})));
}

TEST_CASE("order:halfspace — covering XOR stays an IsSet (#864 CP review)",
          "[order][halfspace][set][xor]") {
  // {x > 10} △ {x < 100}: the union covers the line.  A dormant covering-XOR
  // branch that structured_or once activated returned ¬(A ∩ B) by negating a
  // bare Interval — a Morphism, not a Set.  Removed; the general path must
  // keep △ closed over Set.
  constexpr Comprehension<𝔸<int, Boole>,
                          HS<Direction::Upward, Strictness::Strict>>
      a{HS<Direction::Upward, Strictness::Strict>{10}};
  constexpr Comprehension<𝔸<int, Boole>,
                          HS<Direction::Downward, Strictness::Strict>>
      b{HS<Direction::Downward, Strictness::Strict>{100}};
  STATIC_CHECK(
      IsSetObject<decltype(a ^ b)>);  // a node: a set object, structurally
  // △ = in exactly one: {x ≤ 10} ∪ {x ≥ 100} (the complement of the overlap).
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
    constexpr Comprehension<𝔸<int, Boole>, bool (*)(int)> pos{
        &is_pos};  // x > 0
    constexpr Comprehension<𝔸<int, Boole>,
                            HS<Direction::Downward, Strictness::Strict>>
        cap{HS<Direction::Downward, Strictness::Strict>{10}};  // x < 10
    using M = std::decay_t<decltype(pos & cap)>;
    STATIC_CHECK(
        std::same_as<
            M, Meet<Comprehension<𝔸<int, Boole>, bool (*)(int)>,
                    Comprehension<𝔸<int, Boole>, HS<Direction::Downward,
                                                    Strictness::Strict>>>>);
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
    constexpr auto p = π1 > fix(5_c);
    constexpr auto q = π2 > fix(3_c);
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

TEST_CASE("order:halfspace — Interval size across strictness pairs",
          "[order][halfspace][order_interval]") {
  // An interval is the meet of two halfspaces; its endpoints are values.
  SECTION("strict/strict on ℤ: [1, 4] open") {
    // {n : int | 1 < n < 4} = {2, 3} → size 2
    constexpr auto iv =
        make_interval<Strictness::Strict, Strictness::Strict>(1, 4);
    STATIC_CHECK(dedekind::order::size(iv) == 2u);
  }

  SECTION("strict/non-strict on ℤ: (1, 4]") {
    // {n : int | 1 < n <= 4} = {2, 3, 4} → size 3
    constexpr auto iv =
        make_interval<Strictness::Strict, Strictness::NonStrict>(1, 4);
    STATIC_CHECK(dedekind::order::size(iv) == 3u);
  }

  SECTION("non-strict/non-strict on ℤ: [1, 4]") {
    // {n : int | 1 <= n <= 4} = {1, 2, 3, 4} → size 4
    constexpr auto iv =
        make_interval<Strictness::NonStrict, Strictness::NonStrict>(1, 4);
    STATIC_CHECK(dedekind::order::size(iv) == 4u);
  }

  SECTION("Membership matches boundary semantics") {
    constexpr auto iv =
        make_interval<Strictness::Strict, Strictness::NonStrict>(1, 4);
    using Logic = typename decltype(iv)::logic_species;

    STATIC_CHECK(iv(1) == Logic::False);  // strict lower: 1 excluded
    STATIC_CHECK(iv(2) == Logic::True);
    STATIC_CHECK(iv(4) == Logic::True);  // non-strict upper: 4 included
    STATIC_CHECK(iv(5) == Logic::False);
  }
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
  // The point-free comprehension ℕ | (χ > fix(5_c)) reduces to a bare
  // Halfspace in the universe's species, Boole: decidable membership.  Pairs
  // the module-level static_assert witness with a Codecov-visible runtime
  // membership exercise.
  constexpr auto point_free = ℕ | (χ > fix(5_c));
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
  constexpr auto a =
      make_interval<Strictness::Strict, Strictness::Strict>(0, 5);
  constexpr auto b =
      make_interval<Strictness::Strict, Strictness::Strict>(0, 3);
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

  const auto succ = ℕ * ℕ | π1 + fix(1_c) == π2;  // b = a + 1
  CHECK(succ(std::pair{A, finite_cardinality(5)}));
  CHECK_FALSE(succ(std::pair{A, finite_cardinality(6)}));

  const auto dbl = ℕ * ℕ | π1 * fix(2_c) == π2;  // b = 2a
  CHECK(dbl(std::pair{finite_cardinality(3), finite_cardinality(6)}));
  CHECK_FALSE(dbl(std::pair{finite_cardinality(3), finite_cardinality(7)}));

  const auto res = ℕ * ℕ | π1 % fix(17_c) == π2;  // b = a % 17
  CHECK(res(std::pair{M, finite_cardinality(3)}));
  CHECK_FALSE(res(std::pair{M, finite_cardinality(4)}));
}

TEST_CASE("order:halfspace — structural subset ⊆ and derived >=,<,> (#831)",
          "[order][halfspace][subset]") {
  constexpr Halfspace<int, Direction::Upward, Strictness::Strict> gt5{5};
  constexpr Halfspace<int, Direction::Upward, Strictness::Strict> gt3{3};
  constexpr Halfspace<int, Direction::Upward, Strictness::NonStrict> ge5{5};
  constexpr Halfspace<int, Direction::Downward, Strictness::Strict> lt3{3};
  constexpr Halfspace<int, Direction::Downward, Strictness::Strict> lt5{5};

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

  SECTION("interval subset decides by endpoints; derived ⊇ rides it") {
    constexpr auto i25 =
        make_interval<Strictness::NonStrict, Strictness::NonStrict>(2, 5);
    constexpr auto i16 =
        make_interval<Strictness::NonStrict, Strictness::NonStrict>(1, 6);
    static_assert(bool(i25 <= i16), "[2,5] ⊆ [1,6]");
    static_assert(!bool(i16 <= i25), "[1,6] ⊄ [2,5]");
    static_assert(bool(i16 >= i25), "[1,6] ⊇ [2,5] (derived, rides <=)");
    CHECK(bool(i25 <= i16));
    CHECK_FALSE(bool(i16 <= i25));
  }

  SECTION("empty interval ⊆ every interval (#835 review: ∅ ⊆ X)") {
    // (5,5) is a representable empty interval (χ ≡ False, size 0); the
    // endpoint test alone would wrongly report it ⊄ a disjoint interval.
    constexpr auto empty =
        make_interval<Strictness::Strict, Strictness::Strict>(5, 5);
    static_assert(is_empty(empty));
    static_assert(dedekind::order::size(empty) == 0u);
    constexpr auto i01 =
        make_interval<Strictness::NonStrict, Strictness::NonStrict>(0, 1);
    static_assert(bool(empty <= i01), "∅ ⊆ [0,1] despite disjoint endpoints");
    static_assert(bool(i01 >= empty), "[0,1] ⊇ ∅ (derived)");
    CHECK(bool(empty <= i01));
  }

  SECTION("emptiness/subset are overflow- and sign-safe (#835 re-review)") {
    // (a) Inverted endpoints on an UNSIGNED carrier must NOT wrap to a huge
    //     span: (5u,3u) is empty, though a naive `hi - lo` (3u-5u) wraps to a
    //     large positive span and reads non-empty (#835 re-review).
    static_assert(
        is_empty(make_interval<Strictness::Strict, Strictness::Strict>(5u, 3u)),
        "(5u,3u) is empty, not a wrapped unsigned span");
    // (b) A full-range interval: INT_MAX - INT_MIN overflows int, so the
    //     effective bounds are computed in long long.
    constexpr auto full =
        make_interval<Strictness::NonStrict, Strictness::NonStrict>(
            std::numeric_limits<int>::min(), std::numeric_limits<int>::max());
    static_assert(!is_empty(full), "[INT_MIN,INT_MAX] is non-empty");
    // (c) Open integer gap (5,6): adjacent endpoints, no member.
    static_assert(
        is_empty(make_interval<Strictness::Strict, Strictness::Strict>(5, 6)),
        "(5,6) has no integer strictly between");
    // (d) size of a full range is the exact count in a wide span, not a
    //     wrapped `hi - lo + 1`: |[INT_MIN,INT_MAX]| = 2^32.
    static_assert(dedekind::order::size(full) == 4294967296ull,
                  "|[INT_MIN,INT_MAX]| = 2^32");
  }

  SECTION(
      "discrete intervals normalize to effective carrier bounds (#835 rd 4)") {
    // (1,4) and [2,3] both denote {2,3} over int, so they must compare equal
    // --- a syntactic endpoint compare would wrongly reject (1,4) ⊆ [2,3].
    constexpr auto open14 =
        make_interval<Strictness::Strict, Strictness::Strict>(1, 4);  // {2,3}
    constexpr auto clos23 =
        make_interval<Strictness::NonStrict, Strictness::NonStrict>(
            2,
            3);  // {2,3}
    static_assert(!is_empty(open14) && !is_empty(clos23));
    static_assert(dedekind::order::size(open14) == 2u &&
                  dedekind::order::size(clos23) == 2u);
    static_assert(bool(open14 <= clos23), "(1,4) ⊆ [2,3] (both {2,3})");
    static_assert(bool(clos23 <= open14), "[2,3] ⊆ (1,4) (both {2,3})");
    CHECK(bool(open14 <= clos23));
    CHECK(bool(clos23 <= open14));
  }
}

TEST_CASE("order:halfspace — the factory makes a Halfspace a proper cut (#832)",
          "[order][halfspace][subset]") {
  SECTION("empty cut → Ø, moot cut → the universe, proper cut → Halfspace") {
    // {x>INT_MAX} admits nothing → Ø.
    static_assert(
        std::same_as<
            decltype(make_halfspace<int, std::numeric_limits<int>::max(),
                                    Direction::Upward, Strictness::Strict>()),
            Ø<int, Boole>>,
        "{x>INT_MAX} = Ø");
    // {x≥0} on ℕ is all of ℕ → the universe (halfspace(ℕ,·,Upper) = ℕ).
    static_assert(
        std::same_as<decltype(make_halfspace<Cardinality, 0, Direction::Upward,
                                             Strictness::NonStrict>()),
                     𝔸<Cardinality, Boole>>,
        "{x≥0} on ℕ = ℕ (moot constraint drops)");
    // An interior cut stays a proper Halfspace.
    static_assert(
        std::same_as<
            decltype(make_halfspace<int, 5, Direction::Upward,
                                    Strictness::Strict>()),
            Halfspace<int, Direction::Upward, Strictness::Strict, Boole>>,
        "{x>5} is a proper cut");
  }

  SECTION("the DSL routes through the factory") {
    // The DSL surface collapses a moot cut: {x≥0} on ℕ = ℕ.
    static_assert(std::same_as<std::decay_t<decltype(ℕ | (χ >= fix(0_c)))>,
                               𝔸<Cardinality, Boole>>,
                  "ℕ | (χ >= fix(0_c)) = ℕ");
  }

  SECTION(
      "the signed ℤ-proxy has no floor: {z<0} is a proper cut (#837 review)") {
    // SignedCardinality is IsSaturating but unbounded below, so {z<0} is
    // inhabited (must NOT collapse to Ø) and {z≥0} is not all of ℤ (not moot).
    // The bare IsSaturating floor test got both wrong; HasZeroFloor fixes it.
    static_assert(
        std::same_as<
            decltype(make_halfspace<SignedCardinality, 0, Direction::Downward,
                                    Strictness::Strict>()),
            Halfspace<SignedCardinality, Direction::Downward,
                      Strictness::Strict, Boole>>,
        "{z<0} on ℤ is a proper cut, not Ø");
    static_assert(
        std::same_as<
            decltype(make_halfspace<SignedCardinality, 0, Direction::Upward,
                                    Strictness::NonStrict>()),
            Halfspace<SignedCardinality, Direction::Upward,
                      Strictness::NonStrict, Boole>>,
        "{z≥0} on ℤ is a proper cut, not the universe");
  }
}

TEST_CASE(
    "order:halfspace — the point-free former threads the universe's species "
    "into the halfspace",
    "[order][halfspace][species]") {
  constexpr auto hs = 𝔸<int, Kleene>{} | (π > fix(5_c));  // {x > 5}, L = Kleene
  STATIC_CHECK(std::same_as<typename decltype(hs)::logic_species, Kleene>);
  STATIC_CHECK(
      std::same_as<typename decltype(hs)::Codomain, typename Kleene::Ω>);
  STATIC_CHECK(IsLSet<decltype(hs)> && !IsSet<decltype(hs)>);  // Ω ≠ 𝔹
  STATIC_CHECK_FALSE(HasDecidableMembership<decltype(hs)>);
  CHECK(hs(6) == Kleene::True);
  CHECK(hs(5) == Kleene::False);
}

// The power set 𝔓 (#830) is exercised in order/powerset_test.cpp.
