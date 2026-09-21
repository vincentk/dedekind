/** @file dedekind/order/halfspace_test.cpp
 *
 * Unit coverage for the NTTP halfspace DSL introduced in PR #361:
 * `bound<V>`, `Halfspace<T, Pivot, D, S, L>`, `OrderInterval<T, Lo, Hi, ...>`,
 * `Singleton<auto Value, L>`, `IntervalProduct<A, B>`, and the `structured_and`
 * overloads that dispatch between them.
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
    constexpr Halfspace<int, 5, Direction::Upward, Strictness::Strict> h{};
    using Logic = typename decltype(h)::logic_species;

    STATIC_CHECK(h(6) == Logic::True);
    STATIC_CHECK(h(5) == Logic::False);  // boundary excluded
    STATIC_CHECK(h(4) == Logic::False);
  }

  SECTION("Upward, non-strict: n >= 5") {
    constexpr Halfspace<int, 5, Direction::Upward, Strictness::NonStrict> h{};
    using Logic = typename decltype(h)::logic_species;

    STATIC_CHECK(h(5) == Logic::True);  // boundary included
    STATIC_CHECK(h(6) == Logic::True);
    STATIC_CHECK(h(4) == Logic::False);
  }

  SECTION("Downward, strict: n < 5") {
    constexpr Halfspace<int, 5, Direction::Downward, Strictness::Strict> h{};
    using Logic = typename decltype(h)::logic_species;

    STATIC_CHECK(h(4) == Logic::True);
    STATIC_CHECK(h(5) == Logic::False);
  }

  SECTION("Downward, non-strict: n <= 5") {
    constexpr Halfspace<int, 5, Direction::Downward, Strictness::NonStrict> h{};
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
  constexpr auto n = element<ℕ>;

  SECTION("> constructs Upward/Strict") {
    constexpr auto h = n > bound<7>;
    using H = std::decay_t<decltype(h)>;
    STATIC_CHECK(std::same_as<H, Halfspace<Cardinality, 7, Direction::Upward,
                                           Strictness::Strict>>);
  }

  SECTION(">= constructs Upward/NonStrict") {
    constexpr auto h = n >= bound<7>;
    using H = std::decay_t<decltype(h)>;
    STATIC_CHECK(std::same_as<H, Halfspace<Cardinality, 7, Direction::Upward,
                                           Strictness::NonStrict>>);
  }

  SECTION("< constructs Downward/Strict") {
    constexpr auto h = n < bound<7>;
    using H = std::decay_t<decltype(h)>;
    STATIC_CHECK(std::same_as<H, Halfspace<Cardinality, 7, Direction::Downward,
                                           Strictness::Strict>>);
  }

  SECTION("<= constructs Downward/NonStrict") {
    constexpr auto h = n <= bound<7>;
    using H = std::decay_t<decltype(h)>;
    STATIC_CHECK(std::same_as<H, Halfspace<Cardinality, 7, Direction::Downward,
                                           Strictness::NonStrict>>);
  }
  // Note (post-#409 review): the DSL constraint also rejects negative
  // signed pivots on unsigned carriers (e.g. `element<𝔸<ℕ>> > bound<-1>` no
  // longer compiles, where previously int→unsigned conversion would
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

TEST_CASE("order:halfspace — structured_and on opposing halfspaces",
          "[order][halfspace][structured_and]") {
  SECTION("Disjoint pivots (strict/strict, Lo >= Hi) → EmptyPredicate") {
    constexpr Halfspace<int, 5, Direction::Upward, Strictness::Strict> up{};
    constexpr Halfspace<int, 3, Direction::Downward, Strictness::Strict> dn{};
    using Result = std::decay_t<decltype(structured_and(up, dn))>;
    STATIC_CHECK(std::same_as<Result, EmptyPredicate<int>>);
  }

  SECTION("Touching strict/strict (Lo == Hi) → EmptyPredicate") {
    constexpr Halfspace<int, 5, Direction::Upward, Strictness::Strict> up{};
    constexpr Halfspace<int, 5, Direction::Downward, Strictness::Strict> dn{};
    using Result = std::decay_t<decltype(structured_and(up, dn))>;
    STATIC_CHECK(std::same_as<Result, EmptyPredicate<int>>);
  }

  SECTION("Cardinality 1 (integer strict/strict, Hi-Lo == 2) → Singleton") {
    constexpr Halfspace<int, 3, Direction::Upward, Strictness::Strict> up{};
    constexpr Halfspace<int, 5, Direction::Downward, Strictness::Strict> dn{};
    using Result = std::decay_t<decltype(structured_and(up, dn))>;
    STATIC_CHECK(std::same_as<Result, Singleton<4, ClassicalLogic>>);
  }

  SECTION("Cardinality > 1 → OrderInterval with correct bounds") {
    constexpr Halfspace<int, -21, Direction::Upward, Strictness::Strict> up{};
    constexpr Halfspace<int, 21, Direction::Downward, Strictness::NonStrict>
        dn{};
    constexpr auto iv = structured_and(up, dn);
    using Iv = std::decay_t<decltype(iv)>;

    STATIC_CHECK(Iv::lower_pivot == -21);
    STATIC_CHECK(Iv::upper_pivot == 21);
    STATIC_CHECK(Iv::lower_strictness == Strictness::Strict);
    STATIC_CHECK(Iv::upper_strictness == Strictness::NonStrict);
    STATIC_CHECK(iv.size() == 42u);
  }

  SECTION("Symmetric case (Downward ∩ Upward) delegates correctly") {
    constexpr Halfspace<int, 5, Direction::Downward, Strictness::Strict> dn{};
    constexpr Halfspace<int, 3, Direction::Upward, Strictness::Strict> up{};
    // 3 < x < 5 → Singleton<4>
    using Result = std::decay_t<decltype(structured_and(dn, up))>;
    STATIC_CHECK(std::same_as<Result, Singleton<4, ClassicalLogic>>);
  }
}

TEST_CASE("order:halfspace — structured_and on same-direction halfspaces",
          "[order][halfspace][structured_and]") {
  SECTION("Upward ∩ Upward: stricter pivot wins") {
    constexpr Halfspace<int, 5, Direction::Upward, Strictness::Strict> a{};
    constexpr Halfspace<int, 7, Direction::Upward, Strictness::Strict> b{};
    using Result = std::decay_t<decltype(structured_and(a, b))>;
    STATIC_CHECK(
        std::same_as<Result, Halfspace<int, 7, Direction::Upward,
                                       Strictness::Strict, ClassicalLogic>>);
  }

  SECTION("Upward same pivot, mixed strictness: stricter wins") {
    constexpr Halfspace<int, 5, Direction::Upward, Strictness::Strict> a{};
    constexpr Halfspace<int, 5, Direction::Upward, Strictness::NonStrict> b{};
    using Result = std::decay_t<decltype(structured_and(a, b))>;
    STATIC_CHECK(
        std::same_as<Result, Halfspace<int, 5, Direction::Upward,
                                       Strictness::Strict, ClassicalLogic>>);
  }

  SECTION("Downward ∩ Downward: smaller pivot wins (stricter)") {
    constexpr Halfspace<int, 5, Direction::Downward, Strictness::Strict> a{};
    constexpr Halfspace<int, 3, Direction::Downward, Strictness::Strict> b{};
    using Result = std::decay_t<decltype(structured_and(a, b))>;
    STATIC_CHECK(
        std::same_as<Result, Halfspace<int, 3, Direction::Downward,
                                       Strictness::Strict, ClassicalLogic>>);
  }
}

namespace {
// A ClassicalLogic halfspace over int, abbreviated for the union/meet tests.
template <int Piv, Direction D, Strictness S>
using HS = Halfspace<int, Piv, D, S, ClassicalLogic>;
// A function-pointer predicate (not a class functor): a Set over one must still
// combine through the free set operators (exercised via a MeetSet below).
constexpr bool is_pos(int x) { return x > 0; }
}  // namespace

TEST_CASE("order:halfspace — structured_or joins halfspaces (#365)",
          "[order][halfspace][structured_or]") {
  SECTION("Upward ∪ Upward: the weaker (wider) pivot wins") {
    using Result = std::decay_t<decltype(structured_or(
        HS<5, Direction::Upward, Strictness::Strict>{},
        HS<7, Direction::Upward, Strictness::Strict>{}))>;
    STATIC_CHECK(
        std::same_as<Result, HS<5, Direction::Upward, Strictness::Strict>>);
  }

  SECTION("Downward ∪ Downward: the wider (larger) pivot wins") {
    using Result = std::decay_t<decltype(structured_or(
        HS<5, Direction::Downward, Strictness::Strict>{},
        HS<3, Direction::Downward, Strictness::Strict>{}))>;
    STATIC_CHECK(
        std::same_as<Result, HS<5, Direction::Downward, Strictness::Strict>>);
  }

  SECTION("Covering opposing (x≥3 ∪ x≤5 overlap [3,5]) → the universe") {
    using Result = std::decay_t<decltype(structured_or(
        HS<3, Direction::Upward, Strictness::NonStrict>{},
        HS<5, Direction::Downward, Strictness::NonStrict>{}))>;
    STATIC_CHECK(std::same_as<Result, UniversalSet<int, ClassicalLogic>>);
  }
}

TEST_CASE(
    "order:halfspace — Set::operator| is structural, never a lambda (#365)",
    "[order][halfspace][set][structured_or]") {
  SECTION(
      "routes through structured_or: same-direction union collapses wider") {
    constexpr Set<int, ClassicalLogic,
                  HS<5, Direction::Upward, Strictness::Strict>>
        a{HS<5, Direction::Upward, Strictness::Strict>{}};
    constexpr Set<int, ClassicalLogic,
                  HS<7, Direction::Upward, Strictness::Strict>>
        b{HS<7, Direction::Upward, Strictness::Strict>{}};
    using U = std::decay_t<decltype(a | b)>;
    STATIC_CHECK(
        std::same_as<U, Set<int, ClassicalLogic,
                            HS<5, Direction::Upward, Strictness::Strict>>>);
  }

  SECTION("no collapse (a gap) → a JoinSet carrying both operand sets") {
    using Lo = HS<5, Direction::Upward, Strictness::NonStrict>;    // {x ≥ 5}
    using Hi = HS<2, Direction::Downward, Strictness::NonStrict>;  // {x ≤ 2}
    constexpr Set<int, ClassicalLogic, Lo> a{Lo{}};
    constexpr Set<int, ClassicalLogic, Hi> b{Hi{}};
    using U = std::decay_t<decltype(a | b)>;
    STATIC_CHECK(std::same_as<U, JoinSet<Set<int, ClassicalLogic, Lo>,
                                         Set<int, ClassicalLogic, Hi>>>);
    // {x ≥ 5} ∪ {x ≤ 2}: a genuine gap at 3, 4 (structured_or declines it).
    CHECK((a | b)(7));
    CHECK((a | b)(1));
    CHECK_FALSE((a | b)(3));
  }

  SECTION("meet with no structured_and → a MeetSet carrying both operands") {
    using Lo = HS<5, Direction::Upward, Strictness::NonStrict>;
    using Hi = HS<2, Direction::Downward, Strictness::NonStrict>;
    using Cap = HS<10, Direction::Downward, Strictness::Strict>;  // {x < 10}
    constexpr Set<int, ClassicalLogic, Lo> a{Lo{}};
    constexpr Set<int, ClassicalLogic, Hi> b{Hi{}};
    constexpr Set<int, ClassicalLogic, Cap> c{Cap{}};
    // c ∩ (a ∪ b): meet of a halfspace with a union — no structured_and.
    using M = std::decay_t<decltype(c & (a | b))>;
    STATIC_CHECK(
        std::same_as<M, MeetSet<Set<int, ClassicalLogic, Cap>,
                                JoinSet<Set<int, ClassicalLogic, Lo>,
                                        Set<int, ClassicalLogic, Hi>>>>);
    // x < 10 ∧ (x ≥ 5 ∨ x ≤ 2): {0,1,2} ∪ {5,6,7,8,9}.
    CHECK((c & (a | b))(7));
    CHECK((c & (a | b))(1));
    CHECK_FALSE((c & (a | b))(3));   // 3 < 10 but neither ≥5 nor ≤2
    CHECK_FALSE((c & (a | b))(12));  // ≥5 but not < 10
  }
}

TEST_CASE("order:halfspace — excluded middle B ∪ ¬B = 𝔸 (#365, dual of B∩¬B=Ø)",
          "[order][halfspace][set][complement]") {
  // The user's law: union of a set with its complement is the backing universe,
  // the exact dual of the contradiction B ∩ ¬B = Ø.  Routes through the
  // IsComplementPair fast-path → UniversalSet, unchanged by the #365 rewiring.
  constexpr auto B = ℕ * ℕ | π1 > fix(5_c);
  using U = std::decay_t<decltype(B | ~B)>;
  STATIC_CHECK(std::same_as<U, UniversalSet<std::pair<Cardinality, Cardinality>,
                                            ClassicalLogic>>);
}

TEST_CASE("order:halfspace — covering XOR stays an IsSet (#864 CP review)",
          "[order][halfspace][set][xor]") {
  // {x > 10} △ {x < 100}: the union covers the line.  A dormant covering-XOR
  // branch that structured_or once activated returned ¬(A ∩ B) by negating a
  // bare OrderInterval — a Morphism, not a Set.  Removed; the general path must
  // keep △ closed over Set.
  constexpr Set<int, ClassicalLogic,
                HS<10, Direction::Upward, Strictness::Strict>>
      a{HS<10, Direction::Upward, Strictness::Strict>{}};
  constexpr Set<int, ClassicalLogic,
                HS<100, Direction::Downward, Strictness::Strict>>
      b{HS<100, Direction::Downward, Strictness::Strict>{}};
  STATIC_CHECK(IsSet<std::decay_t<decltype(a ^ b)>>);
  // △ = in exactly one: {x ≤ 10} ∪ {x ≥ 100} (the complement of the overlap).
  CHECK((a ^ b)(5));         // in b, not a
  CHECK((a ^ b)(200));       // in a, not b
  CHECK_FALSE((a ^ b)(50));  // in both → excluded
}

TEST_CASE(
    "order:halfspace — MeetSet/JoinSet fallbacks are directly covered "
    "(#365/#892)",
    "[order][halfspace][set][predicate]") {
  SECTION(
      "function-pointer predicate combines via the free operator& (a "
      "MeetSet)") {
    constexpr Set<int, ClassicalLogic, bool (*)(int)> pos{&is_pos};  // x > 0
    constexpr Set<int, ClassicalLogic,
                  HS<10, Direction::Downward, Strictness::Strict>>
        cap{HS<10, Direction::Downward, Strictness::Strict>{}};  // x < 10
    using M = std::decay_t<decltype(pos & cap)>;
    STATIC_CHECK(
        std::same_as<
            M, MeetSet<Set<int, ClassicalLogic, bool (*)(int)>,
                       Set<int, ClassicalLogic,
                           HS<10, Direction::Downward, Strictness::Strict>>>>);
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
  constexpr Singleton<4> s_classical{};
  constexpr Singleton<4, TernaryLogic> s_ternary{};

  SECTION("Membership at the inhabitant") {
    STATIC_CHECK(s_classical(4) == ClassicalLogic::True);
    STATIC_CHECK(s_classical(5) == ClassicalLogic::False);
  }

  SECTION("Cross-L equality: Singleton<V> is Singleton<V> regardless of L") {
    STATIC_CHECK(s_classical == s_ternary);
  }

  SECTION("Size is 1") { STATIC_CHECK(s_classical.size() == 1u); }
}

TEST_CASE("order:halfspace — OrderInterval size across strictness pairs",
          "[order][halfspace][order_interval]") {
  SECTION("strict/strict on ℤ: [1, 4] open") {
    // {n : int | 1 < n < 4} = {2, 3} → size 2
    constexpr OrderInterval<int, 1, 4, Strictness::Strict, Strictness::Strict>
        iv{};
    STATIC_CHECK(iv.size() == 2u);
  }

  SECTION("strict/non-strict on ℤ: (1, 4]") {
    // {n : int | 1 < n <= 4} = {2, 3, 4} → size 3
    constexpr OrderInterval<int, 1, 4, Strictness::Strict,
                            Strictness::NonStrict>
        iv{};
    STATIC_CHECK(iv.size() == 3u);
  }

  SECTION("non-strict/non-strict on ℤ: [1, 4]") {
    // {n : int | 1 <= n <= 4} = {1, 2, 3, 4} → size 4
    constexpr OrderInterval<int, 1, 4, Strictness::NonStrict,
                            Strictness::NonStrict>
        iv{};
    STATIC_CHECK(iv.size() == 4u);
  }

  SECTION("Membership matches boundary semantics") {
    constexpr OrderInterval<int, 1, 4, Strictness::Strict,
                            Strictness::NonStrict>
        iv{};
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
  STATIC_CHECK(HasDecidableMembership<Singleton<42>>);
  STATIC_CHECK(IsExtensional<Singleton<42>>);

  SECTION("TernaryLogic variant: extensional but not decidable") {
    STATIC_CHECK_FALSE(HasDecidableMembership<Singleton<42, TernaryLogic>>);
    STATIC_CHECK(IsExtensional<Singleton<42, TernaryLogic>>);
  }
}

TEST_CASE("order:halfspace — OrderInterval on ℤ is finite and enumerable",
          "[order][halfspace][order_interval][computability]") {
  constexpr OrderInterval<int, 1, 10, Strictness::Strict, Strictness::Strict>
      iv{};

  STATIC_CHECK(HasDecidableMembership<decltype(iv)>);
  // The 2026-05-09 :sets:cardinality consolidation merged the
  // tag-based distinction (type-level NTTP inhabitants vs runtime
  // value-level inhabitants) into the @c IsExtensional gate;
  // OrderInterval qualifies since @c size() returns @c size_t.  The
  // finer distinction (type-level NTTP inhabitants only) lives in a
  // follow-up concept if/when needed.
  STATIC_CHECK(IsExtensional<decltype(iv)>);
  STATIC_CHECK(iv.size() == 8u);
}

TEST_CASE("order:halfspace — reduction tightens extensionality (post-#622)",
          "[order][halfspace][computability][reduction]") {
  // Mirrored from analysis/pruning_showcases_test.cpp at the unit level.
  //
  // Pre-#622: this test was named "reduction boundary tightens all three
  // tiers" and exhibited Ternary → Classical promotion as the structural
  // reduction collapsed a halfspace to @c Ø / @c Singleton.  Post-#622's
  // cardinality cut, ℕ is countable on the carrier axis and routes to
  // ClassicalLogic directly — so HasDecidableMembership fires on @c gt5
  // / @c gt3 already, before any reduction.  The interesting axis that
  // STILL tightens here is @b extensionality: @c gt5 is not extensional
  // (predicate-shaped, no materialised members); after meet-reduction
  // to @c Ø or @c Singleton, the result IS extensional.
  constexpr auto n = element<ℕ>;

  SECTION("Empty-meet reduction (extensionality tightens)") {
    constexpr auto gt5 = Set{n | (n > bound<5>)};
    constexpr auto lt3 = Set{n | (n < bound<3>)};
    constexpr Ø<Cardinality> meet = gt5 & lt3;

    // Both source and meet are Classical (carrier axis fires on ℕ).
    STATIC_CHECK(HasDecidableMembership<decltype(gt5)>);
    STATIC_CHECK(HasDecidableMembership<decltype(meet)>);

    // Extensionality tightens: gt5 is intensional, meet (=Ø) is extensional.
    STATIC_CHECK_FALSE(IsExtensional<decltype(gt5)>);
    STATIC_CHECK(IsExtensional<decltype(meet)>);
  }

  SECTION("Singleton reduction (extensionality tightens)") {
    constexpr auto gt3 = Set{n | (n > bound<3>)};
    constexpr auto lt5 = Set{n | (n < bound<5>)};
    constexpr Singleton<4> s = gt3 & lt5;

    STATIC_CHECK(HasDecidableMembership<decltype(gt3)>);
    STATIC_CHECK(HasDecidableMembership<decltype(s)>);
    STATIC_CHECK(IsExtensional<decltype(s)>);
  }
}

TEST_CASE("order:halfspace — IntervalProduct preserves cardinality",
          "[order][halfspace][product]") {
  constexpr OrderInterval<int, 0, 5, Strictness::Strict, Strictness::Strict>
      a{};
  constexpr OrderInterval<int, 0, 3, Strictness::Strict, Strictness::Strict>
      b{};
  // a = {1,2,3,4} (size 4), b = {1,2} (size 2)
  constexpr auto box = a * b;

  SECTION("Product cardinality = factor cardinalities") {
    STATIC_CHECK(box.size() == a.size() * b.size());
    STATIC_CHECK(box.size() == 8u);
  }

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
  constexpr Halfspace<int, 5, Direction::Upward, Strictness::Strict> gt5{};
  constexpr Halfspace<int, 3, Direction::Upward, Strictness::Strict> gt3{};
  constexpr Halfspace<int, 5, Direction::Upward, Strictness::NonStrict> ge5{};
  constexpr Halfspace<int, 3, Direction::Downward, Strictness::Strict> lt3{};
  constexpr Halfspace<int, 5, Direction::Downward, Strictness::Strict> lt5{};

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
    static_assert(bool(gt5 <= 𝔸<int>), "{x>5} ⊆ ℤ");
    CHECK(bool(Ø<int>{} <= gt5));
    CHECK(bool(gt5 <= 𝔸<int>));
  }

  SECTION("singleton ⊆ via membership") {
    constexpr Singleton<5, ClassicalLogic> s5{};
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
    constexpr OrderInterval<int, 2, 5, Strictness::NonStrict,
                            Strictness::NonStrict>
        i25{};  // [2,5]
    constexpr OrderInterval<int, 1, 6, Strictness::NonStrict,
                            Strictness::NonStrict>
        i16{};  // [1,6]
    static_assert(bool(i25 <= i16), "[2,5] ⊆ [1,6]");
    static_assert(!bool(i16 <= i25), "[1,6] ⊄ [2,5]");
    static_assert(bool(i16 >= i25), "[1,6] ⊇ [2,5] (derived, rides <=)");
    // (proper-subset < / > on intervals awaits an OrderInterval ==.)
    CHECK(bool(i25 <= i16));
    CHECK_FALSE(bool(i16 <= i25));
  }

  SECTION("empty interval ⊆ every interval (#835 review: ∅ ⊆ X)") {
    // (5,5) is a representable empty interval (χ ≡ False, size() == 0); the
    // endpoint test alone would wrongly report it ⊄ a disjoint interval.
    constexpr OrderInterval<int, 5, 5, Strictness::Strict, Strictness::Strict>
        empty{};
    static_assert(OrderInterval<int, 5, 5, Strictness::Strict,
                                Strictness::Strict>::is_empty);
    static_assert(empty.size() == 0u);
    constexpr OrderInterval<int, 0, 1, Strictness::NonStrict,
                            Strictness::NonStrict>
        i01{};  // [0,1], disjoint from where (5,5) sits
    static_assert(bool(empty <= i01), "∅ ⊆ [0,1] despite disjoint endpoints");
    static_assert(bool(i01 >= empty), "[0,1] ⊇ ∅ (derived)");
    CHECK(bool(empty <= i01));
  }

  SECTION("emptiness/subset are overflow- and sign-safe (#835 re-review)") {
    // (a) Inverted endpoints on an UNSIGNED carrier must NOT wrap to a huge
    //     span: (5u,3u) is empty, though the old `Hi - Lo` (3u-5u) wrapped to a
    //     large positive span and read non-empty (#835 re-review).
    static_assert(OrderInterval<unsigned, 5u, 3u, Strictness::Strict,
                                Strictness::Strict>::is_empty,
                  "(5u,3u) is empty, not a wrapped unsigned span");
    // (b) A full-range interval must COMPILE: INT_MAX - INT_MIN overflows a
    //     constant expression, so emptiness cannot subtract the endpoints.
    constexpr OrderInterval<int, std::numeric_limits<int>::min(),
                            std::numeric_limits<int>::max(),
                            Strictness::NonStrict, Strictness::NonStrict>
        full{};
    static_assert(!decltype(full)::is_empty, "[INT_MIN,INT_MAX] is non-empty");
    // (c) Open integer gap (5,6): adjacent endpoints, no member.
    static_assert(OrderInterval<int, 5, 6, Strictness::Strict,
                                Strictness::Strict>::is_empty,
                  "(5,6) has no integer strictly between");
    // (d) The carrier-aware endpoint order (through which both is_empty and the
    //     interval ⊆ decide) ranks mixed signed/unsigned pivots by mathematical
    //     value, not by C++'s usual conversions: −1 precedes 0u, though the raw
    //     `-1 < 0u` is false (−1 converts to a huge unsigned).  A whole mixed-
    //     sign interval is independently ill-formed (its χ trips -Wsign-compare
    //     on `x > Lo`), so the comparison primitive is what carries soundness.
    static_assert(pivot_less<-1, 0u>(), "−1 < 0u by value");
    static_assert(!pivot_less<0u, -1>(), "0u is not < −1");
    static_assert(pivot_equal<0, 0u>(), "0 == 0u by value");
    static_assert(!pivot_equal<-1, 0u>(), "−1 ≠ 0u");
    // (e) size() of a full range is the exact count in a wide span, not a
    //     wrapped `Hi - Lo + 1`: |[INT_MIN,INT_MAX]| = 2^32.
    static_assert(decltype(full)::is_integer_range);
    static_assert(full.size() == 4294967296ull, "|[INT_MIN,INT_MAX]| = 2^32");
  }

  SECTION(
      "discrete intervals normalize to effective carrier bounds (#835 rd 4)") {
    // (1,4) and [2,3] both denote {2,3} over int, so they must compare equal
    // --- a syntactic pivot compare would wrongly reject (1,4) ⊆ [2,3].
    constexpr OrderInterval<int, 1, 4, Strictness::Strict, Strictness::Strict>
        open14{};  // {2,3}
    constexpr OrderInterval<int, 2, 3, Strictness::NonStrict,
                            Strictness::NonStrict>
        clos23{};  // {2,3}
    static_assert(!decltype(open14)::is_empty && !decltype(clos23)::is_empty);
    static_assert(open14.size() == 2u && clos23.size() == 2u);
    static_assert(bool(open14 <= clos23), "(1,4) ⊆ [2,3] (both {2,3})");
    static_assert(bool(clos23 <= open14), "[2,3] ⊆ (1,4) (both {2,3})");
    CHECK(bool(open14 <= clos23));
    CHECK(bool(clos23 <= open14));
    // Integer-valued FLOATING pivots (DSL-produced) decide exactly: (5.0,6.0)
    // has no integer member, so it is empty.
    static_assert(OrderInterval<int, 5.0, 6.0, Strictness::Strict,
                                Strictness::Strict>::is_empty,
                  "(5.0,6.0) has no integer member");
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
            Ø<int, ClassicalLogic>>,
        "{x>INT_MAX} = Ø");
    // {x≥0} on ℕ is all of ℕ → the universe (halfspace(ℕ,·,Upper) = ℕ).
    static_assert(
        std::same_as<decltype(make_halfspace<Cardinality, 0, Direction::Upward,
                                             Strictness::NonStrict>()),
                     UniversalSet<Cardinality, ClassicalLogic>>,
        "{x≥0} on ℕ = ℕ (moot constraint drops)");
    // An interior cut stays a proper Halfspace.
    static_assert(
        std::same_as<decltype(make_halfspace<int, 5, Direction::Upward,
                                             Strictness::Strict>()),
                     Halfspace<int, 5, Direction::Upward, Strictness::Strict,
                               ClassicalLogic>>,
        "{x>5} is a proper cut");
  }

  SECTION("the DSL and ~ route through the factory") {
    constexpr auto n = element<ℕ>;
    // The DSL surface collapses a moot cut: {x≥0} on ℕ = ℕ.
    static_assert(std::same_as<std::decay_t<decltype(n >= bound<0>)>,
                               UniversalSet<Cardinality, ClassicalLogic>>,
                  "element<ℕ> >= bound<0> = ℕ");
    // ~ of a raw moot cut is its empty complement: ~{x≥0} = {x<0} = Ø, and
    // dually ~Ø = ℕ, so the boundary complement round-trips (involution).
    constexpr Halfspace<Cardinality, 0, Direction::Upward,
                        Strictness::NonStrict>
        raw_all{};
    static_assert(std::same_as<std::decay_t<decltype(~raw_all)>,
                               Ø<Cardinality, ClassicalLogic>>,
                  "~{x≥0} on ℕ = Ø");
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
            Halfspace<SignedCardinality, 0, Direction::Downward,
                      Strictness::Strict, ClassicalLogic>>,
        "{z<0} on ℤ is a proper cut, not Ø");
    static_assert(
        std::same_as<
            decltype(make_halfspace<SignedCardinality, 0, Direction::Upward,
                                    Strictness::NonStrict>()),
            Halfspace<SignedCardinality, 0, Direction::Upward,
                      Strictness::NonStrict, ClassicalLogic>>,
        "{z≥0} on ℤ is a proper cut, not the universe");
  }
}

// The power set 𝔓 (#830) is exercised in order/powerset_test.cpp.
