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
#include <type_traits>
#include <utility>

import dedekind.category;
import dedekind.sets;
import dedekind.order;

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
  // lives in `dedekind.numbers`, which is downstream of `dedekind.order` in
  // the build DAG. Post-#401, ℕ is the unsigned-int carrier, so the test
  // now exercises `Halfspace<unsigned int, ...>` instantiations.
  constexpr auto n = element<Ω<Cardinality>>;

  SECTION("> constructs Upward/Strict") {
    constexpr auto h = n > bound<7>;
    using H = std::decay_t<decltype(h)>;
    STATIC_CHECK(
        std::same_as<H,
                     Halfspace<Cardinality, 7, Direction::Upward, Strictness::Strict>>);
  }

  SECTION(">= constructs Upward/NonStrict") {
    constexpr auto h = n >= bound<7>;
    using H = std::decay_t<decltype(h)>;
    STATIC_CHECK(std::same_as<
                 H, Halfspace<Cardinality, 7, Direction::Upward, Strictness::NonStrict>>);
  }

  SECTION("< constructs Downward/Strict") {
    constexpr auto h = n < bound<7>;
    using H = std::decay_t<decltype(h)>;
    STATIC_CHECK(
        std::same_as<H,
                     Halfspace<Cardinality, 7, Direction::Downward, Strictness::Strict>>);
  }

  SECTION("<= constructs Downward/NonStrict") {
    constexpr auto h = n <= bound<7>;
    using H = std::decay_t<decltype(h)>;
    STATIC_CHECK(
        std::same_as<
            H, Halfspace<Cardinality, 7, Direction::Downward, Strictness::NonStrict>>);
  }
  // Note (post-#409 review): the DSL constraint also rejects negative
  // signed pivots on unsigned carriers (e.g. `element<Ω<ℕ>> > bound<-1>` no
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

TEST_CASE("order:halfspace — Singleton satisfies all three computability tiers",
          "[order][halfspace][singleton][computability]") {
  // Sets-level concepts from dedekind.sets:computability, instantiated on
  // order-level types. The sets test target cannot import dedekind.order,
  // so these downstream conformance checks live here.
  STATIC_CHECK(HasDecidableMembership<Singleton<42>>);
  STATIC_CHECK(IsFiniteSet<Singleton<42>>);
  STATIC_CHECK(IsCompileTimeEnumerable<Singleton<42>>);

  SECTION(
      "TernaryLogic variant: finite + compile-time-enumerable but not "
      "decidable") {
    STATIC_CHECK_FALSE(HasDecidableMembership<Singleton<42, TernaryLogic>>);
    STATIC_CHECK(IsFiniteSet<Singleton<42, TernaryLogic>>);
    STATIC_CHECK(IsCompileTimeEnumerable<Singleton<42, TernaryLogic>>);
  }
}

TEST_CASE("order:halfspace — OrderInterval on ℤ is finite but not enumerable",
          "[order][halfspace][order_interval][computability]") {
  constexpr OrderInterval<int, 1, 10, Strictness::Strict, Strictness::Strict>
      iv{};

  STATIC_CHECK(HasDecidableMembership<decltype(iv)>);
  STATIC_CHECK(IsFiniteSet<decltype(iv)>);
  // The 8 inhabitants live at the value level, not in the type.
  STATIC_CHECK_FALSE(IsCompileTimeEnumerable<decltype(iv)>);
  STATIC_CHECK(iv.size() == 8u);
}

TEST_CASE("order:halfspace — reduction boundary tightens all three tiers",
          "[order][halfspace][computability][reduction]") {
  // Mirrored from analysis/pruning_showcases_test.cpp at the unit level:
  // the reduction boundary IS the computability boundary.
  constexpr auto n = element<Ω<Cardinality>>;

  SECTION("Empty-meet reduction") {
    constexpr auto gt5 = Set{n | (n > bound<5>)};
    constexpr auto lt3 = Set{n | (n < bound<3>)};
    constexpr Ø<Cardinality> meet = gt5 & lt3;

    STATIC_CHECK_FALSE(HasDecidableMembership<decltype(gt5)>);
    STATIC_CHECK_FALSE(IsFiniteSet<decltype(gt5)>);
    STATIC_CHECK_FALSE(IsCompileTimeEnumerable<decltype(gt5)>);

    STATIC_CHECK(HasDecidableMembership<decltype(meet)>);
    STATIC_CHECK(IsFiniteSet<decltype(meet)>);
    STATIC_CHECK(IsCompileTimeEnumerable<decltype(meet)>);
  }

  SECTION("Singleton reduction") {
    constexpr auto gt3 = Set{n | (n > bound<3>)};
    constexpr auto lt5 = Set{n | (n < bound<5>)};
    constexpr Singleton<4> s = gt3 & lt5;

    STATIC_CHECK_FALSE(HasDecidableMembership<decltype(gt3)>);
    STATIC_CHECK(HasDecidableMembership<decltype(s)>);
    STATIC_CHECK(IsCompileTimeEnumerable<decltype(s)>);
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
