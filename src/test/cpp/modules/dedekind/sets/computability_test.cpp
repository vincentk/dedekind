/** @file dedekind/sets/computability_test.cpp
 *
 * Unit coverage for the three-tier computability hierarchy introduced in
 * PR #361: `HasDecidableMembership`, `IsFiniteSet`, `IsCompileTimeEnumerable`.
 *
 * Each concept is tested against a positive witness (a type that should
 * satisfy it) and a negative witness (a type that should fail it), so the
 * refinement direction is mechanically observable.
 */

#include <catch2/catch_test_macros.hpp>
#include <concepts>

import dedekind.category;
import dedekind.sets;
import dedekind.order;

using namespace dedekind::category;
using namespace dedekind::sets;
using namespace dedekind::order;

TEST_CASE("sets:computability — HasDecidableMembership",
          "[sets][computability]") {
  SECTION("ClassicalLogic-classified types satisfy the concept") {
    STATIC_CHECK(HasDecidableMembership<Ø<int>>);
    STATIC_CHECK(HasDecidableMembership<Ø<int, ClassicalLogic>>);
    STATIC_CHECK(HasDecidableMembership<Singleton<42>>);
  }

  SECTION("TernaryLogic-classified types fail the concept") {
    STATIC_CHECK_FALSE(HasDecidableMembership<Ø<int, TernaryLogic>>);
    STATIC_CHECK_FALSE(HasDecidableMembership<Singleton<42, TernaryLogic>>);
  }

  SECTION("Intensional Sets over transfinite carriers fail the concept") {
    constexpr auto n = var<ℕ>;
    constexpr auto s = Set{n % N | (n > bound<5>)};
    // ℕ is transfinite → NaturalLogic picks TernaryLogic.
    STATIC_CHECK_FALSE(HasDecidableMembership<decltype(s)>);
  }
}

TEST_CASE("sets:computability — IsFiniteSet", "[sets][computability]") {
  SECTION("Ø and Singleton are finite") {
    STATIC_CHECK(IsFiniteSet<Ø<int>>);
    STATIC_CHECK(IsFiniteSet<Singleton<42>>);
  }

  SECTION("Integer OrderInterval is finite with correct size") {
    constexpr OrderInterval<int, 1, 10, Strictness::Strict,
                            Strictness::Strict>
        iv{};
    STATIC_CHECK(IsFiniteSet<decltype(iv)>);
    STATIC_CHECK(iv.size() == 8u);  // {2,3,4,5,6,7,8,9}
  }

  SECTION("Intensional Sets over transfinite carriers are not finite") {
    constexpr auto n = var<ℕ>;
    constexpr auto s = Set{n % N | (n > bound<5>)};
    STATIC_CHECK_FALSE(IsFiniteSet<decltype(s)>);
  }
}

TEST_CASE("sets:computability — IsCompileTimeEnumerable",
          "[sets][computability]") {
  SECTION("Ø and Singleton expose elements at the type level") {
    STATIC_CHECK(IsCompileTimeEnumerable<Ø<int>>);
    STATIC_CHECK(IsCompileTimeEnumerable<Singleton<42>>);
  }

  SECTION("OrderInterval is finite but NOT compile-time-enumerable") {
    // The 8 inhabitants are not individually present in the type; the
    // interval is observable as a whole but not element-by-element.
    constexpr OrderInterval<int, 1, 10, Strictness::Strict,
                            Strictness::Strict>
        iv{};
    STATIC_CHECK(IsFiniteSet<decltype(iv)>);
    STATIC_CHECK_FALSE(IsCompileTimeEnumerable<decltype(iv)>);
  }
}

TEST_CASE("sets:computability — Refinement order of the three tiers",
          "[sets][computability]") {
  SECTION("IsCompileTimeEnumerable implies IsFiniteSet") {
    // Structurally: IsCompileTimeEnumerable is defined as `IsFiniteSet<S> &&
    // requires { ... }`, so this is a definitional consequence. Verified
    // here against concrete witnesses.
    STATIC_CHECK(IsCompileTimeEnumerable<Singleton<1>> &&
                 IsFiniteSet<Singleton<1>>);
    STATIC_CHECK(IsCompileTimeEnumerable<Ø<int>> && IsFiniteSet<Ø<int>>);
  }

  SECTION("IsFiniteSet implies HasDecidableMembership on ClassicalLogic") {
    // Not strictly transitive via the concept definitions (IsFiniteSet doesn't
    // name HasDecidableMembership), but holds in practice for all finite
    // extensional witnesses the library exposes today.
    STATIC_CHECK(IsFiniteSet<Ø<int>> && HasDecidableMembership<Ø<int>>);
    STATIC_CHECK(IsFiniteSet<Singleton<42>> &&
                 HasDecidableMembership<Singleton<42>>);
  }
}

TEST_CASE("sets:computability — Reduction boundary observably tightens tiers",
          "[sets][computability][reduction]") {
  // This mirrors showcase_03 and showcase_04 but as a unit-level assertion:
  // the reduction boundary IS the computability boundary.
  constexpr auto n = var<ℕ>;

  SECTION("Empty-meet reduction") {
    constexpr auto gt5 = Set{n % N | (n > bound<5>)};
    constexpr auto lt3 = Set{n % N | (n < bound<3>)};
    constexpr Ø<int> meet = gt5 & lt3;

    // Parent fails all tiers; reduction satisfies all tiers.
    STATIC_CHECK_FALSE(HasDecidableMembership<decltype(gt5)>);
    STATIC_CHECK_FALSE(IsFiniteSet<decltype(gt5)>);
    STATIC_CHECK_FALSE(IsCompileTimeEnumerable<decltype(gt5)>);

    STATIC_CHECK(HasDecidableMembership<decltype(meet)>);
    STATIC_CHECK(IsFiniteSet<decltype(meet)>);
    STATIC_CHECK(IsCompileTimeEnumerable<decltype(meet)>);
  }

  SECTION("Singleton reduction") {
    constexpr auto gt3 = Set{n % N | (n > bound<3>)};
    constexpr auto lt5 = Set{n % N | (n < bound<5>)};
    constexpr Singleton<4> s = gt3 & lt5;

    STATIC_CHECK_FALSE(HasDecidableMembership<decltype(gt3)>);
    STATIC_CHECK(HasDecidableMembership<decltype(s)>);
    STATIC_CHECK(IsCompileTimeEnumerable<decltype(s)>);
  }
}
