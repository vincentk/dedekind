/** @file dedekind/topology/powerset_test.cpp
 *
 * Power set 𝔓(S) over the reified subobject domain Sub(C) (#830).  𝔓(S) is a
 * bona-fide `IsSet` (a `Set` over `Sub<C>`), gated on S coercing to Sub(C),
 * with membership X ⊆ S decided by homogeneous interval nesting.  Home:
 * dedekind.topology (Sub is the runtime-pivot sibling of Ray/Interval/
 * HalfSpace).  Acceptance: clear typing, type-check failure by default,
 * grammar/lattice participation.
 */

#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <type_traits>

import dedekind.category;
import dedekind.sets;
import dedekind.order;    // the NTTP-pivot ordered families (Halfspace, ...)
import dedekind.topology; // 𝔓, Sub

using namespace dedekind::category;
using namespace dedekind::sets;
using namespace dedekind::order;
using namespace dedekind::topology;

TEST_CASE("topology:powerset — 𝔓(S) is a bona-fide IsSet over Sub(C) (#830)",
          "[topology][powerset]") {
  constexpr Halfspace<int, 3, Direction::Upward, Strictness::Strict> gt3{};

  SECTION("clear typing: 𝔓(S) is IsSet, Domain = Sub(C)") {
    constexpr auto P = 𝔓(gt3);
    STATIC_CHECK(IsSet<std::remove_cvref_t<decltype(P)>>);
    STATIC_CHECK(std::same_as<typename std::remove_cvref_t<decltype(P)>::Domain,
                              Sub<int, ClassicalLogic>>);
    STATIC_CHECK(std::same_as<decltype(power_set(gt3)), decltype(𝔓(gt3))>);
  }

  SECTION("membership X ⊆ S decides across the ordered families") {
    constexpr auto P = 𝔓(gt3);  // 𝔓({x>3})
    constexpr Halfspace<int, 5, Direction::Upward, Strictness::Strict> gt5{};
    constexpr Halfspace<int, 3, Direction::Downward, Strictness::Strict> lt3{};
    constexpr Singleton<4, ClassicalLogic> s4{};
    CHECK(bool(P(gt5)));        // {x>5} ⊆ {x>3}
    CHECK_FALSE(bool(P(lt3)));  // {x<3} ⊄ {x>3}
    CHECK(bool(P(s4)));         // {4} ⊆ {x>3} (a Singleton member)
    CHECK(bool(P(gt3)));        // self-membership {x>3} ⊆ {x>3}
  }

  SECTION("𝔓(Ω) accepts anything over the carrier; 𝔓(Ø) is {Ø}") {
    constexpr auto Pu = 𝔓(Ω<int>);
    constexpr Halfspace<int, 5, Direction::Upward, Strictness::Strict> gt5{};
    CHECK(bool(Pu(gt5)));       // X ⊆ Ω
    CHECK(bool(Pu(Ø<int>{})));  // Ø ⊆ Ω
    constexpr auto Pe = 𝔓(Ø<int>{});
    CHECK(bool(Pe(Ø<int>{})));   // Ø ∈ 𝔓(Ø)
    CHECK_FALSE(bool(Pe(gt5)));  // {x>5} ⊄ Ø
  }

  SECTION("lattice: 𝔓(S) plugs into the set lattice (Ø & 𝔓(S) = Ø)") {
    constexpr auto P = 𝔓(gt3);
    using D = Sub<int, ClassicalLogic>;
    CHECK(bool(Ø<D>{} == (Ø<D>{} & P)));  // absorption, available because IsSet
  }

  SECTION("gate: a non-set-shaped base is rejected (type-check failure)") {
    // The gate constraint SetShaped rules out a bare carrier value, so 𝔓(42) is
    // ill-formed by construction (both the deleted default and the ordered
    // overload require SetShaped).  Probe the constraint directly (a
    // `!requires{ 𝔓(42); }` probe is not stable: a deleted candidate inside a
    // requires-expression is not cleanly SFINAE under clang).
    STATIC_CHECK(!SetShaped<int>);
    STATIC_CHECK(SetShaped<decltype(gt3)>);
  }
}
