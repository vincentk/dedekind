/** @file dedekind/order/powerset_test.cpp
 *
 * Power set 𝔓(S) over the reified subobject domain Sub(C) (#830).  𝔓(S) is a
 * bona-fide `IsSet` (a `Set` over `Sub<C>`), gated on S coercing to Sub(C),
 * with membership X ⊆ S decided by homogeneous interval nesting.  Home:
 * dedekind.order (the enabler is an ordered carrier + the subset order, and Sub
 * has no topology dependency).  Acceptance: clear typing, type-check failure by
 * default, grammar/lattice participation.
 */

#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <type_traits>

import dedekind.category;
import dedekind.sets;
import dedekind.order; // 𝔓, Sub, and the NTTP-pivot families (Halfspace, ...)

using namespace dedekind::category;
using namespace dedekind::sets;
using namespace dedekind::order;

TEST_CASE("order:powerset — 𝔓(S) is a bona-fide IsSet over Sub(C) (#830)",
          "[order][powerset]") {
  constexpr Halfspace<int, 3, Direction::Upward, Strictness::Strict> gt3{};

  SECTION("clear typing: 𝔓(S) is IsSet, Domain = Sub(C)") {
    constexpr auto P = 𝔓(gt3);
    STATIC_CHECK(IsSet<std::remove_cvref_t<decltype(P)>>);
    STATIC_CHECK(std::same_as<typename std::remove_cvref_t<decltype(P)>::Domain,
                              Sub<int, ClassicalLogic>>);
    STATIC_CHECK(std::same_as<decltype(power_set(gt3)), decltype(𝔓(gt3))>);
    // 𝔓's elements are themselves first-class sets (Sub folds onto SetExpr), so
    // they are usable in generic set APIs.
    STATIC_CHECK(IsSubobject<Sub<int, ClassicalLogic>, int>);
    STATIC_CHECK(IsSet<Sub<int, ClassicalLogic>>);
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

  SECTION("𝔓(𝔸) is the full universe over Sub(C): accepts every subobject") {
    // 𝔓(𝔸) has the Sub(C) domain (the interval specialisation), so it composes
    // with the ordered families.  (𝔓(∅) = {∅} is the one closed form that needs
    // no Sub --- it is a :sets fast path, exercised in sets/expressions_test.)
    constexpr auto Pu = 𝔓(𝔸<int>);
    STATIC_CHECK(
        std::same_as<typename std::remove_cvref_t<decltype(Pu)>::Domain,
                     Sub<int, ClassicalLogic>>);
    // 𝔓(𝔸) is the universal BOUNDARY type 𝔸<Sub(C)> (a closed form), not a
    // trivially-true filtered Set --- so boundary / lattice identities survive.
    // (Domain == Sub(C) above + the is_universal_boundary tag pins it as
    // UniversalSet<Sub(C)>, robust to the cardinality parameter.)
    STATIC_CHECK(requires {
      typename std::remove_cvref_t<decltype(Pu)>::is_universal_boundary;
    });
    constexpr Halfspace<int, 5, Direction::Upward, Strictness::Strict> gt5{};
    CHECK(bool(Pu(gt5)));       // X ⊆ 𝔸
    CHECK(bool(Pu(Ø<int>{})));  // Ø ⊆ 𝔸
  }

  SECTION(
      "empty intervals canonicalise: all spellings of ∅ are one Sub value") {
    using D = Sub<int, ClassicalLogic>;
    constexpr OrderInterval<int, 5, 5, Strictness::Strict, Strictness::Strict>
        oi_empty{};                        // (5,5) = ∅
    constexpr D from_oi = oi_empty;        // empty, dead bounds lo=hi=5
    constexpr D from_bottom = Ø<int>{};    // empty, bounds default
    STATIC_CHECK(from_oi == from_bottom);  // one ∅, regardless of spelling
    STATIC_CHECK(bool(from_oi <= from_bottom));  // and mutually nested
  }

  SECTION(
      "discrete bounds canonicalise: {x>3} = {x>=4}, (1,4) = [2,3] over int") {
    using D = Sub<int, ClassicalLogic>;
    // Mixed strictness that denotes the SAME subobject over a discrete carrier
    // must be ONE Sub value (effective-bound normalisation, the #835 sibling).
    constexpr Halfspace<int, 3, Direction::Upward, Strictness::Strict> gt3s{};
    constexpr Halfspace<int, 4, Direction::Upward, Strictness::NonStrict> ge4{};
    constexpr D a = gt3s;  // {x>3}
    constexpr D b = ge4;   // {x>=4}
    STATIC_CHECK(a == b);  // same subobject, one value
    // and 𝔓 membership agrees across the two spellings (the CP finding):
    constexpr auto P = 𝔓(ge4);  // 𝔓({x>=4})
    CHECK(bool(P(gt3s)));       // {x>3} ⊆ {x>=4} (extensionally equal)
    // open (1,4) and closed [2,3] both denote {2,3}:
    constexpr OrderInterval<int, 1, 4, Strictness::Strict, Strictness::Strict>
        open14{};
    constexpr OrderInterval<int, 2, 3, Strictness::NonStrict,
                            Strictness::NonStrict>
        closed23{};
    STATIC_CHECK(static_cast<D>(open14) == static_cast<D>(closed23));
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
    // The ordered overload's gate additionally requires a totally-ordered
    // carrier (so an unordered carrier is rejected AT the gate, not at a later
    // membership call): the ordered family passes it.
    STATIC_CHECK(SubReifiable<decltype(gt3)>);
    STATIC_CHECK(IsTotallyOrdered<int>);
  }

  SECTION("gate: a set-shaped base with no Sub(C) coercion hits the wall") {
    // Acceptance #2: a genuine Set (SetShaped, even over an ORDERED carrier)
    // but with no convex Sub(C) coercion is NOT SubReifiable, so 𝔓 of it
    // selects the deleted :sets default (type-check failure) rather than the
    // ordered overload.  A general filtered set {x ∈ ℤ | x > 0} is exactly such
    // a base.
    auto x = element<𝔸<int>>;
    auto positives = Set{x % UniversalSet<int>{} | (x > 0)};
    using G = std::remove_cvref_t<decltype(positives)>;
    STATIC_CHECK(SetShaped<G>);  // it IS a set...
    STATIC_CHECK(
        IsTotallyOrdered<typename G::Domain>);  // ...over ℤ (ordered)...
    STATIC_CHECK(
        !SubReifiable<G>);  // ...but no Sub(C) coercion → deleted gate.
  }
}
