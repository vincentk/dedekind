/** @file test/cpp/modules/dedekind/relational/structure_test.cpp
 *
 * Structure derived through the operations (#1047, step 2): two leaves declare
 * their laws (Δ is equality; ≤ on 𝔹 as a set of pairs inherits the chain laws
 * of std::less_equal<bool>), every composite derives its own.  Each derived
 * law is witnessed twice, on the same constexpr value: the concept fires on
 * the composite's type, and the law holds at every point of 𝔹 (the laws at a
 * point, mereology__The_Laws_At_A_Point).  The two readings must agree; the
 * second is the one a run-time relation (relational:rows, #1045) will make.
 */
#include <catch2/catch_test_macros.hpp>
#include <functional>
#include <type_traits>

import dedekind.category;
import dedekind.relational;
import dedekind.sets;

using namespace dedekind::category;
using namespace dedekind::relational;

namespace {
constexpr bool B[] = {false, true};
/** @brief A law at a point, at every point of 𝔹, 𝔹², 𝔹³. */
template <typename Law>
constexpr bool everywhere1(Law law) {
  for (bool a : B)
    if (!law(a)) return false;
  return true;
}
template <typename Law>
constexpr bool everywhere2(Law law) {
  for (bool a : B)
    for (bool b : B)
      if (!law(a, b)) return false;
  return true;
}
template <typename Law>
constexpr bool everywhere3(Law law) {
  for (bool a : B)
    for (bool b : B)
      for (bool c : B)
        if (!law(a, b, c)) return false;
  return true;
}

constexpr auto delta = diag<bool>();
constexpr auto le = relation_of<bool>(std::less_equal<bool>{});
constexpr auto ge = converse(le);
constexpr auto both =
    le & ge;  // = delta, by the meet rules: reflexive, antisymmetric
constexpr auto either = le | ge;  // = ⊤, by the join rules: reflexive, connex
constexpr auto twice = delta >> delta;  // reflexive by the composition rule

template <typename Rel>
using bare = std::remove_cvref_t<Rel>;
}  // namespace

TEST_CASE("structure: the leaves declare, the composites derive (#1047)",
          "[relational][structure][laws]") {
  SECTION("Δ is equality; equivalence and antisymmetry follow") {
    STATIC_CHECK(IsEquality<bool, bare<decltype(delta)>>);
    STATIC_CHECK(IsEquivalence<bool, bare<decltype(delta)>>);
    STATIC_CHECK(IsAntisymmetric<bool, bare<decltype(delta)>>);
    STATIC_CHECK(
        !IsConnex<bool, bare<decltype(delta)>>);  // not declared, not true
    STATIC_CHECK(
        everywhere2([](bool a, bool b) { return equality_at(delta, a, b); }));
    STATIC_CHECK(everywhere3(
        [](bool a, bool b, bool c) { return transitive_at(delta, a, b, c); }));
  }

  SECTION(
      "≤ as a set of pairs inherits the chain laws of std::less_equal<bool>") {
    STATIC_CHECK(IsTotalOrder<bool, bare<decltype(le)>>);
    STATIC_CHECK(!IsSymmetric<bool, bare<decltype(le)>>);
    STATIC_CHECK(everywhere1([](bool a) { return reflexive_at(le, a); }));
    STATIC_CHECK(
        everywhere2([](bool a, bool b) { return connex_at(le, a, b); }));
    STATIC_CHECK(
        everywhere2([](bool a, bool b) { return antisymmetric_at(le, a, b); }));
    CHECK(le(false, true));
    CHECK_FALSE(le(true, false));
  }

  SECTION("the converse keeps every law of an endo-relation") {
    STATIC_CHECK(IsTotalOrder<bool, bare<decltype(ge)>>);
    STATIC_CHECK(everywhere3(
        [](bool a, bool b, bool c) { return transitive_at(ge, a, b, c); }));
    CHECK(ge(true, false));
    CHECK_FALSE(ge(false, true));
  }

  SECTION("the meet keeps reflexive, transitive, antisymmetric; not connex") {
    STATIC_CHECK(IsReflexive<bool, bare<decltype(both)>>);
    STATIC_CHECK(IsTransitive<bool, bare<decltype(both)>>);
    STATIC_CHECK(IsAntisymmetric<bool, bare<decltype(both)>>);
    STATIC_CHECK(!IsConnex<bool, bare<decltype(both)>>);  // ≤ ∩ ≥ = Δ: no rule,
                                                          // and false
    // Sound, not complete: ≤ ∩ ≥ IS symmetric (it is Δ), but neither leg is,
    // so no rule certifies it; the value-level reading sees it.
    STATIC_CHECK(!IsSymmetric<bool, bare<decltype(both)>>);
    STATIC_CHECK(
        everywhere2([](bool a, bool b) { return symmetric_at(both, a, b); }));
    STATIC_CHECK(
        everywhere2([](bool a, bool b) { return equality_at(both, a, b); }));
  }

  SECTION("the join keeps reflexive and connex; not antisymmetric") {
    STATIC_CHECK(IsReflexive<bool, bare<decltype(either)>>);
    STATIC_CHECK(IsConnex<bool, bare<decltype(either)>>);
    STATIC_CHECK(!IsAntisymmetric<bool, bare<decltype(either)>>);  // ≤ ∪ ≥ = ⊤
    STATIC_CHECK(
        everywhere2([](bool a, bool b) { return connex_at(either, a, b); }));
    STATIC_CHECK(!everywhere2(
        [](bool a, bool b) { return antisymmetric_at(either, a, b); }));
  }

  SECTION("composition keeps reflexive; Δ;Δ = Δ at every point") {
    STATIC_CHECK(IsReflexive<bool, bare<decltype(twice)>>);
    STATIC_CHECK(
        everywhere2([](bool a, bool b) { return equality_at(twice, a, b); }));
  }
}
