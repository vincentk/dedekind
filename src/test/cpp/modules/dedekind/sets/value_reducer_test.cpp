/**
 * @file value_reducer_test.cpp
 * @brief Witnesses for the value-first subobject reducer (#922): the same
 *        lattice laws that @c subobject_reduce_t runs on TYPES run here on
 *        VALUES, at runtime, and a runtime-stateful operand keeps its value
 *        where the normal form is that operand.  This is the dual-phase entry
 *        the #916 Python composition surface will call.
 */
#include <catch2/catch_test_macros.hpp>
#include <type_traits>

import dedekind.category;
import dedekind.sets;

using namespace dedekind::category;
using namespace dedekind::sets;

// The type-only laws (idempotent X∧X=X, complement a∧¬a=⊥) are gated on
// IsIdempotentLeaf.  They decline a runtime-stateful leaf that is not
// value-determined.  That is the value-safety basis for #922: e.g. S{7}∧¬S{3}
// stays unreduced rather than collapsing to ⊥.
static_assert(
    IsIdempotentLeaf<Ø<SignedExtensionalCardinal<>>>,
    "a stateless boundary is value-determined (idempotent-collapsible)");
static_assert(!IsIdempotentLeaf<SingletonSet<SignedExtensionalCardinal<>>>,
              "a runtime-stateful leaf is not idempotent-collapsible");

TEST_CASE(
    "value-first subobject_reduce runs the boundary laws on values (#922)",
    "[sets][reducer][value-first]") {
  using Card = SignedExtensionalCardinal<>;
  const UniversalSet<Card> universe;  // 𝔸 = ⊤ of Sub(Card)
  const Ø<Card> empty;                // Ø = ⊥ of Sub(Card)
  const SingletonSet<Card> seven{7};  // a RUNTIME-stateful leaf (holds 7)

  SECTION("𝔸 ∧ S = S: the stateful singleton survives with its value") {
    const auto r = subobject_reduce(
        Meet<UniversalSet<Card>, SingletonSet<Card>>{universe, seven});
    STATIC_REQUIRE(
        std::same_as<std::remove_cvref_t<decltype(r)>, SingletonSet<Card>>);
    // The value 7 flowed through the reduce (not default-constructed away).
    CHECK(static_cast<bool>(r(7)));
    CHECK(!static_cast<bool>(r(3)));
  }

  SECTION("Ø ∨ S = S: the stateful singleton survives the join unit") {
    const auto r =
        subobject_reduce(Join<Ø<Card>, SingletonSet<Card>>{empty, seven});
    STATIC_REQUIRE(
        std::same_as<std::remove_cvref_t<decltype(r)>, SingletonSet<Card>>);
    CHECK(static_cast<bool>(r(7)));
    CHECK(!static_cast<bool>(r(3)));
  }

  SECTION("Ø ∧ S = Ø: collapses to the initial boundary (annihilator)") {
    const auto r =
        subobject_reduce(Meet<Ø<Card>, SingletonSet<Card>>{empty, seven});
    STATIC_REQUIRE(IsInitialObject<std::remove_cvref_t<decltype(r)>>);
  }

  SECTION("𝔸 ∨ S = 𝔸: collapses to the terminal boundary (annihilator)") {
    const auto r = subobject_reduce(
        Join<UniversalSet<Card>, SingletonSet<Card>>{universe, seven});
    STATIC_REQUIRE(IsTerminalObject<std::remove_cvref_t<decltype(r)>>);
  }

  SECTION("dual-phase: the same reduction runs in constant evaluation") {
    // Not just runtime: subobject_reduce must execute at compile time too, with
    // the stateful value flowing through, so the value-first path is genuinely
    // one reducer across phases (#922).
    constexpr auto r =
        subobject_reduce(Meet<UniversalSet<Card>, SingletonSet<Card>>{
            UniversalSet<Card>{}, SingletonSet<Card>{7}});
    STATIC_REQUIRE(
        std::same_as<std::remove_cvref_t<decltype(r)>, SingletonSet<Card>>);
    STATIC_REQUIRE(static_cast<bool>(r(7)));
    STATIC_REQUIRE(!static_cast<bool>(r(3)));
  }

  SECTION("agrees with the existing type-level boundary operator") {
    // 𝔸 & S via the reify_term path must yield the same value-first result.
    const auto via_operator = universe & seven;
    const auto via_value = subobject_reduce(
        Meet<UniversalSet<Card>, SingletonSet<Card>>{universe, seven});
    STATIC_REQUIRE(std::same_as<std::remove_cvref_t<decltype(via_operator)>,
                                std::remove_cvref_t<decltype(via_value)>>);
    CHECK(static_cast<bool>(via_value(7)) ==
          static_cast<bool>(via_operator(7)));
  }
}
