/** @file dedekind/category/lattice_term_test.cpp
 *
 * Smoke witness for the generic lattice-law term reducer (`:lattice_term`,
 * #865/#888), prototyped on the two simplest bounded lattices — the Boolean
 * lattice `bool` and the `size_t` chain — with the total order INJECTED at the
 * call site (`NumLess`).  These are chains (totally ordered), so they exercise
 * the collapse core (unit / annihilator / idempotence) and the commutative
 * canonicalisation; the `≤`-incomparable tie-break path first bites at the
 * later (non-chain) sets stage.
 */

#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <cstddef>
#include <functional>

import dedekind.category;

using namespace dedekind::category;

namespace lattice_term_smoke {

// A toy interior lattice element (an NTTP-wrapped value); leaves are types.
template <auto V>
struct Lit {
  static constexpr auto value = V;
};

// The injected total order: compare wrapped values.  Total on these chains.
struct NumLess {
  template <typename X, typename Y>
  static consteval bool less() {
    return X::value < Y::value;
  }
};

// ── Boolean lattice: ⊥ = false, ⊤ = true (no interior). ──
using BotB = LatticeBottom<bool, std::less_equal<bool>>;
using TopB = LatticeTop<bool, std::less_equal<bool>>;
static_assert(std::same_as<reduce_t<Meet<TopB, BotB>, NumLess>, BotB>,
              "⊤ ∧ ⊥ = ⊥ (annihilator).");
static_assert(std::same_as<reduce_t<Join<TopB, BotB>, NumLess>, TopB>,
              "⊤ ∨ ⊥ = ⊤ (annihilator, dual).");
static_assert(std::same_as<reduce_t<Meet<TopB, TopB>, NumLess>, TopB>,
              "⊤ ∧ ⊤ = ⊤ (idempotent).");
static_assert(std::same_as<reduce_t<Join<BotB, BotB>, NumLess>, BotB>,
              "⊥ ∨ ⊥ = ⊥ (idempotent, dual).");

// ── size_t chain: ⊥ = 0, ⊤ = SIZE_MAX, interior Lit<3>, Lit<5>. ──
using BotN = LatticeBottom<std::size_t, std::less_equal<std::size_t>>;
using TopN = LatticeTop<std::size_t, std::less_equal<std::size_t>>;
using L3 = Lit<std::size_t{3}>;
using L5 = Lit<std::size_t{5}>;

static_assert(std::same_as<reduce_t<Meet<TopN, L5>, NumLess>, L5>,
              "⊤ ∧ X = X (meet unit).");
static_assert(std::same_as<reduce_t<Meet<BotN, L5>, NumLess>, BotN>,
              "⊥ ∧ X = ⊥ (meet annihilator).");
static_assert(std::same_as<reduce_t<Join<BotN, L5>, NumLess>, L5>,
              "⊥ ∨ X = X (join unit).");
static_assert(std::same_as<reduce_t<Join<TopN, L5>, NumLess>, TopN>,
              "⊤ ∨ X = ⊤ (join annihilator).");
static_assert(std::same_as<reduce_t<Meet<L5, L5>, NumLess>, L5>,
              "X ∧ X = X (idempotent).");

// Absorption on the size_t CHAIN (comparable operands): the meet is the
// glb (min), the join the lub (max) — read off the carrier's own IsPosetal
// order (the Jlt (b) choice), not the injected NumLess.  3 ≤ 5, so:
static_assert(std::same_as<reduce_t<Meet<L5, L3>, NumLess>, L3>,
              "L5 ∧ L3 = L3 (absorption / meet = min on the chain).");
static_assert(std::same_as<reduce_t<Meet<L3, L5>, NumLess>, L3>,
              "…either operand order (absorption is order-agnostic).");
static_assert(std::same_as<reduce_t<Join<L5, L3>, NumLess>, L5>,
              "L5 ∨ L3 = L5 (absorption / join = max on the chain).");

// Nested: reduction recurses into children before applying the node law.
// Meet<TopN, Join<BotN, L5>> → Meet<TopN, L5> → L5.
static_assert(std::same_as<reduce_t<Meet<TopN, Join<BotN, L5>>, NumLess>, L5>,
              "nested: ⊤ ∧ (⊥ ∨ L5) = ⊤ ∧ L5 = L5.");

// ── The semantic (absorption) order is INJECTED (default = the carrier's
//    canonical std::less_equal chain).  This is the fix behind CP's concern
//    that a carrier can bear more than one lattice order (e.g. size_t under
//    numeric ≤ vs the bit-subset lattice, where 1 and 2 are incomparable). ──
static_assert(std::same_as<resolved_order_t<std::size_t, canonical_order>,
                           std::less_equal<std::size_t>>,
              "canonical_order resolves to the carrier's std::less_equal.");

// Fail-closed: an injected order the carrier has NOT proven posetal licenses no
// absorption — the meet stays un-collapsed (only canonicalised by NumLess:
// 3 < 5), rather than mis-absorbing to the numeric min.  So numeric ≤ is never
// silently applied where a different lattice order was meant.
struct NotAnOrder {  // a relation size_t does not prove a partial order
  constexpr bool operator()(std::size_t, std::size_t) const { return true; }
};
static_assert(
    std::same_as<reduce_t<Meet<L5, L3>, NumLess, NotAnOrder>, Meet<L3, L5>>,
    "no posetal proof ⟹ no absorption (canonicalised, not min).");
static_assert(std::same_as<reduce_t<Meet<L5, L3>, NumLess>, L3>,
              "…whereas the canonical order DOES absorb to the min.");

// ── Logic-parametrised comparator (#1): the order returns a LogicalValue in
//    its own `logic` (here TernaryLogic).  An UNDECIDABLE comparison (Unknown)
//    is not definitely-less, so operands keep authoring order — while the other
//    laws still fire.  ClassicalLogic (bool) stays the default (NumLess above).
struct UA {};  // two opaque leaves the comparator cannot order
struct UB {};
struct TernLess {
  using logic = TernaryLogic;
  template <typename, typename>
  static consteval Ternary less() {
    return Ternary::Unknown;  // "cannot decide the order"
  }
};
static_assert(std::same_as<reduce_t<Meet<UB, UA>, TernLess>, Meet<UB, UA>>,
              "undecidable order ⟹ authoring order kept (no swap).");
static_assert(std::same_as<reduce_t<Meet<UA, UB>, TernLess>, Meet<UA, UB>>,
              "…the mirror order likewise stays as authored (not one normal "
              "form — the honest fallback).");
static_assert(std::same_as<reduce_t<Meet<UA, UA>, TernLess>, UA>,
              "idempotence still fires regardless of comparator decidability.");

// Positive canonicalisation WITHOUT collapse: UA/UB expose no ::value, so they
// have no semantic ≤ (no absorption) — the pure reorder path the SETS stage
// will hit for ≤-incomparable subobjects.  A definite injected order swaps them
// into canonical form (both authoring orders → one normal form).
struct OpaqueLess {  // decides UB < UA (arbitrary but total on {UA, UB})
  template <typename X, typename Y>
  static consteval bool less() {
    return std::same_as<X, UB> && std::same_as<Y, UA>;
  }
};
static_assert(
    std::same_as<reduce_t<Meet<UA, UB>, OpaqueLess>, Meet<UB, UA>>,
    "canonicalise (no collapse): definite order swaps to left < right.");
static_assert(std::same_as<reduce_t<Meet<UB, UA>, OpaqueLess>, Meet<UB, UA>>,
              "…and the mirror order reduces to the SAME normal form.");

}  // namespace lattice_term_smoke

TEST_CASE("lattice_term: generic reducer smoke (bool + size_t chains, #888)",
          "[category][lattice][lattice_term]") {
  // All behaviour is compile-time (the static_asserts above); this runtime
  // case exists so the witnesses are linked into a test binary.
  SUCCEED("lattice-term reducer static witnesses compiled.");
}
