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

// Commutative canonicalisation: both operand orders reduce to the SAME
// normal form, left < right under the injected NumLess (Lit<3> before Lit<5>).
static_assert(std::same_as<reduce_t<Meet<L5, L3>, NumLess>, Meet<L3, L5>>,
              "Meet<L5,L3> canonicalises to Meet<L3,L5> (left < right).");
static_assert(std::same_as<reduce_t<Meet<L3, L5>, NumLess>, Meet<L3, L5>>,
              "Meet<L3,L5> is already canonical.");
static_assert(
    std::same_as<reduce_t<Meet<L5, L3>, NumLess>,
                 reduce_t<Meet<L3, L5>, NumLess>>,
    "A ∧ B and B ∧ A share one normal form (commutativity, canonicalised).");

// Nested: reduction recurses into children before applying the node law.
// Meet<TopN, Join<BotN, L5>> → Meet<TopN, L5> → L5.
static_assert(std::same_as<reduce_t<Meet<TopN, Join<BotN, L5>>, NumLess>, L5>,
              "nested: ⊤ ∧ (⊥ ∨ L5) = ⊤ ∧ L5 = L5.");

// Double-negation involution.
static_assert(std::same_as<reduce_t<Not<Not<L5>>, NumLess>, L5>,
              "¬¬X = X (involutive complement).");

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

}  // namespace lattice_term_smoke

TEST_CASE("lattice_term: generic reducer smoke (bool + size_t chains, #888)",
          "[category][lattice][lattice_term]") {
  // All behaviour is compile-time (the static_asserts above); this runtime
  // case exists so the witnesses are linked into a test binary.
  SUCCEED("lattice-term reducer static witnesses compiled.");
}
