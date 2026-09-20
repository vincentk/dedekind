/** @file dedekind/category/lattice_term_test.cpp
 *
 * Witnesses for the lattice-law term reducer (#865/#888).  Two layers:
 *
 *   1. the induced LAWS in `:lattice`, each tested in ISOLATION — the
 *      decomposable, validated parts the reducer is assembled from;
 *   2. the ASSEMBLED reducer (`:lattice_term`, `reduce<>`) on the canonical
 *      carriers, chosen for how much lattice structure they carry:
 *        - `bool`  : the two-element Boolean lattice — OPTIMAL reduction, every
 *                    safe-core law fires (the easy oracle);
 *        - `int`   : a BOUNDED chain (⊥ = INT_MIN, ⊤ = INT_MAX);
 *        - `size_t`: an UNBOUNDED chain (⊥ = 0, no top used) — fewer laws fire.
 *
 * The total order is INJECTED at the call site (`NumLess`); the semantic
 * (absorption / boundedness) order defaults to `canonical_order`.  These are
 * test fixtures only: the reducer is generic and is NOT constrained to any of
 * these carriers.
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

// Carriers.  bool = Boolean lattice; int = bounded chain; size_t = unbounded
// chain (bottom only — its top is intentionally unused here).
using BotB = LatticeBottom<bool, std::less_equal<bool>>;
using TopB = LatticeTop<bool, std::less_equal<bool>>;
using BotI = LatticeBottom<int, std::less_equal<int>>;
using TopI = LatticeTop<int, std::less_equal<int>>;
using I3 = Lit<3>;
using I5 = Lit<5>;
using BotN = LatticeBottom<std::size_t, std::less_equal<std::size_t>>;
using N3 = Lit<std::size_t{3}>;
using N5 = Lit<std::size_t{5}>;

// ══ Layer 1: the induced laws, in ISOLATION (the parts) ═══════════════════

// meet_bounded_law (induced by a bounded lattice): ⊥ annihilates, ⊤ is unit.
static_assert(
    std::same_as<decltype(meet_bounded_law<BotI, I5, canonical_order>())::type,
                 BotI>,
    "⊥ ∧ X = ⊥ (annihilator).");
static_assert(
    std::same_as<decltype(meet_bounded_law<TopI, I5, canonical_order>())::type,
                 I5>,
    "⊤ ∧ X = X (unit).");
static_assert(
    std::same_as<decltype(meet_bounded_law<I5, I3, canonical_order>())::type,
                 law_inactive>,
    "no bound present ⟹ the bounded law does not fire.");

// join_bounded_law: the dual — ⊤ annihilates, ⊥ is the unit.
static_assert(
    std::same_as<decltype(join_bounded_law<TopI, I5, canonical_order>())::type,
                 TopI>,
    "⊤ ∨ X = ⊤ (annihilator, dual).");
static_assert(
    std::same_as<decltype(join_bounded_law<BotI, I5, canonical_order>())::type,
                 I5>,
    "⊥ ∨ X = X (unit, dual).");

// idempotent_law (induced by a semilattice): X ∧ X = X.  Structural — it fires
// regardless of the carrier's order, even for order-opaque leaves.
static_assert(std::same_as<decltype(idempotent_law<I5, I5>())::type, I5>,
              "X ∧ X = X (idempotent).");
static_assert(
    std::same_as<decltype(idempotent_law<I5, I3>())::type, law_inactive>,
    "distinct operands ⟹ idempotence does not fire.");

// meet_absorption_law (induced by a lattice): comparable operands collapse to
// the glb (meet = min on a chain); the dual join collapses to the lub.
static_assert(
    std::same_as<decltype(meet_absorption_law<I5, I3, canonical_order>())::type,
                 I3>,
    "5 ∧ 3 = 3 (absorption / meet = min).");
static_assert(
    std::same_as<decltype(join_absorption_law<I5, I3, canonical_order>())::type,
                 I5>,
    "5 ∨ 3 = 5 (absorption / join = max).");
static_assert(
    std::same_as<decltype(meet_absorption_law<I5, I3, NumLess>())::type,
                 law_inactive>,
    "NumLess is not a registered poset for int ⟹ absorption inactive.");

// The absorption order is INJECTED; `canonical_order` resolves to the carrier's
// own std::less_equal chain.
static_assert(
    std::same_as<resolved_order_t<int, canonical_order>, std::less_equal<int>>,
    "canonical_order resolves to the carrier's std::less_equal.");

// Boundedness is keyed to the injected order (CP: a carrier can bear several
// lattice orders): the numeric ⊥ counts for `canonical_order` but not for an
// unrelated order.
struct BitSubsetOrd {  // stands in for order::bit_subset_eq (a DIFFERENT order)
  constexpr bool operator()(int a, int b) const { return (a & b) == a; }
};
static_assert(
    is_lattice_bottom_for_v<BotI, canonical_order>,
    "LatticeBottom<int,≤> IS the ⊥ of the canonical (numeric) order.");
static_assert(!is_lattice_bottom_for_v<BotI, BitSubsetOrd>,
              "…but NOT the ⊥ of a different lattice order on int.");

// ══ Layer 2: the ASSEMBLED reducer on the canonical carriers ══════════════

// bool — the Boolean lattice: optimal reduction (every safe-core law fires).
static_assert(std::same_as<reduce_t<Meet<TopB, BotB>, NumLess>, BotB>,
              "⊤ ∧ ⊥ = ⊥.");
static_assert(std::same_as<reduce_t<Join<TopB, BotB>, NumLess>, TopB>,
              "⊤ ∨ ⊥ = ⊤.");
static_assert(std::same_as<reduce_t<Meet<TopB, TopB>, NumLess>, TopB>,
              "⊤ ∧ ⊤ = ⊤ (idempotent).");

// int — a bounded chain: units, idempotence, absorption, canonicalisation.
static_assert(std::same_as<reduce_t<Meet<TopI, I5>, NumLess>, I5>,
              "⊤ ∧ X = X (bounded-chain unit).");
static_assert(std::same_as<reduce_t<Meet<BotI, I5>, NumLess>, BotI>,
              "⊥ ∧ X = ⊥ (annihilator).");
static_assert(std::same_as<reduce_t<Meet<I5, I3>, NumLess>, I3>,
              "5 ∧ 3 = 3 (absorption on the chain).");
static_assert(std::same_as<reduce_t<Join<I5, I3>, NumLess>, I5>,
              "5 ∨ 3 = 5 (absorption on the chain).");
// Nested: reduction recurses into children before applying the node law.
static_assert(std::same_as<reduce_t<Meet<TopI, Join<BotI, I5>>, NumLess>, I5>,
              "⊤ ∧ (⊥ ∨ 5) = ⊤ ∧ 5 = 5.");

// size_t — an unbounded chain: no top used; the bottom (0) still annihilates,
// and absorption / idempotence fire on the chain.
static_assert(std::same_as<reduce_t<Meet<BotN, N5>, NumLess>, BotN>,
              "0 ∧ X = 0 (unbounded chain still has a bottom).");
static_assert(std::same_as<reduce_t<Meet<N5, N3>, NumLess>, N3>,
              "5 ∧ 3 = 3 (absorption, no top needed).");
static_assert(std::same_as<reduce_t<Meet<N5, N5>, NumLess>, N5>,
              "X ∧ X = X (idempotent).");

// ══ Layer 3: injected-order safety + logic-parametrised canonicalisation ══

// Fail-closed: an injected order the carrier has NOT proven posetal licenses no
// absorption — the meet stays un-collapsed (canonicalised by NumLess: 3 < 5),
// rather than mis-absorbing to the numeric min.
struct NotAnOrder {  // a relation size_t does not prove a partial order
  constexpr bool operator()(std::size_t, std::size_t) const { return true; }
};
static_assert(
    std::same_as<reduce_t<Meet<N5, N3>, NumLess, NotAnOrder>, Meet<N3, N5>>,
    "no posetal proof ⟹ no absorption (canonicalised, not min).");

// Logic-parametrised comparator: an UNDECIDABLE order (Unknown) is not
// definitely-less, so operands keep authoring order, while idempotence (a
// structural law) still fires.  ClassicalLogic (bool) stays the default.
struct UA {};
struct UB {};
struct TernLess {
  using logic = TernaryLogic;
  template <typename, typename>
  static consteval Ternary less() {
    return Ternary::Unknown;
  }
};
static_assert(std::same_as<reduce_t<Meet<UB, UA>, TernLess>, Meet<UB, UA>>,
              "undecidable order ⟹ authoring order kept (no swap).");
static_assert(std::same_as<reduce_t<Meet<UA, UB>, TernLess>, Meet<UA, UB>>,
              "…the mirror order likewise stays as authored.");
static_assert(std::same_as<reduce_t<Meet<UA, UA>, TernLess>, UA>,
              "idempotence still fires regardless of comparator decidability.");

// Positive canonicalisation WITHOUT collapse: UA/UB expose no ::value, so they
// have no semantic order (no absorption) — the pure reorder path.  A definite
// injected order swaps them into one normal form.
struct OpaqueLess {
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

TEST_CASE("lattice_term: induced laws + assembled reducer (#865/#888)",
          "[category][lattice][lattice_term]") {
  // All behaviour is compile-time (the static_asserts above); this runtime
  // case exists so the witnesses are linked into a test binary.
  SUCCEED("lattice-term reducer static witnesses compiled.");
}
