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
 *        - `int`   : a bounded chain (⊥ = INT_MIN, ⊤ = INT_MAX);
 *        - `size_t`: also a bounded chain (⊤ = SIZE_MAX), exercised WITHOUT a
 *                    top operand (the laws that need no top).
 *
 * The total order is INJECTED at the call site (`NumLess`); the semantic
 * (glb/lub collapse / boundedness) order defaults to `canonical_order`.  These
 * are test fixtures only: the reducer is generic and is NOT constrained to any
 * of these carriers.
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

// Carriers.  bool = Boolean lattice; int, size_t = bounded chains (size_t's
// top SIZE_MAX is simply not used in the terms below).
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

// meet_glb_law (induced by the order-meet consistency): ≤-comparable operands
// collapse to the glb (meet = min on a chain); the dual join to the lub.  This
// is NOT structural absorption a∧(a∨b)=a (deferred) — only comparable operands.
static_assert(
    std::same_as<decltype(meet_glb_law<I5, I3, canonical_order>())::type, I3>,
    "5 ∧ 3 = 3 (glb / meet = min).");
static_assert(
    std::same_as<decltype(join_lub_law<I5, I3, canonical_order>())::type, I5>,
    "5 ∨ 3 = 5 (lub / join = max).");
static_assert(
    std::same_as<decltype(meet_glb_law<I5, I3, NumLess>())::type, law_inactive>,
    "NumLess is not a registered poset for int ⟹ glb collapse inactive.");

// The semantic order is INJECTED; `canonical_order` resolves to the carrier's
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

// Mixed-carrier fail-closed: an int ⊤ is NOT the unit of a bool leaf — the
// boundary law requires both operands share a carrier (SameCarrier).
static_assert(
    std::same_as<
        decltype(meet_bounded_law<TopI, Lit<true>, canonical_order>())::type,
        law_inactive>,
    "mixed-carrier term ⟹ boundary law inactive (fail-closed).");

// …but the carrier PROPAGATES through composites, so ⊤ ∧ X = X still applies
// when X is a same-carrier subterm that has not collapsed (not only a leaf) —
// the case a non-chain lattice (e.g. bit-subset) hits with an un-collapsed
// Join.
static_assert(std::same_as<carrier_of_t<Join<I3, I5>>, int>,
              "carrier propagates through a composite subterm.");
static_assert(SameCarrier<TopI, Join<I3, I5>>,
              "⊤ and a same-carrier composite share a carrier.");
static_assert(!SameCarrier<TopI, Meet<Lit<true>, Lit<false>>>,
              "…a foreign-carrier composite does not (fail-closed).");
static_assert(!SameCarrier<TopI, Join<I3, Lit<true>>>,
              "…nor a NESTED mixed-carrier composite (recursive fail-closed).");

// ══ Layer 2: the ASSEMBLED reducer on the canonical carriers ══════════════

// bool — the Boolean lattice: optimal reduction (every safe-core law fires).
static_assert(std::same_as<reduce_t<Meet<TopB, BotB>, NumLess>, BotB>,
              "⊤ ∧ ⊥ = ⊥.");
static_assert(std::same_as<reduce_t<Join<TopB, BotB>, NumLess>, TopB>,
              "⊤ ∨ ⊥ = ⊤.");
static_assert(std::same_as<reduce_t<Meet<TopB, TopB>, NumLess>, TopB>,
              "⊤ ∧ ⊤ = ⊤ (idempotent).");

// int — a bounded chain: units, idempotence, glb/lub collapse,
// canonicalisation.
static_assert(std::same_as<reduce_t<Meet<TopI, I5>, NumLess>, I5>,
              "⊤ ∧ X = X (bounded-chain unit).");
static_assert(std::same_as<reduce_t<Meet<BotI, I5>, NumLess>, BotI>,
              "⊥ ∧ X = ⊥ (annihilator).");
static_assert(std::same_as<reduce_t<Meet<I5, I3>, NumLess>, I3>,
              "5 ∧ 3 = 3 (glb collapse on the chain).");
static_assert(std::same_as<reduce_t<Join<I5, I3>, NumLess>, I5>,
              "5 ∨ 3 = 5 (lub collapse on the chain).");
// Nested: reduction recurses into children before applying the node law.
static_assert(std::same_as<reduce_t<Meet<TopI, Join<BotI, I5>>, NumLess>, I5>,
              "⊤ ∧ (⊥ ∨ 5) = ⊤ ∧ 5 = 5.");

// size_t — a bounded chain, exercised WITHOUT a top operand: the bottom (0)
// annihilates and glb collapse / idempotence fire without touching the top.
static_assert(std::same_as<reduce_t<Meet<BotN, N5>, NumLess>, BotN>,
              "0 ∧ X = 0 (bottom annihilates).");
static_assert(std::same_as<reduce_t<Meet<N5, N3>, NumLess>, N3>,
              "5 ∧ 3 = 3 (glb collapse, no top needed).");
static_assert(std::same_as<reduce_t<Meet<N5, N5>, NumLess>, N5>,
              "X ∧ X = X (idempotent).");

// ══ Layer 3: injected-order safety + logic-parametrised canonicalisation ══

// Fail-closed: an injected order the carrier has NOT proven posetal licenses no
// glb collapse — the meet stays un-collapsed (canonicalised by NumLess: 3 < 5),
// rather than collapsing to the numeric min.
struct NotAnOrder {  // a relation size_t does not prove a partial order
  constexpr bool operator()(std::size_t, std::size_t) const { return true; }
};
static_assert(
    std::same_as<reduce_t<Meet<N5, N3>, NumLess, NotAnOrder>, Meet<N3, N5>>,
    "no posetal proof ⟹ no glb collapse (canonicalised, not min).");

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
// have no semantic order (no glb collapse) — the pure reorder path.  A definite
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

// A NON-constexpr comparator fails IsLatticeLess (its `less() == True` is not a
// constant expression), so canonicalisation stays fail-closed rather than
// hard-erroring in the consteval helper.
struct RuntimeLess {
  template <typename, typename>
  static bool less() {  // deliberately NOT consteval / constexpr
    return true;
  }
};
static_assert(!IsLatticeLess<RuntimeLess, UA, UB>,
              "non-constexpr comparator ⟹ fails the gate (fail-closed).");
static_assert(std::same_as<reduce_t<Meet<UA, UB>, RuntimeLess>, Meet<UA, UB>>,
              "…so the term keeps authoring order instead of hard-erroring.");

// ══ Structural absorption a∧(a∨b)=a / a∨(a∧b)=a ═══════════════════════════
// A pure lattice-axiom rewrite (no order / carrier), so it is witnessed on the
// order-opaque leaves UA/UB — where the inner Join/Meet does NOT collapse (on a
// chain the inner node would glb/lub-collapse first, pre-empting it).

// The law in isolation:
static_assert(
    std::same_as<
        decltype(meet_structural_absorption_law<UA, Join<UA, UB>>())::type, UA>,
    "a ∧ (a ∨ b) = a.");
static_assert(
    std::same_as<
        decltype(meet_structural_absorption_law<Join<UB, UA>, UA>())::type, UA>,
    "(b ∨ a) ∧ a = a (operand order in the join irrelevant).");
static_assert(
    std::same_as<
        decltype(meet_structural_absorption_law<UA, Join<UB, UB>>())::type,
        law_inactive>,
    "a ∧ (b ∨ b): a absent from the join ⟹ inactive.");
static_assert(
    std::same_as<
        decltype(join_structural_absorption_law<UA, Meet<UA, UB>>())::type, UA>,
    "a ∨ (a ∧ b) = a (join dual).");

// Assembled through reduce<> (TernLess keeps the opaque inner node
// un-collapsed):
static_assert(std::same_as<reduce_t<Meet<UA, Join<UA, UB>>, TernLess>, UA>,
              "assembled: a ∧ (a ∨ b) collapses to a.");
static_assert(std::same_as<reduce_t<Join<UA, Meet<UA, UB>>, TernLess>, UA>,
              "assembled dual: a ∨ (a ∧ b) collapses to a.");

}  // namespace lattice_term_smoke

// ══ Distributivity: X∧(P∨Q) → (X∧P)∨(X∧Q) toward DNF ══════════════════════
// Cannot fire on a chain (the inner join glb/lub-collapses first), so it is
// witnessed on a SYNTHETIC non-chain distributive lattice: three pairwise
// incomparable atoms whose (carrier, order) is ASSERTED distributive — the Jlt
// caller-assertion, the same posture as the injected order.  An Unknown-
// returning comparator keeps the produced DNF order (a value-based comparator
// would not type-check on the composite Meet nodes the DNF contains).
namespace dist_toy {
enum class D3 { a, b, c };
template <D3 V>
struct DLit {
  static constexpr D3 value = V;
};
struct DistOrd {};  // opaque order: the three atoms are pairwise incomparable
struct NonDistOrd {};  // an order NOT asserted distributive (gate stays off)
struct KeepOrder {
  using logic = dedekind::category::TernaryLogic;
  template <typename, typename>
  static consteval dedekind::category::Ternary less() {
    return dedekind::category::Ternary::Unknown;
  }
};
using DA = DLit<D3::a>;
using DB = DLit<D3::b>;
using DC = DLit<D3::c>;
}  // namespace dist_toy

// Assert the toy (carrier, order) is a distributive lattice (Jlt assertion):
namespace dedekind::category {
template <>
inline constexpr bool
    is_distributive_lattice_for_v<dist_toy::D3, dist_toy::DistOrd> = true;
}  // namespace dedekind::category

namespace dist_toy {
using namespace dedekind::category;

// The law in isolation: X ∧ (P ∨ Q) → (X∧P) ∨ (X∧Q) (not yet re-reduced).
static_assert(
    std::same_as<
        decltype(meet_distributivity_law<DC, Join<DA, DB>, DistOrd>())::type,
        Join<Meet<DC, DA>, Meet<DC, DB>>>,
    "distributivity law: X ∧ (P ∨ Q) → (X∧P) ∨ (X∧Q).");
// Gated OFF for an order not asserted distributive:
static_assert(
    std::same_as<
        decltype(meet_distributivity_law<DC, Join<DA, DB>, NonDistOrd>())::type,
        law_inactive>,
    "…inactive for an order not asserted distributive (gated).");

// Assembled through reduce<>: c ∧ (a ∨ b) collapses to the DNF (c∧a) ∨ (c∧b);
// the sub-meets are incomparable so they stay symbolic (a genuine DNF).
static_assert(std::same_as<reduce_t<Meet<DC, Join<DA, DB>>, KeepOrder, DistOrd>,
                           Join<Meet<DC, DA>, Meet<DC, DB>>>,
              "assembled: meet distributes over join to DNF, then re-reduces.");
}  // namespace dist_toy

TEST_CASE("lattice_term: induced laws + assembled reducer (#865/#888)",
          "[category][lattice][lattice_term]") {
  // All behaviour is compile-time (the static_asserts above); this runtime
  // case exists so the witnesses are linked into a test binary.
  SUCCEED("lattice-term reducer static witnesses compiled.");
}
