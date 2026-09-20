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

// A predicate / subobject leaf is an arrow χ:Domain→Ω (IsArrow); carrier_of
// reads its Domain (reusing the category arrow surface, not a bespoke probe) —
// the bridge that lets the carrier-based gates apply to the forthcoming sets
// specialisation.
struct CharLeaf {  // a minimal characteristic arrow int → bool
  using Domain = int;
  using Codomain = bool;
  constexpr bool operator()(const int&) const { return true; }
};
static_assert(IsArrow<CharLeaf>, "the predicate leaf is a category arrow.");
static_assert(std::same_as<carrier_of_t<CharLeaf>, int>,
              "carrier_of reads a predicate leaf's Domain (via IsArrow).");

// ── AST nodes as SET VALUES (#892) ────────────────────────────────────────
// A node constructed from operand values (aggregate init) IS the combined set:
// it evaluates pointwise through the operands' shared logic_species (∧=AND,
// ∨=OR, ¬=RFL).  This is latent for the reducer (which names the nodes only as
// type tags) and the seam toward "the AST is the set".
struct GELeaf {  // χ: n ↦ (n ≥ threshold), a classical characteristic arrow
  using Domain = int;
  using Codomain = bool;
  using logic_species = ClassicalLogic;
  int threshold;
  constexpr bool operator()(const int& n) const { return n >= threshold; }
};
static_assert(Meet<GELeaf, GELeaf>{GELeaf{2}, GELeaf{5}}(7),
              "(·≥2) ∧ (·≥5) holds at 7 — the Meet node evaluates the meet.");
static_assert(!Meet<GELeaf, GELeaf>{GELeaf{2}, GELeaf{5}}(3),
              "(·≥2) ∧ (·≥5) fails at 3 (3 ≥ 5 is false).");
static_assert(Join<GELeaf, GELeaf>{GELeaf{2}, GELeaf{5}}(3),
              "(·≥2) ∨ (·≥5) holds at 3 (3 ≥ 2).");
static_assert(Not<GELeaf>{GELeaf{5}}(3),
              "¬(·≥5) holds at 3 (3 ≥ 5 is false, so its negation is true).");
static_assert(!Not<GELeaf>{GELeaf{5}}(7), "¬(·≥5) fails at 7 (7 ≥ 5).");

// ══ Layer 2: the ASSEMBLED reducer on the canonical carriers ══════════════

// bool — the Boolean lattice: optimal reduction (every safe-core law fires).
static_assert(std::same_as<reduce_t<Meet<TopB, BotB>, NumLess>, BotB>,
              "⊤ ∧ ⊥ = ⊥.");
static_assert(std::same_as<reduce_t<Join<TopB, BotB>, NumLess>, TopB>,
              "⊤ ∨ ⊥ = ⊤.");
static_assert(std::same_as<reduce_t<Meet<TopB, TopB>, NumLess>, TopB>,
              "⊤ ∧ ⊤ = ⊤ (idempotent).");
// bool activates the De Morgan negation in PRODUCTION (registered under
// canonical_order): involution ¬¬⊤ → ⊤.  (The meet/join De Morgan needs a
// non-chain, since a chain collapses the inner meet/join first — see dist_toy.)
static_assert(std::same_as<reduce_t<Not<Not<TopB>>, NumLess>, TopB>,
              "¬¬⊤ → ⊤ on bool (involution, production carrier).");

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

// Fail-closed on a KNOWN/OPAQUE mix: I3 is int, UA is opaque (no evidence it is
// an int-lattice element), so a ∧ (a ∨ opaque) does NOT absorb — kept whole.
static_assert(std::same_as<reduce_t<Meet<I3, Join<I3, UA>>, TernLess>,
                           Meet<I3, Join<I3, UA>>>,
              "known/opaque mix ⟹ structural absorption fails closed.");

}  // namespace lattice_term_smoke

// ══ Distributivity: X∧(P∨Q) → (X∧P)∨(X∧Q) toward DNF ══════════════════════
// Cannot fire on a chain (the inner join glb/lub-collapses first), so it is
// witnessed on a SYNTHETIC non-chain distributive lattice: three pairwise
// incomparable atoms whose (carrier, order) is ASSERTED distributive — the Jlt
// caller-assertion, the same posture as the injected order.  An Unknown-
// returning comparator keeps the produced DNF order (a value-based comparator
// would not type-check on the composite Meet nodes the DNF contains).
namespace dist_toy {
// A GENUINE non-chain distributive lattice: the bit-subset (Boolean) lattice on
// the masks of an integer, a ⊑ b ⟺ (a & b) == a.  DA/DB/DC are three pairwise
// disjoint atoms of 2^3 ⊂ 2^8, hence pairwise incomparable — so their joins /
// meets stay symbolic (the reducer never evaluates a genuine lub/glb), which is
// what exercises distributivity and De Morgan.  2^8 under bit-subset is a
// Boolean algebra, so the distributive + De-Morgan-negation assertions below
// are HONEST (unlike a bare antichain, which is not a lattice at all).
using Mask = unsigned char;
template <Mask V>
struct DLit {
  static constexpr Mask value = V;
};
struct BitSubset {  // a genuine partial order (reflexive, transitive, antisym.)
  constexpr bool operator()(Mask a, Mask b) const { return (a & b) == a; }
};
struct NonDistOrd {};  // an order NOT asserted distributive (gate stays off)
struct KeepOrder {     // Unknown ⟹ keep authoring order (no value comparison)
  using logic = dedekind::category::TernaryLogic;
  template <typename, typename>
  static consteval dedekind::category::Ternary less() {
    return dedekind::category::Ternary::Unknown;
  }
};
using DA = DLit<Mask{0b001}>;
using DB = DLit<Mask{0b010}>;
using DC = DLit<Mask{0b100}>;
// A leaf-combiner supplying the carrier's DOMAIN meet/join (bitwise ∧/∨), the
// stand-in for what `sets` will inject as structured_and / structured_or: it
// computes the actual glb/lub of two order-incomparable leaves.
struct BitCombine {
  template <typename RA, typename RB>
  static consteval auto meet() {
    return std::type_identity<DLit<Mask(RA::value & RB::value)>>{};
  }
  template <typename RA, typename RB>
  static consteval auto join() {
    return std::type_identity<DLit<Mask(RA::value | RB::value)>>{};
  }
};
}  // namespace dist_toy

// The 2^n bit-subset lattice is a Boolean algebra: distributive, with an
// involutive De Morgan negation (Jlt assertions — honest):
namespace dedekind::category {
template <>
inline constexpr bool
    is_distributive_lattice_for_v<dist_toy::Mask, dist_toy::BitSubset> = true;
template <>
inline constexpr bool
    is_de_morgan_negation_for_v<dist_toy::Mask, dist_toy::BitSubset> = true;
// …and genuinely COMPLEMENTED (Boolean): a ∧ ¬a = ⊥, a ∨ ¬a = ⊤.
template <>
inline constexpr bool
    is_complemented_lattice_for_v<dist_toy::Mask, dist_toy::BitSubset> = true;
}  // namespace dedekind::category

namespace dist_toy {
using namespace dedekind::category;

// The distributivity law in isolation: X ∧ (P ∨ Q) → (X∧P) ∨ (X∧Q).
static_assert(
    std::same_as<
        decltype(meet_distributivity_law<DC, Join<DA, DB>, BitSubset>())::type,
        Join<Meet<DC, DA>, Meet<DC, DB>>>,
    "distributivity law: X ∧ (P ∨ Q) → (X∧P) ∨ (X∧Q).");
// Gated OFF for an order not asserted distributive:
static_assert(
    std::same_as<
        decltype(meet_distributivity_law<DC, Join<DA, DB>, NonDistOrd>())::type,
        law_inactive>,
    "…inactive for an order not asserted distributive (gated).");
// Assembled: c ∧ (a ∨ b) collapses to the DNF (c∧a) ∨ (c∧b); the sub-meets are
// incomparable so they stay symbolic (a genuine DNF).
static_assert(
    std::same_as<reduce_t<Meet<DC, Join<DA, DB>>, KeepOrder, BitSubset>,
                 Join<Meet<DC, DA>, Meet<DC, DB>>>,
    "assembled: meet distributes over join to DNF, then re-reduces.");

// ── De Morgan negation (involution ¬¬A→A, De Morgan) on the non-chain ─────
static_assert(std::same_as<reduce_t<Not<Meet<DA, DB>>, KeepOrder, BitSubset>,
                           Join<Not<DA>, Not<DB>>>,
              "¬(a ∧ b) → ¬a ∨ ¬b (De Morgan, negation pushed to leaves).");
static_assert(std::same_as<reduce_t<Not<Join<DA, DB>>, KeepOrder, BitSubset>,
                           Meet<Not<DA>, Not<DB>>>,
              "¬(a ∨ b) → ¬a ∧ ¬b (De Morgan dual).");
static_assert(std::same_as<reduce_t<Not<Not<DA>>, KeepOrder, BitSubset>, DA>,
              "¬¬a → a (involution).");
static_assert(
    std::same_as<reduce_t<Not<Not<DA>>, KeepOrder, NonDistOrd>, Not<Not<DA>>>,
    "…negation laws inactive without an involutive negation (gated).");

// ── Complement collapse (a ∧ ¬a → ⊥, a ∨ ¬a → ⊤) on the complemented toy ──
// Needs an INTERIOR element a with ¬a (bool can't: its only elements are the
// bounds, where ⊤∧¬⊤ collapses via the unit law first).  ⊥/⊤ are the carrier's
// LatticeBottom/Top over its resolved order.
static_assert(
    std::same_as<decltype(meet_complement_law<DA, Not<DA>, BitSubset>())::type,
                 LatticeBottom<Mask, BitSubset>>,
    "law: a ∧ ¬a → ⊥.");
static_assert(std::same_as<reduce_t<Meet<DA, Not<DA>>, KeepOrder, BitSubset>,
                           LatticeBottom<Mask, BitSubset>>,
              "assembled: a ∧ ¬a → ⊥ (contradiction).");
static_assert(std::same_as<reduce_t<Join<DA, Not<DA>>, KeepOrder, BitSubset>,
                           LatticeTop<Mask, BitSubset>>,
              "assembled dual: a ∨ ¬a → ⊤ (excluded middle).");
// Gated OFF where the carrier is not asserted complemented:
static_assert(
    std::same_as<reduce_t<Meet<DA, Not<DA>>, KeepOrder, NonDistOrd>,
                 Meet<DA, Not<DA>>>,
    "…complement collapse inactive without a complemented lattice (gated).");

// ── Injected leaf-combiner: the order-incomparable residual is handed to the
//    carrier's domain ∧/∨ (here bitwise), the mechanism `sets` will use for
//    structured_and/or.  Without a combiner the residual stays a Meet node. ──
static_assert(
    std::same_as<reduce_t<Meet<DA, DB>, KeepOrder, BitSubset, BitCombine>,
                 DLit<Mask{0b000}>>,
    "leaf-combiner computes the domain meet DA ∧ DB = 0 (the glb).");
static_assert(
    std::same_as<reduce_t<Join<DA, DB>, KeepOrder, BitSubset, BitCombine>,
                 DLit<Mask{0b011}>>,
    "leaf-combiner computes the domain join DA ∨ DB (the lub).");
static_assert(
    std::same_as<reduce_t<Meet<DA, DB>, KeepOrder, BitSubset>, Meet<DA, DB>>,
    "…no combiner ⟹ the incomparable residual stays a Meet node.");
}  // namespace dist_toy

TEST_CASE("lattice_term: induced laws + assembled reducer (#865/#888)",
          "[category][lattice][lattice_term]") {
  // Most behaviour is compile-time (the static_asserts above); this runtime
  // case links the witnesses and exercises the AST-node value semantics at
  // run time (Codecov cannot see static_asserts).
  SECTION("AST nodes evaluate as set values (#892)") {
    using lattice_term_smoke::GELeaf;
    const Meet<GELeaf, GELeaf> both{GELeaf{2}, GELeaf{5}};    // ·≥2 ∧ ·≥5
    const Join<GELeaf, GELeaf> either{GELeaf{2}, GELeaf{5}};  // ·≥2 ∨ ·≥5
    const Not<GELeaf> below5{GELeaf{5}};                      // ¬(·≥5)
    CHECK(both(7));
    CHECK_FALSE(both(3));
    CHECK(either(3));
    CHECK(below5(3));
    CHECK_FALSE(below5(7));
  }
}
