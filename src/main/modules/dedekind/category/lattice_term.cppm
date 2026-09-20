/**
 * @file dedekind/category/lattice_term.cppm
 * @partition :lattice_term
 * @brief Generic lattice-law term reducer — a compile-time normal form for a
 *        term over a bounded lattice `<L, ∧, ∨, ⊤, ⊥>`.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section lattice_term__Overview
 * A type-level term tree (`Meet<A,B>`, `Join<A,B>`, leaves = lattice carriers)
 * is reduced to a normal form by applying the lattice laws the carrier proves.
 * It is intended as the reusable engine for the set-expression collapse (epic
 * #888): a later @b sets specialisation will map `& ↦ Meet`, `| ↦ Join`,
 * `𝔸 ↦ ⊤` (`IsTerminalObject`), `Ø ↦ ⊥` (`IsInitialObject`).  The present
 * exhibit still collapses through `structured_and` / `structured_or`; this
 * partition is not yet wired into it.
 *
 * @section lattice_term__This_Increment
 * The @b safe core (#865, first slice), sound for @b any bounded lattice:
 * unit / annihilator (`X∧⊤→X`, `X∧⊥→⊥`, dual on `∨`), idempotence (`X∧X→X`),
 * absorption against an @b injected semantic order, and commutative
 * canonicalisation against an @b injected total order.  Distributivity and the
 * complement laws (`¬¬A→A`, De Morgan) are deferred: each becomes its own
 * reducer @e specialisation requiring the axioms it needs (a distributive- and
 * a Boolean-lattice reducer), because `¬¬A→A` is unsound in a Heyting lattice
 * and distributivity cannot even fire on a chain (absorption pre-empts it).
 *
 * @section lattice_term__Ordering
 * Two orders enter, both @b injected (the Juliet posture: the generic engine
 * clicks against a concept, the caller asserts it over exactly its carriers):
 *  - the @b semantic order @c Ord (`≤`) that @b defines absorption.  A carrier
 *    may bear several lattice orders (e.g. @c size_t under the numeric chain
 *    @e or the bit-subset lattice @c order::bit_subset_eq, where @c 1 and @c 2
 *    are incomparable), and absorption is sound only against the one its
 *    `∧`/`∨` induce, so the caller supplies it.  The default @c canonical_order
 *    is the carrier's @c std::less_equal chain; an order the carrier has not
 *    proven posetal licenses @b no absorption (fail-closed).
 *  - the @b total order @c Less, used only to canonicalise commutative
 *    operands.  It is partial-safe: an @c Unknown (undecidable) comparison
 *    leaves the operands in authoring order (the honest fallback).
 *
 * Wikipedia: "Lattice (order)", "Absorption law", "Distributive lattice".
 *
 * @note "Never in the history of mathematics has a mathematical theory been the
 *  object of such vociferous vituperation as lattice theory."
 *  — Gian-Carlo Rota, "The Many Lives of Lattice Theory", Notices of the AMS
 *  44(11), 1997, p. 1440.
 */
module;

#include <concepts>
#include <functional>
#include <type_traits>

export module dedekind.category:lattice_term;

import :lattice;  // IsBoundedLatticeCategory, LatticeTop / LatticeBottom
import :limit;    // IsInitialObject (⊥), IsTerminalObject (⊤)
import :posetal;  // IsPosetal — a carrier's semantic order (absorption)
import :logic;    // ClassicalLogic (default), Ternary, IsLogicalSpecies

namespace dedekind::category {

// ── The term AST (type-level).  Leaves are lattice-carrier types; the nodes
//    combine them.  Empty tags: a term is a compile-time tree, not a value. ──
export template <typename A, typename B>
struct Meet {};  // A ∧ B
export template <typename A, typename B>
struct Join {};  // A ∨ B

/** @brief The logic species a comparator reports in — @c ClassicalLogic (a
 *  @c bool decision) unless the comparator names its own @c logic typedef (e.g.
 *  @c TernaryLogic, so an @b undecidable comparison can be @c Unknown). */
export template <typename Less>
struct lattice_less_logic {
  using type = ClassicalLogic;
};
export template <typename Less>
  requires requires { typename Less::logic; }
struct lattice_less_logic<Less> {
  using type = typename Less::logic;
};
export template <typename Less>
using lattice_less_logic_t = typename lattice_less_logic<Less>::type;

/** @concept IsLatticeLess
 *  @brief The injected total order: a caller-supplied comparator with a
 *  @c static @c consteval @c less<X,Y>() returning a logic value in its
 *  @c lattice_less_logic (default @c ClassicalLogic, i.e. @c bool).  The
 *  reported logic must be a full @c IsLogicalSpecies, so @c ::True (accessed
 *  by @c lattice_definitely_less) is guaranteed present — a comparator whose
 *  @c logic defines only @c Ω does @b not satisfy this.  The reducer
 *  canonicalises only on a @b definitely-True result, so an @c Unknown
 *  (undecidable) comparison leaves the operands in authoring order.  Total-ness
 *  is the caller's obligation (Jlt: asserted downstream), not checked here. */
export template <typename Less, typename X, typename Y>
concept IsLatticeLess =
    IsLogicalSpecies<lattice_less_logic_t<Less>> && requires {
      {
        Less::template less<X, Y>()
      } -> std::convertible_to<typename lattice_less_logic_t<Less>::Ω>;
    };

/** @brief Did the comparator decide @b definitely @c True? (`Unknown`/`False`
 *  ⟹ don't reorder — keep authoring order.)  For @c ClassicalLogic this is the
 *  bool itself; for @c TernaryLogic it is @c == @c Ternary::True. */
export template <typename Less, typename X, typename Y>
consteval bool lattice_definitely_less() {
  if constexpr (IsLatticeLess<Less, X, Y>) {
    return Less::template less<X, Y>() == lattice_less_logic_t<Less>::True;
  } else {
    return false;  // no order for this pair ⟹ leave as authored
  }
}

/** @brief The default semantic order: absorb against the carrier's own
 *  canonical @c std::less_equal chain (the Jlt (b) choice — the order bundled
 *  with the carrier's axioms).  A caller whose `∧`/`∨` mean a different lattice
 *  on the same carrier (e.g. @c order::bit_subset_eq on @c size_t) injects that
 *  relation as @c Ord instead. */
export struct canonical_order {};

/** @brief The comparator type @c Ord resolves to for a carrier @c T:
 *  @c std::less_equal<T> for @c canonical_order, otherwise @c Ord itself. */
export template <typename T, typename Ord>
using resolved_order_t = std::conditional_t<std::same_as<Ord, canonical_order>,
                                            std::less_equal<T>, Ord>;

/** @brief Does @c A ≤ @c B decide in the @b injected semantic order @c Ord?
 *  For a value-carrier leaf exposing @c ::value of a type @c T that
 *  @c IsPosetal under @c Ord, parthood is @c Ord(A::value, B::value).  A leaf
 *  with no such proven order yields @c false — @b no absorption (the honest,
 *  fail-closed fallback), so the reducer never mis-absorbs against a relation
 *  the carrier has not proven a partial order (e.g. numeric `≤` used where the
 *  intended lattice is the bit-subset one). */
export template <typename A, typename B, typename Ord = canonical_order>
concept HasPosetalValue =
    requires {
      A::value;
      B::value;
    } &&
    std::same_as<std::remove_cvref_t<decltype(A::value)>,
                 std::remove_cvref_t<decltype(B::value)>> &&
    IsPosetal<std::remove_cvref_t<decltype(A::value)>,
              resolved_order_t<std::remove_cvref_t<decltype(A::value)>, Ord>>;

/** @brief Is @c A ≤ @c B in the injected order @c Ord? (`false` when that order
 *  is not proven here — absorption then does not fire.) */
export template <typename A, typename B, typename Ord = canonical_order>
consteval bool semantic_leq() {
  if constexpr (HasPosetalValue<A, B, Ord>) {
    using T = std::remove_cvref_t<decltype(A::value)>;
    return resolved_order_t<T, Ord>{}(A::value, B::value);
  } else {
    return false;
  }
}

/** @brief @c reduce_t<Term, Less, Ord> — the normal form of @c Term under the
 *  lattice laws, commutative operands canonicalised by @c Less, absorption
 *  decided by the semantic order @c Ord (default @c canonical_order). */
export template <typename Term, typename Less, typename Ord = canonical_order>
struct reduce {
  using type = Term;  // a leaf reduces to itself
};
export template <typename Term, typename Less, typename Ord = canonical_order>
using reduce_t = typename reduce<Term, Less, Ord>::type;

namespace detail_lattice_term {

// Meet of two ALREADY-REDUCED operands RA, RB.  Uses std::type_identity so the
// carrier is never constructed (leaves may be non-default-constructible).
template <typename RA, typename RB, typename Less, typename Ord>
consteval auto meet_reduce() {
  if constexpr (IsInitialObject<RA>) {
    return std::type_identity<RA>{};  // ⊥ ∧ X = ⊥ (annihilator)
  } else if constexpr (IsInitialObject<RB>) {
    return std::type_identity<RB>{};
  } else if constexpr (IsTerminalObject<RA>) {
    return std::type_identity<RB>{};  // ⊤ ∧ X = X (unit)
  } else if constexpr (IsTerminalObject<RB>) {
    return std::type_identity<RA>{};
  } else if constexpr (std::same_as<RA, RB>) {
    return std::type_identity<RA>{};  // X ∧ X = X (idempotent)
  } else if constexpr (semantic_leq<RA, RB, Ord>()) {
    return std::type_identity<RA>{};  // absorption: RA ≤ RB ⟹ RA ∧ RB = RA
  } else if constexpr (semantic_leq<RB, RA, Ord>()) {
    return std::type_identity<RB>{};  // RB ≤ RA ⟹ RA ∧ RB = RB
  } else if constexpr (lattice_definitely_less<Less, RB, RA>()) {
    // canonicalise: definitely RB < RA ⟹ swap to left < right.  An Unknown /
    // undecidable comparison is NOT definitely-less, so it keeps authoring
    // order.
    return std::type_identity<Meet<RB, RA>>{};
  } else {
    return std::type_identity<Meet<RA, RB>>{};
  }
}

// Join: the exact dual (⊤ annihilates, ⊥ is the unit).
template <typename RA, typename RB, typename Less, typename Ord>
consteval auto join_reduce() {
  if constexpr (IsTerminalObject<RA>) {
    return std::type_identity<RA>{};  // ⊤ ∨ X = ⊤ (annihilator)
  } else if constexpr (IsTerminalObject<RB>) {
    return std::type_identity<RB>{};
  } else if constexpr (IsInitialObject<RA>) {
    return std::type_identity<RB>{};  // ⊥ ∨ X = X (unit)
  } else if constexpr (IsInitialObject<RB>) {
    return std::type_identity<RA>{};
  } else if constexpr (std::same_as<RA, RB>) {
    return std::type_identity<RA>{};  // idempotent
  } else if constexpr (semantic_leq<RA, RB, Ord>()) {
    return std::type_identity<RB>{};  // absorption: RA ≤ RB ⟹ RA ∨ RB = RB
  } else if constexpr (semantic_leq<RB, RA, Ord>()) {
    return std::type_identity<RA>{};  // RB ≤ RA ⟹ RA ∨ RB = RA
  } else if constexpr (lattice_definitely_less<Less, RB, RA>()) {
    return std::type_identity<Join<RB, RA>>{};  // canonicalise (Unknown keeps
                                                // order)
  } else {
    return std::type_identity<Join<RA, RB>>{};
  }
}

}  // namespace detail_lattice_term

export template <typename A, typename B, typename Less, typename Ord>
struct reduce<Meet<A, B>, Less, Ord> {
  using type =
      typename decltype(detail_lattice_term::meet_reduce<reduce_t<A, Less, Ord>,
                                                         reduce_t<B, Less, Ord>,
                                                         Less, Ord>())::type;
};

export template <typename A, typename B, typename Less, typename Ord>
struct reduce<Join<A, B>, Less, Ord> {
  using type =
      typename decltype(detail_lattice_term::join_reduce<reduce_t<A, Less, Ord>,
                                                         reduce_t<B, Less, Ord>,
                                                         Less, Ord>())::type;
};

}  // namespace dedekind::category
