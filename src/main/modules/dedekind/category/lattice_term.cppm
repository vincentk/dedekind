/**
 * @file dedekind/category/lattice_term.cppm
 * @partition :lattice_term
 * @brief Generic lattice-law term reducer — compile-time normal form for a
 *        term over a bounded lattice `<L, ∧, ∨, ⊤, ⊥, ¬>`.
 *
 * @section lattice_term__Overview
 * A type-level term tree (`Meet<A,B>`, `Join<A,B>`, `Not<A>`, leaves = lattice
 * carriers) is reduced to a normal form by applying the lattice laws the
 * carrier proves.  This is the generic engine behind the set-expression
 * collapse (epic #888): sets specialise it via `& ↦ Meet`, `| ↦ Join`,
 * `~ ↦ Not`, `𝔸 ↦ ⊤` (`IsTerminalObject`), `Ø ↦ ⊥` (`IsInitialObject`).
 *
 * @section lattice_term__This_Increment
 * The @b safe core (#865, first slice): unit / annihilator (`X∧⊤→X`,
 * `X∧⊥→⊥`, dual on `∨`), idempotence (`X∧X→X`), and commutative
 * canonicalisation.  Absorption (semantic `≤`), distributivity and
 * complement / De Morgan (Boolean lattices) are the next increment.
 *
 * @section lattice_term__Ordering
 * The lattice order `Rel` (`≤`) is @b partial, so it cannot canonically order
 * two `≤`-incomparable operands.  Commutative canonicalisation therefore takes
 * an @b injected total order @c Less at the call site (the Juliet posture: the
 * generic engine clicks against a total-order concept; the caller asserts it,
 * case by case, over exactly the carriers in its expression).  Where no `Less`
 * meaningfully orders a pair, the term stays un-canonicalised (the honest
 * fallback) — undecidable / cross-cardinality carriers fall through here.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */
module;

#include <concepts>
#include <functional>
#include <type_traits>

export module dedekind.category:lattice_term;

import :lattice;  // IsBoundedLatticeCategory, LatticeTop/Bottom,
                  // is_complement_v
import :limit;    // IsInitialObject (⊥), IsTerminalObject (⊤)
import :posetal;  // IsPosetal — the carrier's own semantic order (absorption)
import :species;  // law traits (associative / idempotent / distributive / …)
import :logic;    // ClassicalLogic (default), Ternary, IsLogicalSpecies

namespace dedekind::category {

// ── The term AST (type-level).  Leaves are lattice-carrier types; the nodes
//    combine them.  Empty tags: a term is a compile-time tree, not a value. ──
export template <typename A, typename B>
struct Meet {};  // A ∧ B
export template <typename A, typename B>
struct Join {};  // A ∨ B
export template <typename A>
struct Not {};  // ¬A

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
 *  reducer canonicalises only on a @b definitely-True result, so an @c Unknown
 *  (undecidable) comparison leaves the operands in authoring order.  Total-ness
 *  is the caller's obligation (Jlt: asserted downstream), not checked here. */
export template <typename Less, typename X, typename Y>
concept IsLatticeLess = requires {
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

/** @brief The carrier's OWN semantic order — the Jlt (b) choice: absorption
 *  reads the lattice's `≤`, bundled with its axioms, rather than an injected
 *  comparator.  For a value-carrier leaf exposing @c ::value of a type @c T
 * that
 *  @c IsPosetal under its canonical order, parthood is @c Rel(A::value,
 *  B::value).  A leaf with no such order (e.g. a subobject type, whose subset
 *  @c ≤ is decided downstream) yields @c false — no absorption (honest
 *  fallback).  The subobject-carrier order is supplied by the downstream sets
 *  specialisation. */
export template <typename A, typename B>
concept HasPosetalValue =
    requires {
      A::value;
      B::value;
    } &&
    std::same_as<std::remove_cvref_t<decltype(A::value)>,
                 std::remove_cvref_t<decltype(B::value)>> &&
    IsPosetal<std::remove_cvref_t<decltype(A::value)>,
              std::less_equal<std::remove_cvref_t<decltype(A::value)>>>;

/** @brief Is @c A @c ≤ @c B in the carrier's own order? (`false` when the order
 *  is not available here — absorption then does not fire.) */
export template <typename A, typename B>
consteval bool semantic_leq() {
  if constexpr (HasPosetalValue<A, B>) {
    using T = std::remove_cvref_t<decltype(A::value)>;
    return std::less_equal<T>{}(A::value, B::value);
  } else {
    return false;
  }
}

/** @brief @c reduce_t<Term, Less> — the normal form of @c Term under the
 *  lattice laws, with commutative operands canonicalised by @c Less. */
export template <typename Term, typename Less>
struct reduce {
  using type = Term;  // a leaf reduces to itself
};
export template <typename Term, typename Less>
using reduce_t = typename reduce<Term, Less>::type;

namespace detail_lattice_term {

// Meet of two ALREADY-REDUCED operands RA, RB.  Uses std::type_identity so the
// carrier is never constructed (leaves may be non-default-constructible).
template <typename RA, typename RB, typename Less>
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
  } else if constexpr (semantic_leq<RA, RB>()) {
    return std::type_identity<RA>{};  // absorption: RA ≤ RB ⟹ RA ∧ RB = RA
  } else if constexpr (semantic_leq<RB, RA>()) {
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
template <typename RA, typename RB, typename Less>
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
  } else if constexpr (semantic_leq<RA, RB>()) {
    return std::type_identity<RB>{};  // absorption: RA ≤ RB ⟹ RA ∨ RB = RB
  } else if constexpr (semantic_leq<RB, RA>()) {
    return std::type_identity<RA>{};  // RB ≤ RA ⟹ RA ∨ RB = RA
  } else if constexpr (lattice_definitely_less<Less, RB, RA>()) {
    return std::type_identity<Join<RB, RA>>{};  // canonicalise (Unknown keeps
                                                // order)
  } else {
    return std::type_identity<Join<RA, RB>>{};
  }
}

}  // namespace detail_lattice_term

export template <typename A, typename B, typename Less>
struct reduce<Meet<A, B>, Less> {
  using type =
      typename decltype(detail_lattice_term::meet_reduce<
                        reduce_t<A, Less>, reduce_t<B, Less>, Less>())::type;
};

export template <typename A, typename B, typename Less>
struct reduce<Join<A, B>, Less> {
  using type =
      typename decltype(detail_lattice_term::join_reduce<
                        reduce_t<A, Less>, reduce_t<B, Less>, Less>())::type;
};

// Double-negation (¬¬A → A) is an involution law; the general complement / De
// Morgan rules are the next increment.  For now a single ¬ reduces its child.
export template <typename A, typename Less>
struct reduce<Not<A>, Less> {
  using type = Not<reduce_t<A, Less>>;
};
export template <typename A, typename Less>
struct reduce<Not<Not<A>>, Less> {
  using type = reduce_t<A, Less>;  // ¬¬A = A (involutive complement)
};

}  // namespace dedekind::category
