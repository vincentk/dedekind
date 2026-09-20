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
#include <type_traits>

export module dedekind.category:lattice_term;

import :lattice;  // IsBoundedLatticeCategory, LatticeTop/Bottom,
                  // is_complement_v
import :limit;    // IsInitialObject (⊥), IsTerminalObject (⊤)
import :species;  // law traits (associative / idempotent / distributive / …)

namespace dedekind::category {

// ── The term AST (type-level).  Leaves are lattice-carrier types; the nodes
//    combine them.  Empty tags: a term is a compile-time tree, not a value. ──
export template <typename A, typename B>
struct Meet {};  // A ∧ B
export template <typename A, typename B>
struct Join {};  // A ∨ B
export template <typename A>
struct Not {};  // ¬A

/** @concept IsLatticeLess
 *  @brief The injected total order: a caller-supplied comparator with a
 *  @c static @c consteval @c bool @c less<X,Y>() giving a strict total order on
 *  the leaf types present in a term.  Total-ness is the caller's obligation
 *  (Jlt: asserted downstream), not checked here. */
export template <typename Less, typename X, typename Y>
concept IsLatticeLess = requires {
  { Less::template less<X, Y>() } -> std::same_as<bool>;
};

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
  } else if constexpr (IsLatticeLess<Less, RB, RA> &&
                       Less::template less<RB, RA>()) {
    return std::type_identity<Meet<RB, RA>>{};  // canonicalise: left < right
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
  } else if constexpr (IsLatticeLess<Less, RB, RA> &&
                       Less::template less<RB, RA>()) {
    return std::type_identity<Join<RB, RA>>{};  // canonicalise
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
