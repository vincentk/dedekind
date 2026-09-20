/**
 * @file dedekind/category/lattice_term.cppm
 * @partition :lattice_term
 * @brief The assembled lattice-law term reducer — a compile-time normal form
 *        for a term over a bounded lattice, built from the induced laws in
 *        @c :lattice.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section lattice_term__Overview
 * @c :lattice defines the term AST (`Meet`, `Join`, `Not`) and, next to each
 * lattice concept, the equational law it @b induces as a decomposable,
 * independently testable part.  This partition is the @b assembly: @c reduce<>
 * folds those parts into one normal-form reducer, applying exactly the laws the
 * carrier proves.  Reduction power scales with the carrier's structure — a
 * bounded chain collapses the most, and @c bool (the two-element Boolean
 * lattice) is the optimal witness where every law fires.
 *
 * It is intended as the reusable engine for the set-expression collapse (epic
 * #888/#890): a later @b sets specialisation will map `& ↦ Meet`, `| ↦ Join`,
 * `~ ↦ Not`, `𝔸 ↦ ⊤`, `Ø ↦ ⊥`.  The present exhibit still collapses through
 * `structured_and` / `structured_or`; this partition is not yet wired into it.
 *
 * @section lattice_term__Law_Surface
 * The induced laws assembled here (all in @c :lattice): unit / annihilator
 * (@c meet_bounded_law), idempotence (@c idempotent_law), @b structural
 * absorption @c a∧(a∨b)=a (@c meet_structural_absorption_law), @b glb/lub
 * collapse of `≤`-comparable operands (@c meet_glb_law), @b distributivity
 * @c X∧(P∨Q)→(X∧P)∨(X∧Q) toward DNF (@c meet_distributivity_law; @b one
 * direction — meet over join — for termination, @b not the join-over-meet CNF
 * dual), and @b De Morgan negation @c ¬¬A→A / @c ¬(A∧B)→¬A∨¬B
 * (@c de_morgan_law), plus commutative canonicalisation by an @b injected total
 * order.  The complement @b collapse @c a∧¬a→⊥ (needing a genuinely
 * complemented lattice) and associativity-flattening remain deferred (#890).
 *
 * @section lattice_term__Ordering
 * Two orders enter, both @b injected (the Juliet posture: the engine clicks
 * against a concept, the caller asserts it over exactly its carriers):
 *  - the @b semantic order @c Ord that defines the glb/lub collapse and
 *    boundedness (see
 *    @c :lattice); default @c canonical_order is the carrier's @c
 * std::less_equal chain.  A carrier may bear several lattice orders, so the
 * caller supplies the one its `∧`/`∨` induce.
 *  - the @b total order @c Less, used only to canonicalise commutative
 * operands. It is partial-safe: an @c Unknown (undecidable) comparison leaves
 * the operands in authoring order (the honest fallback).
 *
 * Wikipedia: "Lattice (order)", "Absorption law", "Rewriting".
 *
 * @note "Never in the history of mathematics has a mathematical theory been the
 *  object of such vociferous vituperation as lattice theory."
 *  — Gian-Carlo Rota, "The Many Lives of Lattice Theory", Notices of the AMS
 *  44(11), 1997, p. 1440.
 */
module;

#include <concepts>
#include <type_traits>

export module dedekind.category:lattice_term;

import :lattice;  // the term AST + the induced laws (the validated parts)
import :logic;    // ClassicalLogic (default), Ternary, IsLogicalSpecies

namespace dedekind::category {

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
 *  by @c lattice_definitely_less) is guaranteed present.  The reducer
 *  canonicalises only on a @b definitely-True result, so an @c Unknown
 *  (undecidable) comparison leaves the operands in authoring order.  Total-ness
 *  is the caller's obligation (Jlt: asserted downstream), not checked here. */
export template <typename Less, typename X, typename Y>
concept IsLatticeLess =
    IsLogicalSpecies<lattice_less_logic_t<Less>> && requires {
      {
        Less::template less<X, Y>()
      } -> std::convertible_to<typename lattice_less_logic_t<Less>::Ω>;
      // Constant-evaluability gate on the EXACT operation the helper runs
      // (`less() == True`): a non-constexpr comparator fails the concept, so
      // `lattice_definitely_less` takes the fail-closed `else` rather than
      // hard-erroring inside its consteval body.
      typename std::bool_constant<(Less::template less<X, Y>() ==
                                   lattice_less_logic_t<Less>::True)>;
    };

/** @brief Did the comparator decide @b definitely @c True? (`Unknown`/`False`
 *  ⟹ don't reorder — keep authoring order.) */
export template <typename Less, typename X, typename Y>
consteval bool lattice_definitely_less() {
  if constexpr (IsLatticeLess<Less, X, Y>) {
    return Less::template less<X, Y>() == lattice_less_logic_t<Less>::True;
  } else {
    return false;  // no order for this pair ⟹ leave as authored
  }
}

/** @brief @c reduce_t<Term, Less, Ord> — the normal form of @c Term.  Leaves
 *  reduce to themselves; @c Meet / @c Join recurse into their operands, then
 *  assemble the induced laws (@c :lattice), canonicalising the commutative
 *  residue by @c Less. */
export template <typename Term, typename Less, typename Ord = canonical_order>
struct reduce {
  using type = Term;
};
export template <typename Term, typename Less, typename Ord = canonical_order>
using reduce_t = typename reduce<Term, Less, Ord>::type;

namespace detail_lattice_term {

// Assemble the meet laws over two ALREADY-REDUCED operands, most-collapsing
// first (bounded ▸ idempotent ▸ glb collapse), then canonicalise the
// commutative residue.  A law returning `law_inactive` cedes to the next.
template <typename RA, typename RB, typename Less, typename Ord>
consteval auto meet_assemble() {
  using Bounded = typename decltype(meet_bounded_law<RA, RB, Ord>())::type;
  if constexpr (!std::same_as<Bounded, law_inactive>) {
    return std::type_identity<Bounded>{};
  } else {
    using Idem = typename decltype(idempotent_law<RA, RB>())::type;
    if constexpr (!std::same_as<Idem, law_inactive>) {
      return std::type_identity<Idem>{};
    } else {
      using Comp = typename decltype(meet_complement_law<RA, RB, Ord>())::type;
      if constexpr (!std::same_as<Comp, law_inactive>) {
        return std::type_identity<Comp>{};  // a ∧ ¬a = ⊥ (complemented lattice)
      } else {
        using Abs =
            typename decltype(meet_structural_absorption_law<RA, RB>())::type;
        if constexpr (!std::same_as<Abs, law_inactive>) {
          return std::type_identity<Abs>{};  // a ∧ (a ∨ b) = a
        } else {
          using Dist =
              typename decltype(meet_distributivity_law<RA, RB, Ord>())::type;
          if constexpr (!std::same_as<Dist, law_inactive>) {
            // distributed to a join-of-meets; re-reduce toward DNF (terminates
            // — one direction only).
            return std::type_identity<reduce_t<Dist, Less, Ord>>{};
          } else {
            using Glb = typename decltype(meet_glb_law<RA, RB, Ord>())::type;
            if constexpr (!std::same_as<Glb, law_inactive>) {
              return std::type_identity<Glb>{};
            } else if constexpr (lattice_definitely_less<Less, RB, RA>()) {
              return std::type_identity<Meet<RB, RA>>{};  // canonicalise
            } else {
              return std::type_identity<Meet<RA, RB>>{};  // Unknown ⟹ keep
            }
          }
        }
      }
    }
  }
}

// The exact dual for join (⊤ annihilates, ⊥ is the unit; join = lub).
template <typename RA, typename RB, typename Less, typename Ord>
consteval auto join_assemble() {
  using Bounded = typename decltype(join_bounded_law<RA, RB, Ord>())::type;
  if constexpr (!std::same_as<Bounded, law_inactive>) {
    return std::type_identity<Bounded>{};
  } else {
    using Idem = typename decltype(idempotent_law<RA, RB>())::type;
    if constexpr (!std::same_as<Idem, law_inactive>) {
      return std::type_identity<Idem>{};
    } else {
      using Comp = typename decltype(join_complement_law<RA, RB, Ord>())::type;
      if constexpr (!std::same_as<Comp, law_inactive>) {
        return std::type_identity<Comp>{};  // a ∨ ¬a = ⊤ (complemented lattice)
      } else {
        using Abs =
            typename decltype(join_structural_absorption_law<RA, RB>())::type;
        if constexpr (!std::same_as<Abs, law_inactive>) {
          return std::type_identity<Abs>{};  // a ∨ (a ∧ b) = a
        } else {
          using Lub = typename decltype(join_lub_law<RA, RB, Ord>())::type;
          if constexpr (!std::same_as<Lub, law_inactive>) {
            return std::type_identity<Lub>{};
          } else if constexpr (lattice_definitely_less<Less, RB, RA>()) {
            return std::type_identity<Join<RB, RA>>{};
          } else {
            return std::type_identity<Join<RA, RB>>{};
          }
        }
      }
    }
  }
}

}  // namespace detail_lattice_term

export template <typename A, typename B, typename Less, typename Ord>
struct reduce<Meet<A, B>, Less, Ord> {
  using type = typename decltype(detail_lattice_term::meet_assemble<
                                 reduce_t<A, Less, Ord>, reduce_t<B, Less, Ord>,
                                 Less, Ord>())::type;
};

export template <typename A, typename B, typename Less, typename Ord>
struct reduce<Join<A, B>, Less, Ord> {
  using type = typename decltype(detail_lattice_term::join_assemble<
                                 reduce_t<A, Less, Ord>, reduce_t<B, Less, Ord>,
                                 Less, Ord>())::type;
};

// ¬A: reduce the operand, then apply the De Morgan negation law (involution
// ¬¬A→A / De Morgan, gated on an involutive De Morgan negation — NOT a genuine
// complement).  If it fires, the pushed-down result is re-reduced (¬ descends
// toward the leaves, so this terminates); otherwise ¬(reduced) is already
// negation-normal and stays.
export template <typename A, typename Less, typename Ord>
struct reduce<Not<A>, Less, Ord> {
 private:
  using RA = reduce_t<A, Less, Ord>;
  using Pushed = typename decltype(de_morgan_law<RA, Ord>())::type;

 public:
  using type = std::conditional_t<std::same_as<Pushed, law_inactive>, Not<RA>,
                                  reduce_t<Pushed, Less, Ord>>;
};

}  // namespace dedekind::category
