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
#include <functional>  // std::logical_and (the Boolean-lattice glb witness)
#include <type_traits>
#include <utility>  // std::forward

export module dedekind.category:lattice_term;

import :lattice;  // the term AST + the induced laws (the validated parts)
import :logic;    // Boole (default), Ternary, IsOckhamAlgebra
import :cartesian_bicategory;  // Copy / Merge / IsMeetAsRightAdjoint (Δ ⊣ ∧)

namespace dedekind::category {

/** @brief The logic species a comparator reports in — @c Boole (a
 *  @c bool decision) unless the comparator names its own @c logic typedef (e.g.
 *  @c Kleene, so an @b undecidable comparison can be @c Unknown). */
export template <typename Less>
struct lattice_less_logic {
  using type = Boole;
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
 *  @c lattice_less_logic (default @c Boole, i.e. @c bool).  The
 *  reported logic must be a full @c IsOckhamAlgebra, so @c ::True (accessed
 *  by @c lattice_definitely_less) is guaranteed present.  The reducer
 *  canonicalises only on a @b definitely-True result, so an @c Unknown
 *  (undecidable) comparison leaves the operands in authoring order.  Total-ness
 *  is the caller's obligation (Jlt: asserted downstream), not checked here. */
export template <typename Less, typename X, typename Y>
concept IsLatticeLess =
    IsOckhamAlgebra<lattice_less_logic_t<Less>> && requires {
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

/** @brief The default leaf-combiner performs no domain combination.  Two
 *  order-incomparable leaves stay a @c Meet / @c Join node.  A downstream
 *  carrier (e.g.\ @c sets) injects a @c Combine that computes the actual domain
 *  meet / join of two compatible leaves.  @c structured_and of two halfspaces
 *  yields an @c OrderInterval, say.  @c Combine returns @c law_inactive when no
 *  domain combination applies.  This injected policy lets the reducer's
 *  incomparable-leaf residual fall through to the carrier's own `∧`/`∨`. */
export struct no_leaf_combine {
  template <typename, typename>
  static consteval auto meet() {
    return std::type_identity<law_inactive>{};
  }
  template <typename, typename>
  static consteval auto join() {
    return std::type_identity<law_inactive>{};
  }
};

/** @brief The normal form of @c Term under @c reduce_t<Term, Less, Ord,
 *  Combine>.  Leaves reduce to themselves.  @c Meet / @c Join recurse into
 * their operands, then assemble the induced laws (@c :lattice).  @c Less
 * canonicalises the commutative residue.  An order-incomparable residual is
 * offered last to the injected leaf-combiner @c Combine (the carrier's domain
 * `∧`/`∨`). */
export template <typename Term, typename Less, typename Ord = canonical_order,
                 typename Combine = no_leaf_combine>
struct reduce {
  using type = Term;
};
export template <typename Term, typename Less, typename Ord = canonical_order,
                 typename Combine = no_leaf_combine>
using reduce_t = typename reduce<Term, Less, Ord, Combine>::type;

// The reduction laws assembled here are order-theoretic: the meet IS the glb,
// the right adjoint of the diagonal (Δ ⊣ ∧, epic #946).  Pin that reification
// as a compile-time check against the reducer's canonical carrier --- @c bool,
// the two-element Boolean lattice where every induced law fires (see
// @ref lattice_term__Overview).  The injected glb is the Boolean @c AND (the
// meet under the chain @c false ≤ @c true) and @c Copy is the diagonal
// @c bool → @c bool×bool.  This edge makes @c :cartesian_bicategory
// load-bearing: the reducer's meet path type-checks against the category-level
// glb theory rather than importing an orphan partition.
// This is the reducer EDGE type-check, so it stays next to the reducer: the
// carrier-generic coherence witnesses (int / Ternary chains, the Sup false
// positive, and the meet-trichotomy IsProduct bridge) live with the concept in
// :cartesian_bicategory (#946).
static_assert(
    IsMeetAsRightAdjoint<Copy<bool>, Merge<bool, std::logical_and<bool>>>,
    "the reducer's meet path must be the right adjoint of the diagonal "
    "(Δ ⊣ ∧) over the Boolean-lattice witness.");

namespace detail_lattice_term {

// Assemble the meet laws over two ALREADY-REDUCED operands, most-collapsing
// first (bounded ▸ idempotent ▸ complement ▸ absorption ▸ distributivity ▸ glb
// ▸ injected leaf-combine), then canonicalise the commutative residue.  A law
// returning `law_inactive` cedes to the next.
template <typename RA, typename RB, typename Less, typename Ord,
          typename Combine>
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
            // distributed to a join-of-meets; re-reduce toward DNF.  This
            // terminates in one direction only.
            return std::type_identity<reduce_t<Dist, Less, Ord, Combine>>{};
          } else {
            using Glb = typename decltype(meet_glb_law<RA, RB, Ord>())::type;
            if constexpr (!std::same_as<Glb, law_inactive>) {
              return std::type_identity<Glb>{};
            } else {
              // The order-incomparable residual: offer it to the injected
              // domain leaf-combiner (e.g. structured_and of two halfspaces);
              // re-reduce its result.  law_inactive ⟹ keep / canonicalise.
              using Dom =
                  typename decltype(Combine::template meet<RA, RB>())::type;
              if constexpr (!std::same_as<Dom, law_inactive>) {
                return std::type_identity<reduce_t<Dom, Less, Ord, Combine>>{};
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
}

// The exact dual for join (⊤ annihilates, ⊥ is the unit; join = lub).
template <typename RA, typename RB, typename Less, typename Ord,
          typename Combine>
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
          } else {
            using Dom =
                typename decltype(Combine::template join<RA, RB>())::type;
            if constexpr (!std::same_as<Dom, law_inactive>) {
              return std::type_identity<reduce_t<Dom, Less, Ord, Combine>>{};
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
}

}  // namespace detail_lattice_term

export template <typename A, typename B, typename Less, typename Ord,
                 typename Combine>
struct reduce<Meet<A, B>, Less, Ord, Combine> {
  using type = typename decltype(detail_lattice_term::meet_assemble<
                                 reduce_t<A, Less, Ord, Combine>,
                                 reduce_t<B, Less, Ord, Combine>, Less, Ord,
                                 Combine>())::type;
};

export template <typename A, typename B, typename Less, typename Ord,
                 typename Combine>
struct reduce<Join<A, B>, Less, Ord, Combine> {
  using type = typename decltype(detail_lattice_term::join_assemble<
                                 reduce_t<A, Less, Ord, Combine>,
                                 reduce_t<B, Less, Ord, Combine>, Less, Ord,
                                 Combine>())::type;
};

// ¬A: reduce the operand, then apply the De Morgan negation law (involution
// ¬¬A→A / De Morgan, gated on an involutive De Morgan negation — NOT a genuine
// complement).  If it fires, the pushed-down result is re-reduced (¬ descends
// toward the leaves, so this terminates); otherwise ¬(reduced) is already
// negation-normal and stays.
export template <typename A, typename Less, typename Ord, typename Combine>
struct reduce<Not<A>, Less, Ord, Combine> {
 private:
  using RA = reduce_t<A, Less, Ord, Combine>;
  using Pushed = typename decltype(de_morgan_law<RA, Ord>())::type;

 public:
  using type = std::conditional_t<std::same_as<Pushed, law_inactive>, Not<RA>,
                                  reduce_t<Pushed, Less, Ord, Combine>>;
};

// ── Value-first reduce (#922) ──────────────────────────────────────────────
// The runtime/dual-phase twin of @c reduce_t: it reduces a term @b value to its
// normal-form @b value, so the SAME laws run at compile time, at runtime, and
// (via a binding) from Python, with no second reducer.  The reduction DECISION
// (which law fires, hence the normal-form TYPE @c D) is still the type-level
// @c reduce_t; this only reconstructs @c D's value from the node's stored
// operands, preserving a runtime-stateful operand where the normal form IS that
// operand (the win over the type-level path, which can only default-construct a
// stateless normal form).  Mirrors the value-first codomain leg
// @c sets::finalize_combine (#915).

namespace detail_lattice_term {

/** @brief The shared collapse tail: @c D{} for a @b value-determined collapse
 *  (@c IsIdempotentLeaf<D> and default-constructible, so a boundary @c ⊥ / @c ⊤
 *  or a DNF over type-determined leaves), else the @c Fallback (the unreduced
 *  node) built @b lazily from the forwarded operands.
 *  @details The fallback is built @b only in the @c else branch.  The collapse
 *  path therefore constructs no fallback at all.  The deferral is forwarded
 *  args plus @c if @c constexpr, @b not a closure.  The helper takes no
 *  pre-built fallback by const-ref, so it copies no fallback @b temporary out.
 *  (Constructing @c Fallback{args...} still copies the operands into the node,
 *  exactly as the inline form did; those operand copies are not eliminated.)
 *  @note The fail-safe is this slice's intended behavior.  Rebuilding the
 *  distributed / pushed value from the sub-values is a @b separate net-positive
 *  follow-on (under #922), not this unification. */
template <typename D, typename Fallback, typename... Args>
constexpr auto materialize_or_keep(Args&&... args) {
  if constexpr (IsIdempotentLeaf<D> && std::default_initializable<D>) {
    return D{};  // value-determined collapse / DNF: exact (nothing built)
  } else {
    return Fallback{std::forward<Args>(args)...};  // built lazily, here only
  }
}

/** @brief Reconstruct a reduced @b binary node's value from the already-reduced
 *  operands @c ra / @c rb, given its normal-form type @c D.  @c Meet and @c
 * Join are injected as the template-template @c Node and share this one
 * adapter.
 *  @details The @b value-output adapter of the one decision engine (@c reduce_t
 *  decided @c D).  Value-preserving cases: the normal form IS an operand (unit
 * / idempotency / absorption / glb) or the irreducible / canonical residue (a
 *  @c Node of the two, either order).  Every other @c D routes through the
 *  shared @ref materialize_or_keep tail, whose fail-safe lazily rebuilds the
 *  unreduced @c Node. */
template <template <typename, typename> class Node, typename D, typename RA,
          typename RB>
constexpr auto rebuild_binary(const RA& ra, const RB& rb) {
  if constexpr (std::same_as<D, RA>) {
    return ra;
  } else if constexpr (std::same_as<D, RB>) {
    return rb;
  } else if constexpr (std::same_as<D, Node<RA, RB>>) {
    return Node<RA, RB>{ra, rb};
  } else if constexpr (std::same_as<D, Node<RB, RA>>) {
    return Node<RB, RA>{rb, ra};
  } else {
    return materialize_or_keep<D, Node<RA, RB>>(ra, rb);
  }
}

}  // namespace detail_lattice_term

/** @brief Value-first reduce, leaf case: a leaf reduces to itself. */
export template <typename Less, typename Ord, typename Combine, typename Leaf>
constexpr auto reduce_value(const Leaf& leaf) {
  return leaf;
}

/** @brief Value-first reduce of a @c Meet value: reduce the operands, then
 *  reconstruct the value of the type-level normal form. */
export template <typename Less, typename Ord, typename Combine, typename A,
                 typename B>
constexpr auto reduce_value(const Meet<A, B>& node) {
  const auto ra = reduce_value<Less, Ord, Combine>(π_1(node));
  const auto rb = reduce_value<Less, Ord, Combine>(π_2(node));
  using D = reduce_t<Meet<std::remove_cvref_t<decltype(ra)>,
                          std::remove_cvref_t<decltype(rb)>>,
                     Less, Ord, Combine>;
  return detail_lattice_term::rebuild_binary<Meet, D>(ra, rb);
}

/** @brief Value-first reduce of a @c Join value (dual of the @c Meet case). */
export template <typename Less, typename Ord, typename Combine, typename A,
                 typename B>
constexpr auto reduce_value(const Join<A, B>& node) {
  const auto ra = reduce_value<Less, Ord, Combine>(π_1(node));
  const auto rb = reduce_value<Less, Ord, Combine>(π_2(node));
  using D = reduce_t<Join<std::remove_cvref_t<decltype(ra)>,
                          std::remove_cvref_t<decltype(rb)>>,
                     Less, Ord, Combine>;
  return detail_lattice_term::rebuild_binary<Join, D>(ra, rb);
}

/** @brief Value-first reduce of a @c Not value: reduce the operand, keep
 *  @c ¬(reduced) unless De Morgan / involution pushed it to another shape.
 *  @details No push (@c D is @c Not<RB>) keeps @c ¬(reduced); any push routes
 *  through the same @ref detail_lattice_term::materialize_or_keep tail as the
 *  binary nodes.  The @c D==Not<RB> guard stays ahead of the helper: since
 *  @c idempotent_leaf_v<Not<A>> tracks its base, an idempotent @c Not must keep
 *  its operand rather than default-collapse. */
export template <typename Less, typename Ord, typename Combine, typename A>
constexpr auto reduce_value(const Not<A>& node) {
  const auto rb = reduce_value<Less, Ord, Combine>(node.base);
  using RB = std::remove_cvref_t<decltype(rb)>;
  using D = reduce_t<Not<RB>, Less, Ord, Combine>;
  if constexpr (std::same_as<D, Not<RB>>) {
    return Not<RB>{rb};  // no push: already negation-normal
  } else {
    return detail_lattice_term::materialize_or_keep<D, Not<RB>>(rb);
  }
}

}  // namespace dedekind::category
