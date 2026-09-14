/**
 * @file dedekind/topology/powerset.cppm
 * @partition :powerset
 * @brief The power set @f$\mathfrak{P}(S)@f$ as a filtered universe over a
 *        reified subobject domain @c Sub(C) (#830).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @details In the grammar's own vocabulary (empty / universe / filtered base),
 * @f[ \mathfrak{P}(S) \;\equiv\; \Omega\langle \mathrm{Sub}(C)\rangle \,\mid\,
 *     (X \mapsto X \subseteq S), @f]
 * an ordinary @c Set over the subobject domain @c Sub(C).  So @c 𝔓(S) is a
 * bona-fide @c IsSet and inherits the lattice laws (@c &Ø=Ø, @c |Ω=Ω) and
 * grammar-composability from the set machinery, with no bespoke carrier.
 *
 * @c Sub(C) is the gated part (the @c exists / @c forall pattern: an algebraic
 * default lifted by decidable specialisations).  Over an @b ordered carrier the
 * decidable convex subobjects --- @c Ø, @c Ω, @c Singleton, @c Halfspace,
 * @c OrderInterval --- all collapse to ONE @c std::regular type, a runtime
 * @c Sub; membership @f$X \subseteq S@f$ is then a @b homogeneous interval
 * nesting (no family variant, no double dispatch), reusing the #835 endpoint
 * comparison.  A base with no coercion to @c Sub (a general / unordered set)
 * has no @c Sub(C) --- @c 𝔓 is then ill-formed (type-check failure by default),
 * the honest wall.
 *
 * @section powerset__Home Home
 * @c Sub is the runtime-pivot mereological @b unification of this module's
 * @c Ray / @c HalfSpace / @c Interval family together with the empty / full
 * cases, reified as ONE @c std::regular value; hence it lives here, beside its
 * siblings.  @c topology is downstream of both @c order and @c sets, so this
 * partition sees the ordered NTTP-pivot families (@c order::Halfspace, ...) and
 * the deleted @c sets::power_set gate at once; the constrained overload below
 * @b subsumes that gate for the ordered families (#830).
 *
 * @build_order 6.2
 * @dependency :category, :order, :sets
 */

module;

#include <concepts>

export module dedekind.topology:powerset;

import dedekind.category;
import dedekind.sets;  // Ø, UniversalSet, Set, SetShaped (the deleted gate)
import dedekind.order; // Halfspace, Singleton, OrderInterval, Direction, ...

namespace dedekind::topology {

using namespace dedekind::category;
using namespace dedekind::sets;
using namespace dedekind::order;

/** @brief @c Sub(C) for an @b ordered carrier: a subobject reified as a runtime
 *  interval value.  Bounds are @c C values with @c ±∞ (unbounded) flags and an
 *  empty flag; the convex families coerce in via the converting constructors
 *  below (which also @b are the @c 𝔓 gate --- no constructor ⇒ no @c Sub(C)).
 *  @c std::regular (default @c == and members), so @c Set<Sub,…> is an
 *  @c IsSet. */
export template <typename C, typename L = ClassicalLogic>
struct Sub {
  C lo{};
  C hi{};
  bool lo_unbounded = true;  // default: Ω = (−∞, +∞)
  bool hi_unbounded = true;
  Strictness lo_strict = Strictness::Strict;  // open at the infinite ends
  Strictness hi_strict = Strictness::Strict;
  bool empty = false;

  using Domain = C;
  using logic_species = L;
  using Codomain = typename L::Ω;

  constexpr Sub() = default;  // Ω

  // --- the to_sub coercions (and, structurally, the 𝔓 gate) ---
  constexpr Sub(const dedekind::sets::Ø<C, L>&) : empty(true) {}
  template <typename Card>
  constexpr Sub(const dedekind::sets::UniversalSet<C, L, Card>&) {}  // Ω
  template <auto V>
  constexpr Sub(const Singleton<V, L>&)
      : lo(V),
        hi(V),
        lo_unbounded(false),
        hi_unbounded(false),
        lo_strict(Strictness::NonStrict),
        hi_strict(Strictness::NonStrict) {}
  template <auto P, Strictness S>
  constexpr Sub(const Halfspace<C, P, Direction::Upward, S, L>&)
      : lo(P), lo_unbounded(false), lo_strict(S) {}  // (P, +∞)
  template <auto P, Strictness S>
  constexpr Sub(const Halfspace<C, P, Direction::Downward, S, L>&)
      : hi(P), hi_unbounded(false), hi_strict(S) {}  // (−∞, P)
  template <auto Lo, auto Hi, Strictness SL, Strictness SU>
  constexpr Sub(const OrderInterval<C, Lo, Hi, SL, SU, L>&)
      : lo(Lo),
        hi(Hi),
        lo_unbounded(false),
        hi_unbounded(false),
        lo_strict(SL),
        hi_strict(SU),
        empty(OrderInterval<C, Lo, Hi, SL, SU, L>::is_empty) {}

  /** @brief χ: is @c x in this interval? */
  constexpr typename L::Ω operator()(const C& x) const {
    if (empty) return L::False;
    const bool lo_ok = lo_unbounded ||
                       (lo_strict == Strictness::Strict ? (x > lo) : (x >= lo));
    const bool hi_ok = hi_unbounded ||
                       (hi_strict == Strictness::Strict ? (x < hi) : (x <= hi));
    return (lo_ok && hi_ok) ? L::True : L::False;
  }

  constexpr bool operator==(const Sub&) const = default;
};

/** @brief @f$a \subseteq b@f$ --- homogeneous interval nesting (the #835
 *  endpoint comparison on runtime bounds).  @f$\emptyset \subseteq X@f$; a
 *  non-empty interval is no subset of @c ∅; otherwise @c a's ends sit inside
 *  @c b's. */
export template <typename C, typename L>
constexpr typename L::Ω operator<=(const Sub<C, L>& a, const Sub<C, L>& b) {
  if (a.empty) return L::True;
  if (b.empty) return L::False;
  const bool lower = b.lo_unbounded ||
                     (!a.lo_unbounded &&
                      (a.lo > b.lo || (a.lo == b.lo &&
                                       !(a.lo_strict == Strictness::NonStrict &&
                                         b.lo_strict == Strictness::Strict))));
  const bool upper = b.hi_unbounded ||
                     (!a.hi_unbounded &&
                      (a.hi < b.hi || (a.hi == b.hi &&
                                       !(a.hi_strict == Strictness::NonStrict &&
                                         b.hi_strict == Strictness::Strict))));
  return (lower && upper) ? L::True : L::False;
}

/** @brief The membership predicate of @c 𝔓(S): @f$X \mapsto X \subseteq S@f$,
 *  with @c S coerced to @c Sub(C).  A named functor (no lambda). */
export template <typename C, typename L>
struct SubsetOf {
  Sub<C, L> base;
  constexpr typename L::Ω operator()(const Sub<C, L>& x) const {
    return x <= base;
  }
};

/** @brief @f$\mathfrak{P}(S) = \Omega\langle\mathrm{Sub}(C)\rangle \mid
 *  (X \mapsto X \subseteq S)@f$ --- a bona-fide @c Set over @c Sub(C).  Gated
 * on
 *  @c S coercing to @c Sub(C): an unordered / general base is ill-formed. */
export template <typename S>
  requires dedekind::sets::SetShaped<S> &&
           std::convertible_to<
               S, Sub<typename S::Domain, typename S::logic_species>>
constexpr auto power_set(const S& base) {
  using C = typename S::Domain;
  using L = typename S::logic_species;
  return dedekind::sets::Set<Sub<C, L>, L, SubsetOf<C, L>>{
      SubsetOf<C, L>{Sub<C, L>{base}}};
}

/** @brief Textbook fraktur-P alias for @c power_set (blackboard @c 𝔓).  The
 *  @c SetShaped conjunct makes this @b subsume the @c :sets deleted default, so
 *  it wins by partial ordering for the ordered families. */
export template <typename S>
  requires dedekind::sets::SetShaped<S> &&
           std::convertible_to<
               S, Sub<typename S::Domain, typename S::logic_species>>
constexpr auto 𝔓(const S& base) {
  return power_set(base);
}

}  // namespace dedekind::topology
