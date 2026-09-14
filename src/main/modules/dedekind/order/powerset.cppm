/**
 * @file dedekind/order/powerset.cppm
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
 * @b setexpr @b participation --- meet / join / filter as any @c Set --- from
 * the set machinery, with no bespoke carrier.  (This is @b not
 * self-composition:
 * @c 𝔓(𝔓(S)) is @b not decidable-convex --- @f$\downarrow\!S@f$, the principal
 * ideal @f$\{X : X \subseteq S\}@f$, is a down-set in @c Sub(C), not a convex
 * @c Sub value --- so nested @c 𝔓 correctly meets the honest wall, like any
 * non-convex base.)
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
 * The @b enabler here is an @b ordered carrier, not intervals: @c Sub becomes a
 * decidable @c std::regular normal form precisely because @c C is totally
 * ordered, and membership @f$X \subseteq S@f$ @b is the subset order (@c
 * :inclusion).  So the home is @c order, where that order lives --- @b not
 * @c topology (whose @c Interval is about the continuum / neighbourhoods).  @c
 * Sub has @b no topology dependency: it coerces in only the @c :sets boundaries
 * (@c Ø / @c Ω) and the @c order NTTP-pivot families (@c Singleton / @c
 * Halfspace / @c OrderInterval).  Being upstream, this reaches every downstream
 * layer (topology included).  (The earlier @c order::Interval name collided
 * with
 * @c topology::Interval; renaming to @c Sub removed that, so the placement is
 * decided on structure, not on avoiding a clash.)
 *
 * @build_order (order layer)
 * @dependency :category, :sets, :total, :halfspace, :inclusion
 *
 * Wikipedia: Power set, Subobject, Interval (mathematics)
 *
 * @note "A set is a Many that allows itself to be thought of as a One."
 *       --- Georg Cantor (letter to Dedekind, 1899).  @f$\mathfrak{P}(S)@f$
 *       reifies "the subobjects of @c S" as one @c Set, over one carrier.
 */

module;

#include <concepts>

export module dedekind.order:powerset;

import dedekind.category;
import dedekind.sets; // Ø, UniversalSet, Set, SetShaped (the deleted gate)
import :total;        // IsTotallyOrdered --- the ordered-carrier gate
import :halfspace;    // Halfspace, Singleton, OrderInterval, IsRingIntegral
import :inclusion;    // the subset order this 𝔓 filters on

namespace dedekind::order {

using namespace dedekind::category;
using namespace dedekind::sets;

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
  constexpr Sub(const dedekind::sets::Ø<C, L>&) : empty(true) { normalize(); }
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
      : lo(P), lo_unbounded(false), lo_strict(S) {
    normalize();
  }  // (P, +∞)
  template <auto P, Strictness S>
  constexpr Sub(const Halfspace<C, P, Direction::Downward, S, L>&)
      : hi(P), hi_unbounded(false), hi_strict(S) {
    normalize();
  }  // (−∞, P)
  template <auto Lo, auto Hi, Strictness SL, Strictness SU>
  constexpr Sub(const OrderInterval<C, Lo, Hi, SL, SU, L>&)
      : lo(Lo),
        hi(Hi),
        lo_unbounded(false),
        hi_unbounded(false),
        lo_strict(SL),
        hi_strict(SU),
        empty(OrderInterval<C, Lo, Hi, SL, SU, L>::is_empty) {
    normalize();
  }

  /** @brief χ: is @c x in this interval? */
  constexpr typename L::Ω operator()(const C& x) const {
    if (empty) return L::False;
    const bool lo_ok = lo_unbounded ||
                       (lo_strict == Strictness::Strict ? (x > lo) : (x >= lo));
    const bool hi_ok = hi_unbounded ||
                       (hi_strict == Strictness::Strict ? (x < hi) : (x <= hi));
    return (lo_ok && hi_ok) ? L::True : L::False;
  }

  /** @brief Value equality with @b canonical emptiness: every empty interval
   *  denotes @f$\emptyset@f$ regardless of the (dead) bound fields, so all
   *  empties compare equal (and no empty equals a non-empty).  Non-empty
   *  intervals compare fieldwise.  Keeps @c Sub a proper @c std::regular value
   *  whose @c == matches its extension, so @f$\mathfrak{P}@f$ over @c Sub does
   *  not distinguish two spellings of @f$\emptyset@f$. */
  constexpr bool operator==(const Sub& o) const {
    if (empty || o.empty) return empty == o.empty;
    return lo == o.lo && hi == o.hi && lo_unbounded == o.lo_unbounded &&
           hi_unbounded == o.hi_unbounded && lo_strict == o.lo_strict &&
           hi_strict == o.hi_strict;
  }

 private:
  /** @brief Canonicalise to @b effective closed bounds on a DISCRETE carrier
   * --- the runtime sibling of the #835 @c eff_lower / @c eff_upper
   * normalisation. Over an @c IsRingIntegral carrier @c {x>3} and @c {x>=4} are
   * the same subobject, and @c (1,4) = @c [2,3]; folding every strict finite
   * bound to its closed successor / predecessor makes those ONE @c Sub value,
   * so both @c == and @c <= (and hence @c 𝔓 membership) match extension.  A @b
   * continuous carrier keeps its open/closed distinction untouched (@c (1,4) !=
   * @c [1,4]). Empty (any carrier) collapses to a single canonical
   * @f$\emptyset@f$.
   *  FIXME(#838): a strict bound AT the carrier extremum overflows the
   *  @c +1/@c -1; that boundary-pivot corner is #838's carrier-native / clamped
   *  effective-bound story (never produced by @c structured_and / the DSL). */
  constexpr void normalize() {
    if constexpr (IsRingIntegral<C>) {
      if (!empty) {
        if (!lo_unbounded && lo_strict == Strictness::Strict) {
          lo = static_cast<C>(lo + C{1});
          lo_strict = Strictness::NonStrict;
        }
        if (!hi_unbounded && hi_strict == Strictness::Strict) {
          hi = static_cast<C>(hi - C{1});
          hi_strict = Strictness::NonStrict;
        }
        if (!lo_unbounded && !hi_unbounded && lo > hi) empty = true;
      }
    }
    if (empty) {  // one canonical ∅ (dead bound fields), every carrier
      lo = C{};
      hi = C{};
      lo_unbounded = false;
      hi_unbounded = false;
      lo_strict = Strictness::Strict;
      hi_strict = Strictness::Strict;
    }
  }
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

/** @brief A base that reifies as an ordered-carrier subobject @c Sub(C): it is
 *  @c SetShaped, its carrier is @b totally @b ordered (the enabler --- so the
 *  gate itself, not a later membership call, rejects an unordered carrier), @b
 *  and it coerces to @c Sub via one of the converting constructors above.  The
 *  single gate concept for both @c power_set and @c 𝔓 (DRY), and --- because it
 *  conjoins the very @c SetShaped atom the @c :sets default is constrained on
 * --- it @b subsumes that deleted gate, so it wins by partial ordering for the
 *  ordered families. */
export template <typename S>
concept SubReifiable =
    dedekind::sets::SetShaped<S> && IsTotallyOrdered<typename S::Domain> &&
    std::convertible_to<S, Sub<typename S::Domain, typename S::logic_species>>;

/** @brief @f$\mathfrak{P}(S) = \Omega\langle\mathrm{Sub}(C)\rangle \mid
 *  (X \mapsto X \subseteq S)@f$ --- a bona-fide @c Set over @c Sub(C).  Gated
 * on
 *  @c S coercing to @c Sub(C): an unordered / general base is ill-formed. */
export template <SubReifiable S>
constexpr auto power_set(const S& base) {
  using C = typename S::Domain;
  using L = typename S::logic_species;
  return dedekind::sets::Set<Sub<C, L>, L, SubsetOf<C, L>>{
      SubsetOf<C, L>{Sub<C, L>{base}}};
}

/** @brief Textbook fraktur-P alias for @c power_set (blackboard @c 𝔓). */
export template <SubReifiable S>
constexpr auto 𝔓(const S& base) {
  return power_set(base);
}

}  // namespace dedekind::order
