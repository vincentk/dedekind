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
 * @f[ \mathfrak{P}(S) \;\equiv\; \mathbb{A}\langle \mathrm{Sub}(C)\rangle
 * \,\mid\, (X \mapsto X \subseteq S), @f] an ordinary @c Set over the subobject
 * domain @c Sub(C).  So @c 𝔓(S) is a bona-fide @c IsSet and inherits the
 * lattice laws (@c &Ø=Ø, @c |𝔸=𝔸) and
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
 * decidable convex subobjects --- @c Ø, @c 𝔸, @c Singleton, @c Halfspace,
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
 * (@c Ø / @c 𝔸) and the @c order NTTP-pivot families (@c Singleton / @c
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
 *  Folds onto @c SetExpr (so it is itself a first-class @c IsSubobject / @c
 *  IsSet) and is @c std::regular, so @c Set<Sub,…> is an @c IsSet and @c Sub
 *  values are usable in generic set APIs. */
export template <typename C, typename L = ClassicalLogic>
struct Sub : dedekind::sets::SetExpr<Sub<C, L>, C, L> {
  // Domain / Codomain / logic_species / Member / ι / contains are inherited
  // from SetExpr (the ETCS subobject surface), exactly as Halfspace / Singleton
  // / OrderInterval / Ray fold onto it --- so a Sub value is itself a
  // first-class IsSubobject / IsSet (pinned below), not a bespoke half-surface.
  // State is PRIVATE and canonicalised at construction (normalize()), so no
  // caller can fabricate a non-canonical Sub that breaks == / <= / χ.

  constexpr Sub() = default;  // 𝔸

  // --- the to_sub coercions (and, structurally, the 𝔓 gate) ---
  constexpr Sub(const dedekind::sets::Ø<C, L>&) : empty_(true) { normalize(); }
  template <typename Card>
  constexpr Sub(const dedekind::sets::UniversalSet<C, L, Card>&) {}  // 𝔸
  // The singleton's carrier must BE @c C (like the Halfspace / OrderInterval
  // ctors that fix @c C): otherwise @c Singleton<4.5> would silently narrow
  // into a @c Sub<int>, testing a different set.  Cross-carrier needs an
  // explicit order embedding, not an implicit coercion.
  template <auto V>
    requires std::same_as<decltype(V), C>
  constexpr Sub(const Singleton<V, L>&)
      : lo_(V),
        hi_(V),
        lo_unbounded_(false),
        hi_unbounded_(false),
        lo_strict_(Strictness::NonStrict),
        hi_strict_(Strictness::NonStrict) {}
  template <auto P, Strictness S>
  constexpr Sub(const Halfspace<C, P, Direction::Upward, S, L>&)
      : lo_(P), lo_unbounded_(false), lo_strict_(S) {
    normalize();
  }  // (P, +∞)
  template <auto P, Strictness S>
  constexpr Sub(const Halfspace<C, P, Direction::Downward, S, L>&)
      : hi_(P), hi_unbounded_(false), hi_strict_(S) {
    normalize();
  }  // (−∞, P)
  template <auto Lo, auto Hi, Strictness SL, Strictness SU>
  constexpr Sub(const OrderInterval<C, Lo, Hi, SL, SU, L>&)
      : lo_(Lo),
        hi_(Hi),
        lo_unbounded_(false),
        hi_unbounded_(false),
        lo_strict_(SL),
        hi_strict_(SU),
        empty_(OrderInterval<C, Lo, Hi, SL, SU, L>::is_empty) {
    normalize();
  }

  /** @brief χ: is @c x in this interval? */
  constexpr typename L::Ω operator()(const C& x) const {
    if (empty_) return L::False;
    const bool lo_ok =
        lo_unbounded_ ||
        (lo_strict_ == Strictness::Strict ? (x > lo_) : (x >= lo_));
    const bool hi_ok =
        hi_unbounded_ ||
        (hi_strict_ == Strictness::Strict ? (x < hi_) : (x <= hi_));
    return (lo_ok && hi_ok) ? L::True : L::False;
  }

  /** @brief Value equality with @b canonical emptiness: every empty interval
   *  denotes @f$\emptyset@f$ regardless of the (dead) bound fields, so all
   *  empties compare equal (and no empty equals a non-empty).  Non-empty
   *  intervals compare fieldwise (bounds already in canonical form).  Keeps
   *  @c Sub a proper @c std::regular value whose @c == matches its extension.
   */
  constexpr bool operator==(const Sub& o) const {
    if (empty_ || o.empty_) return empty_ == o.empty_;
    return lo_ == o.lo_ && hi_ == o.hi_ && lo_unbounded_ == o.lo_unbounded_ &&
           hi_unbounded_ == o.hi_unbounded_ && lo_strict_ == o.lo_strict_ &&
           hi_strict_ == o.hi_strict_;
  }

  /** @brief @f$a \subseteq b@f$ --- homogeneous interval nesting (the #835
   *  endpoint comparison on the canonical runtime bounds).  A hidden friend so
   *  it reads the private state; @f$\emptyset \subseteq X@f$; a non-empty
   *  interval is no subset of @c ∅; otherwise @c a's ends sit inside @c b's. */
  friend constexpr typename L::Ω operator<=(const Sub& a, const Sub& b) {
    if (a.empty_) return L::True;
    if (b.empty_) return L::False;
    const bool lower =
        b.lo_unbounded_ ||
        (!a.lo_unbounded_ &&
         (a.lo_ > b.lo_ ||
          (a.lo_ == b.lo_ && !(a.lo_strict_ == Strictness::NonStrict &&
                               b.lo_strict_ == Strictness::Strict))));
    const bool upper =
        b.hi_unbounded_ ||
        (!a.hi_unbounded_ &&
         (a.hi_ < b.hi_ ||
          (a.hi_ == b.hi_ && !(a.hi_strict_ == Strictness::NonStrict &&
                               b.hi_strict_ == Strictness::Strict))));
    return (lower && upper) ? L::True : L::False;
  }

 private:
  C lo_{};
  C hi_{};
  bool lo_unbounded_ = true;  // default: 𝔸 = (−∞, +∞)
  bool hi_unbounded_ = true;
  Strictness lo_strict_ = Strictness::Strict;  // open at the infinite ends
  Strictness hi_strict_ = Strictness::Strict;
  bool empty_ = false;

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
      if (!empty_) {
        if (!lo_unbounded_ && lo_strict_ == Strictness::Strict) {
          lo_ = static_cast<C>(lo_ + C{1});
          lo_strict_ = Strictness::NonStrict;
        }
        if (!hi_unbounded_ && hi_strict_ == Strictness::Strict) {
          hi_ = static_cast<C>(hi_ - C{1});
          hi_strict_ = Strictness::NonStrict;
        }
        if (!lo_unbounded_ && !hi_unbounded_ && lo_ > hi_) empty_ = true;
      }
    }
    if (empty_) {  // one canonical ∅ (dead bound fields), every carrier
      lo_ = C{};
      hi_ = C{};
      lo_unbounded_ = false;
      hi_unbounded_ = false;
      lo_strict_ = Strictness::Strict;
      hi_strict_ = Strictness::Strict;
    }
  }
};

static_assert(dedekind::category::IsSubobject<Sub<int>, int>,
              "Sub is a first-class subobject ι: Sub ↣ C (via SetExpr).");
static_assert(dedekind::category::IsSet<Sub<int>>,
              "Sub is a first-class ETCS set, so 𝔓's elements are usable in "
              "generic set APIs (the CP round-2 contract).");

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

/** @brief @f$\mathfrak{P}(S) = \mathbb{A}\langle\mathrm{Sub}(C)\rangle \mid
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

/** @brief @f$\mathfrak{P}(\mathbb{A}) =
 * \mathbb{A}\langle\mathrm{Sub}(C)\rangle@f$ --- the universe of ALL (decidable
 * convex) subobjects, as the universal @b boundary
 *  @b type, @b not a trivially-true filtered @c Set.  A more-specialised
 * overload than the generic @c power_set above, so it wins by partial ordering;
 * keeping the @c 𝔸 type (rather than @c Set<Sub,…,SubsetOf>) preserves the
 *  boundary / cardinality metadata and the lattice identities @c 𝔸|X=𝔸 / @c
 *  𝔸&X=X on @c 𝔓(𝔸).  (Sibling of the @c :sets closed form
 *  @f$\mathfrak{P}(\emptyset)=\{\emptyset\}=\mathbb{A}\langle\varnothing\rangle@f$.)
 */
export template <typename C, typename L, typename Card>
  requires IsTotallyOrdered<C>
constexpr auto power_set(const dedekind::sets::UniversalSet<C, L, Card>&) {
  return dedekind::sets::𝔸<Sub<C, L>, L>;
}

/** @brief Textbook fraktur-P alias for @c power_set (blackboard @c 𝔓). Forwards
 *  to @c power_set, so the @c 𝔸 closed form above is selected for a universe
 *  base and the filtered form for a proper ordered base. */
export template <SubReifiable S>
constexpr auto 𝔓(const S& base) {
  return power_set(base);
}

}  // namespace dedekind::order
