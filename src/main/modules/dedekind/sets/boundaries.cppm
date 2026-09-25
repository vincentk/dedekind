/**
 * @file dedekind/sets/boundaries.cppm
 * @partition :boundaries
 * @brief The Extremal Identities: Universal (V) and Empty (∅) Sets.
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @dependency dedekind.ontology
 *
 * @section boundaries__The_Structural_Limits
 * In the Dedekind topos, the boundaries of a Species define the 'North
 * and South poles' of the set-lattice. This partition implements the
 * identities required for a Bounded Lattice over any Species:
 * - UniversalSet (V): The 'Top' (⊤). The extensional whole of a Species.
 * - EmptySet (∅): The 'Bottom' (⊥). The mereological remainder of the whole.
 *
 * @details
 * These sets are the 'First-Class Citizens' of the Mereological System:
 * 1. Self-Awareness: Each boundary knows its 'Ambient Species' (The context).
 * 2. Duality: They are mutually defined via the Complement Morphism (!).
 * 3. Identity: They serve as the unit elements for Union (|) and Intersection
 * (&).
 *
 * @section boundaries__Semantic_Role
 * While a SingletonSet represents an 'Atomic Part', the boundaries
 * represent the 'Absolute State' of the Species. In a 'Family' (A Set
 * of Sets), these objects serve as the terminal bounds of the collection.
 *
 * @tparam Species The underlying domain (e.g., Integers, Booleans).
 * @tparam L The Subobject Classifier (Ω) governing the truth logic.
 *
 * Wikipedia: Universal set, Empty set, Bounded lattice, Identity element
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "For us there is no ignorabimus."
 *       -- David Hilbert, Konigsberg address (1930)
 */
module;

#include <algorithm>
#include <compare>
#include <concepts>
#include <cstddef>  // std::size_t (emptiness-by-size overload)
#include <functional>
#include <limits>
#include <ranges>  // std::ranges::range / begin / end (emptiness-by-iterator overload)
#include <utility>  // std::pair (Ø × S cartesian-product return type)

export module dedekind.sets:boundaries;

import dedekind.category;

import :cardinality;
import :mereology;

using namespace dedekind::category;

/**
 * @section boundaries__Mereology
 * @section boundaries__Mereology_2
 */
namespace dedekind::sets {

/** @brief ∅: The Initial Object. Extensional (Size 0).
 *
 *  @c T defaults to @c std::nullptr_t (the project's @c Zero carrier), so the
 *  bare @c Ø{} denotes "the empty set" with no carrier to name; it compares
 *  equal to any @c Ø<T> through the cross-carrier @c operator== below, letting
 *  a collapse be asserted as @c (a @c & @c ~a) @c == @c Ø{}. */
export template <typename T = std::nullptr_t, typename L = Boole>
struct Ø final {
  // ~ arrow / morphism / subobject classifier jargon
  using Domain = T;
  using Codomain = typename L::Ω;

  // ~ topoi jargon;
  /** @brief Member-shape mirror of Subobject's.  Vacuously inhabited (Ø has
   *  no members) — but the wrapper type is still required so the
   *  IsSubobject contract has a Member-to-T projection to read through ι. */
  struct Member {
    T value;
  };

  /** @brief ι: Ø ↣ T — the trivial inclusion.  No members are ever
   *  constructed, so ι is unreachable in practice; the body unwraps the
   *  Member's T-value to satisfy the SHAPE the IsSubobject contract
   *  reads. */
  constexpr T ι(const Member& m) const { return m.value; }

  static const Ø χ;

  using logic_species = L;
  using cardinality_type = Finite;
  using base_set_type = Ø<T, L>;

  /** @brief @c Ø is the @b initial object of the subobject poset @c Sub(U): the
   *  bottom @c ⊥, classified by the always-false predicate @c χ_Ø @c ≡ @c ⊥,
   *  with the unique (empty) arrow @c Ø @c → @c S into every subobject.  This
   *  tag opts @c Ø into @c :limit's @c IsInitialObject (the tag-discovery
   *  branch @c :lattice::LatticeBottom already uses), so the Sub(U) bound
   *  participates in the categorical initial-object vocabulary.  #881. */
  using is_initial_object_tag = void;
  // ⊥ = ∅ is clopen in every topology; @c topology::IsOpen / @c IsClosed INFER
  // that from the initiality tag above --- no separate open/closed tag (#903).

  /** @section boundaries__Algebraic_Axioms */
  template <typename Op>
  static constexpr bool is_associative_v =
      std::is_same_v<Op, std::bit_and<base_set_type>> ||
      std::is_same_v<Op, std::bit_or<base_set_type>>;

  template <typename Op>
  static constexpr bool is_idempotent_v =
      std::is_same_v<Op, std::bit_and<base_set_type>> ||
      std::is_same_v<Op, std::bit_or<base_set_type>>;

  constexpr Ø() = default;

  /**
   * @brief Cross-logic identity: the empty set under any logic species is the
   * empty set. Enables writing `Ø<int>` (L defaults to Boole) even
   * when the RHS was produced by a Set whose NaturalLogic selected
   * Kleene — mathematically ∅ = ∅ regardless of logic species.
   */
  template <typename OtherL>
    requires(!std::same_as<OtherL, L>)
  constexpr Ø(const Ø<T, OtherL>&) {}

  /** @section boundaries__Extensionality_Proof */
  constexpr std::size_t size() const { return 0; }

  /** @section boundaries__Lattice_Axiom: Initiality */
  // The Empty Set is a part of everything (including itself)
  // We use a simple template to avoid recursion with IsSet
  template <typename S>
  constexpr typename L::Ω operator<=(const S&) const {
    return L::True;
  }

  // Theorem: Two empty sets of the same species are identical.
  constexpr bool operator==(const Ø&) const { return true; }

  // Two empty sets are equal regardless of carrier / logic species.
  // Models the mathematical identity ∅ = ∅, independent of ambient type.
  template <typename T2, typename L2>
    requires(!std::same_as<T, T2> || !std::same_as<L, L2>)
  constexpr bool operator==(const Ø<T2, L2>&) const {
    return true;
  }

  // Ø == S: constrained emptiness tests only.  There is deliberately NO
  // catch-all; a set whose emptiness cannot be decided here matches no
  // overload, so `Ø == it` is a compile error (the honest Rice wall) rather
  // than a fabricated answer.  Each overload rules out a wrong result.

  // (i) a set-like operand exposing size(): empty iff 0 == size().  Fires for a
  //     Singleton at COMPILE time and for an extensional carrier at RUN time.
  template <typename S>
    requires(IsSet<S> || std::ranges::range<S>) && (!std::same_as<S, Ø>) &&
            requires(const S& s) {
              { s.size() } -> std::convertible_to<std::size_t>;
            }
  constexpr bool operator==(const S& s) const {
    return 0 == s.size();
  }

  // (ii) a std::ranges-backed operand with no size(): empty iff begin == end.
  //      Taken @b by value, not by const ref: an unsized range is typically a
  //      lazy view (e.g. std::views::filter), whose begin() caches and so is
  //      non-const-iterable; a by-value copy is a mutable local we can advance
  //      (and short-circuit) at both run time and compile time.  Copying a view
  //      is cheap by construction; owning containers expose size() and route to
  //      (i) instead, so they never reach here.
  template <typename S>
    requires std::ranges::range<S> && (!std::same_as<S, Ø>) &&
             (!requires(const S& s) { s.size(); })
  constexpr bool operator==(S s) const {
    return std::ranges::begin(s) == std::ranges::end(s);
  }

  // The Duality: !∅ = V
  // Forward declaration to satisfy the compiler for the UniversalSet.
  constexpr auto operator!() const;

  // The Axiom: Total Absence
  constexpr typename L::Ω operator()(const T&) const { return L::False; }

  // Required by IsInitialObject
  constexpr cardinality_type cardinality() const { return cardinality_type{}; }
  constexpr std::size_t upper_bound() const { return 0; }

  // Set-shape gate for the lattice operators below: @c IsSet<S>.  The
  // canonical carriers (@c UniversalSet, @c SingletonSet, @c Set) all
  // satisfy @c IsSet structurally post-#625, so the lattice ops accept
  // anything that does.

  // Ø | S and Ø & S are the subobject-lattice join / meet.  They are no
  // longer hand-spelled here: the reducer's bounded law (⊥∨X=X, ⊥∧X=⊥)
  // supplants them via the free operators below (#865/#890, Phase 2), which
  // recognise Ø as the ⊥ of Sub(T) under subobject_order<L>.

  // Ø ^ S = S  (∅ △ S = S; #469)
  template <typename S>
    requires(IsSet<S>)
  constexpr auto operator^(const S& s) const {
    return s;
  }

  /** @brief Ø × S = Ø<pair<T, S::Domain>, Boole>.  Empty annihilates
   *         the cartesian product on the @b left.  Carrier widens to the pair
   *         type; the codomain is Boolean (an empty product is decided, #894).
   */
  template <typename S>
    requires(IsSet<S>)
  constexpr auto operator*(const S&) const {
    // Codomain leg (#894): an empty product is decided (χ ≡ ⊥), so it carries
    // the Boolean codomain.  Inlined here because this op is upstream of
    // finalize_combine / codomain_reduce_t; it always yields a boundary.
    return Ø<std::pair<T, typename S::Domain>, Boole>{};
  }
};

template <typename T, typename L>
inline const Ø<T, L> Ø<T, L>::χ{};

/** @brief S × Ø = Ø<pair<S::Domain, T2>, Boole>.  Empty annihilates
 * the cartesian product on the @b right.  Symmetric companion to
 *         @c Ø::operator*; carrier widens to the pair type, codomain Boolean
 *         (an empty product is decided, #894). */
export template <typename S, typename T2, typename L>
  requires(IsSet<S> && !std::same_as<S, Ø<typename S::Domain, L>>)
constexpr auto operator*(const S&, const Ø<T2, L>&) {
  // Codomain leg (#894): an empty product is decided, so Boolean codomain.
  return Ø<std::pair<typename S::Domain, T2>, Boole>{};
}

/**
 * @struct UniversalSet
 * @brief U: The Terminal Object.
 * @details Intentional but Decidable: The rule "x ∈ U" always returns True.
 *
 * Per #551 (one-transaction redesign of the set-builder DSL): the @b type
 * is named @c UniversalSet<T, L, C>; the value-level handle is the
 * sibling variable template @c 𝔸<T, L, C> (declared further below in this
 * partition) which spells @c UniversalSet<T, L, C>{}.  Callers therefore
 * spell @c 𝔸<bool> at value-context sites rather than reaching for
 * @c UniversalSet<bool>{}; the type and the variable template share their
 * parameter pack so both names remain reachable at the same arity.  This
 * makes the topos-theoretic reading direct ( @c 𝔸 is the universal
 * (top) set over carrier @c T; the subobject classifier is @c L::Ω),
 * and lets paper Listing 6 read as
 * @c auto @c 𝔹 @c = @c 𝔸<bool>; without the type/value schism the
 * pre-#551 surface had.
 */
export template <typename T, typename L = Boole, typename C = ℵ_0>
struct UniversalSet final {
  // ~ arrow / morphism / subobject classifier jargon
  using Domain = T;
  using Codomain = typename L::Ω;

  // ~ topoi jargon;
  /** @brief Member-shape mirror of Subobject's: every element of T
   *  is a member of U via the always-True classifier; the wrapper
   *  carries the T-value the IsSubobject contract reads back through ι. */
  struct Member {
    T value;
  };

  /** @brief ι: U ↣ T — the canonical identity inclusion.
   *  Every member of the universal set is by construction an element
   *  of the ambient T; ι unwraps the Member's T-value. */
  constexpr T ι(const Member& m) const { return m.value; }

  static const UniversalSet χ;

  using cardinality_type = C;
  using base_set_type = UniversalSet<T, L, C>;
  using is_universal_boundary = void;
  using logic_species = L;

  /** @brief @c UniversalSet is the @b terminal object of the subobject poset
   *  @c Sub(U): the top @c ⊤, classified by the always-true predicate
   *  @c χ_U @c ≡ @c ⊤, with the unique arrow @c S @c → @c U from every
   *  subobject.  Dual to @c Ø's initiality tag; opts @c U into @c :limit's
   *  @c IsTerminalObject (the tag-discovery branch @c :lattice::LatticeTop
   *  already uses).  #881. */
  using is_terminal_object_tag = void;
  // ⊤ = X is clopen in every topology; @c topology::IsOpen / @c IsClosed INFER
  // that from the terminality tag above --- no separate open/closed tag (#903).

  /** @section boundaries__Algebraic_Axioms_2 */
  template <typename Op>
  static constexpr bool is_associative_v =
      std::is_same_v<Op, std::bit_and<base_set_type>> ||
      std::is_same_v<Op, std::bit_or<base_set_type>>;

  template <typename Op>
  static constexpr bool is_idempotent_v =
      std::is_same_v<Op, std::bit_and<base_set_type>> ||
      std::is_same_v<Op, std::bit_or<base_set_type>>;

  constexpr auto operator!() const { return Ø<T, L>{}; }

  /**
   * @section boundaries__Lattice_Axiom_2
   * Everything is a part of the Universal Set.
   * Constraint: Exclude Variables (which have a member T) to let
   * symbolic expressions handle their own comparisons.
   */
  template <typename S>
    requires(!requires { typename S::T; }) &&
            (!requires { typename S::is_variable; })
  friend constexpr typename L::Ω operator<=(const S&, const UniversalSet&) {
    return L::True;
  }

  /** @section boundaries__Lattice_Axiom_3: Reflexivity */
  constexpr typename L::Ω operator<=(const UniversalSet&) const {
    return L::True;
  }

  // Explicitly define equality if <=> is being deleted by members
  constexpr bool operator==(const UniversalSet&) const { return true; }

  // Cross-(L, C) identity: the universe of a carrier T is the universe
  // regardless of logic species or cardinality annotation.  Enables
  // `𝔸<T> == r` when a complement-pair join elevates r to a UniversalSet<T,
  // L2, C2> whose C differs from the reference (e.g. bool's Finite vs the
  // ℵ_0 default), the same spirit as Ø's cross-carrier equality above.
  template <typename L2, typename C2>
    requires(!std::same_as<L2, L> || !std::same_as<C2, C>)
  constexpr bool operator==(const UniversalSet<T, L2, C2>&) const {
    return true;
  }

  // The Axiom: Total Presence
  constexpr typename L::Ω operator()(const T&) const { return L::True; }

  // Value-level membership query (sugar over operator()) per #551.
  // @c UniversalSet<T>.contains(v) reads more directly than @c
  // UniversalSet<T>(v) at paper-listing sites.  Returns @c L::Ω (delegating
  // to @c operator()) so the contract matches @c sets::Set::contains and
  // generic code can call either uniformly.
  constexpr typename L::Ω contains(const T& v) const { return (*this)(v); }

  constexpr cardinality_type cardinality() const { return cardinality_type{}; }

  // U | S = U and U & S = S are the subobject-lattice join / meet.  They are
  // no longer hand-spelled here: the reducer's bounded law (⊤∨X=⊤, ⊤∧X=X)
  // supplants them via the free operators below (#865/#890, Phase 2), which
  // recognise 𝔸 as the ⊤ of Sub(T) under subobject_order<L>.  (The unbound
  // predicate fragment that the old IsSubobject-gated U|S deliberately did NOT
  // capture is not IsSet, so the IsSet-gated free operator| leaves it to bind
  // via its own operator|, as before.)

  // U ^ S = ¬S  (U △ S = ¬S; #469)
  // Pointwise: x ∈ U △ S iff x is in exactly one; x is always in U,
  // so x ∈ U △ S iff x ∉ S, i.e. the complement of S.
  template <typename S>
  constexpr auto operator^(const S& s) const {
    return !s;
  }
};

template <typename T, typename L, typename C>
inline const UniversalSet<T, L, C> UniversalSet<T, L, C>::χ{};

/** @brief The universal (top) set over carrier @c T --- the value-level handle
 *         (per #551).  This is the universe, not the subobject classifier;
 *         the classifier is @c L::Ω.
 *
 *  Variable template producing a default-constructed @c UniversalSet<T,L,C>
 *  instance.  Lets callers spell the ambient as @c 𝔸<bool> rather than
 *  @c UniversalSet<bool>{} — paper Listing 6 reads as @c auto @c 𝔹 @c =
 *  @c 𝔸<bool>; without the type-vs-value schism the pre-#551 surface had.
 */
export template <typename T, typename L = Boole, typename C = ℵ_0>
inline constexpr UniversalSet<T, L, C> 𝔸{};

/** @brief @c 𝔸<bool> specialisation: the Boolean carrier is finite,
 *  so its universal predicate is classified by @c Finite cardinality
 *  (not @c ℵ_0).  Without this specialisation, @c NaturalLogic<𝔸<bool>>
 *  would route through @c Kleene (because @c ℵ_0 is transfinite);
 *  the canonical 𝔹 ambient wants @c Boole.  Mirrors the
 *  pre-#551 @c BooleanSetOf<L,C> default of @c BooleanSetOf<
 *  Boole, Finite>.
 */
export template <>
inline constexpr UniversalSet<bool, Boole, Finite> 𝔸<bool>{};

/** @brief The subset (⊆) order on the subobject lattice @c Sub(T), keyed by the
 *  logic species @c L.  This is the @b injected order (@c Ord) under which the
 *  generic lattice-law term reducer (@c category:lattice_term, #865/#890)
 *  normalises set expressions: @c Ø is its bottom (⊥, initial), @c 𝔸 its top
 *  (⊤, terminal).  Keying by @c L lets the algebra markers distinguish
 *  @c Boole (a @b Boolean subobject lattice, every law) from
 *  @c Kleene (Heyting / De Morgan, no complement collapse). */
export template <typename L = Boole>
struct subobject_order {};

/** @brief The lattice-law term reducer localised to the subobject lattice
 *  @c Sub(T): normalise @c Term under @c subobject_order<L> as @b both the
 *  semantic order (@c Ord, boundedness / distributivity) and the
 *  canonicalisation order (@c Less).  The set-level @b meet and @b join routes
 *  fold through this one alias (the boundary operators below, and the free
 *  @c operator& / @c operator| over @c IsSubobject in @c :expressions), so the
 *  injected-order policy for @c Sub(T) is named in a single place (#865/#890,
 *  Phase 2).  @b Complement does @b not route here: the free @c operator! in
 *  @c :expressions is a certified involution that eliminates double negation
 * via the @c :involution witness (@c !!A ≡ A). */
export template <typename Term, typename L = Boole,
                 typename Combine = no_leaf_combine>
using subobject_reduce_t =
    reduce_t<Term, subobject_order<L>, subobject_order<L>, Combine>;

/** @brief The @b codomain leg of the two-axis reduce (#894).
 *
 *  @details @c subobject_reduce_t reduces the @b domain term (the @c Sub(T)
 *  lattice) at a fixed codomain @c L.  This reduces the @b codomain: a decided
 *  boundary factors through the Rosolini dominance @f$\Sigma = \{\top,\bot\}@f$
 *  (its @f$\chi@f$ is the constant @f$\bot@f$ / @f$\top@f$, valued in @c Σ
 *  whatever the ambient), so a normal form that @b is a boundary carries the
 *  Boolean codomain @c Boole, not the (possibly Kleene) ambient it was
 *  reduced under.  This is what makes "a structural reduction to @c Ø / @c 𝔸
 *  restores decidability" (see @c :computability header) actually hold: the
 *  collapsed boundary reads @c HasDecidableMembership.
 *
 *  The single rule for now is @c boundary @c → @c Boole; identity elsewhere.
 *  FIXME(#894): the general rule is @c image(χ) @c ⊆ @c Σ folded @b alongside
 *  the domain reduce (the pointwise Kleene image lattice), of which this is the
 *  base case.  Singleton is deliberately excluded: its decidability needs a
 *  decidable @c == on the carrier (fails on @c ℝ), so it rides the carrier-axis
 *  resolver, not this rule. */
template <typename R>
struct codomain_reduce {
  using type = R;
};
template <typename T, typename L>
struct codomain_reduce<Ø<T, L>> {
  using type = Ø<T, Boole>;
};
template <typename T, typename L, typename C>
struct codomain_reduce<UniversalSet<T, L, C>> {
  using type = UniversalSet<T, Boole, C>;
};
export template <typename R>
using codomain_reduce_t = typename codomain_reduce<R>::type;

/** @brief The @b value-level twin of @c codomain_reduce_t: finalize a reduced
 *  normal-form value along the codomain leg (#894).  When the reduced type is a
 *  @c IsBoundaryObject whose codomain re-tag actually changes the type, the
 *  result is a decided boundary, so construct the re-tagged (@c Boole)
 * boundary; otherwise (an identity re-tag, or a non-boundary) the value passes
 * through unchanged, so a tagged non-default-constructible survivor keeps its
 * value. This is the single home for the value-finalization law: this
 *  partition's Ø / 𝔸 meet & join operators call it directly (they are declared
 *  below it), and @c :expressions (downstream) reuses it for the general
 *  subobject combine, so the rule is spelled @b once rather than duplicated.
 *  FIXME(#894): @c boundary @c → @c Boole is the only rule for now; the general
 *  form is @c image(χ) @c ⊆ @c Σ folded on the Kleene image lattice. */
export template <typename R>
constexpr auto finalize_combine(R r) {
  if constexpr (IsBoundaryObject<R>) {
    // Re-tag the boundary's codomain to Boole, but ONLY when that actually
    // changes the type.  @c IsBoundaryObject is tag-based and does NOT require
    // default-constructibility (@c category:limit), so a tagged, non-default-
    // constructible @c IsSet that survives a unit reduction (@c Ø|s = s) must
    // keep its value: when @c codomain_reduce_t<R> is the identity, preserve
    // @c r rather than default-constructing.
    if constexpr (!std::same_as<codomain_reduce_t<R>, R>)
      return codomain_reduce_t<R>{};
    else
      return r;
  } else {
    return r;
  }
}

template <typename T, typename L>
constexpr auto Ø<T, L>::operator!() const {
  // FIXME(#894): a boundary complement should carry the Boolean codomain (!Ø is
  // the decided universe), but `!Ø` resolves to the greedy free operator! (a
  // Morphism), not this member, so the codomain leg cannot land here yet.
  return UniversalSet<T, L>{};
}

/** @section boundaries__Engine_Routed_Lattice_Ops
 *
 *  The boundary meet / join are no longer hand-spelled inside @c Ø and
 *  @c UniversalSet.  They route through the generic lattice-law term reducer
 *  (@c category:lattice_term, #865/#890): the term @c Meet<boundary,S> /
 *  @c Join<boundary,S> is reduced under @c subobject_order<L>, where @c Ø is
 *  the ⊥ (initial) and @c 𝔸 the ⊤ (terminal) of @c Sub(T).  The reducer's
 *  bounded law then supplies the four identities the members used to spell by
 *  hand (⊥∧X=⊥, ⊥∨X=X, ⊤∧X=X, ⊤∨X=⊤).
 *
 *  The four Ø / 𝔸 meet & join operators build the @c Meet / @c Join node
 *  @b value and hand it to the value-first @ref subobject_reduce below. */

/** @brief Value-first domain reduce (#922): reduce a subobject lattice @c Node
 *  @b value (a @c Meet / @c Join / @c Not over @c Sub(T), or a leaf) to its
 *  normal-form @b value under @c subobject_order<L>, then finalize the codomain
 *  leg.  The value twin of @c subobject_reduce_t: it runs the same laws but
 *  returns a value, so it works at runtime and preserves a runtime-stateful
 *  operand (e.g.\ a @c SingletonSet holding an extensional value) where the
 *  normal form is that operand.  The four @c Ø / @c 𝔸 meet & join operators
 *  route through here (boundary operands are stateless); this is also the
 *  general entry the #916 Python composition surface will call once the
 *  leaf-combine leg is threaded (slice 2). */

// FIXME(#922 slice 2): constrain @c Node to a valid recursive subobject
// expression.  The leaves must be @c IsSubobject, NOT @c IsSet: a bare
// @c Halfspace / static @c Singleton is @c IsSubobject but not @c IsSet, so an
// @c IsSet leaf gate is too strong (rejects valid @c Sub(T) expressions); and
// the recursion must enforce carrier @b consistency (reject a mixed-carrier
// @c Meet<Ø<int>,Ø<bool>>).  That belongs with the leaf-combine leg (slice 2),
// not a bare @c IsSet check.  Until then this entry is unconstrained.
export template <typename L = Boole, typename Node>
constexpr auto subobject_reduce(const Node& node) {
  return finalize_combine(
      reduce_value<subobject_order<L>, subobject_order<L>, no_leaf_combine>(
          node));
}

/** @brief @c Ø @c & @c S / @c Ø @c | @c S: @c Ø is the ⊥ of @c Sub(T)
 *  meeting / joining any set.  Free operators (the Ø-LHS members were retired);
 *  overload resolution pins them by the @c Ø operand. */
export template <typename T, typename L, typename S>
  requires(IsSet<S> && std::same_as<typename S::Domain, T>)
constexpr auto operator&(const Ø<T, L>&, const S& s) {
  // Codomain leg (#894): wrap the domain normal form so a boundary result is
  // re-tagged to Boole, matching the S-LHS path (S & Ø); otherwise the codomain
  // would be order-dependent.
  return subobject_reduce<L>(Meet<Ø<T, L>, S>{Ø<T, L>{}, s});
}
/** @brief @c Ø @c | @c S = @c S (⊥ is the join unit); see @c operator&. */
export template <typename T, typename L, typename S>
  requires(IsSet<S> && std::same_as<typename S::Domain, T>)
constexpr auto operator|(const Ø<T, L>&, const S& s) {
  return subobject_reduce<L>(Join<Ø<T, L>, S>{Ø<T, L>{}, s});
}

/** @brief @c 𝔸 @c & @c S / @c 𝔸 @c | @c S: @c 𝔸 is the ⊤ of @c Sub(T)
 *  meeting / joining any set.  Free operators (the UniversalSet-LHS members
 * were retired); pinned by the @c UniversalSet operand. */
export template <typename T, typename L, typename C, typename S>
  requires(IsSet<S> && std::same_as<typename S::Domain, T>)
constexpr auto operator&(const UniversalSet<T, L, C>&, const S& s) {
  return subobject_reduce<L>(
      Meet<UniversalSet<T, L, C>, S>{UniversalSet<T, L, C>{}, s});
}
/** @brief @c 𝔸 @c | @c S = @c 𝔸 (⊤ is the join annihilator); see @c operator&.
 */
export template <typename T, typename L, typename C, typename S>
  requires(IsSet<S> && std::same_as<typename S::Domain, T>)
constexpr auto operator|(const UniversalSet<T, L, C>&, const S& s) {
  return subobject_reduce<L>(
      Join<UniversalSet<T, L, C>, S>{UniversalSet<T, L, C>{}, s});
}

// Cardinality metadata drives extensional classification for UniversalSet.
template <typename T, typename L, typename C>
struct is_extensional<UniversalSet<T, L, C>>
    : std::bool_constant<C::is_finite> {};

static_assert(IsSet<decltype(UniversalSet<int>{})>,
              "The universal boundary must lift to an ETCS set object.");

static_assert(IsSet<decltype(ambient_set<int>(UniversalSet<int>{}))>,
              "The universal boundary must lift to an ETCS set object.");

static_assert(IsSet<decltype(Ø<int>{})>,
              "The empty boundary must lift to an ETCS set object.");

static_assert(IsSet<decltype(ambient_set<int>(Ø<int>{}))>,
              "The empty boundary must lift to an ETCS set object.");

// =============================================================
// Architecture note: universe 𝔸<T> vs. classifier <Tower>Of<>
// =============================================================
//
// Two distinct primitives sit at this layer, both rooted in ETCS
// (Lawvere 1964):
//
//   (1) Universe per carrier — @c 𝔸<T> (variable template above)
//       = @c UniversalSet<T,L,C>{}.  Constant-True predicate over
//       carrier T.  Plays the role of "T as its own set" — the
//       monomorphic identity inclusion T ↪ T.  Used by the
//       set-builder DSL as the ambient for @c element<𝔸<T>>
//       (BoundScout factory, post-#551).
//
//   (2) Tower classifier — @c <Tower>Of<L,C> (this and sibling
//       struct templates: @c NaturalNumbersOf, @c IntegersOf in
//       :integer, @c RationalsOf in :rational, @c RealsOf in :real,
//       @c ComplexesOf in :complex, @c DualSetOf in :dual).
//       The characteristic morphism χ_T : tower-ambient → Ω of
//       the subobject T inside its algebraic tower.  Multi-overload:
//       the @c Domain overload always returns @c L::True (T is in
//       T), and the cross-carrier overloads route predecessor
//       types through embedding arrows (e.g.\ @c N(int) checks
//       non-negativity, @c N(unsigned) is trivially True via the
//       canonical embedding @c embed_uint_ℕ_).  This is the
//       textbook "ℕ as a subset of ℤ via the canonical inclusion"
//       reading.
//
//   Asymmetry: @c BooleanSetOf<L,C> ≡ @c UniversalSet<bool,L,C>
//       (alias, not a separate struct) because 𝔹 is the @b bottom
//       of the algebraic tower — no proper super-object — so χ_𝔹
//       collapses to 𝔸<bool>.  See @c algebra:boolean for that
//       collapse note.
//
// Why both: @c 𝔸<T> is the structural primitive (one per carrier;
// uniform DSL surface for set-builder), while @c <Tower>Of<> is
// the engineering pragma that lifts predecessor literals (@c N(0u)
// for @c unsigned, @c N(-7) for @c int) without forcing each
// callsite to thread the embedding manually.  Removing the
// classifiers in favour of 𝔸 alone would lose the cross-carrier
// classification — @c 𝔸<unsigned>{}(-7) is ill-typed, but
// @c N(-7) is well-typed and returns @c False.
//
// Paper alignment: §3.3 (Juliet Posture) names the two-axis split
// (closure / laws); the universe-vs-classifier distinction is a
// third meta-axis (§5 figure breadcrumb).  Listing 6 in the paper
// shows both: @c 𝔹 = @c 𝔸<bool> for the trivial-bottom case;
// @c N = @c NaturalNumbersOf<>{} for the non-trivial classifier
// case.
export template <typename L = Boole, typename C = ℵ_0>
struct NaturalNumbersOf {
  using Domain = dedekind::sets::Cardinality;  // Aligned to the @c Cardinality
                                               // carrier post-#402.
  using Codomain = typename L::Ω;
  using logic_species = L;
  using cardinality_type = C;

  // Canonical signature: every Cardinality value is in ℕ (by definition).
  constexpr typename L::Ω operator()(const Domain&) const { return L::True; }

  // Classifier convenience: every unsigned-integral value lands in ℕ
  // (it embeds via @c ExtensionalCardinal<>{u} into @c Cardinality's
  // finite alternative).  Kept as a separate overload so callsites
  // that pass @c unsigned literals still resolve directly without
  // forcing the variant lift at every call site.
  template <std::unsigned_integral U>
  constexpr typename L::Ω operator()(U) const {
    return L::True;
  }

  // Classifier convenience: ℕ ⊂ ℤ via non-negativity.  Reachable via
  // direct @c N(-7) calls for paper-listing readability.
  constexpr typename L::Ω operator()(int x) const {
    return x >= 0 ? L::True : L::False;
  }

  // Embedded bool (via @c embed_𝔹_uint_): landing in ℕ.
  constexpr typename L::Ω operator()(bool) const { return L::True; }
};

// Non-exported convenience alias used by the value-level @c N constant
// below.  Public surface is @c NaturalNumbersOf<L, C> (the parameterised
// template); callers naming the default form should use
// @c NaturalNumbersOf<> directly or @c decltype(N).  Mirrors the
// @c BooleanSet de-export pattern from #407.
using NaturalNumbers = NaturalNumbersOf<>;

/** @brief The canonical Natural-numbers universe @c ℕ = @c 𝔸<Cardinality>
 *  (post-#559).
 *
 *  @details Per #559's chosen direction (option A): the named species
 *  symbols (@c 𝔹 / @c ℕ / @c ℤ / @c ℚ / @c ℝ / @c ℂ / @c 𝔻) denote the
 *  @b universe values (constexpr instances of @c UniversalSet over the
 *  carrier), not carrier @b types.  Carrier types are spelled directly
 *  (@c bool, @c Cardinality, @c SignedExtensionalCardinal<>, ...) in
 *  template-type-parameter positions; the math symbols denote the sets.
 *
 *  This makes @c element<ℕ> the canonical scout spelling for the
 *  natural-numbers universe — closer to textbook math notation than the
 *  pre-#559 @c element<𝔸<ℕ>> form (which required @c ℕ to be a type
 *  alias for @c Cardinality).
 *
 *  Pre-#559 the spelling was @c using @c ℕ @c = @c
 *  dedekind::sets::Cardinality (carrier-type alias, post-#402); the
 *  ~110 type-context sites of @c ℕ in concept gates and static_asserts
 *  were migrated to @c Cardinality directly in step 1 of the ℕ slice.
 *
 *  Carrier reading retained: @c Cardinality is the variant ℕ-proxy
 *  (= @c std::variant<ExtensionalCardinal<>, ℵ_0>) — saturating to
 *  @c ℵ_0 on overflow rather than wrapping the way @c unsigned @c int
 *  would.  The structural advantage: the variant honestly models ℕ
 *  (no additive inverses; rig-not-ring), whereas @c unsigned @c int
 *  is structurally @b more than ℕ (modular ring with additive
 *  inverses).  Witnesses (@c IsRig, @c IsCommutativeMonoid,
 *  @c IsTotallyOrdered, @c IsDirectedSet, @c IsDirectedPoset, ...)
 *  are now expressed against @c Cardinality directly.
 */
export inline constexpr auto ℕ = 𝔸<Cardinality>;

/** @brief @c 𝔹 --- the Boolean carrier as a value-tag, @c 𝔸<bool>, the
 *  finite universe @f$\{\mathtt{false},\mathtt{true}\}@f$.  Companion to @c ℕ
 *  for the point-free set-builder surface @c 𝔹 @c | @c π @c == @c fix(true_c).
 */
export inline constexpr auto 𝔹 = 𝔸<bool>;

// Canonical ambient-set value used by the sets DSL tests.
export inline constexpr NaturalNumbersOf<> N{};

/**
 * @brief ETCS-aligned upper bound for meet/intersection cardinality.
 * @details For extensional sets with explicit `upper_bound()`, this is
 *          `min(bound(A), bound(B))`. If exactly one operand provides an
 *          explicit finite bound, that bound is still a valid upper bound
 *          for the meet. For fully intensional/transfinite pairs where
 *          neither side exposes `upper_bound()`, the function returns the
 *          maximal finite sentinel.
 */
export template <typename S1, typename S2>
constexpr std::size_t bound_meet(const S1& lhs, const S2& rhs) {
  constexpr bool lhs_has_upper_bound = requires {
    { lhs.upper_bound() } -> std::convertible_to<std::size_t>;
  };
  constexpr bool rhs_has_upper_bound = requires {
    { rhs.upper_bound() } -> std::convertible_to<std::size_t>;
  };

  if constexpr (lhs_has_upper_bound && rhs_has_upper_bound) {
    return std::min(static_cast<std::size_t>(lhs.upper_bound()),
                    static_cast<std::size_t>(rhs.upper_bound()));
  } else if constexpr (lhs_has_upper_bound) {
    return static_cast<std::size_t>(lhs.upper_bound());
  } else if constexpr (rhs_has_upper_bound) {
    return static_cast<std::size_t>(rhs.upper_bound());
  } else {
    return std::numeric_limits<std::size_t>::max();
  }
}

/**
 * @brief ETCS-aligned upper bound for join/union cardinality.
 * @details For extensional sets with explicit `upper_bound()`, this is the
 *          saturating sum `bound(A) + bound(B)`. For intensional/transfinite
 *          species, the function returns the maximal finite sentinel.
 */
export template <typename S1, typename S2>
constexpr std::size_t bound_join(const S1& lhs, const S2& rhs) {
  if constexpr (requires {
                  { lhs.upper_bound() } -> std::convertible_to<std::size_t>;
                  { rhs.upper_bound() } -> std::convertible_to<std::size_t>;
                }) {
    const std::size_t a = static_cast<std::size_t>(lhs.upper_bound());
    const std::size_t b = static_cast<std::size_t>(rhs.upper_bound());
    const std::size_t max_v = std::numeric_limits<std::size_t>::max();
    return (a > max_v - b) ? max_v : a + b;
  } else {
    return std::numeric_limits<std::size_t>::max();
  }
}

};  // namespace dedekind::sets

namespace dedekind::category {

// Cardinality metadata drives transfinite classification for UniversalSet.
template <typename T, typename L, typename C>
struct is_transfinite<dedekind::sets::UniversalSet<T, L, C>>
    : std::bool_constant<!C::is_finite> {};

// ── Term-reducer boundary hookup (#865/#890, Phase 2) ──────────────────────
// Ø is the ⊥ (initial) and 𝔸 the ⊤ (terminal) of the subobject lattice Sub(T)
// under the injected order dedekind::sets::subobject_order<L>, so the reducer's
// bounded law (⊥∧X=⊥, ⊤∨X=⊤, ⊤∧X=X, ⊥∨X=X) recognises them.  The hand-written
// left-biased Ø / 𝔸 operator& / operator| members have now been retired: the
// free engine-routed operators in the sets namespace above delegate to reduce<>
// and materialise the normal form back to a value.
template <typename T, typename L>
struct is_lattice_bottom_for<dedekind::sets::Ø<T, L>,
                             dedekind::sets::subobject_order<L>>
    : std::true_type {};
template <typename T, typename L, typename C>
struct is_lattice_top_for<dedekind::sets::UniversalSet<T, L, C>,
                          dedekind::sets::subobject_order<L>> : std::true_type {
};

// ── Distributivity of the subobject lattice (#865) ─────────────────────────
/** @brief The subobject lattice @c Sub(T) is a @b distributive lattice under
 *  @c subobject_order<L>, for every carrier @c T and classifier @c L.  In a
 *  topos @c Sub(T) is a Heyting algebra (meet distributes over join), and the
 *  classical (@c Boole) case is the Boolean specialisation of that.  The DNF
 *  rewrite, however, acts on the @b pointwise @c L::AND / @c L::OR of the
 *  predicates, so its soundness needs @b those operations to distribute, which
 *  the @c IsOckhamAlgebra shape gate does @b not certify.  So the marker is
 *  gated on a @b species-level @b distributivity @b certificate: @c
 *  IsBoundedDeMorganChain<L>.  A bounded De Morgan chain is (by
 * chain-normality) a distributive lattice, so its @c L::AND / @c L::OR
 * distribute; every shipped species (@c Boole, @c Kleene, @c Chain<T>, @c
 * Percent) satisfies it, while an unconstrained / non-distributive custom @c L
 * is @b excluded (fail-closed). When gated, it licenses the reducer's @c
 * meet_distributivity_law to rewrite a
 *  @b genuinely non-collapsing @c Sub(T) meet-over-join into disjunctive normal
 *  form (@c X∧(P∨Q)→(X∧P)∨(X∧Q)).
 *  FIXME(#907/#923): replace @c IsBoundedDeMorganChain with a dedicated
 *  species-level distributivity certificate (via the @c :species-trait bridge)
 *  so non-chain distributive logics can also opt in.
 *
 *  @note This gate fires only for a @b bare @c category::Join reducer node; the
 *  value-level set operators materialise an irreducible union as a @c JoinSet
 *  (a distinct type the reducer treats as an opaque leaf), so this marker does
 *  @b not change the value-level normal form (@c A∩(B∪C) still materialises as
 *  a @c MeetSet carrying its operands, per #892).  Driving the DNF rewrite
 *  through the value path is a separate normal-form decision, deferred (#865).
 */
template <typename T, typename L>
  requires IsBoundedDeMorganChain<L>
inline constexpr bool
    is_distributive_lattice_for_v<T, dedekind::sets::subobject_order<L>> = true;

// ── Complementedness of the subobject lattice (#834/#829) ──────────────────
// Sub(T) inherits its complement structure POINTWISE from the codomain Ω, so
// the reducer's complement laws (which gate on codomain_of, not the domain
// carrier) are registered against Ω under subobject_order<L>: Sub(T) is
// COMPLEMENTED (a∧¬a→⊥, a∨¬a→⊤ collapse) iff Ω is Boolean.  Boole's Ω = bool
// (the 2-chain) is; Kleene's Ω = Ternary (the 3-chain, ¬U=U) is NOT, so that
// codomain is deliberately left unregistered and a Kleene a∧¬a stays
// un-collapsed.  Partial spec over L (mirroring the distributivity marker
// above) so it is reachable from the reducer's instantiation point.
template <typename L>
  requires std::same_as<L, Boole>
inline constexpr bool
    is_complemented_lattice_for_v<bool, dedekind::sets::subobject_order<L>> =
        true;

// Foot-in-the-door witness: the engine now sees Ø as the ⊥ and 𝔸 as the ⊤ of
// Sub(T) under subobject_order, so its bounded law reduces boundary meets/joins
// (the annihilator / unit laws the hand-written Ø / 𝔸 operators currently
// spell by hand (retired next).
static_assert(
    std::same_as<
        dedekind::sets::subobject_reduce_t<
            Meet<dedekind::sets::Ø<int>, dedekind::sets::UniversalSet<int>>>,
        dedekind::sets::Ø<int>>,
    "Ø ∧ 𝔸 → Ø (Ø recognised as the subobject-lattice ⊥).");
static_assert(
    std::same_as<
        dedekind::sets::subobject_reduce_t<
            Join<dedekind::sets::Ø<int>, dedekind::sets::UniversalSet<int>>>,
        dedekind::sets::UniversalSet<int>>,
    "Ø ∨ 𝔸 → 𝔸 (𝔸 recognised as the subobject-lattice ⊤).");

}  // namespace dedekind::category
