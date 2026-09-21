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
export template <typename T = std::nullptr_t, typename L = ClassicalLogic>
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
   * empty set. Enables writing `Ø<int>` (L defaults to ClassicalLogic) even
   * when the RHS was produced by a Set whose NaturalLogic selected
   * TernaryLogic — mathematically ∅ = ∅ regardless of logic species.
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

  /** @brief Ø × S = Ø<pair<T, S::Domain>, L> — empty annihilates the
   *         cartesian product on the @b left.  Carrier widens to the
   *         pair type so the result is type-correct as a set of pairs. */
  template <typename S>
    requires(IsSet<S>)
  constexpr auto operator*(const S&) const {
    return Ø<std::pair<T, typename S::Domain>, L>{};
  }
};

template <typename T, typename L>
inline const Ø<T, L> Ø<T, L>::χ{};

/** @brief S × Ø = Ø<pair<S::Domain, T2>, L> — empty annihilates the
 *         cartesian product on the @b right.  Symmetric companion to
 *         @c Ø::operator*; carrier widens to the pair type. */
export template <typename S, typename T2, typename L>
  requires(IsSet<S> && !std::same_as<S, Ø<typename S::Domain, L>>)
constexpr auto operator*(const S&, const Ø<T2, L>&) {
  return Ø<std::pair<typename S::Domain, T2>, L>{};
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
export template <typename T, typename L = ClassicalLogic, typename C = ℵ_0>
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
export template <typename T, typename L = ClassicalLogic, typename C = ℵ_0>
inline constexpr UniversalSet<T, L, C> 𝔸{};

/** @brief @c 𝔸<bool> specialisation: the Boolean carrier is finite,
 *  so its universal predicate is classified by @c Finite cardinality
 *  (not @c ℵ_0).  Without this specialisation, @c NaturalLogic<𝔸<bool>>
 *  would route through @c TernaryLogic (because @c ℵ_0 is transfinite);
 *  the canonical 𝔹 ambient wants @c ClassicalLogic.  Mirrors the
 *  pre-#551 @c BooleanSetOf<L,C> default of @c BooleanSetOf<
 *  ClassicalLogic, Finite>.
 */
export template <>
inline constexpr UniversalSet<bool, ClassicalLogic, Finite> 𝔸<bool>{};

/** @brief The subset (⊆) order on the subobject lattice @c Sub(T), keyed by the
 *  logic species @c L.  This is the @b injected order (@c Ord) under which the
 *  generic lattice-law term reducer (@c category:lattice_term, #865/#890)
 *  normalises set expressions: @c Ø is its bottom (⊥, initial), @c 𝔸 its top
 *  (⊤, terminal).  Keying by @c L lets the algebra markers distinguish
 *  @c ClassicalLogic (a @b Boolean subobject lattice — every law) from
 *  @c TernaryLogic (Heyting / De Morgan, no complement collapse). */
export template <typename L = ClassicalLogic>
struct subobject_order {};

/** @brief The lattice-law term reducer localised to the subobject lattice
 *  @c Sub(T): normalise @c Term under @c subobject_order<L> as @b both the
 *  semantic order (@c Ord, boundedness / distributivity / complement) and the
 *  canonicalisation order (@c Less).  Every set-level meet / join / complement
 *  route (the boundary operators below, and @c Set::operator&/|/~ in
 *  @c :expressions) folds through this one alias, so the injected-order policy
 *  for @c Sub(T) is named in a single place (#865/#890, Phase 2). */
export template <typename Term, typename L = ClassicalLogic,
                 typename Combine = no_leaf_combine>
using subobject_reduce_t =
    reduce_t<Term, subobject_order<L>, subobject_order<L>, Combine>;

template <typename T, typename L>
constexpr auto Ø<T, L>::operator!() const {
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
 *  @c materialize_boundary: the reducer works on @b types, so the normal form
 *  is turned back into a value.  Either the term collapsed to a stateless
 *  boundary (@c Ø / @c UniversalSet — default-construct it), or the surviving
 *  operand is the normal form (return the operand value @c s).  These are the
 *  only two shapes a bounded-law collapse can produce for a boundary term. */
namespace detail_boundary {
template <typename R, typename S>
constexpr auto materialize(const S& s) {
  if constexpr (std::same_as<R, std::remove_cvref_t<S>>) {
    return s;  // the operand survived as the normal form (unit law)
  } else {
    return R{};  // the term collapsed to a stateless boundary (annihilator)
  }
}
}  // namespace detail_boundary

/** @brief @c Ø @c & @c S / @c Ø @c | @c S --- @c Ø is the ⊥ of @c Sub(T)
 *  meeting / joining any set.  Free operators (the Ø-LHS members were retired);
 *  overload resolution pins them by the @c Ø operand. */
export template <typename T, typename L, typename S>
  requires(IsSet<S>)
constexpr auto operator&(const Ø<T, L>&, const S& s) {
  return detail_boundary::materialize<subobject_reduce_t<Meet<Ø<T, L>, S>, L>>(
      s);
}
/** @brief @c Ø @c | @c S = @c S (⊥ is the join unit); see @c operator&. */
export template <typename T, typename L, typename S>
  requires(IsSet<S>)
constexpr auto operator|(const Ø<T, L>&, const S& s) {
  return detail_boundary::materialize<subobject_reduce_t<Join<Ø<T, L>, S>, L>>(
      s);
}

/** @brief @c 𝔸 @c & @c S / @c 𝔸 @c | @c S --- @c 𝔸 is the ⊤ of @c Sub(T)
 *  meeting / joining any set.  Free operators (the UniversalSet-LHS members
 * were retired); pinned by the @c UniversalSet operand. */
export template <typename T, typename L, typename C, typename S>
  requires(IsSet<S>)
constexpr auto operator&(const UniversalSet<T, L, C>&, const S& s) {
  return detail_boundary::materialize<
      subobject_reduce_t<Meet<UniversalSet<T, L, C>, S>, L>>(s);
}
/** @brief @c 𝔸 @c | @c S = @c 𝔸 (⊤ is the join annihilator); see @c operator&.
 */
export template <typename T, typename L, typename C, typename S>
  requires(IsSet<S>)
constexpr auto operator|(const UniversalSet<T, L, C>&, const S& s) {
  return detail_boundary::materialize<
      subobject_reduce_t<Join<UniversalSet<T, L, C>, S>, L>>(s);
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
export template <typename L = ClassicalLogic, typename C = ℵ_0>
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

// Foot-in-the-door witness: the engine now sees Ø as the ⊥ and 𝔸 as the ⊤ of
// Sub(T) under subobject_order, so its bounded law reduces boundary meets/joins
// (the annihilator / unit laws the hand-written Ø / 𝔸 operators currently
// spell by hand — retired next).
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
