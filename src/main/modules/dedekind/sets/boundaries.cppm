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
#include <functional>
#include <limits>

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

struct Boundaries {};

/** @section boundaries__Structural_Resolution
 *
 * @c resolve_species / @c element_of_t resolve a type to its underlying
 * element type: predicate-set carriers (anything with a nested
 * @c ::Domain) project to their domain, primitive types are their own
 * elements (the @b carrier reading after #399).  Lifted from
 * @c :family to @c :boundaries so the upstream @c Variable / @c var
 * machinery in @c :expressions can use the same trait without
 * duplicating the resolution logic.
 *
 * @section boundaries__Picking_Policy_Cross_Reference
 *
 * @c element_of_t is one of @b three Domain-resolving helpers in the
 * project — see @c :morphism's "Picking policy" comment block (post-
 * #411) for the canonical decision rule.  In short:
 *
 *   * @c Dom<F> / @c Cod<F> ( @c :morphism) — for @c IsArrow-strict
 *     contexts.
 *   * @c element_of_t<S> ( @b here) — for sites that must accept @b
 *     primitive carriers as well as predicate-set carriers.  This is
 *     the only helper with a primitive fallback; per-symbol carrier
 *     migrations (post-#401 / #402 etc.) prefer this on consumer sites
 *     to avoid the "primitive doesn't satisfy @c Species::Domain"
 *     cascade.
 *   * @c MorphicBridge<signature_extractor<F>::type>::Domain ( @c
 *     :morphism) — for typed-lambda contexts without nested @c ::Domain.
 *
 * Bare @c typename @c T::Domain is acceptable only at @b producer sites
 * (where @c using @c Domain @c = @c T; is the @b defining clause), or
 * inside a @c requires that already excludes primitives.
 */
export template <typename T>
struct resolve_species {
  using type = T;  // Fallback for primitives (int, bool, ...) — the carrier IS
                   // the element.
};

export template <typename T>
  requires requires { typename T::Domain; }
struct resolve_species<T> {
  using type = typename T::Domain;  // Extract from formal Species.
};

export template <typename T>
using element_of_t = typename resolve_species<T>::type;

/**
 * @brief Sentinel carrier for the parameter-free form `Ø{}`.
 *
 * Enables a deduction guide that lets listings write `Ø{}` (no type args)
 * and have it compare equal to any `Ø<T, L>`, regardless of carrier and
 * logic species. This is purely for paper-listing readability; a typed
 * empty set `Ø<int, TernaryLogic>{}` is still the underlying object of record.
 */
struct AnyDomain {};

/** @brief ∅: The Initial Object. Extensional (Size 0). */
export template <typename T = AnyDomain, typename L = ClassicalLogic>
struct Ø final : Boundaries {
  using Domain = T;
  using Codomain = typename L::Ω;
  using logic_species = L;
  using cardinality_type = Finite;
  using is_extensional_tag = void;
  using is_compile_time_extensional_tag = void;
  using base_set_type = Ø<T, L>;

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

  // Necessary for (a | b) == b where b might be Ø
  template <typename S>
  constexpr bool operator==(const S&) const {
    if constexpr (std::is_same_v<S, Ø>) return true;
    return false;  // A non-empty set cannot be equal to Ø
  }

  // The Duality: !∅ = V
  // Forward declaration to satisfy the compiler for the UniversalSet.
  constexpr auto operator!() const;

  // The Axiom: Total Absence
  constexpr typename L::Ω operator()(const T&) const { return L::False; }

  // Required by IsInitialObject
  constexpr cardinality_type cardinality() const { return cardinality_type{}; }
  constexpr std::size_t upper_bound() const { return 0; }

  // Ø | S = S
  // Note: blocked for the parameter-free form `Ø<AnyDomain>` (i.e. `Ø{}`) —
  // that form is a comparison-only tag; using it in a meet/join would
  // silently propagate `AnyDomain` as the result carrier, which is almost
  // never what the caller meant. Spell the carrier explicitly instead.
  template <typename S>
    requires(!std::same_as<T, AnyDomain>)
  constexpr auto operator|(const S& s) const {
    return s;
  }
  // Ø & S = Ø
  template <typename S>
    requires(!std::same_as<T, AnyDomain>)
  constexpr auto operator&(const S&) const {
    return *this;
  }
  // Ø ^ S = S  (∅ △ S = S; #469)
  // Same AnyDomain block as | / & above.
  template <typename S>
    requires(!std::same_as<T, AnyDomain>)
  constexpr auto operator^(const S& s) const {
    return s;
  }
};

/**
 * @brief Deduction guide: `Ø{}` (no template args) resolves to `Ø<>`.
 *
 * Enables paper-quality listings like `static_assert(s == Ø{});` without
 * forcing the reader to spell out carrier / logic species. The cross-type
 * `operator==` on `Ø` makes `Ø<AnyDomain, ClassicalLogic>` equal to any
 * other `Ø<T2, L2>`, so the comparison is semantically "is s the empty set?"
 */
Ø() -> Ø<>;

/**
 * @struct UniversalSet
 * @brief U: The Terminal Object.
 * @details Intentional but Decidable: The rule "x ∈ U" always returns True.
 *
 * Per #551 (one-transaction redesign of the set-builder DSL): the @b type
 * is named @c UniversalSet<T, L, C>; the value-level handle is the
 * variable template @c UniversalSet<T, L, C> below, so callers spell
 * @c UniversalSet<bool> rather than @c UniversalSet<bool>{}.  This makes the
 * topos-theoretic reading direct ( @c Ω is the subobject classifier
 * value at carrier @c T), and lets paper Listing 6 read as
 * @c auto @c 𝔹 @c = @c UniversalSet<bool>; without the type/value schism the
 * pre-#551 surface had.
 */
export template <typename T, typename L = ClassicalLogic, typename C = ℵ_0>
struct UniversalSet final : Boundaries {
  using Domain = T;
  using Codomain = typename L::Ω;
  using cardinality_type = C;
  using base_set_type = UniversalSet<T, L, C>;
  using is_universal_boundary = void;
  using logic_species = L;

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

  // Note: You'll eventually want overloads for:
  // Universal | Any = Universal
  // Universal & Any = Any

  // The Axiom: Total Presence
  constexpr typename L::Ω operator()(const T&) const { return L::True; }

  // Value-level membership query (sugar over operator()) per #551.
  // @c UniversalSet<T>.contains(v) reads more directly than @c
  // UniversalSet<T>(v) at paper-listing sites.
  constexpr bool contains(const T&) const { return true; }

  constexpr cardinality_type cardinality() const { return cardinality_type{}; }

  // U | S = U
  template <typename S>
  constexpr auto operator|(const S&) const {
    return *this;
  }

  // U & S = S
  template <typename S>
  constexpr auto operator&(const S& s) const {
    return s;
  }

  // U ^ S = ¬S  (U △ S = ¬S; #469)
  // Pointwise: x ∈ U △ S iff x is in exactly one; x is always in U,
  // so x ∈ U △ S iff x ∉ S, i.e. the complement of S.
  template <typename S>
  constexpr auto operator^(const S& s) const {
    return !s;
  }
};

/** @brief The universal-predicate value at carrier @c T (subobject-classifier
 *         reading per #551).
 *
 *  Variable template producing a default-constructed @c UniversalSet<T,L,C>
 *  instance.  Lets callers spell the ambient as @c UniversalSet<bool> rather
 * than
 *  @c UniversalSet<bool>{} — paper Listing 6 reads as @c auto @c 𝔹 @c =
 *  @c UniversalSet<bool>; without the type-vs-value schism the pre-#551 surface
 * had.
 */
export template <typename T, typename L = ClassicalLogic, typename C = ℵ_0>
inline constexpr UniversalSet<T, L, C> Ω{};

/** @brief @c Ω<bool> specialisation: the Boolean carrier is finite,
 *  so its universal predicate is classified by @c Finite cardinality
 *  (not @c ℵ_0).  Without this specialisation, @c NaturalLogic<Ω<bool>>
 *  would route through @c TernaryLogic (because @c ℵ_0 is transfinite);
 *  the canonical 𝔹 ambient wants @c ClassicalLogic.  Mirrors the
 *  pre-#551 @c BooleanSetOf<L,C> default of @c BooleanSetOf<
 *  ClassicalLogic, Finite>.
 */
export template <>
inline constexpr UniversalSet<bool, ClassicalLogic, Finite> Ω<bool>{};

template <typename T, typename L>
constexpr auto Ø<T, L>::operator!() const {
  return UniversalSet<T, L>{};
}

// Cardinality metadata drives extensional classification for UniversalSet.
template <typename T, typename L, typename C>
struct is_extensional<UniversalSet<T, L, C>>
    : std::bool_constant<C::is_finite> {};

static_assert(
    dedekind::category::IsSet<decltype(ambient_set<int>(UniversalSet<int>{}))>,
    "The universal boundary must lift to an ETCS set object.");
static_assert(dedekind::category::IsSet<decltype(ambient_set<int>(Ø<int>{}))>,
              "The empty boundary must lift to an ETCS set object.");
static_assert(dedekind::category::HasCanonicalSetCCC<int>,
              "Breadcrumb to :cartesian: boundary ambient int has canonical "
              "CCC witness.");

// =============================================================
// Architecture note: universe Ω<T> vs. classifier <Tower>Of<>
// =============================================================
//
// Two distinct primitives sit at this layer, both rooted in ETCS
// (Lawvere 1964):
//
//   (1) Universe per carrier — @c Ω<T> (variable template above)
//       = @c UniversalSet<T,L,C>{}.  Constant-True predicate over
//       carrier T.  Plays the role of "T as its own set" — the
//       monomorphic identity inclusion T ↪ T.  Used by the
//       set-builder DSL as the ambient for @c element<Ω<T>>
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
//       collapses to Ω<bool>.  See @c algebra:boolean for that
//       collapse note.
//
// Why both: @c Ω<T> is the structural primitive (one per carrier;
// uniform DSL surface for set-builder), while @c <Tower>Of<> is
// the engineering pragma that lifts predecessor literals (@c N(0u)
// for @c unsigned, @c N(-7) for @c int) without forcing each
// callsite to thread the embedding manually.  Removing the
// classifiers in favour of Ω alone would lose the cross-carrier
// classification — @c Ω<unsigned>{}(-7) is ill-typed, but
// @c N(-7) is well-typed and returns @c False.
//
// Paper alignment: §3.3 (Juliet Posture) names the two-axis split
// (closure / laws); the universe-vs-classifier distinction is a
// third meta-axis (§5 figure breadcrumb).  Listing 6 in the paper
// shows both: @c 𝔹 = @c Ω<bool> for the trivial-bottom case;
// @c N = @c NaturalNumbersOf<>{} for the non-trivial classifier
// case.
export template <typename L = ClassicalLogic, typename C = ℵ_0>
struct NaturalNumbersOf {
  using Domain =
      dedekind::sets::Cardinality;  // Aligned to the @c ℕ carrier post-#402.
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

/** @brief The canonical Natural-numbers carrier symbol @c ℕ = @c Cardinality.
 *
 *  @details Per #402 (math-wins-over-C++ retarget).  @c ℕ is now the
 *  variant ℕ-proxy carrier @c Cardinality (= @c
 *  std::variant<ExtensionalCardinal<>, ℵ_0>) — saturating to @c ℵ_0
 *  on overflow rather than wrapping the way @c unsigned @c int (the
 *  earlier post-#401 reading) did.  The structural advantage: the
 *  variant honestly models ℕ (no additive inverses; rig-not-ring),
 *  whereas @c unsigned @c int is structurally @b more than ℕ (modular
 *  ring with additive inverses, since @c 0 @c - @c 1 wraps to
 *  @c UINT_MAX).  Callers wanting the bounded machine carrier
 *  explicitly spell @c unsigned @c int directly.
 *
 *  Witness layer: @c Cardinality carries the homogeneous operator +
 *  trait + concept surface shipped in PR #425 (#424) — @c +, @c *,
 *  @c /, @c %, @c <=>, @c ==; @c IsRig, @c IsCommutativeMonoid under
 *  each op, @c HasPartialOrderOperators / @c HasTotalOrderOperators,
 *  @c IsTotallyOrdered, @c IsDirectedSet / @c IsDirectedPoset,
 *  @c IsDividableChain.  Subtraction is intentionally absent: ℕ × ℕ →
 *  ℤ is the @c SignedCardinality job.
 */
export using ℕ = dedekind::sets::Cardinality;

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

}  // namespace dedekind::category
