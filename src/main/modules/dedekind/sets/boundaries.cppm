/**
 * @file dedekind/sets/boundaries.cppm
 * @partition :boundaries
 * @brief The Extremal Identities: Universal (V) and Empty (∅) Sets.
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @module dedekind.sets:boundaries
 * @dependency dedekind.ontology
 *
 * @section The_Structural_Limits: ⊥ and ⊤
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
 * @section Semantic_Role
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
 * @section Mereology: The study of parts and wholes.
 * @section Mereology: The Hierarchy of Order.
 */
namespace dedekind::sets {

struct Boundaries {};

/** @section Structural_Resolution
 *
 * @c resolve_species / @c element_of_t resolve a type to its underlying
 * element type: predicate-set carriers (anything with a nested
 * @c ::Domain) project to their domain, primitive types are their own
 * elements (the @b carrier reading after #399).  Lifted from
 * @c :family to @c :boundaries so the upstream @c Variable / @c var
 * machinery in @c :expressions can use the same trait without
 * duplicating the resolution logic.
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

  /** @section Algebraic_Axioms */
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

  /** @section Extensionality_Proof */
  constexpr std::size_t size() const { return 0; }

  /** @section Lattice_Axiom: Initiality */
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
 */
export template <typename T, typename L = ClassicalLogic, typename C = ℵ_0>
struct Ω final : Boundaries {
  using Domain = T;
  using Codomain = typename L::Ω;
  using cardinality_type = C;
  using base_set_type = Ω<T, L, C>;
  using is_universal_boundary = void;
  using logic_species = L;

  /** @section Algebraic_Axioms */
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
   * @section Lattice_Axiom: Terminality
   * Everything is a part of the Universal Set.
   * Constraint: Exclude Variables (which have a member T) to let
   * symbolic expressions handle their own comparisons.
   */
  template <typename S>
    requires(!requires { typename S::T; }) &&
            (!requires { typename S::is_variable; })
  friend constexpr typename L::Ω operator<=(const S&, const Ω&) {
    return L::True;
  }

  /** @section Lattice_Axiom: Reflexivity */
  constexpr typename L::Ω operator<=(const Ω&) const { return L::True; }

  // Explicitly define equality if <=> is being deleted by members
  constexpr bool operator==(const Ω&) const { return true; }

  // Note: You'll eventually want overloads for:
  // Universal | Any = Universal
  // Universal & Any = Any

  // The Axiom: Total Presence
  constexpr typename L::Ω operator()(const T&) const { return L::True; }

  constexpr cardinality_type cardinality() const { return cardinality_type{}; }

  // Ω | S = Ω
  template <typename S>
  constexpr auto operator|(const S&) const {
    return *this;
  }

  // Ω & S = S
  template <typename S>
  constexpr auto operator&(const S& s) const {
    return s;
  }
};

template <typename T, typename L>
constexpr auto Ø<T, L>::operator!() const {
  return Ω<T, L>{};
}

// Cardinality metadata drives extensional classification for Ω.
template <typename T, typename L, typename C>
struct is_extensional<Ω<T, L, C>> : std::bool_constant<C::is_finite> {};

static_assert(dedekind::category::IsSet<decltype(ambient_set<int>(Ω<int>{}))>,
              "The universal boundary must lift to an ETCS set object.");
static_assert(dedekind::category::IsSet<decltype(ambient_set<int>(Ø<int>{}))>,
              "The empty boundary must lift to an ETCS set object.");
static_assert(dedekind::category::HasCanonicalSetCCC<int>,
              "Breadcrumb to :cartesian: boundary ambient int has canonical "
              "CCC witness.");

// Predicate-set classifier for ℕ (the natural-numbers ambient species).
// Per #401 the canonical species symbol @c ℕ migrated to a carrier-type
// alias (@c ℕ = @c unsigned @c int); this predicate-set retained as the
// set-builder DSL handle ("is this value in ℕ?"), with its @c Domain
// aligned to the carrier so @c var<ℕ> @c % @c N composes cleanly.
//
// The @c int-typed @c operator() overload survives as a callsite
// convenience on the @b classifier reading (ℕ ⊂ ℤ via the
// non-negativity check); it is not on the formal predicate-set
// signature (which is @c Domain @c = @c unsigned @c int) but is
// reachable via direct @c N(-7) calls for paper-listing readability.
export template <typename L = ClassicalLogic, typename C = ℵ_0>
struct NaturalNumbersOf {
  using Domain = unsigned int;  // Aligned to the @c ℕ carrier, post-#401.
  using Codomain = typename L::Ω;
  using logic_species = L;
  using cardinality_type = C;

  // Canonical signature: every unsigned value is in ℕ.
  template <std::unsigned_integral U>
  constexpr typename L::Ω operator()(U) const {
    return L::True;
  }

  // Classifier convenience: ℕ ⊂ ℤ via non-negativity.  Reachable via direct
  // @c N(-7) calls; the @c Set DSL routes through @c Domain @c = @c
  // unsigned @c int, so this overload does @b not fire from
  // @c Set{var<ℕ> @c % @c N}-flavoured callsites.
  constexpr typename L::Ω operator()(int x) const {
    return x >= 0 ? L::True : L::False;
  }

  // Embedded bool (via @c embed_𝔹_ℕ → unsigned): landing in ℕ.
  constexpr typename L::Ω operator()(bool) const { return L::True; }
};

// Non-exported convenience alias used by the value-level @c N constant
// below.  Public surface is @c NaturalNumbersOf<L, C> (the parameterised
// template); callers naming the default form should use
// @c NaturalNumbersOf<> directly or @c decltype(N).  Mirrors the
// @c BooleanSet de-export pattern from #407.
using NaturalNumbers = NaturalNumbersOf<>;

/** @brief The canonical Natural-numbers carrier symbol @c ℕ = @c unsigned @c
 * int.
 *
 *  @details Per #401 (carrier-type migration of the canonical species
 *  symbols).  The machine-ℕ realisation is @c unsigned @c int — a
 *  modular ring ℤ/2³²ℤ; the formal "infinite number line" reading
 *  lives on @c ExtensionalCardinal<> (which saturates to ℵ₀ rather
 *  than wrapping; see PR #396).  Both carriers satisfy
 *  @c IsNatural; @c ℕ defaults to the machine flavour for showcase
 *  performance, while callers wanting the exact unbounded reading
 *  spell @c var<ExtensionalCardinal<>> directly.
 */
export using ℕ = unsigned int;

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

// Cardinality metadata drives transfinite classification for Ω.
template <typename T, typename L, typename C>
struct is_transfinite<dedekind::sets::Ω<T, L, C>>
    : std::bool_constant<!C::is_finite> {};

}  // namespace dedekind::category
