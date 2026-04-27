/**
 * @file dedekind/sets/expressions.cppm
 * @partition :expressions
 * @brief Level 1.2: Set-Builder DSL -- comprehension, membership, Boolean
 * connectives, subsets, relations, functions and the power set.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Description
 * This partition provides the principal "set-builder" abstraction:
 * Set<T, L, Predicate> -- an intensional set whose membership test is a
 * compile-time callable predicate ranging into a subobject classifier L::Omega.
 *
 * Key constructs exported:
 *  - Set<T,L,P>           -- ETCS-compatible intensional set.
 *  - Variable<S> / var<S> -- symbolic scouts for comprehension syntax.
 *  - Boolean connectives &&, ||, ! lifted to predicate combinators.
 *  - operator<=           -- subset relation (same-predicate -> True;
 *                            heterogeneous -> Unknown via TernaryLogic).
 *  - cartesian_product    -- A x B as a Set of pairs.
 *  - Relation, SetFunction -- subobjects of products.
 *  - power_set            -- P(A) encoded as a Set of sets.
 *  - relates, is_single_valued_at -- point-wise witnesses.
 *
 * @section Canonical_Examples
 * ```cpp
 * auto n = var<ℕ>;
 * const int size = 512;
 * const auto xs = Set{n % N | (n < size)};
 * const auto grid = cartesian_product(xs, xs);  // xs x xs
 * ```
 *
 * @section References
 * - Lawvere, F.W. (1964) -- ETCS axioms @cite lawvere1964etcs
 * - Lambek & Scott (1988) -- higher-order categorical logic @cite
 * lambek1988higher
 * - Pierce (1991) -- basic category theory @cite pierce1991basic
 *
 * @quote
 * "Future users of large data banks must be protected from having to know how
 * the data is organized in the machine."
 * -- E. F. Codd, A Relational Model of Data for Large Shared Data Banks (1970)
 *
 * @note "What is objective must be common to many minds and consequently
 * transmissible from one to the other."
 *       -- Henri Poincare, The Value of Science (1905)
 */
module;

#include <compare>
#include <concepts>
#include <functional>
#include <type_traits>
#include <utility>
#include <variant>

export module dedekind.sets:expressions;

import dedekind.category;
import :boundaries;    // For Ω, Ø
import :cardinality;   // For Cardinality / SignedCardinality (cross-carrier meet)
import :mereology;     // For NaturalLogic

namespace dedekind::sets {
using namespace dedekind::category;

export template <typename Base, typename Predicate>
struct Comprehension {
  const Base& base;
  Predicate predicate;
  using Domain = typename Base::Domain;

  template <typename Op>
  static constexpr bool is_associative_v = true;
  template <typename Op>
  static constexpr bool is_idempotent_v = true;
};

/** @brief The Membership Binding: Bridges a Variable to its Domain. */
template <typename Species>
struct MembershipBinding {
  const Species& base;

  /** @section The_Comprehension_Pipe */
  template <typename P>
  constexpr auto operator|(P&& p) const {
    return Comprehension<Species, std::decay_t<P>>{base, std::forward<P>(p)};
  }
};

/**
 * @class Variable
 * @brief The 'Symbolic Scout' (id_S) of a Species.
 * @details Represents a point-in-potentia for set construction.
 *
 * The underlying element type is resolved via the canonical
 * @c element_of_t trait (in @c :boundaries): predicate-set carriers
 * project to their nested @c ::Domain, primitive carriers are their own
 * elements.  This is the trait that lets @c var<bool>, @c var<𝔹>,
 * @c var<ℕ> work uniformly across the show-to-a-wider-audience API
 * (#399).
 */
export template <typename Species>
struct Variable {
  using T = element_of_t<Species>;
  using is_variable = void;

  /** @brief The Membership Morphism (x % S). Mimics 'x \in S'. */
  constexpr auto operator%(const Species& s) const {
    return MembershipBinding<Species>{s};
  }

  /**
   * @brief The Membership Morphism.
   * Allows binding this variable to any set S,
   * provided they share the same underlying element type.
   */
  template <typename SubSpecies>
    requires std::same_as<T, typename SubSpecies::Domain>
  constexpr auto operator%(const SubSpecies& s) const {
    return MembershipBinding<SubSpecies>{s};
  }
};

/** @brief Global factory for symbolic scouts. */
export template <typename S>
inline constexpr Variable<S> var{};

/** @brief The universal predicate: accepts every element of T. */
export template <typename T>
struct UniversalPredicate {
  using Domain = T;
  constexpr bool operator()(const T&) const { return true; }
};

/**
 * @brief The empty predicate: rejects every element of T.
 *
 * Used as the structural witness of a contradiction proven at compile time
 * (e.g. `(x > 5) && (x < 3)` collapsing via `structured_and`). When a `Set`'s
 * predicate is `EmptyPredicate<T>`, the set equals `Ø<T, L>`.
 */
export template <typename T>
struct EmptyPredicate {
  using Domain = T;
  constexpr bool operator()(const T&) const { return false; }
};

/** @brief Predicate-level complement wrapper used for set-collapse detection.
 */
export template <typename Predicate>
struct NegatedPredicate {
  Predicate base;

  template <typename T>
  constexpr auto operator()(const T& v) const {
    return !base(v);
  }
};

template <typename P1, typename P2>
struct IsComplementPair : std::false_type {};

template <typename P, typename Q>
struct IsComplementPair<NegatedPredicate<P>, Q>
    : std::bool_constant<std::is_empty_v<P> &&
                         std::same_as<std::decay_t<P>, std::decay_t<Q>>> {};

template <typename P, typename Q>
struct IsComplementPair<P, NegatedPredicate<Q>>
    : std::bool_constant<std::is_empty_v<Q> &&
                         std::same_as<std::decay_t<P>, std::decay_t<Q>>> {};

template <typename P1, typename P2>
inline constexpr bool IsComplementPair_v =
    IsComplementPair<std::decay_t<P1>, std::decay_t<P2>>::value;

export template <typename T, typename L, typename Predicate>
class Set;

/** @brief Boolean equality predicate for compile-time pruning over 𝔹. */
export struct BooleanEqPredicate {
  bool expected;

  constexpr bool operator()(bool v) const { return v == expected; }
};

/** @brief Extensional finite bool-domain result for collapsed 𝔹 operations. */
export template <typename L>
struct FiniteBooleanSet {
  using Domain = bool;
  using Codomain = typename L::Ω;
  using logic_species = L;
  using cardinality_type = Finite;

  typename L::Ω at_false;
  typename L::Ω at_true;

  constexpr typename L::Ω operator()(bool v) const {
    return v ? at_true : at_false;
  }

  constexpr bool operator==(const Ø<bool, L>&) const {
    return at_false == L::False && at_true == L::False;
  }

  constexpr bool operator==(const Ω<bool, L, Finite>&) const {
    return at_false == L::True && at_true == L::True;
  }

  friend constexpr bool operator==(const Ø<bool, L>& empty,
                                   const FiniteBooleanSet& s) {
    return s == empty;
  }

  friend constexpr bool operator==(const Ω<bool, L, Finite>& universe,
                                   const FiniteBooleanSet& s) {
    return s == universe;
  }

  constexpr auto operator|(const FiniteBooleanSet& other) const {
    return FiniteBooleanSet{
        L::OR(at_false, other.at_false),
        L::OR(at_true, other.at_true),
    };
  }

  constexpr auto operator&(const FiniteBooleanSet& other) const {
    return FiniteBooleanSet{
        L::AND(at_false, other.at_false),
        L::AND(at_true, other.at_true),
    };
  }
};

export template <typename L>
constexpr auto operator|(const Set<bool, L, BooleanEqPredicate>& lhs,
                         const FiniteBooleanSet<L>& rhs) {
  return FiniteBooleanSet<L>{
      L::OR(lhs(false), rhs(false)),
      L::OR(lhs(true), rhs(true)),
  };
}

export template <typename L>
constexpr auto operator|(const FiniteBooleanSet<L>& lhs,
                         const Set<bool, L, BooleanEqPredicate>& rhs) {
  return rhs | lhs;
}

export template <typename L>
constexpr auto operator&(const Set<bool, L, BooleanEqPredicate>& lhs,
                         const FiniteBooleanSet<L>& rhs) {
  return FiniteBooleanSet<L>{
      L::AND(lhs(false), rhs(false)),
      L::AND(lhs(true), rhs(true)),
  };
}

export template <typename L>
constexpr auto operator&(const FiniteBooleanSet<L>& lhs,
                         const Set<bool, L, BooleanEqPredicate>& rhs) {
  return rhs & lhs;
}

// ---------------------------------------------------------------------------
// Cross-carrier meet on the variant pair (existential proof, slice of #362)
// ---------------------------------------------------------------------------
//
// When two sets are intersected and their carriers are related by the
// canonical embedding ℕ ↪ ℤ, the result's carrier should be the @b
// tighter of the two — the library picks the pullback along the
// embedding.  Mathematically:
//
//   A ⊂ ℕ,  B ⊂ ℤ,  ℕ ↪ ℤ
//   ⇒  A ∩ B ⊂ ℕ  (not ⊂ ℤ).
//
// This is the @b carrier @b strength-reduction rule: the carrier
// "lattice" (ℕ < ℤ < ℚ < ℝ < ℂ) drives the resulting carrier of binary
// set operations.  #362 tracks the @b general framework (any pair in
// the lattice); the overloads below land the @b existential @b proof
// for the variant pair (Cardinality, SignedCardinality), which is the
// load-bearing case for Paper 3's type-directed-collapse story.
//
// The result predicate evaluates @c lhs(v) @c && @c rhs(lift(v)) where
// @c v has the smaller carrier's type and @c lift is the canonical ℕ
// ↪ ℤ embedding (mirrors @c detail::lift_cardinality_to_signed in
// @c :cardinality; inlined here so @c expressions.cppm doesn't need to
// reach into the detail namespace cross-partition).
//
// Future iterations under #362 will replace this hand-coded pair with
// a @c carrier_lattice_meet_t<T1, T2> trait and a generic overload
// that dispatches across the full lattice.  See
// @c docs/design/carrier-lattice.md for the design discussion.

namespace detail {
/** @brief Lift @c Cardinality into @c SignedCardinality via the
 *         canonical ℕ ↪ ℤ embedding.  Local to @c :expressions for
 *         the cross-carrier @c operator& below. */
constexpr SignedCardinality lift_natural_to_signed(
    const Cardinality& c) noexcept {
  if (std::holds_alternative<ℵ_0>(c)) {
    return SignedCardinality{PositiveInfinity{}};
  }
  SignedExtensionalCardinal<> result;
  result.magnitude = std::get<ExtensionalCardinal<>>(c);
  result.negative = false;
  return SignedCardinality{result};
}
}  // namespace detail

/** @brief Cross-carrier meet @c Set<Cardinality> @c & @c
 *         Set<SignedCardinality> @c → @c Set<Cardinality> (carrier
 *         strength-reduction; closes a slice of #362).  The
 *         intersection is contained in ℕ, so the result carrier is
 *         @c Cardinality. */
export template <typename L, typename P1, typename P2>
constexpr auto operator&(const Set<Cardinality, L, P1>& lhs,
                         const Set<SignedCardinality, L, P2>& rhs) {
  auto predicate = [lhs, rhs](const Cardinality& v) {
    return L::AND(lhs(v), rhs(detail::lift_natural_to_signed(v)));
  };
  return Set<Cardinality, L, decltype(predicate)>{predicate};
}

/** @brief Symmetric: @c Set<SignedCardinality> @c & @c
 *         Set<Cardinality> delegates to the canonical direction.  The
 *         result still tightens to @c Set<Cardinality>. */
export template <typename L, typename P1, typename P2>
constexpr auto operator&(const Set<SignedCardinality, L, P1>& lhs,
                         const Set<Cardinality, L, P2>& rhs) {
  return rhs & lhs;
}

namespace detail {
/** @brief Partial inverse of @c lift_natural_to_signed: project a
 *         @c SignedCardinality back to @c Cardinality when the value
 *         is non-negative and not a NaZ.  Returns the projected
 *         @c Cardinality on success; returns @c std::nullopt when
 *         @c v is negative, @c -ℵ_0, or @c NaZ — i.e. when @c v @b
 *         not in ℕ.  This is the operational shadow of "ℕ ⊂ ℤ as a
 *         partial reverse": injection ℕ ↪ ℤ has a partial inverse on
 *         the non-negative fragment of ℤ. */
constexpr bool sc_is_negative_finite(const SignedCardinality& v) noexcept {
  if (!std::holds_alternative<SignedExtensionalCardinal<>>(v)) return false;
  return std::get<SignedExtensionalCardinal<>>(v).negative;
}

constexpr bool sc_is_in_natural_image(const SignedCardinality& v) noexcept {
  if (std::holds_alternative<NaZ>(v)) return false;
  if (std::holds_alternative<NegativeInfinity>(v)) return false;
  if (sc_is_negative_finite(v)) return false;
  return true;
}

constexpr Cardinality project_signed_to_natural(
    const SignedCardinality& v) noexcept {
  // Pre: @c sc_is_in_natural_image(v) holds.
  if (std::holds_alternative<PositiveInfinity>(v)) {
    return Cardinality{ℵ_0{}};
  }
  return Cardinality{std::get<SignedExtensionalCardinal<>>(v).magnitude};
}
}  // namespace detail

/** @brief Cross-carrier join @c Set<Cardinality> @c | @c
 *         Set<SignedCardinality> @c → @c Set<SignedCardinality>
 *         (carrier widening; closes a slice of #362).  The union may
 *         contain negative integers from the @c rhs side, so the
 *         result carrier widens to ℤ.
 *
 *  Membership for @c v @c : @c ℤ:
 *    * If @c v lives in the image of ℕ ↪ ℤ (non-negative, not @c NaZ,
 *      not @c -ℵ_0), evaluate @c lhs on the projected ℕ value and OR
 *      with @c rhs(v).
 *    * Otherwise @c v is not in ℕ, so @c lhs(v) is structurally @c
 *      false; the union reduces to @c rhs(v). */
export template <typename L, typename P1, typename P2>
constexpr auto operator|(const Set<Cardinality, L, P1>& lhs,
                         const Set<SignedCardinality, L, P2>& rhs) {
  auto predicate = [lhs, rhs](const SignedCardinality& v) {
    if (detail::sc_is_in_natural_image(v)) {
      return L::OR(lhs(detail::project_signed_to_natural(v)), rhs(v));
    }
    return rhs(v);
  };
  return Set<SignedCardinality, L, decltype(predicate)>{predicate};
}

/** @brief Symmetric: @c Set<SignedCardinality> @c | @c Set<Cardinality>
 *         delegates to the canonical direction.  The result still
 *         widens to @c Set<SignedCardinality>. */
export template <typename L, typename P1, typename P2>
constexpr auto operator|(const Set<SignedCardinality, L, P1>& lhs,
                         const Set<Cardinality, L, P2>& rhs) {
  return rhs | lhs;
}

/** @brief Convenience Alias: If S is a type, var<S> is the scout. */
// This allows: auto x = var<int>; where int is mapped to Ω<int>
export template <typename T>
inline constexpr Variable<Ω<T>> var_for_type{};

export template <typename T, typename L, typename Predicate>
class Set {
 public:
  using Domain = T;
  using Codomain = typename L::Ω;
  using logic_species = L;
  using cardinality_type = ℵ_0;

  template <typename Op>
  static constexpr bool is_associative_v = true;

  template <typename Op>
  static constexpr bool is_idempotent_v = true;

  // Store the predicate as a concrete type, not a std::function
  constexpr Set(Predicate p) : predicate_(std::move(p)) {}

  template <typename B, typename P>
    requires std::same_as<Predicate, P>
  constexpr Set(Comprehension<B, P> cp) : predicate_(std::move(cp.predicate)) {}

  template <typename B>
    requires std::same_as<T, typename B::Domain> && std::same_as<Predicate, B>
  constexpr Set(MembershipBinding<B> b) : predicate_(b.base) {}

  template <typename B>
    requires std::same_as<T, typename B::Domain> &&
             std::same_as<Predicate, UniversalPredicate<T>> &&
             requires { typename B::is_universal_boundary; }
  constexpr Set(MembershipBinding<B>) : predicate_{} {}

  constexpr auto operator()(const T& v) const {
    return dedekind::category::lift_logic<L>(predicate_(v));
  }

  constexpr cardinality_type cardinality() const { return {}; }

  constexpr auto operator!() const {
    return Set<T, L, NegatedPredicate<Predicate>>{
        NegatedPredicate<Predicate>{predicate_}};
  }

  // FIXME(#362): operator| / operator& currently require the same carrier
  // type `T`. Heterogeneous meets (`Set<ℕ> & Set<ℝ>`) should dispatch through
  // the canonical-embedding lattice (`embed_ℕ_ℝ`, …) so the result tightens
  // to the sharper of the two carriers; same applies below for operator&.
  template <typename OtherPredicate>
  constexpr auto operator|(const Set<T, L, OtherPredicate>& other) const {
    if constexpr (IsComplementPair_v<Predicate, OtherPredicate>) {
      return Ω<T, L>{};
    } else if constexpr (std::same_as<T, bool> &&
                         std::same_as<Predicate, BooleanEqPredicate> &&
                         std::same_as<OtherPredicate, BooleanEqPredicate>) {
      return FiniteBooleanSet<L>{
          L::OR((*this)(false), other(false)),
          L::OR((*this)(true), other(true)),
      };
    } else {
      // FIXME(#365): symmetric of the operator& fallback below — lattice-law
      // rewriting (dually: absorption, De Morgan) could collapse `A ∪ B`
      // structurally before falling through to this opaque lambda.
      auto predicate = [lhs = predicate_, rhs = other.predicate_](const T& v) {
        return lhs(v) || rhs(v);
      };
      return Set<T, L, decltype(predicate)>{predicate};
    }
  }

  template <typename OtherPredicate>
  constexpr auto operator&(const Set<T, L, OtherPredicate>& other) const {
    if constexpr (IsComplementPair_v<Predicate, OtherPredicate>) {
      return Ø<T, L>{};
    } else if constexpr (std::same_as<T, bool> &&
                         std::same_as<Predicate, BooleanEqPredicate> &&
                         std::same_as<OtherPredicate, BooleanEqPredicate>) {
      return FiniteBooleanSet<L>{
          L::AND((*this)(false), other(false)),
          L::AND((*this)(true), other(true)),
      };
    } else if constexpr (requires {
                           structured_and(predicate_, other.predicate_);
                         }) {
      // Evaluate the reduction once. Calling `structured_and` multiple times
      // inflates compile time (each call is a fresh template instantiation)
      // and would risk inconsistency if a future overload produced a value-
      // carrying (non-empty) result whose default-construction differs from
      // the original call's result.
      auto reduced = structured_and(predicate_, other.predicate_);
      using Result = std::decay_t<decltype(reduced)>;
      if constexpr (std::same_as<Result, EmptyPredicate<T>>) {
        return Ø<T, L>{};
      } else if constexpr (requires {
                             typename Result::is_static_singleton_tag;
                           }) {
        // Cardinality-1 reduction (e.g. integer halfspace meet): elevate to a
        // bare Singleton-typed value, paralleling the Ø collapse for empty.
        return reduced;
      } else if constexpr (requires { typename Result::cardinality_type; } &&
                           std::same_as<typename Result::cardinality_type,
                                        Finite>) {
        // Structured reduction to a named finite object (e.g. an integer
        // OrderInterval with compile-time-computed size): elevate it out of
        // the Set wrapper so downstream code can observe size() / bounds /
        // computability classification directly on the reduced type.
        return reduced;
      } else {
        return Set<T, L, Result>{std::move(reduced)};
      }
    } else {
      // FIXME(#365): lambda fallback erases predicate structure. Lattice-law
      // rewriting (distributivity / absorption / De Morgan) could expose
      // collapses here that the local structured_and branches miss — e.g.
      // `(A ∪ B) ∩ ¬A` normalising to `B ∩ ¬A` before this fallback fires.
      auto predicate = [lhs = predicate_, rhs = other.predicate_](const T& v) {
        return lhs(v) && rhs(v);
      };
      return Set<T, L, decltype(predicate)>{predicate};
    }
  }

  /** @brief Same-predicate subset: always True (identity). */
  constexpr typename L::Ω operator<=(const Set& /*other*/) const {
    return L::True;
  }

  /**
   * @brief Subset test: this ⊆ other for heterogeneous predicate types.
   *
   * For intensional sets over potentially infinite domains, the general subset
   * question is undecidable without witnesses. This overload is only available
   * when the ambient logic supports Unknown.
   */
  template <typename OtherPredicate>
    requires(!std::same_as<Predicate, OtherPredicate>) &&
            requires { L::Unknown; }
  constexpr typename L::Ω operator<=(const Set<T, L, OtherPredicate>&) const {
    return L::Unknown;
  }

  /**
   * @brief Point-wise subset evidence: returns true iff this(x) implies
   * other(x) at the given witness point x.
   */
  template <typename OtherPredicate>
  constexpr typename L::Ω is_subset_of_at(
      const Set<T, L, OtherPredicate>& other, const T& x) const {
    const auto in_this = (*this)(x);
    const auto in_other = other(x);
    // Implication in Ω: a => b  is  (!a) OR b.
    return L::OR(L::NOT(in_this), in_other);
  }

 private:
  template <typename, typename, typename>
  friend class Set;

  Predicate predicate_;
};

/** @brief Explicit set complement overload to avoid picking category morphism
 * `!`. */
export template <typename T, typename L, typename Predicate>
constexpr auto operator!(const Set<T, L, Predicate>& s) {
  return s.operator!();
}

/** @brief lvalue set complement overload for stable prefix `!` resolution. */
export template <typename T, typename L, typename Predicate>
constexpr auto operator!(Set<T, L, Predicate>& s) {
  return s.operator!();
}

/** @brief rvalue set complement overload for composed temporary expressions. */
export template <typename T, typename L, typename Predicate>
constexpr auto operator!(Set<T, L, Predicate>&& s) {
  return s.operator!();
}

export template <typename B, typename P>
Set(Comprehension<B, P>)
    -> Set<typename B::Domain, typename NaturalLogic<B>::type, P>;

export template <typename S>
  requires(!requires { typename S::is_universal_boundary; })
Set(MembershipBinding<S>)
    -> Set<typename S::Domain, typename NaturalLogic<S>::type, S>;

export template <typename S>
  requires requires { typename S::is_universal_boundary; }
Set(MembershipBinding<S>)
    -> Set<typename S::Domain, typename NaturalLogic<S>::type,
           UniversalPredicate<typename S::Domain>>;

static_assert(
    dedekind::category::IsSet<decltype(dedekind::category::ambient_set<int>(
        Set<int, dedekind::category::ClassicalLogic, UniversalPredicate<int>>{
            UniversalPredicate<int>{}}))>,
    "The canonical intensional Set<T, L, Predicate> must lift to an ETCS set "
    "object.");

static_assert(
    dedekind::category::HasCanonicalSetCCC<int>,
    "Breadcrumb to :cartesian: ambient int carries canonical CCC witness.");

/** @section Identity_CTAD */
template <typename Species>
Set(Species) -> Set<typename Species::Domain,
                    typename NaturalLogic<Species>::type, Species>;

/** @section Relational_Lifting (Level 1) */

// Note: We move these OUTSIDE the Variable struct, into namespace
// dedekind::sets

export template <typename Species, typename Rhs>
constexpr auto operator<(const Variable<Species>&, const Rhs& rhs) {
  return [rhs](const element_of_t<Species>& v) { return v < rhs; };
}

export template <typename Species, typename Rhs>
constexpr auto operator<=(const Variable<Species>&, const Rhs& rhs) {
  return [rhs](const element_of_t<Species>& v) { return v <= rhs; };
}

export template <typename Species, typename Rhs>
constexpr auto operator>(const Variable<Species>&, const Rhs& rhs) {
  return [rhs](const element_of_t<Species>& v) { return v > rhs; };
}

export template <typename Species, typename Rhs>
constexpr auto operator>=(const Variable<Species>&, const Rhs& rhs) {
  return [rhs](const element_of_t<Species>& v) { return v >= rhs; };
}

export template <typename Species, typename Rhs>
constexpr auto operator==(const Variable<Species>&, const Rhs& rhs) {
  return [rhs](const element_of_t<Species>& v) { return v == rhs; };
}

export template <typename Species>
  requires std::same_as<element_of_t<Species>, bool>
constexpr auto operator==(const Variable<Species>&, bool rhs) {
  return BooleanEqPredicate{rhs};
}

/** @brief Unary negation of a boolean variable: `!b` ≡ `b == false`.
 *
 *  Delegates to the `operator==` overload above so the bool-domain encoding
 *  lives in exactly one place: a future change to the predicate type or
 *  pruning hooks only needs to be made once.
 */
export template <typename Species>
  requires std::same_as<element_of_t<Species>, bool>
constexpr auto operator!(const Variable<Species>& v) {
  return v == false;
}

/** @section Logical_Lifting */

/**
 * @brief Downstream specialization hook for structured predicate conjunction.
 * @details
 * If a downstream module defines a free `structured_and(p1, p2)` overload
 * discoverable via ADL for the predicate types, `operator&&` will dispatch to
 * that overload. Otherwise it falls back to an opaque lambda composition.
 */
template <typename P1, typename P2>
concept HasStructuredAnd =
    requires(const std::decay_t<P1>& p1, const std::decay_t<P2>& p2) {
      { structured_and(p1, p2) };
    };

/**
 * @brief Downstream specialization hook for structured predicate disjunction.
 * @details Mirrors `HasStructuredAnd` through `structured_or(p1, p2)`.
 */
template <typename P1, typename P2>
concept HasStructuredOr =
    requires(const std::decay_t<P1>& p1, const std::decay_t<P2>& p2) {
      { structured_or(p1, p2) };
    };

export template <typename P1, typename P2>
constexpr auto operator&&(P1&& p1, P2&& p2) {
  if constexpr (HasStructuredAnd<P1, P2>) {
    return structured_and(std::forward<P1>(p1), std::forward<P2>(p2));
  } else {
    return [p1 = std::forward<P1>(p1), p2 = std::forward<P2>(p2)](
               const auto& v) { return p1(v) && p2(v); };
  }
}

export template <typename P1, typename P2>
constexpr auto operator||(P1&& p1, P2&& p2) {
  if constexpr (HasStructuredOr<P1, P2>) {
    return structured_or(std::forward<P1>(p1), std::forward<P2>(p2));
  } else {
    return [p1 = std::forward<P1>(p1), p2 = std::forward<P2>(p2)](
               const auto& v) { return p1(v) || p2(v); };
  }
}

}  // namespace dedekind::sets

namespace dedekind::sets {

/**
 * @brief Cartesian product of two sets: {(a,b) | a ∈ A, b ∈ B}.
 *
 * Constructs a Set whose domain is std::pair<T1,T2> and whose membership
 * predicate checks element-wise membership in both component sets.
 */
export template <typename T1, typename L1, typename P1, typename T2,
                 typename L2, typename P2>
  requires std::same_as<L1, L2>
constexpr auto cartesian_product(const Set<T1, L1, P1>& a,
                                 const Set<T2, L2, P2>& b) {
  using Pair = std::pair<T1, T2>;
  auto pred = [pa = a, pb = b](const Pair& p) {
    return pa(p.first) && pb(p.second);
  };
  return Set<Pair, L1, decltype(pred)>{pred};
}

/**
 * @brief Cartesian product over ambient species values.
 *
 * Lifts each ambient species to its full carrier set and delegates to the
 * Set×Set cartesian product.
 */
export template <typename A, typename B>
  requires requires {
    typename std::remove_cvref_t<A>::Domain;
    typename std::remove_cvref_t<B>::Domain;
    typename NaturalLogic<std::remove_cvref_t<A>>::type;
    typename NaturalLogic<std::remove_cvref_t<B>>::type;
  } && std::same_as<typename NaturalLogic<std::remove_cvref_t<A>>::type,
                    typename NaturalLogic<std::remove_cvref_t<B>>::type>
constexpr auto cartesian_product(const A& a, const B& b) {
  auto xa = var<std::remove_cvref_t<A>>;
  auto xb = var<std::remove_cvref_t<B>>;
  const auto left = Set{xa % a};
  const auto right = Set{xb % b};
  return cartesian_product(left, right);
}

/** @brief Infix sugar for cartesian product over sets. */
export template <typename T1, typename L1, typename P1, typename T2,
                 typename L2, typename P2>
  requires std::same_as<L1, L2>
constexpr auto operator*(const Set<T1, L1, P1>& a, const Set<T2, L2, P2>& b) {
  return cartesian_product(a, b);
}

/** @brief Infix sugar for cartesian product over ambient species values. */
export template <typename A, typename B>
  requires requires {
    typename std::remove_cvref_t<A>::Domain;
    typename std::remove_cvref_t<B>::Domain;
    typename NaturalLogic<std::remove_cvref_t<A>>::type;
    typename NaturalLogic<std::remove_cvref_t<B>>::type;
  } && std::same_as<typename NaturalLogic<std::remove_cvref_t<A>>::type,
                    typename NaturalLogic<std::remove_cvref_t<B>>::type>
constexpr auto operator*(const A& a, const B& b) {
  return cartesian_product(a, b);
}

using CanonicalIntSet =
    Set<int, dedekind::category::ClassicalLogic, UniversalPredicate<int>>;
using CanonicalIntProductSet =
    decltype(cartesian_product(std::declval<const CanonicalIntSet&>(),
                               std::declval<const CanonicalIntSet&>()));
using CanonicalIntProductDomain = typename CanonicalIntProductSet::Domain;

static_assert(
    dedekind::category::IsProduct<CanonicalIntProductDomain, int, int>,
    "sets::cartesian_product must expose a std::pair product domain.");
static_assert(
    dedekind::category::HasCanonicalSetCCC<CanonicalIntProductDomain>,
    "Breadcrumb to :cartesian: cartesian_product domain carries canonical "
    "CCC witness.");

/**
 * @brief A Relation from A to B is a set of pairs: a subset of A × B.
 * @details ETCS reading: relations are subobjects of products.
 * @see Lambek and Scott @cite lambek1988higher
 *
 * @tparam T1  Element type of domain set A.
 * @tparam T2  Element type of codomain set B.
 * @tparam L   Logic species shared by both component sets.
 * @tparam P   Predicate on std::pair<T1,T2>.
 */
export template <typename T1, typename T2, typename L, typename P>
using Relation = Set<std::pair<T1, T2>, L, P>;

/**
 * @brief A (set-level) Function is a Relation where each domain element maps
 * to exactly one codomain element.  The type alias admits the same structure
 * as a Relation; functional totality and single-valuedness are enforced at the
 * call-site via witness elements.
 * @see Pierce @cite pierce1991basic
 */
export template <typename T1, typename T2, typename L, typename P>
using SetFunction = Relation<T1, T2, L, P>;

/**
 * @brief Concept: a set S whose ambient type is std::pair<T1,T2> is a
 * valid binary relation on T1 and T2.
 */
export template <typename S, typename T1, typename T2>
concept IsRelation = requires { typename S::Domain; } &&
                     std::same_as<typename S::Domain, std::pair<T1, T2>>;

/**
 * @brief Power set witness over same-predicate subsets.
 *
 * Membership in P(A) is decided by the subset predicate `candidate <= base`.
 * This conservative form keeps the domain monomorphic (`Set<T,L,P>`) and is
 * sufficient for finite / homogeneous DSL constructions.
 * @see Lambek and Scott @cite lambek1988higher
 */
export template <typename T, typename L, typename P>
constexpr auto power_set(const Set<T, L, P>& base) {
  using Candidate = Set<T, L, P>;
  auto pred = [base](const Candidate& candidate) { return candidate <= base; };
  return Set<Candidate, L, decltype(pred)>{pred};
}

/**
 * @brief Textbook fraktur-P alias for @c power_set.
 *
 * @details In standard set-theory texts (Halmos, Munkres, Lambek--Scott)
 * the power-set operator is written @c 𝔓 (fraktur capital P).  Exposed as
 * a one-line forwarding wrapper so callers can write @c 𝔓(A) for the
 * power set of @c A and have it read the same way it reads on the
 * blackboard.  Mirrored on the Python side as @c dedekind.sets.𝔓.
 */
export template <typename T, typename L, typename P>
constexpr auto 𝔓(const Set<T, L, P>& base) {
  return power_set(base);
}

/** @brief Relation membership witness: (a,b) ∈ R. */
export template <typename T1, typename T2, typename L, typename P>
constexpr typename L::Ω relates(const Relation<T1, T2, L, P>& r, const T1& a,
                                const T2& b) {
  return r(std::pair<T1, T2>{a, b});
}

/**
 * @brief Point-wise single-valuedness witness for a set-function relation.
 *
 * If both y1 and y2 are related to x, they must be equal.
 */
export template <typename T1, typename T2, typename L, typename P>
constexpr typename L::Ω is_single_valued_at(const SetFunction<T1, T2, L, P>& f,
                                            const T1& x, const T2& y1,
                                            const T2& y2) {
  const auto m1 = relates(f, x, y1);
  const auto m2 = relates(f, x, y2);
  const auto both_related = L::AND(m1, m2);
  const auto equal_outputs = lift_logic<L>(y1 == y2);
  // ((x,y1) ∈ f && (x,y2) ∈ f) => (y1 == y2)
  return L::OR(L::NOT(both_related), equal_outputs);
}

}  // namespace dedekind::sets
