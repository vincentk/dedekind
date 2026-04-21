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

export module dedekind.sets:expressions;

import dedekind.category;
import :boundaries;  // For Ω, Ø
import :mereology;   // For NaturalLogic

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
 */
export template <typename Species>
struct Variable {
  using T = typename Species::Domain;
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
  constexpr bool operator()(const T&) const { return true; }
};

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
    auto predicate = [base = predicate_](const T& v) { return !base(v); };
    return Set<T, L, decltype(predicate)>{predicate};
  }

  template <typename OtherPredicate>
  constexpr auto operator|(const Set<T, L, OtherPredicate>& other) const {
    auto predicate = [lhs = predicate_, rhs = other.predicate_](const T& v) {
      return lhs(v) || rhs(v);
    };
    return Set<T, L, decltype(predicate)>{predicate};
  }

  template <typename OtherPredicate>
  constexpr auto operator&(const Set<T, L, OtherPredicate>& other) const {
    auto predicate = [lhs = predicate_, rhs = other.predicate_](const T& v) {
      return lhs(v) && rhs(v);
    };
    return Set<T, L, decltype(predicate)>{predicate};
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
  return [rhs](const typename Species::Domain& v) { return v < rhs; };
}

export template <typename Species, typename Rhs>
constexpr auto operator<=(const Variable<Species>&, const Rhs& rhs) {
  return [rhs](const typename Species::Domain& v) { return v <= rhs; };
}

export template <typename Species, typename Rhs>
constexpr auto operator>(const Variable<Species>&, const Rhs& rhs) {
  return [rhs](const typename Species::Domain& v) { return v > rhs; };
}

export template <typename Species, typename Rhs>
constexpr auto operator>=(const Variable<Species>&, const Rhs& rhs) {
  return [rhs](const typename Species::Domain& v) { return v >= rhs; };
}

export template <typename Species, typename Rhs>
constexpr auto operator==(const Variable<Species>&, const Rhs& rhs) {
  return [rhs](const typename Species::Domain& v) { return v == rhs; };
}

/** @section Logical_Lifting */

export template <typename P1, typename P2>
constexpr auto operator&&(P1&& p1, P2&& p2) {
  return [p1 = std::forward<P1>(p1), p2 = std::forward<P2>(p2)](const auto& v) {
    return p1(v) && p2(v);
  };
}

export template <typename P1, typename P2>
constexpr auto operator||(P1&& p1, P2&& p2) {
  return [p1 = std::forward<P1>(p1), p2 = std::forward<P2>(p2)](const auto& v) {
    return p1(v) || p2(v);
  };
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
