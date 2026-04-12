module;

#include <compare>
#include <concepts>
#include <functional>

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

  constexpr typename L::Ω operator<=(const Set&) const { return L::True; }

 private:
  template <typename, typename, typename>
  friend class Set;

  Predicate predicate_;
};

export template <typename B, typename P>
Set(Comprehension<B, P>)
    -> Set<typename B::Domain, typename NaturalLogic<B>::type, P>;

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
