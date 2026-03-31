module;

#include <compare>
#include <concepts>
#include <functional>

export module dedekind.sets:expressions;

import dedekind.category;
import :boundaries;  // For Ω, Ø

namespace dedekind::sets {
using namespace dedekind::category;

export template <typename Base, typename Predicate>
struct Comprehension {
  const Base& base;
  Predicate predicate;
  using element_type = typename Base::element_type;
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
  using T = typename Species::element_type;
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
    requires std::same_as<T, typename SubSpecies::element_type>
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

export template <typename T, typename L = ClassicalLogic>
class Set {
 public:
  using logic_species = L;
  template <typename B, typename P>
  constexpr Set(Comprehension<B, P> cp)
      : predicate_([p = cp.predicate](const T& v) {
          return dedekind::category::lift_logic<L>(p(v));
        }) {}

  /** @brief Identity Constructor: Set{ Ω } */
  template <typename Species>
    requires std::same_as<T, typename Species::element_type>
  constexpr Set(const Species&)
      : predicate_([](const T&) { return L::True; }) {}

  auto operator()(const T& v) const { return predicate_(v); }

 private:
  std::function<typename L::type(const T&)> predicate_;
};

export template <typename B, typename P>
Set(Comprehension<B, P>)
    -> Set<typename B::element_type,
           typename dedekind::ontology::NaturalLogic<B>::type>;

/** @section Identity_CTAD */
template <typename Species>
Set(Species) -> Set<typename Species::element_type,
                    typename dedekind::ontology::NaturalLogic<Species>::type>;

/** @section Relational_Lifting (Level 1) */

// Note: We move these OUTSIDE the Variable struct, into namespace
// dedekind::sets

export template <typename Species, typename Rhs>
constexpr auto operator<(const Variable<Species>&, const Rhs& rhs) {
  return [rhs](const typename Species::element_type& v) { return v < rhs; };
}

export template <typename Species, typename Rhs>
constexpr auto operator<=(const Variable<Species>&, const Rhs& rhs) {
  return [rhs](const typename Species::element_type& v) { return v <= rhs; };
}

export template <typename Species, typename Rhs>
constexpr auto operator>(const Variable<Species>&, const Rhs& rhs) {
  return [rhs](const typename Species::element_type& v) { return v > rhs; };
}

export template <typename Species, typename Rhs>
constexpr auto operator>=(const Variable<Species>&, const Rhs& rhs) {
  return [rhs](const typename Species::element_type& v) { return v >= rhs; };
}

export template <typename Species, typename Rhs>
constexpr auto operator==(const Variable<Species>&, const Rhs& rhs) {
  return [rhs](const typename Species::element_type& v) { return v == rhs; };
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
