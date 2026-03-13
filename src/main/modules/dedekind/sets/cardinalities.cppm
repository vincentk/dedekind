module;

#include <concepts>
#include <ranges>
#include <type_traits>
#include <variant>

export module dedekind.sets:cardinalities;

// C++23 feature, disabled for now:
// import std;

namespace dedekind::sets {

// The "Seal": Only classes in this namespace can inherit from this
class CardinalityBase {
 protected:  // Key change
  constexpr CardinalityBase() = default;

  // We keep these to ensure only our hierarchy can derive from it
  friend struct Uncountable;
  friend struct Countable;
  friend struct Finite;
  friend struct Extensional;
  friend struct Empty;
  template <std::size_t N>
  friend struct ℵ;
  template <std::size_t N>
  friend struct ℶ;
};

// 1. Define the "Seal" check
template <typename C>
concept IsCardinality = std::is_base_of_v<CardinalityBase, C>;

// Aleph-0 OR Uncountable
export struct Transfinite : public CardinalityBase {
  // Public constructor allows global operators to create Uncountable and its
  // children
  constexpr Transfinite() : CardinalityBase() {}
};

// Uncountable: Transfinite but NOT Countable
export struct Uncountable : public Transfinite {};

// Finite OR Aleph-0
export struct Countable : public CardinalityBase {
  // Public constructor allows global operators to create Uncountable and its
  // children
  constexpr Countable() : CardinalityBase() {}
};

// 3. Finite: Terminating, but size might be unknown or > 2^64
export struct Finite : public Countable {};

// 2. Extensional (The Bounded)
export struct Extensional : public Finite {
  std::size_t bound;
  explicit constexpr Extensional(std::size_t n) : bound(n) {}
};

// 3. Empty (The Identity / Unit)
export struct Empty : public Extensional {
  constexpr Empty() : Extensional(0) {}
};

// Base for all Aleph numbers
export template <std::size_t N>
struct ℵ : public Uncountable {};

// Specialization: Aleph-0 is Countable (not Uncountable)
export template <>
struct ℵ<0> : public Countable {};

// Base for all Beth numbers
export template <std::size_t N>
struct ℶ : public Uncountable {};

// Specialization: Beth-0 is Countable
export template <>
struct ℶ<0> : public Countable {};

// Common Aliases
export using ℵ_0 = ℵ<0>;  // Countably Infinite
export using ℵ_1 = ℵ<1>;  // First Uncountable Cardinal
export using ℶ_1 = ℶ<1>;  // Cardinality of the Continuum (R)

// The "Arithmetic" Alias
export using Zero = Empty;

// A meta-function to find the "Higher" type in the hierarchy
template <IsCardinality L, IsCardinality R>
struct higher_cardinality {
  using type = std::conditional_t<std::is_base_of_v<R, L>, R, L>;
};

export template <IsCardinality L, IsCardinality R>
using higher_t = typename higher_cardinality<L, R>::type;

export template <IsCardinality L, IsCardinality R>
using lower_t = std::conditional_t<std::is_same_v<higher_t<L, R>, L>, R, L>;

// 1. Define the Tiers (The Linear Proof)
template <typename T>
consteval int tier() {
  if constexpr (std::is_same_v<T, Empty>) return 0;
  if constexpr (std::is_base_of_v<Extensional, T>) return 1;
  if constexpr (std::is_base_of_v<Finite, T>) return 2;
  if constexpr (std::is_base_of_v<ℵ<0>, T>) return 3;  // Aleph-0
  if constexpr (std::is_base_of_v<Uncountable, T>) return 4;
  return 5;
}

// In union, the Higher Tier always wins (the "Absorbing Law"). If both are
// extensional, we sum the bounds.
export template <IsCardinality L, IsCardinality R>
constexpr auto operator|(L l, R r) {
  if constexpr (std::is_base_of_v<Extensional, L> &&
                std::is_base_of_v<Extensional, R>) {
    return Extensional(l.bound + r.bound);
  } else {
    // Use () instead of {} to be explicit, and
    // ensure the higher type has a valid constructor call.
    return higher_t<L, R>();
  }
}

// In intersection, the Lower Tier always wins. If tiers are equal and finite,
// we take the minimum bound.
export template <IsCardinality L, IsCardinality R>
constexpr auto operator&(L l, R r) {
  if constexpr (std::is_base_of_v<Extensional, L> &&
                std::is_base_of_v<Extensional, R>) {
    return Extensional(std::min(l.bound, r.bound));
  } else {
    return lower_t<L, R>();
  }
}

export template <std::size_t N, std::size_t M>
constexpr auto operator&(ℵ<N>, ℵ<M>) {
  return ℵ<std::min(N, M)>{};
}

// 1. Finite * Finite = Finite (Product of bounds)
export template <IsCardinality L, IsCardinality R>
constexpr auto operator*(L l, R r)
  requires(std::is_base_of_v<Extensional, L> &&
           std::is_base_of_v<Extensional, R>)
{
  return Extensional(l.bound * r.bound);
}

// For Cartesian products, the rule is: Finite * Finite = Product, but Anything
// * Infinite = Infinite Max.
export template <IsCardinality L, IsCardinality R>
constexpr auto operator*(L l, R r) {
  // Identity: 0 * X = 0
  if constexpr (std::is_same_v<L, Empty> || std::is_same_v<R, Empty>) {
    return Empty{};
  }
  // 1. Both Finite: Numerical product
  else if constexpr (tier<L>() == 0 && tier<R>() == 0) {
    return Extensional(l.bound * r.bound);
  }
  // 2. At least one is Infinite: The higher tier (max) wins
  else {
    return higher_t<L, R>{};
  }
}

// --- Cardinality Successors (2^S) for Power Sets ---

// 2^Empty = 1
export constexpr Extensional operator<<(unsigned int base, Empty) {
  return Extensional(1);
}

// 2^Extensional = 2^n
export constexpr Extensional operator<<(unsigned int base, Extensional e) {
  return Extensional(1ULL << e.bound);
}

// 2^Countable (aleph_0) = Continuum (beth_1)
export constexpr ℶ<1> operator<<(unsigned int base, Countable) { return {}; }

// 2^beth_n = beth_{n+1}
export template <std::size_t N>
constexpr ℶ<N + 1> operator<<(unsigned int base, ℶ<N>) {
  return {};
}

// 1. Equality: Two cardinalities are equal if they are the same type
// AND (if extensional) they have the same bound.
export template <IsCardinality L, IsCardinality R>
constexpr bool operator==(const L& lhs, const R& rhs) {
  // 1. If BOTH are Bounded (Extensional, Empty, or Zero), compare the bounds.
  if constexpr (std::is_base_of_v<Extensional, L> &&
                std::is_base_of_v<Extensional, R>) {
    return lhs.bound == rhs.bound;
  }
  // 2. Otherwise, they must be the exact same symbolic type (e.g. ℵ₀ == ℵ₀)
  else {
    return std::is_same_v<L, R>;
  }
}

// 2. Strict Less-Than: The "Theorem" of Size
export template <IsCardinality L, IsCardinality R>
constexpr bool operator<(const L& lhs, const R& rhs) {
  // If tiers are different, the lower tier is strictly smaller
  if constexpr (tier<L>() != tier<R>()) {
    return tier<L>() < tier<R>();
  }
  // If tiers are the same and it's an Extensional tier, compare the bounds
  else if constexpr (std::is_base_of_v<Extensional, L>) {
    return lhs.bound < rhs.bound;
  }
  // Otherwise, they are the same symbolic infinite cardinal
  return false;
}

// 3. Less-Than-or-Equal (Subset cardinality check)
export template <IsCardinality L, IsCardinality R>
constexpr bool operator<=(const L& lhs, const R& rhs) {
  return (lhs == rhs) || (lhs < rhs);
}

// 1. Not Equal
export template <IsCardinality L, IsCardinality R>
constexpr bool operator!=(const L& lhs, const R& rhs) {
  return !(lhs == rhs);
}

// 2. Greater Than
export template <IsCardinality L, IsCardinality R>
constexpr bool operator>(const L& lhs, const R& rhs) {
  return rhs < lhs;
}

// 3. Greater Than or Equal
export template <IsCardinality L, IsCardinality R>
constexpr bool operator>=(const L& lhs, const R& rhs) {
  return rhs <= lhs;
}

}  // namespace dedekind::sets
