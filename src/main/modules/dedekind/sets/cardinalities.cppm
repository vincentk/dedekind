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
concept IsCardinality = std::is_base_of_v<CardinalityBase, C> && requires(C c) {
  { power(c) } -> std::derived_from<CardinalityBase>;
};

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

// Tier 2: Finite sets that are too big for a CPU register (> 2^64)
// e.g. P(N8) = 2^256
export struct LargeFinite : public Finite {
  constexpr LargeFinite() : Finite() {}
};

// Symbolic bound for a space of N bits (e.g., 2^32, 2^64)
export template <std::size_t N>
struct ℕ : public LargeFinite {
  static constexpr std::size_t bits = N;
  constexpr ℕ() : LargeFinite() {}
};

export using ℕ1 = ℕ<1>;
export using ℕ8 = ℕ<8>;
export using ℕ32 = ℕ<32>;
export using ℕ64 = ℕ<64>;

// Link Extensional to the native pointer width (usually ℕ64)
export using PlatformNative = ℕ<sizeof(std::size_t) * 8>;

// 2. Extensional (The Bounded)
export struct Extensional : public PlatformNative {
  std::size_t bound;
  explicit constexpr Extensional(std::size_t n) : PlatformNative(), bound(n) {}
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

template <typename T>
consteval int tier() {
  if constexpr (std::is_same_v<T, Empty>) return 0;
  if constexpr (std::is_base_of_v<Extensional, T>) return 1;

  // Machine Tier: Any type with a 'bits' member (ℕ<N>)
  if constexpr (requires { T::bits; }) return 2;

  if constexpr (std::is_base_of_v<Finite, T>) return 3;
  if constexpr (std::is_base_of_v<ℵ_0, T>) return 4;
  if constexpr (std::is_base_of_v<Uncountable, T>) return 5;
  return 6;
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

// Intersection (&): The smaller bit-width restricts the space
export template <std::size_t N, std::size_t M>
constexpr ℕ<(N < M ? N : M)> operator&(ℕ<N>, ℕ<M>) {
  return {};
}

// Union (|): The larger bit-width absorbs the smaller
export template <std::size_t N, std::size_t M>
constexpr ℕ<(N > M ? N : M)> operator|(ℕ<N>, ℕ<M>) {
  return {};
}

// Mixed Math: Extensional vs Machine Symbol
// Theorem: A specific value bound always loses to a bit-width symbol
export template <std::size_t N>
constexpr Extensional operator&(Extensional e, ℕ<N>) {
  return e;
}

export template <std::size_t N>
constexpr ℕ<N> operator|(Extensional, ℕ<N> n) {
  return n;
}

// --- Union Logic ---
// N64 | N64 = N64 (Identical spaces merge)
// N64 | LargeFinite = LargeFinite (The larger bound absorbs)
export constexpr LargeFinite operator|(ℕ64, LargeFinite) {
  return LargeFinite();
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
// --- Cartesian Product Logic ---
// N64 * N64 = 2^128 -> Strictly LargeFinite
export constexpr LargeFinite operator*(ℕ64, ℕ64) { return LargeFinite(); }

// --- Cardinality Successors (2^S) for Power Sets ---

// 2^0 = 1
export constexpr Extensional power(Empty) { return Extensional(1); }

// 2^n = Extensional bound
export constexpr Extensional power(Extensional e) {
  // Note: We use 1ULL to ensure 64-bit math for the bound
  return Extensional(1ULL << e.bound);
}

// 2^Finite = Finite (We know it's finite, but bound is unknown)
export constexpr Finite power(Finite) { return Finite(); }

// --- Machine Cardinality Successors ---

// 2^ℕ8 (2^256) -> Fits in a LargeFinite symbolic type
export constexpr LargeFinite power(ℕ8) { return LargeFinite(); }

// 2^ℕ32 -> LargeFinite
export constexpr LargeFinite power(ℕ32) { return LargeFinite(); }

// 2^ℕ64 (2^(2^64)) -> LargeFinite
// This is the "Theorem" that machine types eventually "overflow"
// into symbolic finiteness.
export constexpr LargeFinite power(ℕ64) { return LargeFinite(); }

// 2^LargeFinite -> LargeFinite (Symbolic closure)
export constexpr LargeFinite power(LargeFinite) { return LargeFinite(); }

// 2^ℵ₀ = ℶ₁ (The jump from Countable to Continuum)
export constexpr ℶ_1 power(Countable) { return ℶ_1(); }

// 2^ℶ_n = ℶ_{n+1} (Successive Power Sets)
export template <std::size_t N>
constexpr ℶ<N + 1> power(ℶ<N>) {
  return ℶ<N + 1>();
}

// 2^ℵ_n = ℶ_{n+1} (Generalized jump)
export template <std::size_t N>
constexpr ℶ<N + 1> power(ℵ<N>) {
  return ℶ<N + 1>();
}

// Theorem: Arithmetic of Sizes (Subtraction/Relative Complement)
export template <IsCardinality L, IsCardinality R>
constexpr auto operator-(const L& l, const R& r) {
  if constexpr (std::is_base_of_v<Extensional, L> &&
                std::is_base_of_v<Extensional, R>) {
    // Finite math: Clamp at zero
    return Extensional(l.bound > r.bound ? l.bound - r.bound : 0);
  } else {
    // Transfinite math: For an MVP, we assume Infinity - X is still Infinity
    // (This is the standard "absorption" logic for cardinal estimates)
    return l;
  }
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

/** @brief Theorem: ℵ_0 + n = ℵ_0 */
export template <std::size_t Bits>
constexpr ℕ<Bits> operator+(ℕ<Bits> n, Extensional) {
  return n;
}

/** @brief Symmetric version */
export template <std::size_t Bits, typename T>
constexpr ℕ<Bits> operator+(Extensional, ℕ<Bits> n) {
  return n;
}

/** @brief ℵ_0 + ℵ_0 = ℵ_0 */
export template <std::size_t Bits>
constexpr ℕ<Bits> operator+(ℕ<Bits> n, ℕ<Bits> /*m*/) {
  return n;
}

/** @brief Theorem: Finite + Finite = Finite (Upper bound: |A|*|B|) */
export constexpr Extensional operator+(Extensional a, Extensional b) {
  // Note: We use the product because in the worst case (all sums unique),
  // the Minkowski sum has a size up to the product of the sizes.
  return Extensional(a.bound * b.bound);
}

/** @brief Theorem: Uncountable + Anything = Uncountable */
export constexpr Uncountable operator+(Uncountable u, auto /*any*/) {
  return u;
}

}  // namespace dedekind::sets
