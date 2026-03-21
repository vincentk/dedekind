module;

#include <compare>
#include <concepts>
#include <ranges>
#include <type_traits>

export module dedekind.sets:cardinalities;
import dedekind.ontology;

namespace dedekind::sets {
using ontology::IsCardinality;

// Tier 2: Finite sets that are too big for a CPU register (> 2^64)
// e.g. P(N8) = 2^256
export struct LargeFinite {
  static constexpr bool is_countable = true;
  static constexpr bool is_finite = true;
};

// Symbolic bound for a space of N bits (e.g., 2^32, 2^64)
// 1. The General Case (N < 64)
template <size_t N>
struct ℕ {
  static constexpr size_t value = N;
  static constexpr bool is_countable = true;
  static constexpr bool is_finite = true;

  constexpr std::partial_ordering operator<=>(const ℕ&) const = default;

  // We only calculate 2^N if it fits in a size_t
  using power_type = ℕ<(1ULL << N)>;
};

// 2. The Specialization (The "Hardware Horizon")
// This stops the infinite shift and jumps to the next species.
template <>
struct ℕ<64> {
  static constexpr size_t value = 64;
  static constexpr bool is_countable = true;
  static constexpr bool is_finite = true;

  constexpr std::partial_ordering operator<=>(const ℕ&) const = default;

  // THE JUMP: The power set of 2^64 is still countably finite
  // but no longer with a known finite representation.
  using power_type = LargeFinite;
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
  static constexpr bool is_countable = true;
  static constexpr bool is_finite = true;
  constexpr std::partial_ordering operator<=>(const Extensional& other) const =
      default;

  explicit constexpr Extensional(std::size_t n) : PlatformNative(), bound(n) {}
};

// Convenience for the additive identity
export struct Zero : public Extensional {
  constexpr Zero() : Extensional(0) {}
};

// 1. Finite * Finite = Finite (Product of bounds)
export template <IsCardinality L, IsCardinality R>
constexpr auto operator*(L l, R r)
  requires(std::is_base_of_v<Extensional, L> &&
           std::is_base_of_v<Extensional, R>)
{
  return Extensional(l.bound * r.bound);
}

// For Cartesian products, the rule is: Finite * Finite = Product, but
// Anything
// * Infinite = Infinite Max.
export template <IsCardinality L, IsCardinality R>
constexpr auto operator*(L l, R r) {
  // 1. Both Finite: Numerical product
  if constexpr (tier<L>() == 0 && tier<R>() == 0) {
    return Extensional(l.bound * r.bound);
  }
  // 2. At least one is Infinite: The higher tier (max) wins
  else {
    return higher_val<L, R>(l, r);
  }
}
// --- Cartesian Product Logic ---
// N64 * N64 = 2^128 -> Strictly LargeFinite
export constexpr LargeFinite operator*(ℕ64, ℕ64) { return LargeFinite(); }

// --- Cardinality Successors (2^S) for Power Sets ---

// 2^n = Extensional bound
export constexpr Extensional power(Extensional e) {
  // Note: We use 1ULL to ensure 64-bit math for the bound
  return Extensional(1ULL << e.bound);
}

// 2^Finite = Finite (We know it's finite, but bound is unknown)
// export constexpr Finite power(Finite) { return Finite(); }

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

}  // namespace dedekind::sets
