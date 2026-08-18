/**
 * @file dedekind/algebra/tropical.cppm
 * @partition :tropical
 * @brief The tropical semiring @c Tropical<T> = T ∪ {∞}, a dioid.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section tropical__Overview
 * @c Tropical<T, Max> layers over a totally-ordered monoid @c T: the
 * multiplication @c ⊗ is @c T's own @c + (exposed as @c operator+), and the
 * addition @c ⊕ is the order-extremum (@c max when @c Max, else @c min),
 * with the sentinel @c ∞ as its identity (@c −∞ for max, @c +∞ for min).  It
 * is an idempotent semiring --- a @b dioid --- since @c ⊕ is a semilattice.
 * @c MaxPlus / @c MinPlus alias the two directions; @c MaxPlus (longest path)
 * is the critical-path carrier.
 *
 * The multiplication is the naturals' addition (Maslov dequantization:
 * @c ×↦+, @c +↦max, @c 1↦0, @c 0↦−∞); @c ℕ and @c Tropical branch on the
 * idempotence of @c ⊕.
 */
module;

#include <concepts>
#include <functional>

export module dedekind.algebra:tropical;

import dedekind.category; // trait registry + IsSemiring, is_idempotent_v

namespace dedekind::algebra {

export struct TropicalPlus;  // ⊕ (defined below; only its identity is used
                             // here)

/**
 * @brief Tropical carrier @c T ∪ {∞}.  @c finite==false is the @c ⊕-identity
 *        sentinel (@c −∞ for @c Max, @c +∞ for @c Min), absorbing under @c ⊗.
 *        @c operator+ is @c ⊗ (honest addition); @c ⊕ is @ref TropicalPlus.
 */
export template <typename T, bool Max = true>
struct Tropical {
  bool finite = false;
  T val = T{};

  friend constexpr bool operator==(const Tropical&, const Tropical&) = default;

  /** @brief @c ⊗ = @c + (the naturals' addition); @c ∞ absorbs. */
  friend constexpr Tropical operator+(const Tropical& a, const Tropical& b) {
    if (!a.finite || !b.finite) return Tropical{false, T{}};
    return Tropical{true, a.val + b.val};
  }

  static constexpr Tropical of(T v) { return Tropical{true, v}; }

  // Self-declared law traits, discovered by the :species registry.  Only ⊕
  // (TropicalPlus) is idempotent --- the dioid law; ⊗ (+) is not.
  template <typename Op>
  static constexpr bool is_associative_v = true;
  template <typename Op>
  static constexpr bool is_commutative_v = true;
  template <typename Op>
  static constexpr bool is_idempotent_v = std::same_as<Op, TropicalPlus>;
};

/** @brief @c ⊕: the tropical addition, the order-extremum (a semilattice
 *  join for @c Max, meet for @c Min).  Named, not an operator, so @c a+b never
 *  silently means @c max. */
export struct TropicalPlus {
  template <typename T, bool M>
  constexpr Tropical<T, M> operator()(const Tropical<T, M>& a,
                                      const Tropical<T, M>& b) const {
    if (!a.finite) return b;
    if (!b.finite) return a;
    if constexpr (M)
      return Tropical<T, M>{true, a.val < b.val ? b.val : a.val};
    else
      return Tropical<T, M>{true, b.val < a.val ? b.val : a.val};
  }
};

export template <typename T>
using MaxPlus = Tropical<T, true>;
export template <typename T>
using MinPlus = Tropical<T, false>;

}  // namespace dedekind::algebra

// --- Register the tropical algebra with the :species trait registry. ---
namespace dedekind::category {

/** @brief @c ⊕-identity is the @c ∞ sentinel. */
template <typename T, bool M>
struct identity_trait<dedekind::algebra::Tropical<T, M>,
                      dedekind::algebra::TropicalPlus, void> {
  static constexpr dedekind::algebra::Tropical<T, M> value{false, T{}};
};
/** @brief @c ⊗-identity is @c 0. */
template <typename T, bool M>
struct identity_trait<dedekind::algebra::Tropical<T, M>,
                      std::plus<dedekind::algebra::Tropical<T, M>>, void> {
  static constexpr dedekind::algebra::Tropical<T, M> value{true, T{}};
};
/** @brief @c ⊗ is total via the @c ∞-absorbing saturating certificate. */
template <typename T, bool M>
struct is_saturating<dedekind::algebra::Tropical<T, M>,
                     std::plus<dedekind::algebra::Tropical<T, M>>>
    : std::true_type {};
/** @brief @c ⊗ distributes over @c ⊕. */
template <typename T, bool M>
inline constexpr bool
    is_distributive_v<dedekind::algebra::Tropical<T, M>,
                      std::plus<dedekind::algebra::Tropical<T, M>>,
                      dedekind::algebra::TropicalPlus> = true;

}  // namespace dedekind::category

namespace dedekind::algebra {

/**
 * @concept IsTropical
 * @brief A dioid: an @c IsSemiring whose additive monoid @c Add is idempotent
 *        (a semilattice).  This is the algebraic gate for the tropical
 *        carriers, refining @c IsSemiring exactly by @c ⊕-idempotence.
 */
export template <typename S, typename Add, typename Mult>
concept IsTropical = dedekind::category::IsSemiring<S, Add, Mult> &&
                     dedekind::category::is_idempotent_v<S, Add>;

/**
 * @brief Canonical semiring operations of a carrier, so combinators can
 *        default their @c Add / @c Mult without a call site spelling them.
 *        @c Tropical → @c (⊕ = TropicalPlus, ⊗ = +); @c bool → @c (∨, ∧).
 */
export template <typename S>
struct semiring_ops;

template <typename T, bool M>
struct semiring_ops<Tropical<T, M>> {
  using add = TropicalPlus;
  using mult = std::plus<Tropical<T, M>>;
};
template <>
struct semiring_ops<bool> {
  using add = std::logical_or<bool>;
  using mult = std::logical_and<bool>;
};

// Cross-partition invariant: the tropical carrier is a certified dioid.
static_assert(IsTropical<MaxPlus<long long>, TropicalPlus,
                         std::plus<MaxPlus<long long>>>);

}  // namespace dedekind::algebra
