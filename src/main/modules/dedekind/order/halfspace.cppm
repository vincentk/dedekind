/**
 * @file dedekind/order/halfspace.cppm
 * @partition :halfspace
 * @brief Level 1.5b: Compile-time halfspace predicates on ordered carriers.
 *
 * Structured compile-time halfspace predicates over an ordered carrier. The
 * pivot is carried in the predicate's TYPE as a non-type template parameter,
 * which is what lets `(n > bound<5>) && (n < bound<3>)` collapse structurally
 * to an empty predicate at compile time. Contrast with the lambda-returning
 * variable operators in `dedekind.sets`, which erase the pivot into a closure.
 *
 * DSL surface (paper-facing):
 *
 *   inline constexpr auto n = var<ℕ>;
 *   inline constexpr auto big   = Set{n % N | (n > bound<5>)};
 *   inline constexpr auto small = Set{n % N | (n < bound<3>)};
 *   // (big ∩ small) = ∅  — witnessed at compile time via structured_and
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */
module;
#include <algorithm>
#include <concepts>
#include <cstddef>

export module dedekind.order:halfspace;

import dedekind.category;
import dedekind.sets;

namespace dedekind::order {
using namespace dedekind::sets;
using namespace dedekind::category;

/** @brief Orientation of a halfspace along the chain (internal). */
enum class Direction { Upward, Downward };

/** @brief Whether the boundary is strict (`>`, `<`) or inclusive (`>=`, `<=`).
 */
enum class Strictness { Strict, NonStrict };

/** @brief Compile-time bound tag: `bound<5>` carries `5` in its type. */
template <auto V>
struct Bound {
  using value_type = decltype(V);
  static constexpr value_type value = V;
};

/** @brief Variable-template factory for compile-time bounds. */
export template <auto V>
inline constexpr Bound<V> bound{};

/**
 * @brief Halfspace predicate { x ∈ T | x ⋈ Pivot } with Pivot at the type
 * level.
 *
 * `⋈` ∈ { >, >=, <, <= }, selected by `D` (direction) and `S` (strictness).
 */
export template <typename T, T Pivot, Direction D, Strictness S,
                 typename L = ClassicalLogic>
struct Halfspace {
  using Domain = T;
  using Codomain = typename L::Ω;
  using logic_species = L;

  static constexpr T pivot = Pivot;
  static constexpr Direction direction = D;
  static constexpr Strictness strictness = S;

  constexpr Codomain operator()(const T& x) const {
    if constexpr (D == Direction::Upward) {
      const bool hit = (S == Strictness::Strict) ? (x > Pivot) : (x >= Pivot);
      return hit ? L::True : L::False;
    } else {
      const bool hit = (S == Strictness::Strict) ? (x < Pivot) : (x <= Pivot);
      return hit ? L::True : L::False;
    }
  }
};

/**
 * @brief Compile-time singleton predicate: `{x : decltype(Value) | x ==
 * Value}`.
 *
 * Emitted when a halfspace meet on a discrete (integral) carrier is reduced
 * by cardinality analysis to exactly one inhabitant. The value lives in the
 * TYPE, so `Singleton<4>` and `Singleton<7>` are distinct types — the
 * compiler proves `{n | 3<n<5} = {4}` by structural pattern matching.
 *
 * L defaults to `ClassicalLogic` because a cardinality-1 extensional set
 * has decidable membership regardless of ambient logic species.
 */
export template <auto Value, typename L = ClassicalLogic>
struct Singleton {
  using Domain = decltype(Value);
  using Codomain = typename L::Ω;
  using logic_species = L;
  using cardinality_type = Finite;
  using is_extensional_tag = void;
  using is_static_singleton_tag =
      void;  // For Set::operator& collapse detection

  static constexpr Domain value = Value;

  constexpr Codomain operator()(const Domain& x) const {
    return (x == Value) ? L::True : L::False;
  }

  constexpr std::size_t size() const { return 1; }

  // Cross-logic identity: `Singleton<V, L1>` and `Singleton<V, L2>` represent
  // the same singleton; enables the reveal `s == Singleton<V>{}` when s's
  // logic species was inherited from a Set (e.g. TernaryLogic over ℕ).
  template <typename OtherL>
  constexpr bool operator==(const Singleton<Value, OtherL>&) const {
    return true;
  }
};

/** @brief Meet of two opposing halfspaces — an order-theoretic interval. */
export template <typename T, T Lo, T Hi, Strictness SL, Strictness SU,
                 typename L = ClassicalLogic>
struct OrderInterval {
  using Domain = T;
  using Codomain = typename L::Ω;
  using logic_species = L;

  static constexpr T lower_pivot = Lo;
  static constexpr T upper_pivot = Hi;

  constexpr Codomain operator()(const T& x) const {
    const bool lo_ok = (SL == Strictness::Strict) ? (x > Lo) : (x >= Lo);
    const bool hi_ok = (SU == Strictness::Strict) ? (x < Hi) : (x <= Hi);
    return (lo_ok && hi_ok) ? L::True : L::False;
  }
};

/** @section Halfspace_Variable_DSL — Variable<S> × Bound<V> → Halfspace. */

export template <typename Species, auto V>
  requires std::same_as<typename Species::Domain, decltype(V)>
constexpr auto operator>(const Variable<Species>&, Bound<V>) {
  using T = typename Species::Domain;
  return Halfspace<T, V, Direction::Upward, Strictness::Strict>{};
}

export template <typename Species, auto V>
  requires std::same_as<typename Species::Domain, decltype(V)>
constexpr auto operator>=(const Variable<Species>&, Bound<V>) {
  using T = typename Species::Domain;
  return Halfspace<T, V, Direction::Upward, Strictness::NonStrict>{};
}

export template <typename Species, auto V>
  requires std::same_as<typename Species::Domain, decltype(V)>
constexpr auto operator<(const Variable<Species>&, Bound<V>) {
  using T = typename Species::Domain;
  return Halfspace<T, V, Direction::Downward, Strictness::Strict>{};
}

export template <typename Species, auto V>
  requires std::same_as<typename Species::Domain, decltype(V)>
constexpr auto operator<=(const Variable<Species>&, Bound<V>) {
  using T = typename Species::Domain;
  return Halfspace<T, V, Direction::Downward, Strictness::NonStrict>{};
}

/** @section Halfspace_Structural_Algebra — ADL hooks for operator&&. */

/**
 * @brief Intersection of an upward and a downward halfspace.
 *
 * Three-way reduction, evaluated at compile time on the NTTP pivots:
 *   1. disjoint       → `EmptyPredicate<T>` (Lo, Hi straddle no T)
 *   2. exactly one T  → `Singleton<unique, L>` (only for integral T)
 *   3. otherwise      → `OrderInterval<T, Lo, Hi, SL, SU, L>`
 *
 * The cardinality formula over integral T, by strictness pair:
 *   strict/strict         : Hi - Lo - 1
 *   strict/non-strict     : Hi - Lo
 *   non-strict/strict     : Hi - Lo
 *   non-strict/non-strict : Hi - Lo + 1
 *
 * …clamped at 0. Cardinality 0 is the empty case; cardinality 1 picks out
 * the unique inhabitant and elevates the meet to a `Singleton`.
 */
export template <typename T, T Lo, T Hi, Strictness SL, Strictness SU,
                 typename L>
constexpr auto structured_and(Halfspace<T, Lo, Direction::Upward, SL, L>,
                              Halfspace<T, Hi, Direction::Downward, SU, L>) {
  constexpr bool either_strict =
      (SL == Strictness::Strict) || (SU == Strictness::Strict);
  constexpr bool disjoint = either_strict ? (Lo >= Hi) : (Lo > Hi);
  if constexpr (disjoint) {
    return EmptyPredicate<T>{};
  } else if constexpr (std::integral<T>) {
    // Cardinality of {x : T | Lo ⋈ x ⋈ Hi} over integral T.
    constexpr bool lo_open = (SL == Strictness::Strict);
    constexpr bool hi_open = (SU == Strictness::Strict);
    constexpr T span =
        Hi - Lo + (lo_open ? T{0} : T{1}) + (hi_open ? T{-1} : T{0});
    if constexpr (span == T{1}) {
      // Unique inhabitant: the smallest x admitted by the lower boundary.
      constexpr T unique = lo_open ? T{Lo + 1} : Lo;
      return Singleton<unique, L>{};
    } else {
      return OrderInterval<T, Lo, Hi, SL, SU, L>{};
    }
  } else {
    return OrderInterval<T, Lo, Hi, SL, SU, L>{};
  }
}

/** @brief Symmetric case: downward ∩ upward → delegate to the canonical order.
 */
export template <typename T, T Hi, T Lo, Strictness SU, Strictness SL,
                 typename L>
constexpr auto structured_and(Halfspace<T, Hi, Direction::Downward, SU, L>,
                              Halfspace<T, Lo, Direction::Upward, SL, L>) {
  return structured_and(Halfspace<T, Lo, Direction::Upward, SL, L>{},
                        Halfspace<T, Hi, Direction::Downward, SU, L>{});
}

/** @brief Same-direction upward meet: the stricter pivot wins. */
export template <typename T, T P1, T P2, Strictness S1, Strictness S2,
                 typename L>
constexpr auto structured_and(Halfspace<T, P1, Direction::Upward, S1, L>,
                              Halfspace<T, P2, Direction::Upward, S2, L>) {
  if constexpr (P1 > P2) {
    return Halfspace<T, P1, Direction::Upward, S1, L>{};
  } else if constexpr (P2 > P1) {
    return Halfspace<T, P2, Direction::Upward, S2, L>{};
  } else {
    // Same pivot: stricter strictness wins.
    constexpr Strictness S =
        (S1 == Strictness::Strict || S2 == Strictness::Strict)
            ? Strictness::Strict
            : Strictness::NonStrict;
    return Halfspace<T, P1, Direction::Upward, S, L>{};
  }
}

/** @brief Same-direction downward meet: the stricter pivot wins. */
export template <typename T, T P1, T P2, Strictness S1, Strictness S2,
                 typename L>
constexpr auto structured_and(Halfspace<T, P1, Direction::Downward, S1, L>,
                              Halfspace<T, P2, Direction::Downward, S2, L>) {
  if constexpr (P1 < P2) {
    return Halfspace<T, P1, Direction::Downward, S1, L>{};
  } else if constexpr (P2 < P1) {
    return Halfspace<T, P2, Direction::Downward, S2, L>{};
  } else {
    constexpr Strictness S =
        (S1 == Strictness::Strict || S2 == Strictness::Strict)
            ? Strictness::Strict
            : Strictness::NonStrict;
    return Halfspace<T, P1, Direction::Downward, S, L>{};
  }
}

}  // namespace dedekind::order
