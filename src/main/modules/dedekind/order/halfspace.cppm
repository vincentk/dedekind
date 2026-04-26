/**
 * @file dedekind/order/halfspace.cppm
 * @partition :halfspace
 * @brief Level 1.5b: Compile-time halfspace predicates on ordered carriers.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Halfspaces_As_Types
 * Structured compile-time halfspace predicates over an ordered carrier. The
 * pivot is carried in the predicate's TYPE as a non-type template parameter,
 * which is what lets `(n > bound<5>) && (n < bound<3>)` collapse structurally
 * to an empty predicate at compile time. Contrast with the lambda-returning
 * variable operators in `dedekind.sets`, which erase the pivot into a closure.
 *
 * @section DSL_Surface (paper-facing)
 *
 *     inline constexpr auto n = var<ℕ>;
 *     inline constexpr auto big   = Set{n % N | (n > bound<5>)};
 *     inline constexpr auto small = Set{n % N | (n < bound<3>)};
 *     // (big ∩ small) = ∅  — witnessed at compile time via structured_and
 *
 * Wikipedia: Half-space (geometry), Separating hyperplane theorem,
 * Non-type template parameter
 *
 * @note "Was beweisbar ist, soll in der Wissenschaft nicht ohne Beweis
 *       geglaubt werden."
 *       — Richard Dedekind, *Was sind und was sollen die Zahlen?* (1888),
 *         Vorwort.
 *       [Trans: "What is provable should not be believed without proof
 *       in science."]
 */
module;
#include <algorithm>
#include <concepts>
#include <cstddef>
#include <utility>

export module dedekind.order:halfspace;

import dedekind.category;
import dedekind.sets;

namespace dedekind::order {
using namespace dedekind::sets;
using namespace dedekind::category;

/**
 * @concept IsRingIntegral
 * @brief Carrier types that admit integer-range arithmetic semantics.
 *
 * @details Generalises @c std::integral to also recognise the project's
 * variant ℕ-/ℤ-proxy carriers (@c Cardinality, @c SignedCardinality
 * from PR #396 / @c sets:cardinality).  The concept names "carriers
 * for which @c OrderInterval can compute a finite cardinality from
 * compile-time bounds and strictness pairs" — the integer-range
 * reading.
 *
 * Pre-#414, @c OrderInterval gated this surface as @c is_integer_range
 * @c = @c std::integral<T>, which excluded the variant carriers (they
 * are @c std::variant<...>, not built-in integral types) even though
 * they semantically satisfy the integer-range reading.  Post-#414,
 * @c OrderInterval gates on @c IsRingIntegral<T>, so the upcoming
 * @c ℕ @c = @c Cardinality / @c ℤ @c = @c SignedCardinality retarget
 * (#402) keeps the existing showcases (6, 7, 8) compiling without
 * losing the @c size() / @c lower_pivot / @c upper_pivot surface on
 * the variant carriers.
 *
 * Floating-point carriers (@c float / @c double / @c Real<Q>) are
 * @b not admitted: their range cardinalities are uncountable in the
 * abstract reading and lossy under IEEE rounding in the operational
 * reading; @c OrderInterval over a continuous carrier correctly
 * returns @c cardinality_type @c = @c ℵ_0.
 *
 * Sibling of the @c HasRingOperators / @c IsRing pattern from PR #394:
 * @b shape concept rather than axiom.  No claim about closure under
 * arithmetic, additive inverses, or strict ring laws is made here —
 * only that the carrier reads as an integer-magnitude domain for
 * cardinality-counting purposes.
 */
export template <typename T>
concept IsRingIntegral =
    std::integral<T> ||
    std::same_as<T, dedekind::sets::Cardinality> ||
    std::same_as<T, dedekind::sets::SignedCardinality>;

/** @section Formal_Verification (IsRingIntegral) */

// Positive witnesses: built-in integrals + the variant ℕ-/ℤ-proxy carriers.
static_assert(IsRingIntegral<int>);
static_assert(IsRingIntegral<unsigned int>);
static_assert(IsRingIntegral<long>);
static_assert(IsRingIntegral<long long>);
static_assert(IsRingIntegral<std::size_t>);
static_assert(IsRingIntegral<bool>);
static_assert(IsRingIntegral<dedekind::sets::Cardinality>,
              "Cardinality must satisfy IsRingIntegral — the variant ℕ-proxy "
              "is the canonical exact-ℕ integer-range carrier (post-#414).");
static_assert(IsRingIntegral<dedekind::sets::SignedCardinality>,
              "SignedCardinality must satisfy IsRingIntegral — the variant "
              "ℤ-proxy is the canonical exact-ℤ integer-range carrier "
              "(post-#414).");

// Negative witnesses: continuous / non-integer carriers correctly refused.
static_assert(!IsRingIntegral<double>);
static_assert(!IsRingIntegral<float>);
static_assert(!IsRingIntegral<long double>);

/** @brief Orientation of a halfspace along the chain. */
export enum class Direction { Upward, Downward };

/** @brief Whether the boundary is strict (`>`, `<`) or inclusive (`>=`, `<=`).
 */
export enum class Strictness { Strict, NonStrict };

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
export template <typename T, auto Pivot, Direction D, Strictness S,
                 typename L = ClassicalLogic>
struct Halfspace {
  using Domain = T;
  using Codomain = typename L::Ω;
  using logic_species = L;

  static constexpr auto pivot = Pivot;
  static constexpr Direction direction = D;
  static constexpr Strictness strictness = S;

  // `Pivot` may be a different structural type than `T` (e.g., pivot = 5.0 as
  // double, T = Real<double>). The carrier's converting ctor / overload set
  // handles the comparison; we only assume `T` is comparable with the pivot.
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
  using is_compile_time_extensional_tag = void;
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
export template <typename T, auto Lo, auto Hi, Strictness SL, Strictness SU,
                 typename L = ClassicalLogic>
struct OrderInterval {
  using Domain = T;
  using Codomain = typename L::Ω;
  using logic_species = L;

  static constexpr auto lower_pivot = Lo;
  static constexpr auto upper_pivot = Hi;
  static constexpr Strictness lower_strictness = SL;
  static constexpr Strictness upper_strictness = SU;

  constexpr Codomain operator()(const T& x) const {
    const bool lo_ok = (SL == Strictness::Strict) ? (x > Lo) : (x >= Lo);
    const bool hi_ok = (SU == Strictness::Strict) ? (x < Hi) : (x <= Hi);
    return (lo_ok && hi_ok) ? L::True : L::False;
  }

  // For integer-range carriers, cardinality is compile-time-decidable
  // from the bounds and strictness pair.  Gate the size() / cardinality_type
  // surface so that continuous carriers (like Real<double>) correctly fail
  // IsFiniteSet, AND so that the variant ℕ-/ℤ-proxy carriers from
  // sets:cardinality (Cardinality, SignedCardinality) keep this surface
  // post-#402 retarget.  The IsRingIntegral concept (above) is the
  // post-#414 generalisation of std::integral — same semantics for the
  // built-in integers, plus admission of the variant carriers.
  static constexpr bool is_integer_range = IsRingIntegral<T>;

  constexpr std::size_t size() const
    requires is_integer_range
  {
    constexpr bool lo_open = (SL == Strictness::Strict);
    constexpr bool hi_open = (SU == Strictness::Strict);
    constexpr auto span = Hi - Lo + (lo_open ? 0 : 1) + (hi_open ? -1 : 0);
    return span > 0 ? static_cast<std::size_t>(span) : 0u;
  }

  // Advertise Finite only when the cardinality is computable.
  using cardinality_type = std::conditional_t<is_integer_range, Finite, ℵ_0>;
};

/** @section Halfspace_Variable_DSL — Variable<S> × Bound<V> → Halfspace. */

export template <typename Species, auto V>
  requires std::convertible_to<decltype(V),
                               dedekind::sets::element_of_t<Species>> &&
           // Reject negative signed pivots on unsigned carriers: the
           // implicit signed→unsigned conversion would wrap (-1 → UINT_MAX)
           // and produce nonsense semantics (`x > -1` becomes `x > UINT_MAX`,
           // which is False for every reachable x).  Real-valued bounds
           // (e.g. bound<-21.0> on var<ℝ>) remain admissible because
           // signed_integral excludes floating-point types.
           (!std::unsigned_integral<dedekind::sets::element_of_t<Species>> ||
            !std::signed_integral<decltype(V)> || V >= 0)
constexpr auto operator>(const Variable<Species>&, Bound<V>) {
  using T = dedekind::sets::element_of_t<Species>;
  return Halfspace<T, V, Direction::Upward, Strictness::Strict>{};
}

export template <typename Species, auto V>
  requires std::convertible_to<decltype(V),
                               dedekind::sets::element_of_t<Species>> &&
           // Reject negative signed pivots on unsigned carriers: the
           // implicit signed→unsigned conversion would wrap (-1 → UINT_MAX)
           // and produce nonsense semantics (`x > -1` becomes `x > UINT_MAX`,
           // which is False for every reachable x).  Real-valued bounds
           // (e.g. bound<-21.0> on var<ℝ>) remain admissible because
           // signed_integral excludes floating-point types.
           (!std::unsigned_integral<dedekind::sets::element_of_t<Species>> ||
            !std::signed_integral<decltype(V)> || V >= 0)
constexpr auto operator>=(const Variable<Species>&, Bound<V>) {
  using T = dedekind::sets::element_of_t<Species>;
  return Halfspace<T, V, Direction::Upward, Strictness::NonStrict>{};
}

export template <typename Species, auto V>
  requires std::convertible_to<decltype(V),
                               dedekind::sets::element_of_t<Species>> &&
           // Reject negative signed pivots on unsigned carriers: the
           // implicit signed→unsigned conversion would wrap (-1 → UINT_MAX)
           // and produce nonsense semantics (`x > -1` becomes `x > UINT_MAX`,
           // which is False for every reachable x).  Real-valued bounds
           // (e.g. bound<-21.0> on var<ℝ>) remain admissible because
           // signed_integral excludes floating-point types.
           (!std::unsigned_integral<dedekind::sets::element_of_t<Species>> ||
            !std::signed_integral<decltype(V)> || V >= 0)
constexpr auto operator<(const Variable<Species>&, Bound<V>) {
  using T = dedekind::sets::element_of_t<Species>;
  return Halfspace<T, V, Direction::Downward, Strictness::Strict>{};
}

export template <typename Species, auto V>
  requires std::convertible_to<decltype(V),
                               dedekind::sets::element_of_t<Species>> &&
           // Reject negative signed pivots on unsigned carriers: the
           // implicit signed→unsigned conversion would wrap (-1 → UINT_MAX)
           // and produce nonsense semantics (`x > -1` becomes `x > UINT_MAX`,
           // which is False for every reachable x).  Real-valued bounds
           // (e.g. bound<-21.0> on var<ℝ>) remain admissible because
           // signed_integral excludes floating-point types.
           (!std::unsigned_integral<dedekind::sets::element_of_t<Species>> ||
            !std::signed_integral<decltype(V)> || V >= 0)
constexpr auto operator<=(const Variable<Species>&, Bound<V>) {
  using T = dedekind::sets::element_of_t<Species>;
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
export template <typename T, auto Lo, auto Hi, Strictness SL, Strictness SU,
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
    constexpr auto span = Hi - Lo + (lo_open ? 0 : 1) + (hi_open ? -1 : 0);
    if constexpr (span == 1) {
      // Unique inhabitant: the smallest x admitted by the lower boundary.
      // static_cast rather than brace-init to permit real-valued bounds on an
      // integer carrier (e.g. `bound<-21.0>` on `var<ℤ>`).
      constexpr T unique =
          lo_open ? static_cast<T>(Lo + 1) : static_cast<T>(Lo);
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
export template <typename T, auto Hi, auto Lo, Strictness SU, Strictness SL,
                 typename L>
constexpr auto structured_and(Halfspace<T, Hi, Direction::Downward, SU, L>,
                              Halfspace<T, Lo, Direction::Upward, SL, L>) {
  return structured_and(Halfspace<T, Lo, Direction::Upward, SL, L>{},
                        Halfspace<T, Hi, Direction::Downward, SU, L>{});
}

/** @brief Same-direction upward meet: the stricter pivot wins. */
export template <typename T, auto P1, auto P2, Strictness S1, Strictness S2,
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
export template <typename T, auto P1, auto P2, Strictness S1, Strictness S2,
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

/** @section Interval_Cartesian_Product — 2D structural products. */

/**
 * @brief Cartesian product of two reduced extensional structures (typically
 * `OrderInterval`s on integer carriers). Preserves size / logic / tags so the
 * 2D product participates in the same computability classification as the
 * 1D factors: `IsFiniteSet<IntervalProduct<I1, I2>>` holds whenever each
 * factor satisfies `IsFiniteSet`.
 */
export template <typename A, typename B>
  requires std::same_as<typename A::logic_species, typename B::logic_species>
struct IntervalProduct {
  A a;
  B b;

  using Domain = std::pair<typename A::Domain, typename B::Domain>;
  using Codomain = typename A::Codomain;
  using logic_species = typename A::logic_species;
  using is_extensional_tag = void;

  // Cardinality is only finite when both factors are — for a product whose
  // factors include a non-integral `OrderInterval` (cardinality ℵ_0), the
  // product is likewise transfinite.
  using cardinality_type = std::conditional_t<requires {
    typename A::cardinality_type;
    typename B::cardinality_type;
    requires std::same_as<typename A::cardinality_type, Finite>;
    requires std::same_as<typename B::cardinality_type, Finite>;
  }, Finite, ℵ_0>;

  constexpr Codomain operator()(const Domain& p) const {
    using L = logic_species;
    return (a(p.first) == L::True && b(p.second) == L::True) ? L::True
                                                             : L::False;
  }

  // `size()` is only available when both factors expose a `size()` returning
  // convertible-to-`std::size_t`. This keeps the API honest for continuous
  // factors (attempting `.size()` on a product of real-valued intervals is a
  // compile error, not a silent nonsense).
  constexpr std::size_t size() const
    requires requires(const A& factor_a, const B& factor_b) {
      { factor_a.size() } -> std::convertible_to<std::size_t>;
      { factor_b.size() } -> std::convertible_to<std::size_t>;
    }
  {
    return a.size() * b.size();
  }
};

/** @brief Infix `*` on two `OrderInterval`s → structural `IntervalProduct`. */
export template <typename T1, auto Lo1, auto Hi1, Strictness SL1,
                 Strictness SU1, typename L1, typename T2, auto Lo2, auto Hi2,
                 Strictness SL2, Strictness SU2, typename L2>
  requires std::same_as<L1, L2>
constexpr auto operator*(OrderInterval<T1, Lo1, Hi1, SL1, SU1, L1> a,
                         OrderInterval<T2, Lo2, Hi2, SL2, SU2, L2> b) {
  return IntervalProduct<decltype(a), decltype(b)>{a, b};
}

}  // namespace dedekind::order
