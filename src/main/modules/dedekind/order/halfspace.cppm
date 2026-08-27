/**
 * @file dedekind/order/halfspace.cppm
 * @partition :halfspace
 * @brief Compile-time halfspace predicates on ordered carriers.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section halfspace__Halfspaces_As_Types
 * Structured compile-time halfspace predicates over an ordered carrier. The
 * pivot is carried in the predicate's TYPE as a non-type template parameter,
 * which is what lets `(n > bound<5>) && (n < bound<3>)` collapse structurally
 * to an empty predicate at compile time. Contrast with the lambda-returning
 * scout operators in `dedekind.sets`, which erase the pivot into a closure.
 *
 * @section halfspace__DSL_Surface
 *
 *     inline constexpr auto n = element<Ω<ℕ>>;
 *     inline constexpr auto big   = Set{n | (n > bound<5>)};
 *     inline constexpr auto small = Set{n | (n < bound<3>)};
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
#include <type_traits>
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
namespace detail_isringintegral {
template <typename>
struct is_signed_extensional_cardinal : std::false_type {};
template <std::size_t N>
struct is_signed_extensional_cardinal<
    dedekind::sets::SignedExtensionalCardinal<N>> : std::true_type {};
template <typename>
struct is_extensional_cardinal : std::false_type {};
template <std::size_t N>
struct is_extensional_cardinal<dedekind::sets::ExtensionalCardinal<N>>
    : std::true_type {};
}  // namespace detail_isringintegral

export template <typename T>
concept IsRingIntegral =
    std::integral<std::remove_cvref_t<T>> ||
    std::same_as<std::remove_cvref_t<T>, dedekind::sets::Cardinality> ||
    std::same_as<std::remove_cvref_t<T>, dedekind::sets::SignedCardinality> ||
    detail_isringintegral::is_signed_extensional_cardinal<
        std::remove_cvref_t<T>>::value ||
    detail_isringintegral::is_extensional_cardinal<
        std::remove_cvref_t<T>>::value;

/** @section halfspace__Formal_Verification (IsRingIntegral) */

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
// Note on the SEC<N> / EC<N> extension and OrderInterval::size():
// @c IsRingIntegral<T> gates the @c size() / cardinality surface on
// the @b carrier @c T (the runtime element type), not on the @b NTTP
// @c Hi and @c Lo bound values.  Current usage uniformly supplies
// int-typed NTTPs (e.g.\ @c bound<-21>), so @c span @c = @c Hi @c -
// @c Lo @c + @c ... reduces to int arithmetic and
// @c static_cast<size_t>(span) is well-defined.  If a future spelling
// places SEC-/EC-valued NTTPs in the bound slot (e.g.\ @c bound<SEC<>
// @c {42}>), the @c size() formula would need a multi-limb-aware
// reformulation.  Tracked alongside the SEC<>↔real comparison surface
// in the #399 slice 3 follow-ups (#551).
static_assert(
    IsRingIntegral<dedekind::sets::SignedExtensionalCardinal<>>,
    "SignedExtensionalCardinal<> must satisfy IsRingIntegral — the bounded "
    "exact-ℤ carrier underlying the @c ℤ alias post-#399 slice 3.");
static_assert(IsRingIntegral<dedekind::sets::ExtensionalCardinal<>>,
              "ExtensionalCardinal<> must satisfy IsRingIntegral — the "
              "bounded exact-ℕ carrier (sibling of SEC<> on the unsigned "
              "side).");

// Cv-/ref-qualified spellings: the @c std::remove_cvref_t normalisation
// in the concept body lets @c IsRingIntegral fire in deduced contexts
// (matches the @c dedekind.sets:computability convention, per Copilot
// review on PR #422).
static_assert(IsRingIntegral<const int>);
static_assert(IsRingIntegral<int&>);
static_assert(IsRingIntegral<const dedekind::sets::Cardinality&>);
static_assert(IsRingIntegral<dedekind::sets::SignedCardinality&&>);

// Negative witnesses: continuous / non-integer carriers correctly refused.
static_assert(!IsRingIntegral<double>);
static_assert(!IsRingIntegral<float>);
static_assert(!IsRingIntegral<long double>);

/** @brief Orientation of a halfspace along the chain. */
export enum class Direction { Upward, Downward };

/** @brief Whether the boundary is strict (`>`, `<`) or inclusive (`>=`, `<=`).
 */
export enum class Strictness { Strict, NonStrict };

/** @brief Compile-time bound tag: `bound<5>` carries `5` in its type.
 *
 * Exported (post-#664) so downstream partitions outside `:halfspace`
 * (e.g.\ `:algebra:scout_algebra`) can declare overloads on `Bound<V>`
 * directly; previously only the `bound` variable template was exported,
 * which made cross-partition operator signatures awkward.
 */
export template <auto V>
struct Bound {
  using value_type = decltype(V);
  static constexpr value_type value = V;
};

/** @brief Variable-template factory for compile-time bounds. */
export template <auto V>
inline constexpr Bound<V> bound{};

/**
 * @brief Compile-time integer literal: @c 5_c carries @c 5 in its type as a
 *        @c std::integral_constant.
 *
 * @details The bracket-free way to lift a value to the type level: a
 * user-defined literal encodes the value in the *type* of the returned object
 * with no @c <> in sight.  Paired with @c fix below, so the DSL surface spells
 * a compile-time bound @c fix(5_c) instead of @c bound<5>.  Digits only (the
 * sign is a separate unary @c operator- on the result).
 */
export template <char... Cs>
consteval auto operator""_c() {
  constexpr int v = [] {
    int r = 0;
    ((r = r * 10 + (Cs - '0')), ...);
    return r;
  }();
  return std::integral_constant<int, v>{};
}

/** @brief Named compile-time boolean constants; @c true / @c false are not
 *  literal tokens a user-defined literal can suffix, so @c true_c / @c false_c
 *  are the @c bool analogues of @c 5_c for @c fix(true_c). */
export inline constexpr std::integral_constant<bool, true> true_c{};
export inline constexpr std::integral_constant<bool, false> false_c{};

/**
 * @brief @c fix lifts a compile-time constant to a @c Bound pivot --- the
 *        bracket-free spelling of @c bound<V>, so @c fix(5_c) @b is @c
 * bound<5>.
 *
 * @details The runtime companion @c fix(v) (a value-level bound, for the
 * dynamic / future-Python path) is a separate overload added with the
 * value-carrier machinery; this one is the compile-time, type-level lift.
 */
export template <typename T, T V>
consteval Bound<V> fix(std::integral_constant<T, V>) {
  return {};
}

// The compile-time literal round-trips to the existing bound tag.
static_assert(std::same_as<decltype(fix(5_c)), Bound<5>>,
              "fix(5_c) is the bracket-free spelling of bound<5>.");
static_assert(fix(5_c).value == 5, "5_c carries its value in the type.");
static_assert(std::same_as<decltype(fix(true_c)), Bound<true>>,
              "fix(true_c) is bound<true>, the bool analogue.");

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

  // Subobject inclusion ι: S ↣ T — the missing arrow that makes a bare
  // Halfspace a first-class @c IsSubobject (it already IS its own χ via
  // @c operator() above).  Same Member-unwrap pattern as SingletonSet.
  struct Member {
    T value;
  };
  constexpr T ι(const Member& m) const { return m.value; }
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

  /** @brief Heterogeneous membership query: cross-type @c == against
   *         @c Value.  @c Singleton::Domain is the type of @c Value
   *         (typically @c int when emitted by the post-#402 variant
   *         branch of @c structured_and), but the variant carriers
   *         @c Cardinality / @c SignedCardinality (and any other
   *         cross-type-comparable @c U) need to query membership too.
   *         Routes through the cross-type @c == landed in PR #423 /
   *         #425.  Constrained to @c U distinct from @c Domain so the
   *         non-template overload above wins on exact matches. */
  template <typename U>
    requires(!std::same_as<std::remove_cvref_t<U>, Domain>) &&
            requires(const U& x) {
              { x == Value } -> std::convertible_to<bool>;
            }
  constexpr Codomain operator()(const U& x) const {
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

  // Subobject inclusion ι: {value} ↣ Domain — makes the static Singleton a
  // first-class @c IsSubobject (it already IS its own χ via @c operator()).
  struct Member {
    Domain value;
  };
  constexpr Domain ι(const Member& m) const { return m.value; }
};

/** @section halfspace__Static_Singleton_Complement_Lattice
 *
 * The absorbing laws of the complement lattice, at the type level, for the
 * @b static singleton (value in the type).  These make @c Singleton a
 * first-class member of the lattice the §3 pruning listing exhibits, on a
 * finite carrier: the collapse is structural (never enumerated), and the
 * bool-only gates encode the two facts that hold only on a two-element
 * universe.  Left as free functions, mirroring the @c structured_and surface.
 */

/** @brief Complement of a static singleton on a @b two-element (bool) carrier:
 *         the other singleton.  On a larger carrier the complement of a point
 *         is not a point, so there is deliberately no overload there. */
export template <auto Value, typename L>
  requires std::same_as<decltype(Value), bool>
constexpr auto operator~(const Singleton<Value, L>&) {
  return Singleton<!Value, L>{};
}

/** @brief Meet of two static singletons: the same singleton if the values
 *         coincide, otherwise @c Ø.  Distinct points are disjoint, so the
 *         empty collapse is structural on @b any carrier. */
export template <auto A, typename LA, auto B, typename LB>
  requires std::same_as<decltype(A), decltype(B)>
constexpr auto operator&(const Singleton<A, LA>& a, const Singleton<B, LB>&) {
  if constexpr (A == B) {
    return a;
  } else {
    return dedekind::sets::Ø<decltype(A), LA>{};
  }
}

/** @brief Join of two static singletons: the same singleton if the values
 *         coincide; on a @b two-element (bool) carrier two distinct points
 *         @b cover the universe, so @c UniversalSet.  On a larger carrier the
 *         join is a two-point set, out of scope here, so no overload fires. */
export template <auto A, typename LA, auto B, typename LB>
  requires std::same_as<decltype(A), decltype(B)> &&
           (A == B || std::same_as<decltype(A), bool>)
constexpr auto operator|(const Singleton<A, LA>& a, const Singleton<B, LB>&) {
  if constexpr (A == B) {
    return a;
  } else {
    return dedekind::sets::UniversalSet<bool, LA>{};
  }
}

/** @section halfspace__Halfspace_Complement_Lattice
 *
 * The same complement-lattice surface for the @b halfspace, so a bare
 * @c Halfspace is a first-class @c IsSet lattice member (not only when wrapped
 * in a @c Set): the ℕ column of the §3 pruning listing then reads bare and
 * telling, symmetric with the bool @c Singleton column.  Narrow and gated, so
 * ordinary (non-complement) halfspace pairs still route to @c structured_and /
 * @c OrderInterval unchanged.
 */

// Direction / strictness flips: the pieces of the halfspace complement.
// ~{x > P} = {x <= P} — opposite direction, flipped strictness.  Internal.
constexpr Direction flip(Direction d) {
  return d == Direction::Upward ? Direction::Downward : Direction::Upward;
}
constexpr Strictness flip(Strictness s) {
  return s == Strictness::Strict ? Strictness::NonStrict : Strictness::Strict;
}

/** @brief Complement of a halfspace: the opposite halfspace. */
export template <typename T, auto Pivot, Direction D, Strictness S, typename L>
constexpr auto operator~(const Halfspace<T, Pivot, D, S, L>&) {
  return Halfspace<T, Pivot, flip(D), flip(S), L>{};
}

/** @brief Complement-pair join: same pivot, opposite direction, flipped
 *         strictness is a complement pair whose union is the universe.  The
 *         @c (D1!=D2 && S1!=S2) gate rules out non-complement pairs (they keep
 *         routing to @c structured_and / @c OrderInterval). */
export template <typename T, auto Pivot, Direction D1, Strictness S1,
                 Direction D2, Strictness S2, typename L>
  requires(D1 != D2 && S1 != S2)
constexpr auto operator|(const Halfspace<T, Pivot, D1, S1, L>&,
                         const Halfspace<T, Pivot, D2, S2, L>&) {
  return dedekind::sets::UniversalSet<T, L>{};
}

/** @brief Complement-pair meet: dually, the empty set. */
export template <typename T, auto Pivot, Direction D1, Strictness S1,
                 Direction D2, Strictness S2, typename L>
  requires(D1 != D2 && S1 != S2)
constexpr auto operator&(const Halfspace<T, Pivot, D1, S1, L>&,
                         const Halfspace<T, Pivot, D2, S2, L>&) {
  return dedekind::sets::Ø<T, L>{};
}

/** @brief Telling aliases for the two ℕ halfspaces the §3 listing uses:
 *         @c Above<N> = {x>N}, @c AtMost<N> = ~Above<N> = {x<=N}. */
export template <auto N, typename L = ClassicalLogic>
using Above = Halfspace<dedekind::sets::Cardinality, N, Direction::Upward,
                        Strictness::Strict, L>;
export template <auto N, typename L = ClassicalLogic>
using AtMost = Halfspace<dedekind::sets::Cardinality, N, Direction::Downward,
                         Strictness::NonStrict, L>;

// A bare Halfspace / Singleton is a first-class @c IsSubobject (ι: S ↣ A plus
// its own χ), though NOT a full ETCS @c IsSet: @c IsSet additionally demands
// the ETCS-axiom surface (@c HasETCSAxioms + the CCC witness) that only the
// ambient universe @c Ω<T> carries.  Subobject-hood is the right membership —
// it is what the complement-lattice operators above operate on.
static_assert(IsSubobject<Above<5>, dedekind::sets::Cardinality>,
              "a Halfspace is a first-class subobject ι: S ↣ ℕ.");
static_assert(IsSubobject<Singleton<true>, bool>,
              "a static Singleton is a first-class subobject.");

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
  // IsExtensional, AND so that the variant ℕ-/ℤ-proxy carriers from
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

/** @section halfspace__Halfspace_BoundScout_DSL — BoundScout<auto> × Bound<V>
 *  → Halfspace.
 *
 * Free-function overloads on the post-#551 NTTP-parameterised scout
 * @c BoundScout<auto @c Ambient>.  Same
 * Halfspace<T, V, D, S> result type; downstream collapse machinery
 * (structured_and on halfspace pairs) is unchanged. */

export template <auto Ambient, auto V>
  requires std::convertible_to<
               decltype(V), typename dedekind::sets::BoundScout<Ambient>::T> &&
           (!std::unsigned_integral<
                typename dedekind::sets::BoundScout<Ambient>::T> ||
            !std::signed_integral<decltype(V)> || V >= 0)
constexpr auto operator>(const dedekind::sets::BoundScout<Ambient>&, Bound<V>) {
  using T = typename dedekind::sets::BoundScout<Ambient>::T;
  return Halfspace<T, V, Direction::Upward, Strictness::Strict>{};
}

export template <auto Ambient, auto V>
  requires std::convertible_to<
               decltype(V), typename dedekind::sets::BoundScout<Ambient>::T> &&
           (!std::unsigned_integral<
                typename dedekind::sets::BoundScout<Ambient>::T> ||
            !std::signed_integral<decltype(V)> || V >= 0)
constexpr auto operator>=(const dedekind::sets::BoundScout<Ambient>&,
                          Bound<V>) {
  using T = typename dedekind::sets::BoundScout<Ambient>::T;
  return Halfspace<T, V, Direction::Upward, Strictness::NonStrict>{};
}

export template <auto Ambient, auto V>
  requires std::convertible_to<
               decltype(V), typename dedekind::sets::BoundScout<Ambient>::T> &&
           (!std::unsigned_integral<
                typename dedekind::sets::BoundScout<Ambient>::T> ||
            !std::signed_integral<decltype(V)> || V >= 0)
constexpr auto operator<(const dedekind::sets::BoundScout<Ambient>&, Bound<V>) {
  using T = typename dedekind::sets::BoundScout<Ambient>::T;
  return Halfspace<T, V, Direction::Downward, Strictness::Strict>{};
}

export template <auto Ambient, auto V>
  requires std::convertible_to<
               decltype(V), typename dedekind::sets::BoundScout<Ambient>::T> &&
           (!std::unsigned_integral<
                typename dedekind::sets::BoundScout<Ambient>::T> ||
            !std::signed_integral<decltype(V)> || V >= 0)
constexpr auto operator<=(const dedekind::sets::BoundScout<Ambient>&,
                          Bound<V>) {
  using T = typename dedekind::sets::BoundScout<Ambient>::T;
  return Halfspace<T, V, Direction::Downward, Strictness::NonStrict>{};
}

/** @section halfspace__Halfspace_Structural_Algebra — ADL hooks for operator&&.
 */

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
  } else if constexpr (IsRingIntegral<T>) {
    // Cardinality of {x : T | Lo ⋈ x ⋈ Hi} over an integer-flavoured T
    // (@c IsRingIntegral admits @c std::integral plus the variant
    // proxies @c Cardinality / @c SignedCardinality, post-#414).
    constexpr bool lo_open = (SL == Strictness::Strict);
    constexpr bool hi_open = (SU == Strictness::Strict);
    constexpr auto span = Hi - Lo + (lo_open ? 0 : 1) + (hi_open ? -1 : 0);
    if constexpr (span == 1) {
      // Unique inhabitant: the smallest x admitted by the lower boundary.
      // The Singleton's NTTP value is computed in the @b bound's primitive
      // type (typically @c int), @b not cast to @c T --- @c Cardinality /
      // @c SignedCardinality are @c std::variant carriers and therefore
      // not structural-NTTP types in C++20, so casting through them would
      // make the Singleton ill-formed.  The Singleton's @c Domain is
      // @c decltype(unique) (= the bound's type, e.g.\ @c int); runtime
      // queries with @c T-valued arguments are routed through the
      // cross-type @c == path landed in PR #423.
      //
      // For @c std::integral @c T the cast is preserved verbatim so the
      // pre-#402 behaviour on primitive carriers (@c Singleton<4u> on
      // @c unsigned @c int, @c Singleton<int_value> for real-pivot-on-int
      // showcases like @c bound<-21.0> on @c element<Ω<int>>) doesn't shift.
      if constexpr (std::integral<T>) {
        constexpr T unique =
            lo_open ? static_cast<T>(Lo + 1) : static_cast<T>(Lo);
        return Singleton<unique, L>{};
      } else {
        constexpr auto unique = lo_open ? (Lo + 1) : Lo;
        return Singleton<unique, L>{};
      }
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

/** @section halfspace__Interval_Cartesian_Product — 2D structural products. */

/**
 * @brief Cartesian product of two reduced extensional structures (typically
 * `OrderInterval`s on integer carriers). Preserves size / logic / tags so the
 * 2D product participates in the same computability classification as the
 * 1D factors: `IsExtensional<IntervalProduct<I1, I2>>` holds whenever each
 * factor satisfies `IsExtensional`.
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

/** @brief Meet on two same-carrier `OrderInterval`s: the intersection.
 *
 *  @details The meet of @c [a, b] and @c [c, d] (with appropriate
 *  strictness on each side) is @c [max(a,c), min(b,d)] — the
 *  more-restrictive bound wins, and at a tie the @b strictest strictness
 *  wins.  The result is always an @c OrderInterval; an @b empty
 *  intersection is represented honestly as an @c OrderInterval whose
 *  bounds make @c size() @c = @c 0 (rather than three-way-reducing to
 *  @c EmptyPredicate / @c Singleton as the halfspace-halfspace overloads
 *  do — the OI tower is structurally closed under intersection, and
 *  closure is the load-bearing fact for the @c :ranges halfspace ↔
 *  iota_view bridge to compose with this meet).
 *
 *  This is the lattice @c ∧ on the OrderInterval carrier, supplying the
 *  meet operation @c structured_and on halfspaces lifts to its bounded
 *  child.  Same-T, same-L overloads only — heterogeneous-carrier
 *  intersection is not a lattice operation.
 *
 *  @see dedekind::sequences::bridge_meet_witness in @c :sequences:ranges —
 *       the type-level static_asserts that pin the bridge respects this
 *       meet (lattice-homomorphism). */
export template <typename T, auto Lo1, auto Hi1, Strictness SL1, Strictness SU1,
                 auto Lo2, auto Hi2, Strictness SL2, Strictness SU2, typename L>
  requires std::convertible_to<decltype(Lo1), T> &&
           std::convertible_to<decltype(Hi1), T> &&
           std::convertible_to<decltype(Lo2), T> &&
           std::convertible_to<decltype(Hi2), T>
constexpr auto structured_and(OrderInterval<T, Lo1, Hi1, SL1, SU1, L>,
                              OrderInterval<T, Lo2, Hi2, SL2, SU2, L>) {
  // Compute the meet bounds in the common type of the source NTTPs — not
  // by casting through T.  Casting through T would (a) lose the original
  // pivot type (e.g. with cross-type pivots) and (b) break carriers whose
  // T isn't a structural NTTP type (e.g. Cardinality / SignedCardinality
  // — std::variant carriers can't be NTTPs).  The returned OrderInterval
  // keeps T as its carrier and the bounds as their common NTTP type.
  using LoC = std::common_type_t<decltype(Lo1), decltype(Lo2)>;
  using HiC = std::common_type_t<decltype(Hi1), decltype(Hi2)>;
  constexpr LoC lo1 = static_cast<LoC>(Lo1);
  constexpr LoC lo2 = static_cast<LoC>(Lo2);
  constexpr HiC hi1 = static_cast<HiC>(Hi1);
  constexpr HiC hi2 = static_cast<HiC>(Hi2);

  // The bigger lower / smaller upper wins; at a tie the strictest
  // strictness wins (a Strict edge subsumes a NonStrict edge at the same
  // pivot).
  constexpr LoC new_lo = lo1 > lo2 ? lo1 : lo2;
  constexpr Strictness new_SL =
      (lo1 > lo2)   ? SL1
      : (lo2 > lo1) ? SL2
      : (SL1 == Strictness::Strict || SL2 == Strictness::Strict)
          ? Strictness::Strict
          : Strictness::NonStrict;

  constexpr HiC new_hi = hi1 < hi2 ? hi1 : hi2;
  constexpr Strictness new_SU =
      (hi1 < hi2)   ? SU1
      : (hi2 < hi1) ? SU2
      : (SU1 == Strictness::Strict || SU2 == Strictness::Strict)
          ? Strictness::Strict
          : Strictness::NonStrict;

  return OrderInterval<T, new_lo, new_hi, new_SL, new_SU, L>{};
}

// ── The point-free projection scout ────────────────────────────────────────
/**
 * @brief @c π --- the point-free variable, a @b domain-less scout.
 *
 * @details Where @c in<ℕ> bakes the carrier into the scout's type, @c π leaves
 * it open.  A comparison @c π @c ⋈ @c fix(V) fixes only the @b shape (direction
 * and pivot) as an @c UnboundHalfspace / @c UnboundSingleton; a later
 * @c carrier @c | @c ... instantiates it at the carrier's @c Domain, reusing
 * @c Halfspace / @c Singleton.  @c π is the unary projection; the product
 * coordinates @c π1 / @c π2 follow with the relational surface (#783), reusing
 * @c :cartesian projections where they fit.
 */
export template <std::size_t Slot>
struct Projection {};

export inline constexpr Projection<0> π{};

/** @brief Unbound predicates: shape fixed, @c Domain deferred until a carrier
 *  binds them via @c operator| below. */
export template <Direction D, Strictness S, auto V>
struct UnboundHalfspace {};
export template <auto V>
struct UnboundSingleton {};

// π ⋈ fix(V) → the unbound halfspace / singleton (pivot fixed, carrier open).
export template <auto V>
constexpr UnboundHalfspace<Direction::Upward, Strictness::Strict, V> operator>(
    Projection<0>, Bound<V>) {
  return {};
}
export template <auto V>
constexpr UnboundHalfspace<Direction::Upward, Strictness::NonStrict, V>
operator>=(Projection<0>, Bound<V>) {
  return {};
}
export template <auto V>
constexpr UnboundHalfspace<Direction::Downward, Strictness::Strict, V>
operator<(Projection<0>, Bound<V>) {
  return {};
}
export template <auto V>
constexpr UnboundHalfspace<Direction::Downward, Strictness::NonStrict, V>
operator<=(Projection<0>, Bound<V>) {
  return {};
}
export template <auto V>
constexpr UnboundSingleton<V> operator==(Projection<0>, Bound<V>) {
  return {};
}

// carrier | unbound → the Domain-bound predicate, reusing Halfspace /
// Singleton. The RHS type is distinct from Set, so this does not clash with the
// union operator| on a UniversalSet (that one takes a Set).
export template <typename T, typename L, typename C, Direction D, Strictness S,
                 auto V>
constexpr Halfspace<T, V, D, S, L> operator|(const UniversalSet<T, L, C>&,
                                             const UnboundHalfspace<D, S, V>&) {
  return {};
}
export template <typename T, typename L, typename C, auto V>
constexpr Singleton<V, L> operator|(const UniversalSet<T, L, C>&,
                                    const UnboundSingleton<V>&) {
  return {};
}

// The point-free surface reproduces the existing halfspace exactly.
static_assert(
    std::same_as<decltype(ℕ | (π > fix(5_c))),
                 decltype(dedekind::sets::in<ℕ> > bound<5>)>,
    "ℕ | π > fix(5_c) is the Above<5> halfspace, spelled point-free.");

// And the equality shape gives the extensional Singleton, membership-checked.
static_assert(std::same_as<decltype(𝔹 | (π == fix(true_c))),
                           Singleton<true, ClassicalLogic>>,
              "𝔹 | π == fix(true_c) is Singleton<true>, spelled point-free.");
static_assert(static_cast<bool>((𝔹 | (π == fix(true_c)))(true)),
              "true ∈ {true}.");
static_assert(!static_cast<bool>((𝔹 | (π == fix(true_c)))(false)),
              "false ∉ {true}.");

// The complement-pair collapse is unchanged by the point-free spelling: the
// meet of a halfspace with its complement gives the same empty result as the
// scout spelling (the §5 Theorem-1 witness, now bracket-free).
static_assert(
    std::same_as<decltype((ℕ | (π > fix(5_c))) & ~(ℕ | (π > fix(5_c)))),
                 decltype((dedekind::sets::in<ℕ> >
                           bound<5>)&~(dedekind::sets::in<ℕ> > bound<5>))>,
    "point-free complement-meet is identical to the scout collapse (→ Ø).");

// The collapse compared to the bare empty set --- the exact Listing 2 spelling.
static_assert(((ℕ | (π > fix(5_c))) & ~(ℕ | (π > fix(5_c)))) == Ø{},
              "point-free: (n > 5) ∩ ¬(n > 5) == Ø.");
static_assert(((𝔹 | (π == fix(true_c))) & ~(𝔹 | (π == fix(true_c)))) == Ø{},
              "point-free: {true} ∩ ¬{true} == Ø.");

// ── Product projections: the relational (point-free) variables ─────────────
/**
 * @brief @c π1 / @c π2 --- the coordinate projections of a pair, the positional
 *        variables of the point-free relational surface.
 *
 * @details A comparison @c π_I @c ⋈ @c π_J or @c π_I @c ⋈ @c fix(V) builds a
 * @b strongly-typed predicate on a pair (no lambda); @c & conjoins them; and
 * @c product @c | @c predicate restricts the product to the relation.  So
 * @c ℕ*ℕ @c | @c π1 @c < @c π2 @c & @c π1 @c > @c fix(5_c) is the relation
 * @f$\{(x,y) \mid x<y \wedge x>5\}@f$ as an @c IsSet on @c ℕ×ℕ.
 */
export inline constexpr Projection<1> π1{};
export inline constexpr Projection<2> π2{};

/** @brief The @c I-th component of a pair (1 = @c first, 2 = @c second). */
template <std::size_t I, typename P>
constexpr decltype(auto) coord(const P& p) {
  if constexpr (I == 1)
    return (p.first);
  else
    return (p.second);
}

/** @brief Comparison flavour for the relational predicates. */
export enum class Rel { Lt, Le, Gt, Ge, Eq, Ne };

template <Rel R, typename X, typename Y>
constexpr bool rel_apply(const X& x, const Y& y) {
  if constexpr (R == Rel::Lt)
    return x < y;
  else if constexpr (R == Rel::Le)
    return x <= y;
  else if constexpr (R == Rel::Gt)
    return x > y;
  else if constexpr (R == Rel::Ge)
    return x >= y;
  else if constexpr (R == Rel::Eq)
    return x == y;
  else
    return x != y;
}

/** @brief Nested-typedef marker so @c & / @c | fire only on relational
 *  predicates; kept off the class hierarchy so the predicates stay aggregates.
 */
export template <typename T>
concept IsRelPredicate = requires { typename T::is_rel_predicate; };

/** @brief @f$\pi_I \bowtie \pi_J@f$ --- a strongly-typed predicate on a pair.
 */
export template <std::size_t I, Rel R, std::size_t J>
struct ProjProj {
  using is_rel_predicate = void;
  template <typename P>
  constexpr bool operator()(const P& p) const {
    return rel_apply<R>(coord<I>(p), coord<J>(p));
  }
};

/** @brief @f$\pi_I \bowtie \mathrm{fix}(V)@f$ --- a strongly-typed pair
 *  predicate. */
export template <std::size_t I, Rel R, auto V>
struct ProjBound {
  using is_rel_predicate = void;
  template <typename P>
  constexpr bool operator()(const P& p) const {
    return rel_apply<R>(coord<I>(p), V);
  }
};

/** @brief Meet (conjunction) of two relational predicates. */
export template <typename A, typename B>
struct RelAnd {
  using is_rel_predicate = void;
  A a;
  B b;
  template <typename P>
  constexpr bool operator()(const P& p) const {
    return a(p) && b(p);
  }
};

// π_I ⋈ π_J  →  ProjProj (projection-vs-projection).
export template <std::size_t I, std::size_t J>
constexpr ProjProj<I, Rel::Lt, J> operator<(Projection<I>, Projection<J>) {
  return {};
}
export template <std::size_t I, std::size_t J>
constexpr ProjProj<I, Rel::Le, J> operator<=(Projection<I>, Projection<J>) {
  return {};
}
export template <std::size_t I, std::size_t J>
constexpr ProjProj<I, Rel::Gt, J> operator>(Projection<I>, Projection<J>) {
  return {};
}
export template <std::size_t I, std::size_t J>
constexpr ProjProj<I, Rel::Ge, J> operator>=(Projection<I>, Projection<J>) {
  return {};
}
export template <std::size_t I, std::size_t J>
constexpr ProjProj<I, Rel::Eq, J> operator==(Projection<I>, Projection<J>) {
  return {};
}
export template <std::size_t I, std::size_t J>
constexpr ProjProj<I, Rel::Ne, J> operator!=(Projection<I>, Projection<J>) {
  return {};
}

// π_I ⋈ fix(V), I >= 1  →  ProjBound (I == 0 is the unary π of §M1 above).
export template <std::size_t I, auto V>
  requires(I >= 1)
constexpr ProjBound<I, Rel::Lt, V> operator<(Projection<I>, Bound<V>) {
  return {};
}
export template <std::size_t I, auto V>
  requires(I >= 1)
constexpr ProjBound<I, Rel::Gt, V> operator>(Projection<I>, Bound<V>) {
  return {};
}
export template <std::size_t I, auto V>
  requires(I >= 1)
constexpr ProjBound<I, Rel::Eq, V> operator==(Projection<I>, Bound<V>) {
  return {};
}
export template <std::size_t I, auto V>
  requires(I >= 1)
constexpr ProjBound<I, Rel::Ne, V> operator!=(Projection<I>, Bound<V>) {
  return {};
}

// meet of relational predicates.
export template <IsRelPredicate A, IsRelPredicate B>
constexpr RelAnd<A, B> operator&(A a, B b) {
  return {a, b};
}

// product | relPred  →  the relation as an IsSet on A × B.
export template <typename T1, typename T2, typename L, typename P,
                 IsRelPredicate RP>
constexpr auto operator|(const Set<std::pair<T1, T2>, L, P>&, RP rp) {
  return Set<std::pair<T1, T2>, L, RP>{rp};
}

/** @section halfspace__Formal_Verification (relational surface) */

// less-than on 𝔹×𝔹, a strongly-typed point-free relation, membership-checked.
static_assert((𝔹 * 𝔹 | π1 < π2)(std::pair{false, true}),
              "(false, true) ∈ {(x,y) | x < y}.");
static_assert(!(𝔹 * 𝔹 | π1 < π2)(std::pair{true, true}),
              "(true, true) ∉ {(x,y) | x < y}.");

// a meet of two projection predicates: {(x,y) | x ≤ y ∧ y == true}.
static_assert((𝔹 * 𝔹 | π1 <= π2 & π2 == fix(true_c))(std::pair{false, true}),
              "(false, true) satisfies x ≤ y ∧ y = true.");
static_assert(!(𝔹 * 𝔹 | π1 <= π2 & π2 == fix(true_c))(std::pair{false, false}),
              "(false, false) fails y = true.");

// ── converse and the bracket-free relation query ───────────────────────────
/** @brief The swapped predicate for @c converse: @f$R^\smile(b,a) = R(a,b)@f$.
 */
export template <typename P>
struct SwapPred {
  using is_rel_predicate = void;
  P p;
  template <typename Pair>
  constexpr auto operator()(const Pair& pr) const {
    return p(std::pair{pr.second, pr.first});
  }
};

/** @brief @c converse(R) --- the transpose @f$R^\smile \subseteq B \times A@f$
 *  of a relation @f$R \subseteq A \times B@f$ (Tarski's @f$R^\smile@f$). */
export template <typename A, typename B, typename L, typename P>
constexpr auto converse(const Set<std::pair<A, B>, L, P>& r) {
  return Set<std::pair<B, A>, L, SwapPred<P>>{SwapPred<P>{r.predicate()}};
}

/** @brief Pair-like Domain test for @c is_relation. */
template <typename D>
concept IsPairLike = requires {
  typename D::first_type;
  typename D::second_type;
};

/** @brief @c is_relation(R) --- the bracket-free query: @c R is a relation, an
 *  @c IsSet whose Domain is a product @f$A \times B@f$. */
export template <typename S>
consteval bool is_relation(const S&) {
  return dedekind::category::IsSet<S> && IsPairLike<typename S::Domain>;
}

// converse swaps the coordinates; is_relation certifies the product Domain.
static_assert(converse(𝔹* 𝔹 | π1 < π2)(std::pair{true, false}),
              "converse of < contains (true, false): false < true.");
static_assert(is_relation(𝔹* 𝔹 | π1 < π2),
              "𝔹*𝔹 | π1 < π2 is a relation (IsSet on a product).");

// ── Projection arithmetic (for the divides relation, Listing 7) ────────────
/** @brief @f$\pi_I \% \pi_J@f$ --- a value expression on a pair, awaiting a
 *  comparison to a bound. */
export template <std::size_t I, std::size_t J>
struct ProjMod {};
export template <std::size_t I, std::size_t J>
constexpr ProjMod<I, J> operator%(Projection<I>, Projection<J>) {
  return {};
}

/** @brief @f$(\pi_I \% \pi_J) \bowtie \mathrm{fix}(V)@f$ --- a strongly-typed
 *  pair predicate (the modular / divisibility shape). */
export template <std::size_t I, std::size_t J, Rel R, auto V>
struct ProjModBound {
  using is_rel_predicate = void;
  template <typename P>
  constexpr bool operator()(const P& p) const {
    return rel_apply<R>(coord<I>(p) % coord<J>(p), V);
  }
};
export template <std::size_t I, std::size_t J, auto V>
constexpr ProjModBound<I, J, Rel::Eq, V> operator==(ProjMod<I, J>, Bound<V>) {
  return {};
}

// divides: {(a,b) | b % a == 0 ∧ a != 0} = ℕ*ℕ | π2 % π1 == fix(0_c) & π1 != 0.
// The && in RelAnd short-circuits the guard first, so a == 0 never reaches %.
static_assert((ℕ * ℕ | π1 != fix(0_c) & π2 % π1 == fix(0_c))(std::pair{
                  finite_cardinality(2), finite_cardinality(6)}),
              "6 % 2 == 0: (2,6) ∈ divides.");
static_assert(!(ℕ * ℕ | π1 != fix(0_c) & π2 % π1 == fix(0_c))(std::pair{
                  finite_cardinality(4), finite_cardinality(6)}),
              "6 % 4 != 0: (4,6) ∉ divides.");

}  // namespace dedekind::order
