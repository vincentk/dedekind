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
#include <functional>  // std::plus (the argmax carrier's additive-group gate)
#include <type_traits>
#include <utility>

export module dedekind.order:halfspace;

import dedekind.category;
import dedekind.sets;
import :poset;  // IsPartiallyOrdered — the SEMANTIC order certificate for
                // max/min

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
 * a compile-time bound @c fix(5_c) instead of @c bound<5>.  Decimal digits
 * only: a non-decimal spelling (@c 1.5_c, @c 0x10_c) is a hard error, not a
 * silently mis-parsed value.
 */
export template <char... Cs>
  requires((('0' <= Cs && Cs <= '9') && ...))
consteval auto operator""_c() {
  constexpr int v = [] {
    int r = 0;
    ((r = r * 10 + (Cs - '0')), ...);
    return r;
  }();
  return std::integral_constant<int, v>{};
}

/** @brief Unary minus on a compile-time @c _c constant, so a NEGATIVE pivot or
 *  shift spells @c -3_c (@c = @c integral_constant<int,-3>) and @c fix(-3_c) is
 *  @c Bound<-3> --- e.g. the converse of a translation graph, @c fix(-K). */
export template <int V>
consteval std::integral_constant<int, -V> operator-(
    std::integral_constant<int, V>) {
  return {};
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

/** @section halfspace__Halfspace_Structural_Join — @c structured_or, the JOIN
 *  (∪) dual of @c structured_and: it makes the union COLLAPSE symmetrically to
 *  the meet, so the join is no longer a declared-but-unimplemented hook.
 *  Same-direction halfspaces union to the WEAKER bound (the smaller pivot up /
 *  larger pivot down, non-strict winning at an equal pivot).  Opposing
 *  halfspaces that OVERLAP cover the line (→ universe); a genuine GAP does not
 *  collapse, so no overload matches and @c operator|| falls to the honest
 *  point-wise union.  This is why @c image(abs) = @c image(x↦x on x≥0) ∪
 *  @c image(x↦−x on x<0) = @c {y≥0} ∪ @c {y>0} collapses to @c {y≥0}. */

/** @brief Same-direction upward union: {x≥p1} ∪ {x≥p2} = {x ≥ min(p1,p2)}. */
export template <typename T, auto P1, auto P2, Strictness S1, Strictness S2,
                 typename L>
constexpr auto structured_or(Halfspace<T, P1, Direction::Upward, S1, L>,
                             Halfspace<T, P2, Direction::Upward, S2, L>) {
  if constexpr (P1 < P2) {
    return Halfspace<T, P1, Direction::Upward, S1, L>{};
  } else if constexpr (P2 < P1) {
    return Halfspace<T, P2, Direction::Upward, S2, L>{};
  } else {
    // Same pivot: the WEAKER (non-strict) bound wins the union.
    constexpr Strictness S =
        (S1 == Strictness::NonStrict || S2 == Strictness::NonStrict)
            ? Strictness::NonStrict
            : Strictness::Strict;
    return Halfspace<T, P1, Direction::Upward, S, L>{};
  }
}

/** @brief Same-direction downward union: {x≤p1} ∪ {x≤p2} = {x ≤ max(p1,p2)}. */
export template <typename T, auto P1, auto P2, Strictness S1, Strictness S2,
                 typename L>
constexpr auto structured_or(Halfspace<T, P1, Direction::Downward, S1, L>,
                             Halfspace<T, P2, Direction::Downward, S2, L>) {
  if constexpr (P1 > P2) {
    return Halfspace<T, P1, Direction::Downward, S1, L>{};
  } else if constexpr (P2 > P1) {
    return Halfspace<T, P2, Direction::Downward, S2, L>{};
  } else {
    constexpr Strictness S =
        (S1 == Strictness::NonStrict || S2 == Strictness::NonStrict)
            ? Strictness::NonStrict
            : Strictness::Strict;
    return Halfspace<T, P1, Direction::Downward, S, L>{};
  }
}

/** @brief Opposing union that COVERS the line → universe.  {x≥Lo} ∪ {x≤Hi}
 *  covers iff every point is in one, i.e. @c Lo≤Hi (or @c Lo<Hi when both are
 *  strict) --- the exact dual of @c structured_and's disjointness test.  A GAP
 *  (@c Lo>Hi) is deliberately unmatched: it does not collapse to a halfspace,
 * so
 *  @c operator|| keeps the honest point-wise union. */
export template <typename T, auto Lo, auto Hi, Strictness SL, Strictness SU,
                 typename L>
  requires((SL == Strictness::Strict && SU == Strictness::Strict) ? (Lo < Hi)
                                                                  : (Lo <= Hi))
constexpr auto structured_or(Halfspace<T, Lo, Direction::Upward, SL, L>,
                             Halfspace<T, Hi, Direction::Downward, SU, L>) {
  return dedekind::sets::UniversalSet<T, L>{};
}
export template <typename T, auto Hi, auto Lo, Strictness SU, Strictness SL,
                 typename L>
  requires((SL == Strictness::Strict && SU == Strictness::Strict) ? (Lo < Hi)
                                                                  : (Lo <= Hi))
constexpr auto structured_or(Halfspace<T, Hi, Direction::Downward, SU, L>,
                             Halfspace<T, Lo, Direction::Upward, SL, L>) {
  return dedekind::sets::UniversalSet<T, L>{};
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
// Restricted to a carrier whose value type IS the pivot's type: Singleton<V,L>
// has Domain = decltype(V), so a mismatch (e.g. ℕ | π == fix(5_c), Cardinality
// vs int) would give the singleton the wrong carrier.  It is an honest compile
// error there; a singleton over such a carrier needs a T-valued pivot.
export template <typename T, typename L, typename C, auto V>
  requires std::same_as<T, decltype(V)>
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

/** @brief The @c I-th component of a pair (1 = @c first, 2 = @c second).
 *  Binary products only, so an out-of-range slot (the unary @c π, or @c π3) is
 *  a hard error rather than a silent alias for @c .second. */
template <std::size_t I, typename P>
  requires(I == 1 || I == 2)
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
constexpr ProjBound<I, Rel::Le, V> operator<=(Projection<I>, Bound<V>) {
  return {};
}
export template <std::size_t I, auto V>
  requires(I >= 1)
constexpr ProjBound<I, Rel::Gt, V> operator>(Projection<I>, Bound<V>) {
  return {};
}
export template <std::size_t I, auto V>
  requires(I >= 1)
constexpr ProjBound<I, Rel::Ge, V> operator>=(Projection<I>, Bound<V>) {
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

/** @brief The product's own membership conjoined with a relational
 *  restriction, so a bind over a @b restricted product keeps that membership
 *  and cannot admit pairs outside it (a full product contributes a
 *  trivially-true @c product). */
export template <typename P, typename RP>
struct ProductRestrict {
  using is_rel_predicate = void;
  P product;
  RP rp;
  template <typename Pair>
  constexpr bool operator()(const Pair& p) const {
    return static_cast<bool>(product(p)) && rp(p);
  }
};

// Ω<A×B> | relPred  →  the relation as an IsSet on A × B.  The @b universal
// product carries no factor restriction, so the relation's membership @b is
// the rel-predicate: the pure product universe refined to a subobject.
export template <typename T1, typename T2, typename L, typename C,
                 IsRelPredicate RP>
constexpr auto operator|(const UniversalSet<std::pair<T1, T2>, L, C>&, RP rp) {
  return Set<std::pair<T1, T2>, L, RP>{rp};
}

// product | relPred  →  the relation as an IsSet on A × B, keeping the
// product's own membership (so a restricted product bounds the relation).
export template <typename T1, typename T2, typename L, typename P,
                 IsRelPredicate RP>
constexpr auto operator|(const Set<std::pair<T1, T2>, L, P>& prod, RP rp) {
  return Set<std::pair<T1, T2>, L, ProductRestrict<P, RP>>{
      ProductRestrict<P, RP>{prod.predicate(), rp}};
}

/**
 * @section halfspace__Restricted_Products
 * @brief A @b restricted factor lifts to a @b cylinder on its axis, so
 *        @c operator* keeps the factor predicates instead of dropping them.
 *
 * @details The product is the universal set of products @c Ω<pair> refined by
 * the two @b cylinders @f$A\times B = \pi_1^{-1}(A)\cap\pi_2^{-1}(B)@f$.  A
 * total factor's cylinder is the whole universe (nothing to add); a halfspace
 * factor @f$\{x \bowtie p\}@f$ lifts to the projection halfspace
 * @f$\pi_I \bowtie \mathrm{fix}(p)@f$ --- the @b same @c ProjBound
 * rel-predicate the graph surface uses, so a restricted domain and a functional
 * graph read in one vocabulary and @c dom can recover the factor from the @c
 * π_I conjunct.
 */

/** @brief The comparison a halfspace @c (Direction, Strictness) lifts to. */
export constexpr Rel rel_of(Direction d, Strictness s) {
  if (d == Direction::Upward) {
    return s == Strictness::Strict ? Rel::Gt : Rel::Ge;
  }
  return s == Strictness::Strict ? Rel::Lt : Rel::Le;
}

/** @brief @f$\pi_I^{-1}@f$ of a halfspace factor: the cylinder
 *  @f$\pi_I \bowtie \mathrm{fix}(\text{pivot})@f$ on the product. */
export template <std::size_t I, typename T, auto Pivot, Direction D,
                 Strictness S, typename L>
constexpr auto cylinder(const Halfspace<T, Pivot, D, S, L>&) {
  return ProjBound<I, rel_of(D, S), Pivot>{};
}

// restricted × total:  {x ⋈ p} × Ω  =  Ω<pair> | (π1 ⋈ fix(p)).
export template <typename T, auto P, Direction D, Strictness S, typename L,
                 typename T2, typename L2, typename C2>
  requires std::same_as<L, L2>
constexpr auto operator*(const Halfspace<T, P, D, S, L>& a,
                         const UniversalSet<T2, L2, C2>&) {
  return Ω<std::pair<T, T2>, L> | cylinder<1>(a);
}

// total × restricted:  Ω × {y ⋈ q}  =  Ω<pair> | (π2 ⋈ fix(q)).
export template <typename T1, typename L1, typename C1, typename T, auto Q,
                 Direction D, Strictness S, typename L>
  requires std::same_as<L1, L>
constexpr auto operator*(const UniversalSet<T1, L1, C1>&,
                         const Halfspace<T, Q, D, S, L>& b) {
  return Ω<std::pair<T1, T>, L> | cylinder<2>(b);
}

// restricted × restricted:  Ω<pair> | (π1 ⋈ fix(p)) & (π2 ⋈ fix(q)).
export template <typename Ta, auto Pa, Direction Da, Strictness Sa, typename La,
                 typename Tb, auto Qb, Direction Db, Strictness Sb, typename Lb>
  requires std::same_as<La, Lb>
constexpr auto operator*(const Halfspace<Ta, Pa, Da, Sa, La>& a,
                         const Halfspace<Tb, Qb, Db, Sb, Lb>& b) {
  return Ω<std::pair<Ta, Tb>, La> | (cylinder<1>(a) & cylinder<2>(b));
}

/**
 * @section halfspace__Projections
 * @brief @c dom / @c cod as the projections @f$\pi_A / \pi_B@f$, recovering the
 *        factor from the relation's @b structure (the inverse of @c cylinder).
 *
 * @details These are the @b π_A / π_B side of Table 3 (the four properties of a
 * relation): @c dom is @f$\pi_A(R)@f$, @c cod is @f$\pi_B(R)@f$.  Read off the
 * axis-@f$I@f$ cylinder @c ProjBound structurally: a comparison bound becomes
 * the halfspace factor, no axis-@f$I@f$ bound leaves the declared universe
 * @c Ω<T_I> (honest exactly when @c R is entire on that side).  This is the
 * @b free case; the @b existential @f$\{a\mid\exists b.R(a,b)\}@f$ that a
 * coupled, non-entire relation needs is the separate Rice-gated operation.
 * The order-layer overloads specialise the sets-layer @c Ω<T1> fallback (they
 * win by @c IsRelPredicate, and ADL reaches them through the @c ProjBound
 * predicate's own namespace).
 */

/** @brief The @c Direction a comparison @c Rel lifts a halfspace to. */
export constexpr Direction dir_of(Rel r) {
  return (r == Rel::Gt || r == Rel::Ge) ? Direction::Upward
                                        : Direction::Downward;
}
/** @brief The @c Strictness a comparison @c Rel lifts a halfspace to. */
export constexpr Strictness strict_of(Rel r) {
  return (r == Rel::Gt || r == Rel::Lt) ? Strictness::Strict
                                        : Strictness::NonStrict;
}
/** @brief Whether a @c Rel is one of the four order comparisons (not Eq/Ne). */
export constexpr bool is_order_rel(Rel r) {
  return r == Rel::Lt || r == Rel::Le || r == Rel::Gt || r == Rel::Ge;
}

/** @brief Recover the axis-@c I factor from a relational predicate.  Default:
 *  no axis-@c I structure, so the declared universe @c Ω<TI>. */
export template <std::size_t I, typename TI, typename P>
constexpr auto axis_factor(const P&) {
  return Ω<TI>;
}

/** @brief A cylinder @c ProjBound on axis @c I: the halfspace it lifted from
 *  (only an order comparison is a halfspace; Eq/Ne fall to the default). */
export template <std::size_t I, typename TI, Rel R, auto V>
  requires(is_order_rel(R))
constexpr auto axis_factor(const ProjBound<I, R, V>&) {
  return Halfspace<TI, V, dir_of(R), strict_of(R)>{};
}

/** @brief A meet of cylinders: the factor on axis @c I is the @b intersection
 *  of both children's factors on that axis, so two bounds on the same axis
 *  (@c π1<=5 & @c π1<=3) meet to the tighter one rather than dropping either.
 */
export template <std::size_t I, typename TI, typename A, typename B>
constexpr auto axis_factor(const RelAnd<A, B>& r) {
  auto fa = axis_factor<I, TI>(r.a);
  auto fb = axis_factor<I, TI>(r.b);
  if constexpr (requires { typename decltype(fa)::is_universal_boundary; }) {
    return fb;  // a does not constrain axis I; the factor is b's
  } else if constexpr (requires {
                         typename decltype(fb)::is_universal_boundary;
                       }) {
    return fa;  // b does not constrain axis I; the factor is a's
  } else {
    return fa & fb;  // BOTH constrain axis I: intersect (structured_and)
  }
}

/** @brief A restricted product bounding a graph: the factor lives on the
 *  product (cylinder) side; the graph @c rp couples the axes, so it is
 *  transparent to a single-axis projection. */
export template <std::size_t I, typename TI, typename Pp, typename RP>
constexpr auto axis_factor(const ProductRestrict<Pp, RP>& r) {
  return axis_factor<I, TI>(r.product);
}

/** @brief @f$\pi_A(R)@f$ --- the domain factor, recovered structurally. */
export template <typename T1, typename T2, typename L, IsRelPredicate P>
constexpr auto dom(const Set<std::pair<T1, T2>, L, P>& r) {
  return axis_factor<1, T1>(r.predicate());
}

/** @brief @f$\pi_B(R)@f$ --- the codomain factor, recovered structurally. */
export template <typename T1, typename T2, typename L, IsRelPredicate P>
constexpr auto cod(const Set<std::pair<T1, T2>, L, P>& r) {
  return axis_factor<2, T2>(r.predicate());
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
  if constexpr (requires { typename S::Domain; })
    return dedekind::category::IsSet<S> && IsPairLike<typename S::Domain>;
  else
    return false;  // no Domain: not a set, hence not a relation (total query)
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

/** @brief @f$\pi_I \% \mathrm{fix}(V)@f$ --- projection mod a @b constant, a
 *  value expression on a pair awaiting a comparison (sibling of @c ProjMod,
 *  whose modulus is the projection @c π_J rather than a fixed @c V). */
export template <std::size_t I, auto V>
struct ProjModConst {};
// The modulus must be positive: @c coord % 0 is undefined behaviour (and fails
// constant evaluation), matching the @c Modular<N> requirement @c N>0.
export template <std::size_t I, auto V>
  requires(V > 0)
constexpr ProjModConst<I, V> operator%(Projection<I>, Bound<V>) {
  return {};
}

/** @brief @f$(\pi_I \% \mathrm{fix}(V)) \bowtie \pi_J@f$ --- compare a
 *  projection-mod-constant to another projection: the residue-class graph
 *  @f$b = a \bmod V@f$. */
export template <std::size_t I, auto V, Rel R, std::size_t J>
struct ProjModConstProj {
  using is_rel_predicate = void;
  template <typename P>
  constexpr bool operator()(const P& p) const {
    return rel_apply<R>(coord<I>(p) % V, coord<J>(p));
  }
};
export template <std::size_t I, auto V, std::size_t J>
constexpr ProjModConstProj<I, V, Rel::Eq, J> operator==(ProjModConst<I, V>,
                                                        Projection<J>) {
  return {};
}

/** @brief @f$(\pi_I \% \mathrm{fix}(V)) \bowtie \mathrm{fix}(W)@f$ --- a
 *  @b congruence @b class predicate @f$\pi_I \equiv W \pmod V@f$ (sibling of
 *  @c ProjModConstProj, whose right side is the projection @c π_J rather than a
 *  fixed residue @c W).  Restricts an axis to a residue class. */
export template <std::size_t I, auto V, Rel R, auto W>
struct ProjModConstBound {
  using is_rel_predicate = void;
  template <typename P>
  constexpr bool operator()(const P& p) const {
    return rel_apply<R>(coord<I>(p) % V, W);
  }
};
export template <std::size_t I, auto V, auto W>
constexpr ProjModConstBound<I, V, Rel::Eq, W> operator==(ProjModConst<I, V>,
                                                         Bound<W>) {
  return {};
}

// residue-class graph: {(a,b) | b = a % 17} = ℕ * ℕ | π1 % fix(17_c) == π2.
static_assert((ℕ * ℕ | π1 % fix(17_c) == π2)(std::pair{finite_cardinality(20),
                                                       finite_cardinality(3)}),
              "20 % 17 == 3: (20,3) ∈ the residue graph.");
static_assert(!(ℕ * ℕ | π1 % fix(17_c) == π2)(std::pair{finite_cardinality(20),
                                                        finite_cardinality(4)}),
              "20 % 17 != 4: (20,4) ∉ the residue graph.");

/** @brief @f$\pi_I + \mathrm{fix}(V)@f$ / @f$\pi_I \cdot \mathrm{fix}(V)@f$ ---
 *  a projection plus / times a @b constant, a value expression on a pair
 *  awaiting a comparison to another projection (siblings of @c ProjModConst).
 *  Enough to spell the successor and scaling graphs natively point-free. */
export template <std::size_t I, auto V>
struct ProjAddConst {};
export template <std::size_t I, auto V>
constexpr ProjAddConst<I, V> operator+(Projection<I>, Bound<V>) {
  return {};
}
export template <std::size_t I, auto V>
struct ProjMulConst {};
export template <std::size_t I, auto V>
constexpr ProjMulConst<I, V> operator*(Projection<I>, Bound<V>) {
  return {};
}

/** @brief @f$(\pi_I + \mathrm{fix}(V)) \bowtie \pi_J@f$ --- the
 * successor-shaped graph @f$b = a + V@f$. */
export template <std::size_t I, auto V, Rel R, std::size_t J>
struct ProjAddConstProj {
  using is_rel_predicate = void;
  template <typename P>
  constexpr bool operator()(const P& p) const {
    const auto a = coord<I>(p);  // cast V into the carrier: Cardinality + int
    using C = std::remove_cvref_t<decltype(a)>;  // is ambiguous, + Cardinality
    return rel_apply<R>(a + static_cast<C>(V), coord<J>(p));  // is not
  }
};
export template <std::size_t I, auto V, std::size_t J>
constexpr ProjAddConstProj<I, V, Rel::Eq, J> operator==(ProjAddConst<I, V>,
                                                        Projection<J>) {
  return {};
}

/** @brief @f$(\pi_I \cdot \mathrm{fix}(V)) \bowtie \pi_J@f$ --- the scaling
 *  graph @f$b = a \cdot V@f$ (e.g. the doubler @f$b = 2a@f$). */
export template <std::size_t I, auto V, Rel R, std::size_t J>
struct ProjMulConstProj {
  using is_rel_predicate = void;
  template <typename P>
  constexpr bool operator()(const P& p) const {
    const auto a = coord<I>(p);
    using C = std::remove_cvref_t<decltype(a)>;
    return rel_apply<R>(a * static_cast<C>(V), coord<J>(p));
  }
};
export template <std::size_t I, auto V, std::size_t J>
constexpr ProjMulConstProj<I, V, Rel::Eq, J> operator==(ProjMulConst<I, V>,
                                                        Projection<J>) {
  return {};
}

// successor graph: {(a,b) | b = a + 1} = ℕ * ℕ | π1 + fix(1_c) == π2.
static_assert((ℕ * ℕ | π1 + fix(1_c) == π2)(std::pair{finite_cardinality(4),
                                                      finite_cardinality(5)}),
              "4 + 1 == 5: (4,5) ∈ the successor graph.");
static_assert(!(ℕ * ℕ | π1 + fix(1_c) == π2)(std::pair{finite_cardinality(4),
                                                       finite_cardinality(6)}),
              "4 + 1 != 6: (4,6) ∉ the successor graph.");
// A RESTRICTED domain: the successor graph over {x ≤ 5} × ℕ.  operator* keeps
// the ≤5 bound by lifting it to the π1 cylinder, so x = 7 is excluded even
// though 8 = 7+1: the factor predicate is not dropped.
static_assert(((ℕ | (π <= fix(5_c))) * ℕ | π1 + fix(1_c) == π2)(std::pair{
                  finite_cardinality(4), finite_cardinality(5)}),
              "(4,5): 4 ≤ 5 ∧ 5 = 4+1, in the restricted successor graph.");
static_assert(!((ℕ | (π <= fix(5_c))) * ℕ | π1 + fix(1_c) == π2)(std::pair{
                  finite_cardinality(7), finite_cardinality(8)}),
              "(7,8): 7 ≰ 5, excluded though 8 = 7+1.");
// doubling graph: {(a,b) | b = 2a} = ℕ * ℕ | π1 * fix(2_c) == π2.
static_assert((ℕ * ℕ | π1 * fix(2_c) == π2)(std::pair{finite_cardinality(3),
                                                      finite_cardinality(6)}),
              "3 * 2 == 6: (3,6) ∈ the doubling graph.");
static_assert(!(ℕ * ℕ | π1 * fix(2_c) == π2)(std::pair{finite_cardinality(3),
                                                       finite_cardinality(7)}),
              "3 * 2 != 7: (3,7) ∉ the doubling graph.");

// dom / cod are the projections π_A / π_B (Table 3), recovered from the graph's
// STRUCTURE: they return the DECLARED domain/codomain, not the effective image.
// The residue graph has no axis cylinder, so both recover the declared universe
// ℕ (unqualified so ADL finds the order-layer recovery).  cod is thus the
// declared ℕ --- its EFFECTIVE second projection is only the residue class
// {0,…,16}, but reading that off is the ∃-elimination behind the Rice wall
// (§3.3), so cod does not claim surjectivity here.
static_assert(dom(ℕ* ℕ | π1 % fix(17_c) == π2)(finite_cardinality(100)),
              "π_A of the residue graph = declared ℕ (entire), contains 100.");
static_assert(cod(ℕ* ℕ | π1 % fix(17_c) == π2)(finite_cardinality(3)),
              "π_B = declared codomain ℕ, contains 3 (NOT the effective "
              "{0,…,16} residue image, which would need the ∃-projection).");

// With a RESTRICTED factor, dom recovers the halfspace off the π1 cylinder:
// π_A((ℕ|≤5)*ℕ) = {a ≤ 5}, while cod stays the unrestricted ℕ.
static_assert(dom((ℕ | (π <= fix(5_c))) * ℕ)(finite_cardinality(4)),
              "π_A recovers {a ≤ 5}: 4 ≤ 5.");
static_assert(!dom((ℕ | (π <= fix(5_c))) * ℕ)(finite_cardinality(7)),
              "π_A recovers {a ≤ 5}: 7 ≰ 5.");
static_assert(cod((ℕ | (π <= fix(5_c))) * ℕ)(finite_cardinality(99)),
              "π_B is unrestricted ℕ: contains 99.");
// The restriction survives a graph refinement (dom digs through
// ProductRestrict).
static_assert(dom((ℕ | (π <= fix(5_c))) * ℕ |
                  π1 + fix(1_c) == π2)(finite_cardinality(4)),
              "π_A of the restricted successor still recovers {a ≤ 5}.");

// relational application: apply(R, a) is the fibre {b | (a,b) ∈ R}.  For the
// residue graph (a function) it is the singleton {a % 17}: apply(R,20) = {3}.
static_assert(
    dedekind::sets::apply(ℕ* ℕ | π1 % fix(17_c) == π2,
                          finite_cardinality(20))(finite_cardinality(3)),
    "apply(R,20) = {3}: (20,3) ∈ R since 20 % 17 == 3.");
static_assert(
    !dedekind::sets::apply(ℕ * ℕ | π1 % fix(17_c) == π2,
                           finite_cardinality(20))(finite_cardinality(4)),
    "apply(R,20) does not contain 4.");

// ── Intensional extrema (Bird & de Moor @cite birddemoor1997aop)
// ──────────────
// @f$\max@f$ is one GENERIC definition, the @f$\forall@f$-projection of the
// order relation onto its second coordinate:
// @f[ \max S \;=\; \{\pi_2 \in S \mid \forall \pi_1 \in S.\; \pi_1 \le \pi_2\}
//              \;=\; S \cap \mathrm{upperbounds}(S) \;=\; (\in) \cap (R/\ni).
//              @f]
// The @f$\forall@f$-projection @c R/\ni @b is @c upperbounds: the region
// dominating all of @c S.  IT is the pluggable point (as @f$\forall@f$/
// @f$\exists@f$ plug into @c Ø::operator==), decided SYMBOLICALLY per structure
// so no candidate is enumerated: a halfspace bounded ABOVE (@c {x ≤ p}) is
// dominated exactly by @c {x ≥ p}; one unbounded above (@c {x ≥ p}) has no
// upper bound (@c Ø).  @c lowerbounds is the dual.  The generic @c max/min
// below then meet @c S with them --- and the meet, @c structured_and, collapses
// the interval to the attained pivot (@c {x≤p} ∩ {x≥p} = @c {p}) or, when the
// sup is not attained (strict) or absent (unbounded), to @c Ø.
export template <typename T, auto p, Strictness S, typename L>
constexpr auto upperbounds(Halfspace<T, p, Direction::Downward, S, L>) {
  if constexpr (S == Strictness::Strict && IsRingIntegral<T>) {
    // DISCRETE strict {x<p}: the sup p is not in S, but the predecessor p−1 IS
    // (the greatest integer below p), so the upper bounds start at p−1 and the
    // meet {x<p} ∩ {x≥p−1} = {p−1} attains the max.
    return Halfspace<T, p - 1, Direction::Upward, Strictness::NonStrict, L>{};
  } else {
    // {x≤p}: sup p attained.  Continuous {x<p}: sup p unattained (dense carrier
    // has no predecessor), so upper bounds {x≥p} and the meet is Ø (no max).
    return Halfspace<T, p, Direction::Upward, Strictness::NonStrict, L>{};
  }
}
export template <typename T, auto p, Strictness S, typename L>
constexpr auto upperbounds(Halfspace<T, p, Direction::Upward, S, L>) {
  return Ø<T, L>{};  // unbounded above: no upper bound
}
export template <typename T, auto p, Strictness S, typename L>
constexpr auto lowerbounds(Halfspace<T, p, Direction::Upward, S, L>) {
  if constexpr (S == Strictness::Strict && IsRingIntegral<T>) {
    // DISCRETE strict {x>p}: the min is the attained successor p+1.
    return Halfspace<T, p + 1, Direction::Downward, Strictness::NonStrict, L>{};
  } else {
    return Halfspace<T, p, Direction::Downward, Strictness::NonStrict, L>{};
  }
}
export template <typename T, auto p, Strictness S, typename L>
constexpr auto lowerbounds(Halfspace<T, p, Direction::Downward, S, L>) {
  return Ø<T, L>{};  // unbounded below: no lower bound
}
// 𝔹: the whole carrier is bounded --- ⊤ dominates it, ⊥ is dominated by it.
export template <typename L, typename C>
constexpr auto upperbounds(const UniversalSet<bool, L, C>&) {
  return Singleton<true, L>{};
}
export template <typename L, typename C>
constexpr auto lowerbounds(const UniversalSet<bool, L, C>&) {
  return Singleton<false, L>{};
}

// @c & IS the meet on bare order operands: it forwards to the
// @c structured_and customization point, exactly as @c Set::operator& does for
// wrapped predicates, so no @c Set{} wrapping is needed.  The complement-pair
// @c operator& above (opposite direction AND flipped strictness → @c Ø) is more
// specialized and still claims its case; every other halfspace pair
// (overlapping, same-direction) routes here.
export template <typename T, auto P1, Direction D1, Strictness S1, auto P2,
                 Direction D2, Strictness S2, typename L>
constexpr auto operator&(Halfspace<T, P1, D1, S1, L> a,
                         Halfspace<T, P2, D2, S2, L> b)
  requires requires { structured_and(a, b); }
{
  return structured_and(a, b);
}
// @c | IS the join on bare order operands, dual to the @c & meet: it forwards
// to
// @c structured_or, so a same-direction or overlapping halfspace union
// collapses.  The complement-pair @c operator| above (→ universe) is more
// specialized and still claims its case; a genuine GAP has no @c structured_or,
// so @c | there is an honest hard error (use the Set-level union instead).
export template <typename T, auto P1, Direction D1, Strictness S1, auto P2,
                 Direction D2, Strictness S2, typename L>
constexpr auto operator|(Halfspace<T, P1, D1, S1, L> a,
                         Halfspace<T, P2, D2, S2, L> b)
  requires requires { structured_or(a, b); }
{
  return structured_or(a, b);
}
// @c Ø absorbs the meet (no upper bound ⟹ no max) --- the completion the
// @f$\forall@f$-projection needs at the unbounded end.  (The finite end, the
// universe meet-identity @c Ω ∩ X = X so @c 𝔹 ∩ {⊤} = {⊤}, is already the
// universe's own @c operator& member in @c :boundaries.)
export template <typename T, auto p, Direction D, Strictness S, typename L,
                 typename LZ>
constexpr auto operator&(Halfspace<T, p, D, S, L>, Ø<T, LZ>) {
  return Ø<T, L>{};
}

// The generic extremum: @c S met (@c ∩) with its own @f$\forall@f$-dominators.
// One definition for any ordered @c S whose @c upperbounds and meet are
// defined; the structural collapse lives entirely in @c upperbounds + the meet,
// so there is no generic search.  @c min is the dual (@c S met with its
// minorants).
// Gated on the SEMANTIC order: greatest/least element is a @b partial-order
// notion, so the carrier's domain must certify @c IsPartiallyOrdered
// (dedekind's order axioms --- reflexive, transitive, antisymmetric --- which
// subsume
// @c std::totally_ordered one level up in @c IsTotallyOrdered).  A carrier that
// is not an ordered set --- e.g.\ @c SignedCardinality, which carries the
// unordered @c NaZ like an IEEE NaN --- is honestly rejected: you cannot take
// the max of a set that may contain a NaN.
export template <typename S>
  requires IsPartiallyOrdered<typename S::Domain> &&
           requires(const S& s) { s & upperbounds(s); }
constexpr auto max(const S& s) {
  return s & upperbounds(s);
}
export template <typename S>
  requires IsPartiallyOrdered<typename S::Domain> &&
           requires(const S& s) { s & lowerbounds(s); }
constexpr auto min(const S& s) {
  return s & lowerbounds(s);
}

inline constexpr auto ℤ =
    Ω<SignedCardinality>;  // local alias (:integer is downstream)
// Exhibit (intensional, infinite case) over ℕ = @c Ω<Cardinality>, a registered
// TOTAL order (⊃ partial).  @c ℤ = @c SignedCardinality carries the unordered
// @c NaZ (NaN-like), so it is NOT an ordered set and the @c IsPartiallyOrdered
// gate correctly rejects @c max/min on it; the max/min VALUES are identical on
// ℕ (they are non-negative).
inline constexpr auto ℕ = Ω<Cardinality>;
inline constexpr auto le5 = ℕ | (π <= fix(5_c));  // {x ∈ ℕ | x ≤ 5}
inline constexpr auto ge5 = ℕ | (π >= fix(5_c));  // {x ∈ ℕ | x ≥ 5}
static_assert(max(le5)(5), "5 = max {x ≤ 5} (read off the pivot).");
static_assert(!max(le5)(3), "3 is not the greatest element of {x ≤ 5}.");
static_assert(min(ge5)(5), "5 = min {x ≥ 5}.");
static_assert(!min(ge5)(7), "7 is not the least element of {x ≥ 5}.");
// DISCRETE strict: {x<5} on ℕ has attained max 4 (the predecessor), NOT ∅.
static_assert(max(ℕ | (π < fix(5_c)))(4),
              "4 = max {x < 5} on ℕ (predecessor).");
static_assert(!max(ℕ | (π < fix(5_c)))(5), "5 ∉ {x < 5}, so not its max.");
static_assert(min(ℕ | (π > fix(5_c)))(6), "6 = min {x > 5} on ℕ (successor).");

// inverse of a translation-graph relation = its CONVERSE B*A | P⁻¹: the same
// graph read backwards, x ↦ x−K, again a GRAPH (a relation, not an arrow), so
// it stays on the surface and composes.  Functions ARE graphs here, so inverse
// is a relational operation --- the converse with the shift negated.
export template <typename T, auto K, typename L>
constexpr auto inverse(
    const Set<std::pair<T, T>, L, ProjAddConstProj<1, K, Rel::Eq, 2>>&) {
  return Set<std::pair<T, T>, L, ProjAddConstProj<1, -K, Rel::Eq, 2>>{
      ProjAddConstProj<1, -K, Rel::Eq, 2>{}};
}

/** @brief Two translation graphs are the same relation iff they carry the same
 *  shift: structural equality on the graph, compile-time. */
export template <typename T, auto K1, auto K2, typename L>
constexpr bool operator==(
    const Set<std::pair<T, T>, L, ProjAddConstProj<1, K1, Rel::Eq, 2>>&,
    const Set<std::pair<T, T>, L, ProjAddConstProj<1, K2, Rel::Eq, 2>>&) {
  return K1 == K2;
}

/** @brief @b Symbolic composition of translation graphs: @f$T_a \circ T_b =
 *  T_{a+b}@f$, the shifts added, with @b no @f$\exists@f$ over the intermediate
 *  (contrast the Boolean relative product below).  This is the group law read
 *  off the structure; @c + commutes, so the order of composition is immaterial
 *  --- the abelian translation group, at compile time. */
export template <typename T, auto A, auto B, typename L>
constexpr auto operator>>(
    const Set<std::pair<T, T>, L, ProjAddConstProj<1, A, Rel::Eq, 2>>&,
    const Set<std::pair<T, T>, L, ProjAddConstProj<1, B, Rel::Eq, 2>>&) {
  return Set<std::pair<T, T>, L, ProjAddConstProj<1, A + B, Rel::Eq, 2>>{
      ProjAddConstProj<1, A + B, Rel::Eq, 2>{}};
}

// The translation group, at compile time: closure (T₂∘T₃ = T₅), the converse as
// inverse (T₃⁻¹ = T₋₃), and the abelian cancellation f∘g∘h∘g⁻¹ = f∘h (g and its
// inverse annihilate through h because + commutes).
static_assert(((ℤ * ℤ | π1 + fix(2_c) == π2) >>
               (ℤ * ℤ | π1 + fix(3_c) == π2)) == (ℤ * ℤ | π1 + fix(5_c) == π2),
              "closure: T₂ ∘ T₃ = T₅ (shifts add, symbolically).");
static_assert(inverse(ℤ* ℤ | π1 + fix(3_c) == π2) ==
                  (ℤ * ℤ | π1 + fix(-3_c) == π2),
              "inverse = converse graph: T₃⁻¹ = T₋₃.");
// Contravariant inversion: (f∘g)⁻¹ = g⁻¹∘f⁻¹.  For an invertible map the
// inverse IS the retract, so this is retract composition in the total case
// (Listing 12).
static_assert(inverse((ℤ * ℤ | π1 + fix(3_c) == π2) >>
                      (ℤ * ℤ | π1 + fix(2_c) == π2)) ==
                  (inverse(ℤ * ℤ | π1 + fix(2_c) == π2) >>
                   inverse(ℤ * ℤ | π1 + fix(3_c) == π2)),
              "contravariant inversion: (f∘g)⁻¹ = g⁻¹∘f⁻¹.");
static_assert(((ℤ * ℤ | π1 + fix(2_c) == π2) >> (ℤ * ℤ | π1 + fix(3_c) == π2) >>
               (ℤ * ℤ | π1 + fix(5_c) == π2) >>
               inverse(ℤ * ℤ | π1 + fix(3_c) == π2)) ==
                  ((ℤ * ℤ | π1 + fix(2_c) == π2) >>
                   (ℤ * ℤ | π1 + fix(5_c) == π2)),
              "abelian: f∘g∘h∘g⁻¹ = f∘h (g cancels through h; + commutes).");
// Identity element and conjugation, for the group panel of Listing 12: g∘g⁻¹ is
// the identity translation T₀, and conjugating f by g is trivial (g∘f∘g⁻¹ = f)
// because + commutes.  Associativity is implicit in the flat >> chains.
static_assert(((ℤ * ℤ | π1 + fix(2_c) == π2) >>
               inverse(ℤ * ℤ | π1 + fix(2_c) == π2)) ==
                  (ℤ * ℤ | π1 + fix(0_c) == π2),
              "inverse law: g ∘ g⁻¹ = id (T₀).");
static_assert(((ℤ * ℤ | π1 + fix(2_c) == π2) >> (ℤ * ℤ | π1 + fix(3_c) == π2) >>
               inverse(ℤ * ℤ | π1 + fix(2_c) == π2)) ==
                  (ℤ * ℤ | π1 + fix(3_c) == π2),
              "abelian conjugation: g ∘ f ∘ g⁻¹ = f (+ commutes).");

/** @brief Two halfspaces are the same set iff they share pivot, direction and
 *  strictness (the carrier and logic already match): structural set equality,
 *  compile-time. */
export template <typename T, auto P1, Direction D1, Strictness S1, auto P2,
                 Direction D2, Strictness S2, typename L>
constexpr bool operator==(Halfspace<T, P1, D1, S1, L>,
                          Halfspace<T, P2, D2, S2, L>) {
  return P1 == P2 && D1 == D2 && S1 == S2;
}

/** @brief A halfspace over the @b finite carrier @c bool decides emptiness /
 *  totality by exhausting @c {false, true}: the 𝔹 leg of the s|p quantifier,
 *  so @c forall(𝔹, π ⋈ fix(v)) and @c exists(𝔹, …) materialise for a ≤/≥
 *  fragment (the == fragment goes through @c Singleton).  ADL via @c Halfspace
 *  / @c Ø / @c UniversalSet. */
export template <auto P, Direction D, Strictness S, typename L>
constexpr bool operator==(const Halfspace<bool, P, D, S, L>& h,
                          const Ø<bool, L>&) {
  return !static_cast<bool>(h(false)) && !static_cast<bool>(h(true));
}
export template <auto P, Direction D, Strictness S, typename L>
constexpr bool operator==(const Ø<bool, L>& e,
                          const Halfspace<bool, P, D, S, L>& h) {
  return h == e;
}
export template <auto P, Direction D, Strictness S, typename L, typename C>
constexpr bool operator==(const Halfspace<bool, P, D, S, L>& h,
                          const UniversalSet<bool, L, C>&) {
  return static_cast<bool>(h(false)) && static_cast<bool>(h(true));
}
export template <auto P, Direction D, Strictness S, typename L, typename C>
constexpr bool operator==(const UniversalSet<bool, L, C>& u,
                          const Halfspace<bool, P, D, S, L>& h) {
  return h == u;
}

/** @brief A @c Singleton over @c bool is never all of @c 𝔹 (two elements), so
 *  @c == Ω is @c false: the forall (scheme B) leg for the @c == fragment on 𝔹
 *  (@c Ω<bool> | (π == fix(v)) collapses to @c Singleton<v>). */
export template <auto V, typename L, typename C>
  requires std::same_as<decltype(V), bool>
constexpr bool operator==(const Singleton<V, L>&,
                          const UniversalSet<bool, L, C>&) {
  return false;
}
export template <auto V, typename L, typename C>
  requires std::same_as<decltype(V), bool>
constexpr bool operator==(const UniversalSet<bool, L, C>& u,
                          const Singleton<V, L>& s) {
  return s == u;
}

// image = the RANGE (π_B projection) of a functional graph, read structurally.
// A translation is surjective, so the range of the unrestricted graph is the
// whole line; bounded by a π1-halfspace the range is that halfspace pushed
// forward by K --- the AFFINE PUSHFORWARD, again a halfspace of the same shape.
export template <typename T, auto K, typename L>
constexpr auto image(
    const Set<std::pair<T, T>, L, ProjAddConstProj<1, K, Rel::Eq, 2>>&) {
  return Ω<T>;
}
export template <typename T, auto K, Rel R, auto P, typename L>
constexpr auto image(
    const Set<std::pair<T, T>, L,
              ProductRestrict<ProjAddConstProj<1, K, Rel::Eq, 2>,
                              ProjBound<1, R, P>>>&) {
  return Halfspace<T, P + K, dir_of(R), strict_of(R)>{};
}
static_assert(image(ℤ* ℤ | π1 + fix(3_c) == π2) == ℤ,
              "image(graph of x+3) = ℤ: a translation is onto.");
static_assert(image((ℤ * ℤ | π1 + fix(3_c) == π2) | π1 <= fix(5_c)) ==
                  (ℤ | (π <= fix(8_c))),
              "image over {x ≤ 5} pushes forward to {y ≤ 8}.");

// image of a restricted REFLECTION x ↦ c·x (c = ±1) on {x ⋈ P}: the domain
// halfspace scaled by c --- pivot c·P, with the sense FLIPPED when c<0.  These
// are the branches of the sign-fold epi @c abs = (x↦x on x≥0) ⊔ (x↦−x on x<0):
// each branch is a mono reflection, so its image is a plain halfspace pushed
// forward, no search.  (|c|>1 would also induce the residue @c {y≡0 mod c},
// whose materialisation is a downstream :numbers concern; the sign-fold is
// c=±1, so the range stays a bare halfspace here.)
export template <typename T, auto C, Rel R, auto P, typename L>
  requires(C == 1 || C == -1)
constexpr auto image(
    const Set<std::pair<T, T>, L,
              ProductRestrict<ProjMulConstProj<1, C, Rel::Eq, 2>,
                              ProjBound<1, R, P>>>&) {
  constexpr Direction d = (C < 0) ? flip(dir_of(R)) : dir_of(R);
  return Halfspace<T, C * P, d, strict_of(R)>{};
}
// The sign-fold epi's two branches, and its non-injective image decided
// point-free: abs over {x<0} is the negate branch, whose image {y>0} catches
// the preimage −3 of 3 that a canonical retract (+3 ∉ {x<0}) would miss ---
// soundness from the fibre, no enumeration.
static_assert(image(ℤ* ℤ | π1 * fix(1_c) == π2 | π1 >= fix(0_c)) ==
                  (ℤ | π >= fix(0_c)),
              "image(abs on x≥0) = {y≥0}: the identity reflection.");
static_assert(
    image(ℤ* ℤ | π1 * fix(-1_c) == π2 | π1 < fix(0_c)) == (ℤ | π > fix(0_c)),
    "image(abs on x<0) = {y>0}: the negate reflection flips the sense.");
static_assert(image(ℤ* ℤ | π1 * fix(-1_c) == π2 | π1 < fix(0_c))(3),
              "3 ∈ abs({x<0}) via −3, though the canonical +3 ∉ {x<0}.");
static_assert(!image(ℤ * ℤ | π1 * fix(-1_c) == π2 | π1 < fix(0_c))(-2),
              "abs is never negative: −2 ∉ image(abs).");
// The FULL image of abs is the JOIN of the two branch images, and structured_or
// collapses it: {y≥0} ∪ {y>0} = {y≥0} (the weaker, non-strict bound wins) = ℕ,
// so abs is onto ℕ.  The join is now symmetric with the meet, no easter egg.
static_assert((image(ℤ * ℤ | π1 * fix(1_c) == π2 | π1 >= fix(0_c)) |
               image(ℤ * ℤ | π1 * fix(-1_c) == π2 | π1 < fix(0_c))) ==
                  (ℤ | π >= fix(0_c)),
              "image(abs) = {y≥0} ∪ {y>0} = {y≥0}: the sign-fold is onto ℕ.");

// The successor graph over ℕ: the image the OPAQUE arrow leaves Unknown (the
// Rice wall of Listing 15) is DECIDED here by the pushforward --- {n>5} ↦
// {n>6}, structure buying decidability that opacity cannot.
static_assert(
    image((ℕ * ℕ | π1 + fix(1_c) == π2) | π1 > fix(5_c)) ==
        (ℕ | (π > fix(6_c))),
    "image(succ, {n>5}) = {n>6}: the graph decides where opacity walls.");

// Circle back to the complement-lattice collapse of Listing 2: the constrained
// image of a COMPOSITE folds to ∅.  hc = T₂∘T₃ = T₅ pushes the domain {x≤1}
// forward to {y≤6}, which meets the incompatible codomain {y>6}; the two
// complementary halfspaces collapse to Ø --- the SAME meet-to-empty as
// {x≤5}∩{x>5}, now EMERGENT from composition rather than posited.
static_assert(
    (image(((ℤ * ℤ | π1 + fix(2_c) == π2) >> (ℤ * ℤ | π1 + fix(3_c) == π2)) |
           π1 <= fix(1_c)) &
     (ℤ | (π > fix(6_c)))) == Ø{},
    "constrained image of the composite T₂∘T₃ collapses: {y≤6} ∩ {y>6} = ∅.");

/** @brief @c is_function(R) --- the bracket-free query: @c R is a bona fide
 *  function.  A graph @f$\pi_2 = \pi_1 + K@f$ is single-valued in @f$\pi_2@f$
 *  (functional) and total (entire, a translation is defined everywhere), so it
 *  meets both bounds of Table~3's @f$\pi_A@f$ column. */
export template <typename T, auto K, typename L>
consteval bool is_function(
    const Set<std::pair<T, T>, L, ProjAddConstProj<1, K, Rel::Eq, 2>>&) {
  return true;
}
static_assert(is_function(ℤ* ℤ | π1 + fix(3_c) == π2),
              "the graph of x+3 is a total function (functional ∧ entire).");

/** @brief @c is_entire(R): does @c R cover its whole declared domain?  The bare
 *  translation graph is total; ANY restriction --- here a codomain constraint
 *  on @f$\pi_2@f$ --- pulls its domain back to a proper subset, so it drops to
 *  a @b partial function (functional, not entire; Table~3). */
export template <typename T, auto K, typename L>
consteval bool is_entire(
    const Set<std::pair<T, T>, L, ProjAddConstProj<1, K, Rel::Eq, 2>>&) {
  return true;
}
// A CODOMAIN constraint on π2 (an upper/lower bound, or its meet with a
// residue) pulls the domain back to a PROPER subset through the graph, so the
// graph drops to a partial function.  Narrowed to π2 shapes deliberately: a
// restriction that does NOT shrink the domain (e.g. re-imposing the graph
// itself) is not matched here, so is_entire makes no false non-entire claim for
// it.
export template <typename T, auto K, Rel R, auto P, typename L>
consteval bool is_entire(
    const Set<std::pair<T, T>, L,
              ProductRestrict<ProjAddConstProj<1, K, Rel::Eq, 2>,
                              ProjBound<2, R, P>>>&) {
  return false;
}
export template <typename T, auto K, Rel R, auto P, auto V, auto W, typename L>
consteval bool is_entire(
    const Set<std::pair<T, T>, L,
              ProductRestrict<ProjAddConstProj<1, K, Rel::Eq, 2>,
                              RelAnd<ProjBound<2, R, P>,
                                     ProjModConstBound<2, V, Rel::Eq, W>>>>&) {
  return false;
}

/** @brief @f$\arg\max@f$ over a @b partial function: the translation graph
 *  @f$x \mapsto x+K@f$ into a codomain bounded above (@f$\pi_2 \le P@f$) and
 *  restricted to a residue class (@f$\pi_2 \equiv W \pmod V@f$).  A translation
 *  is monotone, so @f$\arg\max = \max@f$ of the @b feasible domain
 *  @f$\{x \le P-K \wedge x \equiv W-K \pmod V\}@f$ = the largest such @f$x@f$,
 *  read off structurally: a compile-time constrained integer optimum, no
 *  search.  The codomain constraint, pulled back through the graph, IS the
 *  domain restriction (§3.3). */
// Gated on the carrier being an ADDITIVE GROUP (@c IsAbelianGroup under @c +):
// the arithmetic below assumes a domain unbounded below, so @c {x ≤ P−K ∧ x ≡ r
// mod V} is always non-empty and @c m is a valid optimum.  Additive inverses
// are exactly what make an integral carrier unbounded below (ℤ certifies it; ℕ
// =
// @c Cardinality is a rig, no negation, bounded below by 0), so this predicate
// admits any signed integral carrier and excludes ℕ --- where a codomain bound
// @c P<K would pull the feasible domain empty while the formula still returned
// a negative singleton.
export template <typename T, auto K, auto P, auto V, auto W, typename L>
  requires dedekind::category::IsAbelianGroup<T, std::plus<T>>
constexpr auto argmax(
    const Set<std::pair<T, T>, L,
              ProductRestrict<ProjAddConstProj<1, K, Rel::Eq, 2>,
                              RelAnd<ProjBound<2, Rel::Le, P>,
                                     ProjModConstBound<2, V, Rel::Eq, W>>>>&) {
  constexpr auto p = P - K;                      // domain bound {x ≤ P−K}
  constexpr auto r = ((W - K) % V + V) % V;      // residue x ≡ (W−K) mod V
  constexpr auto m = p - ((p - r) % V + V) % V;  // largest x ≤ p with x ≡ r
  return Singleton<m, L>{};
}
static_assert(is_entire(ℤ* ℤ | π1 + fix(3_c) == π2),
              "the bare translation graph is total (entire).");
static_assert(!is_entire(ℤ * ℤ | π1 + fix(3_c) == π2 |
                         π2 <= fix(8_c) & π2 % fix(3_c) == fix(0_c)),
              "constraining the codomain makes the graph a partial function.");
static_assert(argmax(ℤ* ℤ | π1 + fix(3_c) == π2 |
                     π2 <= fix(8_c) & π2 % fix(3_c) == fix(0_c))(3),
              "argmax = max{x ≤ 5 ∧ x ≡ 0 mod 3} = 3: a compile-time "
              "constrained optimum.");
static_assert(!argmax(ℤ * ℤ | π1 + fix(3_c) == π2 |
                      π2 <= fix(8_c) & π2 % fix(3_c) == fix(0_c))(4),
              "4 is feasible-adjacent but not the optimiser (4 ≢ 0 mod 3).");

// Modelling witness ("Theorems for Free", type-checked): the SPECIFIC pivot
// overload AGREES with the ABSTRACT definition max R = (∈) ∩ (R/∋) at the
// pivot.  5 is the max because 5 ∈ {x≤5} AND ∀a∈{x≤5}. a ≤ 5 --- the latter
// (the R/∋ division) decided by the counterexample set {x≤5} ∩ {x>5} collapsing
// to ∅ via the complement-pair meet.  So the specialisation is checked against
// the general law, not merely trusted (the Wadler free theorem, mechanised).
static_assert(max(le5)(5) == (le5(5) && ((le5 & (ℕ | (π > fix(5_c)))) == Ø{})),
              "specific max(le5) models (∈) ∩ (R/∋) at the pivot.");
static_assert(min(ge5)(5) == (ge5(5) && ((ge5 & (ℕ | (π < fix(5_c)))) == Ø{})),
              "specific min(ge5) models (∈) ∩ (R/∋) at the pivot.");

// Exhibit (finite case): max 𝔹 = {true}, min 𝔹 = {false} --- the SAME generic
// max/min above, its ∀-projection settled by 𝔹's upperbounds/lowerbounds ({⊤} /
// {⊥}) and the universe-identity meet 𝔹 ∩ {⊤} = {⊤}.
static_assert(max(Ω<bool>)(true), "max 𝔹 = {true}.");
static_assert(!max(Ω<bool>)(false), "false is not the greatest element of 𝔹.");
static_assert(min(Ω<bool>)(false), "min 𝔹 = {false}.");
static_assert(!min(Ω<bool>)(true), "true is not the least element of 𝔹.");
// Modelling witness ("Theorems for Free", type-checked) with a STRUCTURAL
// IsPredicate --- not an opaque lambda, which cannot feed the collapse.  The
// specific max(𝔹) models the abstract (∈) ∩ (R/∋): at false the dominance
// ∀a∈𝔹. a ≤ false FAILS (true ⋠ false), spelled as the halfspace {a ≤ false},
// so false is correctly NOT the max.
static_assert(max(Ω<bool>)(false) ==
                  (Ω<bool>(false) && forall(Ω<bool>, π <= fix(false_c))),
              "specific max(𝔹) models (∈) ∩ (R/∋), structurally.");

// (The image of a halfspace under a translation --- the pivot shifted by K ---
// is now the affine pushforward on the GRAPH surface: image(graph | π1 ⋈ p) =
// {y ⋈ p+K}, below.  The earlier Translation<T,K>-arrow version is superseded.)

// ── Relative product: composition of relations (Tarski) ────────────────────
/** @brief The composed predicate @f$(R \gg S)(a,c) = \exists b.\, R(a,b) \wedge
 *  S(b,c)@f$.  Decidable exactly when the intermediate carrier is finite; here
 *  the @f$\exists b@f$ enumerates the two Booleans, so it folds at compile
 *  time. */
template <typename PR, typename PS>
struct ComposePred {
  PR r;
  PS s;
  template <typename Pair>
  constexpr bool operator()(const Pair& ac) const {
    return (r(std::pair{ac.first, false}) && s(std::pair{false, ac.second})) ||
           (r(std::pair{ac.first, true}) && s(std::pair{true, ac.second}));
  }
};

/** @brief @c R @c >> @c S --- the relative product of two relations over a
 *  @b Boolean intermediate, the ∃-over-the-middle enumerated on @c {false,
 *  true}.  A larger intermediate needs the finite-quotient handle (§3.1);
 *  there is deliberately no overload, so it is an honest compile error. */
export template <typename A, typename B, typename C, typename L, typename PR,
                 typename PS>
  requires std::same_as<B, bool>
constexpr auto operator>>(const Set<std::pair<A, B>, L, PR>& r,
                          const Set<std::pair<B, C>, L, PS>& s) {
  return Set<std::pair<A, C>, L, ComposePred<PR, PS>>{
      ComposePred<PR, PS>{r.predicate(), s.predicate()}};
}

// ≤ ∘ ≤ = ≤ (transitivity), decidable because the intermediate is Boolean.
static_assert(static_cast<bool>(((𝔹 * 𝔹 | π1 <= π2) >>
                                 (𝔹 * 𝔹 | π1 <= π2))(std::pair{false, true})),
              "≤ ∘ ≤ contains (false, true).");
static_assert(!static_cast<bool>(((𝔹 * 𝔹 | π1 <= π2) >>
                                  (𝔹 * 𝔹 | π1 <= π2))(std::pair{true, false})),
              "≤ ∘ ≤ excludes (true, false): transitivity recovers ≤.");

}  // namespace dedekind::order
