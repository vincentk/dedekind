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
 *     inline constexpr auto n = element<𝔸<ℕ>>;
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
#include <limits>      // std::numeric_limits (machine-carrier boundary check)
#include <type_traits>
#include <utility>

export module dedekind.order:halfspace;

import dedekind.category;
import dedekind.sets;
import dedekind.relational; // IsFunctional/IsEntire (:graph), used by the order
                            // relation witnesses below (#792)
import :poset;  // IsPartiallyOrdered — the SEMANTIC order certificate for
                // max/min
import :total;  // IsTotallyOrdered — the no-incomparable-element certificate
                // that keeps the covering join sound (excludes NaZ carriers)

namespace dedekind::order {
using namespace dedekind::sets;
using namespace dedekind::relational;  // converse/diagonal/reflexive/RelAnd/...
using namespace dedekind::category;
// The relation operators (moved to :relational, #792) must be pulled in by
// NAME, not only the directive above: @c order declares its OWN operator>> /
// operator+ / operator& (on halfspaces / projections), and a nearer-scope
// declaration hides using-DIRECTIVE names from unqualified (operator) lookup.
// Before the move these were reached by ADL on the @c Set<pair> operand (ADL
// ignores such hiding); the using-DECLARATIONS restore exactly that candidate
// set, joining --- not losing to --- order's own operators.
using dedekind::relational::operator>>;  // the relative product R;S
using dedekind::relational::operator&;   // relation meet R & S
// relation union is the set-grammar | (the free operator|; no using
// needed)

// @c IsRingIntegral moved to @c :sets:cardinality (#878): it depends only
// on @c std::integral and the cardinal carriers, so it belongs at the
// cardinality authority, not accreted here.  Reached via @c import
// @c dedekind.sets (already imported above).

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

/** @brief Flip a @c Direction --- the direction half of a halfspace complement
 *  (@c ~{x>P} is @c {x≤P}).  Reused by the reflection-image pushforward
 *  (@c dedekind.algebra:halfspace_transport) to flip a halfspace's sense under
 *  @f$x \mapsto -x@f$. */
export constexpr Direction flip(Direction d) {
  return d == Direction::Upward ? Direction::Downward : Direction::Upward;
}
/** @brief Flip a @c Strictness --- the strictness half of the same complement
 *  (@c ~{x>P} is @c {x≤P}: @c > relaxes to @c ≤). */
export constexpr Strictness flip(Strictness s) {
  return s == Strictness::Strict ? Strictness::NonStrict : Strictness::Strict;
}

/** @brief A carrier bounded below at @c 0 --- @c bool, an @c unsigned machine
 *  integer, and the ℕ proxy @c Cardinality --- so no value is @c < @c 0.
 *  @b Not the signed ℤ proxy @c SignedCardinality, which is @c IsSaturating too
 *  but is unbounded below: the trap the bare @c IsSaturating floor test fell
 *  into (#837 review) --- it rejected the valid cut @c {x<0} on ℤ and misread
 *  @c {x≥0} as moot.  The dedicated lower-bound classification for the
 *  emptiness oracles below. */
template <typename T>
concept HasZeroFloor =
    std::unsigned_integral<T> || std::same_as<T, dedekind::sets::Cardinality>;

/** @brief Is the strict lower cut @c {x<p} empty (i.e. @c p at/below the
 *  carrier's least element)?  The @c if constexpr isolates @c numeric_limits so
 *  it is instantiated ONLY for a signed machine int --- a floor-0 carrier (ℕ /
 *  unsigned / bool) tests @c p≤0, a dense/unbounded-below carrier never
 * empties.
 */
template <typename T, auto p>
consteval bool strict_lower_cut_empty() {
  if constexpr (std::signed_integral<T>)
    return p <= std::numeric_limits<T>::min();
  else if constexpr (HasZeroFloor<T>)
    return p <= 0;
  else
    return false;
}
/** @brief Is the strict upper cut @c {x>p} empty (@c p at/above the carrier's
 *  greatest element)?  Only a bounded MACHINE integer (@c numeric_limits::max)
 *  can empty here; ℕ is unbounded above and a dense carrier never empties. */
template <typename T, auto p>
consteval bool strict_upper_cut_empty() {
  if constexpr (std::integral<T>)
    return p >= std::numeric_limits<T>::max();
  else
    return false;
}

/** @brief Does the halfspace @f$\{x \mathbin{\lrcorner} p\}@f$ denote the empty
 *  set on carrier @c T?  The single emptiness oracle over all four
 *  direction/strictness combinations: the strict cuts reuse the tests above; a
 *  @b non-strict cut empties only when its pivot escapes the carrier's range
 *  entirely (@c {x≥p} with @c p above @c max(T), @c {x≤p} with @c p below @c
 *  min(T)) --- impossible for a representable pivot, so @c false for an
 * ordinary halfspace and @c true only at the machine ceiling / floor. */
template <typename T, auto Pivot, Direction D, Strictness S>
consteval bool halfspace_is_empty() {
  if constexpr (D == Direction::Upward) {
    if constexpr (S == Strictness::Strict)
      return strict_upper_cut_empty<T, Pivot>();  // {x>p}: p ≥ max
    else if constexpr (std::integral<T>)
      return Pivot > std::numeric_limits<T>::max();  // {x≥p}: p > max
    else
      return false;
  } else {  // Downward
    if constexpr (S == Strictness::Strict)
      return strict_lower_cut_empty<T, Pivot>();  // {x<p}: p ≤ min
    else if constexpr (std::signed_integral<T>)
      return Pivot < std::numeric_limits<T>::min();  // {x≤p}: p < min
    else if constexpr (HasZeroFloor<T>)
      return Pivot < 0;  // floor-0 carrier: {x≤p} empty iff p < 0
    else
      return false;
  }
}

/** @brief Is the halfspace @b moot --- its χ ≡ ⊤, so it is all of @c T (e.g.
 *  @c {x≥0} on ℕ, or @c {x>−1} below the floor)?  A halfspace is universal iff
 *  its complement is empty, so this is exactly @c halfspace_is_empty on the
 *  flipped cut --- one oracle, both boundary degeneracies. */
template <typename T, auto Pivot, Direction D, Strictness S>
consteval bool halfspace_is_moot() {
  return halfspace_is_empty<T, Pivot, flip(D), flip(S)>();
}

/** @brief Cardinality class of a halfspace's carrier @c T (#848/#927).
 *  @details A carrier that declares its own @c cardinality_type is the
 *  authority (e.g.\ @c numbers::Rational → @c ℵ_0, @c ExtensionalCardinal →
 *  @c Finite): this partition (@c :order) is @b upstream of @c :numbers, so a
 *  countable @b non-integral carrier such as ℚ cannot be recognised
 *  structurally here (@c IsRingIntegral is false on @c Rational), and must
 *  self-declare so the point-free @c A|pred halfspace classifies identically
 *  to the scout comprehension (which inherits the ambient @c C directly).
 *  Otherwise fall back to the @c IsRingIntegral discriminator: the structural
 *  integers @b and the @c Cardinality / @c SignedCardinality (ℕ/ℤ) proxies are
 *  @c ℵ_0, and the real proxies (@c QuadraticReal, @c double) are the continuum
 *  @c ℶ_1.  The bound only has to be tight enough for the @c NaturalLogic
 *  verdict (countable ⟹ decidable @c Boole, uncountable ⟹ @c Kleene). */
// Module-private: the fallback is a halfspace-classification heuristic, not a
// general carrier-cardinality authority (that is @c :sets:cardinality).  It
// only feeds @c Halfspace::cardinality_type below; external carriers customise
// through their own @c T::cardinality_type, not this helper.  A finite @b enum
// carrier (e.g.\ @c Ternary) cannot self-declare a member typedef, so it is
// recognised structurally as countable and classified @c ℵ_0 --- the same
// conservative countable BOUND @c bool already gets via @c IsRingIntegral, NOT
// @c Finite.  @c Finite is the @c elevate_meet "return bare, do not @c Set
// -wrap" signal (@c :expressions); a halfspace must not trigger it, and @c ℵ_0
// keeps the pre-existing @c Set-wrapping path while @c NaturalLogic still
// verdicts @c Boole (@c ℵ_0 is countable), matching @c Ternary's @c 𝕂3 ambient.
// The @c IsRingIntegral integers / ℕ,ℤ proxies are @c ℵ_0 likewise; the real
// proxies are @c ℶ_1.
template <typename T>
struct carrier_cardinality {
  using type =
      std::conditional_t<std::is_enum_v<T> || IsRingIntegral<T>, ℵ_0, ℶ_1>;
};
template <typename T>
  requires IsCardinality<typename T::cardinality_type>
struct carrier_cardinality<T> {
  // Fires only when the self-declared alias is a genuine @c IsCardinality (a
  // carrier with an unrelated / incomplete @c cardinality_type falls to the
  // primary, not a hard error), and classifies it with the canonical
  // @c IsCountable concept, as @c NaturalLogic does.  Collapse to the
  // COUNTABILITY BOUND (never pass a bare @c Finite through): @c
  // ExtensionalCardinal declares @c Finite, and letting that reach @c
  // Halfspace::cardinality_type would trip @c elevate_meet's Finite "return
  // bare" path for an @c ExtensionalCardinal halfspace.  Countable (incl.\
  // @c Finite) ⟹ @c ℵ_0, uncountable ⟹ @c ℶ_1; @c NaturalLogic's decidable/
  // @c Boole verdict is unchanged by the countable collapse.
  using type =
      std::conditional_t<IsCountable<typename T::cardinality_type>, ℵ_0, ℶ_1>;
};
template <typename T>
using carrier_cardinality_t = typename carrier_cardinality<T>::type;

// A finite enum carrier classifies ℵ_0 (a conservative countable bound, like
// bool), so a Ternary halfspace stays decidable (Boole), not Kleene (#927), and
// does NOT trip the elevate_meet Finite "return bare" path.
static_assert(std::same_as<carrier_cardinality_t<Ternary>, ℵ_0>,
              "a finite enum carrier (e.g. Ternary) is countable ℵ_0, not the "
              "continuum ℶ_1 (and not Finite, the elevate_meet bare signal).");
// A self-declared @c Finite carrier is likewise collapsed to @c ℵ_0, so its
// halfspace stays Set-wrapped by @c elevate_meet (not returned bare).
static_assert(std::same_as<carrier_cardinality_t<ExtensionalCardinal<>>, ℵ_0>,
              "a self-declared Finite carrier collapses to the countable bound "
              "ℵ_0, never the bare-signal Finite.");
namespace detail_halfspace_witness {
// A carrier whose @c cardinality_type alias is NOT an @c IsCardinality: the
// self-declaring specialization must @b not fire (its IsCardinality constraint
// fails), so @c carrier_cardinality falls back to the primary rather than
// hard-erroring on the missing @c is_countable member.
struct UnrelatedCardinalityAlias {
  using cardinality_type = int;  // not a cardinality (no is_countable etc.)
};
}  // namespace detail_halfspace_witness
static_assert(
    std::same_as<carrier_cardinality_t<
                     detail_halfspace_witness::UnrelatedCardinalityAlias>,
                 ℶ_1>,
    "a carrier with an unrelated/incomplete cardinality_type falls back to the "
    "primary (ℶ_1 here), never making Halfspace ill-formed.");

/**
 * @brief Halfspace predicate { x ∈ T | x ⋈ Pivot } with Pivot at the type
 * level.
 *
 * `⋈` ∈ { >, >=, <, <= }, selected by `D` (direction) and `S` (strictness).
 */
export template <typename T, auto Pivot, Direction D, Strictness S,
                 typename L = Boole>
struct Halfspace : dedekind::sets::SetExpr<Halfspace<T, Pivot, D, S, L>, T, L> {
  // A Halfspace value is an INHABITED cut by construction (#832): an empty
  // configuration (@c {x>max(T)}, @c {x<min(T)}) is ill-formed here and must be
  // spelt @c Ø --- @c make_halfspace collapses it, so no code path forms one.
  // The gate closes raw construction, making @f$\emptyset = \text{Halfspace}@f$
  // a genuine type-level impossibility rather than a factory convention.
  static_assert(!halfspace_is_empty<T, Pivot, D, S>(),
                "empty halfspace is not representable: construct through the "
                "DSL / make_halfspace (which yields Ø), never the raw type");
  // Domain / Codomain / logic_species / Member / ι are inherited from SetExpr
  // (the ETCS subobject surface): a bare Halfspace is a first-class
  // @c IsSubobject (ι: S ↣ T) whose χ is @c operator() below.  This is the same
  // mixin @c Interval / @c Ray / @c Singleton fold onto, so the subobject
  // boilerplate lives in exactly one place (#806 follow-up dedup).
  static constexpr auto pivot = Pivot;
  static constexpr Direction direction = D;
  static constexpr Strictness strictness = S;

  /** @brief Carrier-axis cardinality of the cut, threaded so the decidability
   *  classifier (@c sets::NaturalLogic) reads a halfspace the SAME way it reads
   *  the ambient it was carved from (#848/#927).
   *
   *  @details A @b conservative classification bound, NOT the cut's exact size:
   *  a halfspace is at most equinumerous with its carrier, so the bound is
   *  @f$\aleph_0@f$ over a countable carrier and @f$\beth_1@f$ over a
   * continuum. A @b bounded cut (e.g.\ @c {x∈ℕ|x<5}) is actually @c Finite; the
   * bound only has to be tight enough for the @c NaturalLogic verdict
   * (countable ⟹
   *  @c Boole/decidable, uncountable ⟹ @c Kleene), which the finite and
   *  @f$\aleph_0@f$ cases share.  The class is resolved by @ref
   *  carrier_cardinality: a self-declaring carrier (@c ℚ = @c Rational →
   *  @c ℵ_0) is trusted, else the @c IsRingIntegral discriminator applies. This
   *  reproduces the tag the ambient @c UniversalSet<T,L,C> carries for the
   *  canonical carriers, so the point-free @c A @c | @c pred comprehension
   *  classifies identically to the (deprecated) scout @c element<A> @c | @c
   * pred spelling, whose @c Comprehension inherits @c C directly.  Without this
   *  typedef @c NaturalLogic<Halfspace> hit its pessimistic primary-template
   *  fallback (@c Kleene / @c TernaryLogic).
   *
   *  @note This is the carrier-axis @b magnitude, NOT the ambient's own @c C
   *  slot, which the @c Halfspace type does not carry.  The two need not be the
   *  @b identical tag; what @c NaturalLogic reads off is the @b countability
   *  @b class (countable ⟹ @c Boole, uncountable ⟹ @c Kleene), and parity with
   *  the scout holds whenever the carrier axis and the ambient @c C share that
   *  class.  The @b canonical and @b self-declaring carriers satisfy this:
   *  ℕ/ℤ via @c IsRingIntegral, ℚ (and any carrier that self-declares) via its
   *  own @c cardinality_type, and ℝ (@c QuadraticReal) as the continuum.  A
   *  @b custom ordered carrier that is countable but neither @c IsRingIntegral
   *  nor self-declaring falls through to @c ℶ_1 here, even though its default
   *  @c 𝔸 ambient carries @c ℵ_0; it must self-declare (as ℚ does) to classify
   *  decidably.  The exact tags may also differ within a class, e.g.\
   *  @c 𝔸<bool> carries @c Finite (@c boundaries.cppm) while this fallback maps
   *  @c bool to @c ℵ_0 --- both countable, same @c Boole verdict.  Only a
   *  @b deliberately incoherent tag
   *  that crosses classes is not honoured: an int carrier advertised as the
   *  continuum (@c UniversalSet<int,Boole,ℶ_1>, the Mandelbrot stand-in at
   *  @c computability_test.cpp) classifies @c ℵ_0 by its integer carrier while
   *  the scout keeps @c ℶ_1.  That does not arise from a real halfspace (no
   *  continuum is genuinely carried by @c int), so the carrier axis is the
   *  honest source.  The dual incoherence (a countable carrier tagged with
   *  @c Kleene logic, @c UniversalSet<int,Kleene>) can surface a @c Set
   * codomain mismatch when the promoted @c Boole class disagrees with the
   * predicate's own @c Kleene species; that @c Set / @c NaturalLogic
   * interaction is tracked in FIXME(#928).  Reproducing an arbitrary explicit
   * @c C exactly would require threading it as a sixth @c Halfspace template
   * parameter (FIXME(#848): ~120 pattern-matched sites). */
  using cardinality_type = carrier_cardinality_t<T>;

  // `Pivot` may be a different structural type than `T` (e.g., pivot = 5.0 as
  // double, T = Real<double>). The carrier's converting ctor / overload set
  // handles the comparison; we only assume `T` is comparable with the pivot.
  // Return type is spelt @c L::Ω (not the inherited @c Codomain, which
  // unqualified lookup would miss through the dependent SetExpr base).
  constexpr typename L::Ω operator()(const T& x) const {
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
 * L defaults to `Boole` because a cardinality-1 extensional set
 * has decidable membership regardless of ambient logic species.
 */
export template <auto Value, typename L = Boole>
struct Singleton
    : dedekind::sets::SetExpr<Singleton<Value, L>, decltype(Value), L> {
  // Domain / Codomain / logic_species / Member / ι are inherited from SetExpr
  // (the ETCS subobject surface): the static Singleton is a first-class
  // @c IsSubobject (ι: {value} ↣ Domain) whose χ is @c operator() below — same
  // mixin @c Halfspace / @c Interval / @c Ray fold onto (#806 follow-up dedup).
  using Domain = decltype(Value);
  using cardinality_type = Finite;
  using is_extensional_tag = void;
  using is_compile_time_extensional_tag = void;
  using is_static_singleton_tag = void;  // For operator& collapse detection

  static constexpr Domain value = Value;

  // Return type spelt @c L::Ω, not the inherited (dependent-base) @c Codomain.
  constexpr typename L::Ω operator()(const Domain& x) const {
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
  constexpr typename L::Ω operator()(const U& x) const {
    return (x == Value) ? L::True : L::False;
  }

  constexpr std::size_t size() const { return 1; }

  // Cross-logic identity: `Singleton<V, L1>` and `Singleton<V, L2>` represent
  // the same singleton; enables the reveal `s == Singleton<V>{}` when s's
  // logic species was inherited from a Set (e.g. Kleene over ℕ).
  template <typename OtherL>
  constexpr bool operator==(const Singleton<Value, OtherL>&) const {
    return true;
  }
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
  requires std::same_as<decltype(A), decltype(B)> && std::same_as<LA, LB>
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
  requires std::same_as<decltype(A), decltype(B)> && std::same_as<LA, LB> &&
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

/** @brief The halfspace factory (#832): a @c Halfspace @b value denotes a
 *  @b proper cut by construction.  A degenerate configuration collapses to the
 *  canonical boundary set instead --- an empty cut to @c Ø, a moot cut to the
 *  universe @c UniversalSet --- so @f$\emptyset = \text{Halfspace}@f$ and
 *  @f$\mathbb{A} = \text{Halfspace}@f$ never arise as values and the boundary
 * cases are decided by @c Ø / @c 𝔸's own initial / terminal machinery.  The
 * return type is heterogeneous but statically resolved by @c if @c constexpr
 * (no type erasure); every halfspace-producing surface routes through it. */
export template <typename T, auto V, Direction D, Strictness S,
                 typename L = Boole>
constexpr auto make_halfspace() {
  // Codomain leg (#894): a degenerate halfspace collapses to a decided
  // boundary, so it carries the Boolean codomain whatever the ambient.
  if constexpr (halfspace_is_empty<T, V, D, S>())
    return dedekind::sets::codomain_reduce_t<dedekind::sets::Ø<T, L>>{};
  else if constexpr (halfspace_is_moot<T, V, D, S>())
    return dedekind::sets::codomain_reduce_t<
        dedekind::sets::UniversalSet<T, L>>{};
  else
    return Halfspace<T, V, D, S, L>{};
}

/** @brief Complement of a halfspace: the opposite halfspace, through the
 *  factory so a boundary complement collapses (@c ~{x≥0} on ℕ is @c {x<0} = Ø,
 *  and dually @c ~Ø = 𝔸 keeps the involution). */
export template <typename T, auto Pivot, Direction D, Strictness S, typename L>
constexpr auto operator~(const Halfspace<T, Pivot, D, S, L>&) {
  return make_halfspace<T, Pivot, flip(D), flip(S), L>();
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
  // Codomain leg (#894): the universe is decided → Boolean codomain.
  return dedekind::sets::codomain_reduce_t<
      dedekind::sets::UniversalSet<T, L>>{};
}

/** @brief Complement-pair meet: dually, the empty set. */
export template <typename T, auto Pivot, Direction D1, Strictness S1,
                 Direction D2, Strictness S2, typename L>
  requires(D1 != D2 && S1 != S2)
constexpr auto operator&(const Halfspace<T, Pivot, D1, S1, L>&,
                         const Halfspace<T, Pivot, D2, S2, L>&) {
  // Codomain leg (#894): the empty set is decided → Boolean codomain.
  return dedekind::sets::codomain_reduce_t<dedekind::sets::Ø<T, L>>{};
}

/** @brief Telling aliases for the two ℕ halfspaces the §3 listing uses:
 *         @c Above<N> = {x>N}, @c AtMost<N> = ~Above<N> = {x<=N}. */
export template <auto N, typename L = Boole>
using Above = Halfspace<dedekind::sets::Cardinality, N, Direction::Upward,
                        Strictness::Strict, L>;
export template <auto N, typename L = Boole>
using AtMost = Halfspace<dedekind::sets::Cardinality, N, Direction::Downward,
                         Strictness::NonStrict, L>;

// A bare Halfspace / Singleton is a first-class @c IsSubobject (ι: S ↣ A plus
// its own χ), though NOT a full ETCS @c IsSet: @c IsSet additionally demands
// the ETCS-axiom surface (@c HasETCSAxioms + the CCC witness) that only the
// ambient universe @c 𝔸<T> carries.  Subobject-hood is the right membership —
// it is what the complement-lattice operators above operate on.
static_assert(IsSubobject<Above<5>, dedekind::sets::Cardinality>,
              "a Halfspace is a first-class subobject ι: S ↣ ℕ.");
static_assert(IsSubobject<Singleton<true>, bool>,
              "a static Singleton is a first-class subobject.");

/** @brief Carrier-aware ordering of two interval-endpoint NTTPs.  Integral
 *  pivots compare by @b mathematical value through @c std::cmp_less /
 *  @c std::cmp_equal, so a signed and an unsigned pivot are not silently
 *  mis-ranked by C++'s usual arithmetic conversions (@c -1 @c > @c 0u is @c
 *  true as a plain comparison, but @c −1 precedes @c 0 in the carrier order);
 *  any other carrier uses its native @c < / @c ==.  Interval emptiness and
 *  subset are decided through these --- never by subtracting endpoints, which
 *  wraps on an unsigned carrier and overflows a full-range signed interval in a
 *  constant expression (#835 review). */
export template <auto A, auto B>
consteval bool pivot_less() {
  if constexpr (std::integral<decltype(A)> && std::integral<decltype(B)>)
    return std::cmp_less(A, B);
  else
    return A < B;
}
export template <auto A, auto B>
consteval bool pivot_equal() {
  if constexpr (std::integral<decltype(A)> && std::integral<decltype(B)>)
    return std::cmp_equal(A, B);
  else
    return A == B;
}

/** @brief Manual @c constexpr floor / ceil to an integer --- truncation toward
 *  zero, adjusted by sign --- avoiding a @c <cmath> @c constexpr dependency.
 *  Normalises a floating interval pivot to its effective carrier integer. */
consteval long long cfloor(double d) {
  const long long t = static_cast<long long>(d);  // toward zero
  return static_cast<double>(t) > d ? t - 1 : t;
}
consteval long long cceil(double d) {
  const long long t = static_cast<long long>(d);
  return static_cast<double>(t) < d ? t + 1 : t;
}
template <auto p>
consteval long long pivot_floor() {
  if constexpr (std::integral<decltype(p)>)
    return static_cast<long long>(p);
  else
    return cfloor(static_cast<double>(p));
}
template <auto p>
consteval long long pivot_ceil() {
  if constexpr (std::integral<decltype(p)>)
    return static_cast<long long>(p);
  else
    return cceil(static_cast<double>(p));
}

/** @brief The @b effective inclusive carrier bounds of a DISCRETE interval
 *  boundary (#835 review): the tightest carrier integer the boundary admits.
 *  So distinct pivot / strictness pairs that denote the @b same discrete set
 *  --- @c (1,4) and @c [2,3] are both @c {2,3} over @c int --- normalise equal,
 *  and an open or fractional bound (@c (5.0,6.0) has no member) is decided
 *  exactly.  Computed in a wide @c long @c long, so the successor / predecessor
 *  never wraps the pivot type (spans beyond @c size_t are the documented policy
 *  corner, #838).  This is the local realisation of the @f$\mathbb{Z}
 *  \hookrightarrow \mathbb{R}@f$ pullback of #838.
 *  @c eff_lower: smallest integer admitted by @f$\{x > p\}@f$ / @f$\{x \ge
 *  p\}@f$; @c eff_upper: largest admitted by @f$\{x < p\}@f$ / @f$\{x \le
 *  p\}@f$. */
export template <auto p, Strictness S>
consteval long long eff_lower() {
  return S == Strictness::Strict ? pivot_floor<p>() + 1 : pivot_ceil<p>();
}
export template <auto p, Strictness S>
consteval long long eff_upper() {
  return S == Strictness::Strict ? pivot_ceil<p>() - 1 : pivot_floor<p>();
}

/** @brief Meet of two opposing halfspaces — an order-theoretic interval. */
export template <typename T, auto Lo, auto Hi, Strictness SL, Strictness SU,
                 typename L = Boole>
struct OrderInterval
    : dedekind::sets::SetExpr<OrderInterval<T, Lo, Hi, SL, SU, L>, T, L> {
  // Domain / Codomain / logic_species / Member / ι inherited from SetExpr — the
  // order-layer twin of topology::Interval, now on the same subobject mixin
  // (#806 follow-up dedup); χ is @c operator() below.
  static constexpr auto lower_pivot = Lo;
  static constexpr auto upper_pivot = Hi;
  static constexpr Strictness lower_strictness = SL;
  static constexpr Strictness upper_strictness = SU;

  // Return type spelt @c L::Ω, not the inherited (dependent-base) @c Codomain.
  constexpr typename L::Ω operator()(const T& x) const {
    const bool lo_ok = (SL == Strictness::Strict) ? (x > Lo) : (x >= Lo);
    const bool hi_ok = (SU == Strictness::Strict) ? (x < Hi) : (x <= Hi);
    return (lo_ok && hi_ok) ? L::True : L::False;
  }

  // For integer-range carriers, cardinality is compile-time-decidable
  // from the bounds and strictness pair.  Gate the size() / cardinality_type
  // surface so that continuous carriers (like Real<double>) correctly fail
  // IsExtensional, AND so that the variant ℕ-/ℤ-proxy carriers from
  // sets:cardinality (Cardinality, SignedCardinality) keep this surface
  // post-#402 retarget.  The IsRingIntegral concept (in :sets:cardinality,
  // relocated #878) is the post-#414 generalisation of std::integral — same
  // semantics for the built-in integers, plus admission of the variant
  // carriers.
  static constexpr bool is_integer_range = IsRingIntegral<T>;

  // @brief Whether the interval denotes the empty set (χ ≡ False).  A DISCRETE
  // carrier decides on the @b effective carrier bounds (@c eff_lower /
  // @c eff_upper): empty ⟺ the tightest admitted lower integer exceeds the
  // tightest admitted upper integer.  This is exact for every strictness combo,
  // an inverted or open gap (@c (5,6) empty), a fractional or integer-valued
  // floating pivot (@c (5.0,6.0) empty), and a full range (no endpoint
  // subtraction to wrap or overflow) --- and it is the same normalisation the
  // subset test and @c size() use, so distinct pivots denoting the same set
  // agree (#835 review).  A CONTINUOUS carrier has distinct pivots for distinct
  // sets, so endpoint degeneracy suffices (@c Lo>Hi, or @c Lo==Hi with an open
  // end --- @c [5,5] is the singleton).  Empty intervals are representable, so
  // @c :inclusion recognises @f$\emptyset \subseteq X@f$ for every @c X.
  static constexpr bool is_empty = [] {
    if constexpr (is_integer_range)
      // Discrete: empty ⟺ no carrier integer between the effective bounds.  One
      // comparison over the normalised bounds handles every strictness combo,
      // an inverted or open gap, and a full range --- no endpoint arithmetic.
      return eff_lower<Lo, SL>() > eff_upper<Hi, SU>();
    else
      // Continuous: distinct pivots are distinct sets; endpoint degeneracy
      // only.
      return pivot_less<Hi, Lo>() ||
             (pivot_equal<Lo, Hi>() &&
              (SL == Strictness::Strict || SU == Strictness::Strict));
  }();

  constexpr std::size_t size() const
    requires is_integer_range
  {
    constexpr long long lo = eff_lower<Lo, SL>();
    constexpr long long hi = eff_upper<Hi, SU>();
    if constexpr (hi < lo)
      return 0u;  // empty
    else
      return static_cast<std::size_t>(hi - lo + 1);  // wide span; fits size_t
                                                     // for any ≤64-bit range
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
  return make_halfspace<T, V, Direction::Upward, Strictness::Strict>();
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
  return make_halfspace<T, V, Direction::Upward, Strictness::NonStrict>();
}

export template <auto Ambient, auto V>
  requires std::convertible_to<
               decltype(V), typename dedekind::sets::BoundScout<Ambient>::T> &&
           (!std::unsigned_integral<
                typename dedekind::sets::BoundScout<Ambient>::T> ||
            !std::signed_integral<decltype(V)> || V >= 0)
constexpr auto operator<(const dedekind::sets::BoundScout<Ambient>&, Bound<V>) {
  using T = typename dedekind::sets::BoundScout<Ambient>::T;
  return make_halfspace<T, V, Direction::Downward, Strictness::Strict>();
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
  return make_halfspace<T, V, Direction::Downward, Strictness::NonStrict>();
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
      // showcases like @c bound<-21.0> on @c element<𝔸<int>>) doesn't shift.
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
  requires(
      IsTotallyOrdered<T> &&
      ((SL == Strictness::Strict && SU == Strictness::Strict) ? (Lo < Hi)
       : (SL == Strictness::NonStrict && SU == Strictness::NonStrict &&
          IsRingIntegral<T>)
           // discrete: adjacent bounds cover (no int gap).  Spelled
           // Lo−1≤Hi (⟺ Lo≤Hi+1) but WITHOUT Hi+1: the short-circuit only
           // reaches Lo−1 when Lo>Hi≥min, so the predecessor is boundary-
           // safe where Hi+1 would overflow a signed / wrap an unsigned max.
           ? (Lo <= Hi || Lo - 1 <= Hi)
           : (Lo <= Hi)))
constexpr auto structured_or(Halfspace<T, Lo, Direction::Upward, SL, L>,
                             Halfspace<T, Hi, Direction::Downward, SU, L>) {
  // Codomain leg (#894): the covering union is the decided universe → Boole.
  return dedekind::sets::codomain_reduce_t<
      dedekind::sets::UniversalSet<T, L>>{};
}
export template <typename T, auto Hi, auto Lo, Strictness SU, Strictness SL,
                 typename L>
  requires(
      IsTotallyOrdered<T> &&
      ((SL == Strictness::Strict && SU == Strictness::Strict) ? (Lo < Hi)
       : (SL == Strictness::NonStrict && SU == Strictness::NonStrict &&
          IsRingIntegral<T>)
           // discrete: adjacent bounds cover (no int gap).  Spelled
           // Lo−1≤Hi (⟺ Lo≤Hi+1) but WITHOUT Hi+1: the short-circuit only
           // reaches Lo−1 when Lo>Hi≥min, so the predecessor is boundary-
           // safe where Hi+1 would overflow a signed / wrap an unsigned max.
           ? (Lo <= Hi || Lo - 1 <= Hi)
           : (Lo <= Hi)))
constexpr auto structured_or(Halfspace<T, Hi, Direction::Downward, SU, L>,
                             Halfspace<T, Lo, Direction::Upward, SL, L>) {
  // Codomain leg (#894): the covering union is the decided universe → Boole.
  return dedekind::sets::codomain_reduce_t<
      dedekind::sets::UniversalSet<T, L>>{};
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

// Projection tags + coord moved to :sets:expressions (#878 inc 1); reached via
// import dedekind.sets + `using namespace dedekind::sets`.

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

/** @brief @c !pred on an unbound halfspace: negate the predicate by flipping
 *  the halfspace's sense, @c !(x @c > @c V) @c = @c (x @c <= @c V).
 *
 *  @details The predicate-level dual of @c operator~ on a @b bound
 *  @c Halfspace (a SET, line ~422): same @c flip(D)/flip(S), one level up on
 *  the unbound comprehension query.  This is the @c p1 of the §3 grammar
 *  (Listing~\\ref{lst:set-grammar}): @c ! negates a @b predicate, @c ~
 *  complements a @b set.  Only the atom level is covered here; the wider
 *  discipline (compound De Morgan, retiring @c !set) is FIXME(#829). */
export template <Direction D, Strictness S, auto V>
constexpr UnboundHalfspace<flip(D), flip(S), V> operator!(
    const UnboundHalfspace<D, S, V>&) {
  return {};
}

// carrier | unbound → the Domain-bound predicate, reusing Halfspace /
// Singleton. The RHS type is distinct from Set, so this does not clash with the
// union operator| on a UniversalSet (that one takes a Set).
export template <typename T, typename L, typename C, Direction D, Strictness S,
                 auto V>
constexpr auto operator|(const UniversalSet<T, L, C>&,
                         const UnboundHalfspace<D, S, V>&) {
  // Through the factory (#837 review): a degenerate binder collapses like any
  // other construction --- @c 𝔸<bool> | (π > fix(true_c)) is @c {x>true} = Ø,
  // not a raw (gate-tripping) halfspace.
  return make_halfspace<T, V, D, S, L>();
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
static_assert(
    std::same_as<decltype(𝔹 | (π == fix(true_c))), Singleton<true, Boole>>,
    "𝔹 | π == fix(true_c) is Singleton<true>, spelled point-free.");
static_assert(static_cast<bool>((𝔹 | (π == fix(true_c)))(true)),
              "true ∈ {true}.");
static_assert(!static_cast<bool>((𝔹 | (π == fix(true_c)))(false)),
              "false ∉ {true}.");

// !pred (grammar p1): negating an unbound halfspace flips its sense, and binds
// to the same set as the flipped comparison — the predicate-level dual of ~set.
static_assert(
    std::same_as<decltype(!(π > fix(5_c))), decltype(π <= fix(5_c))>,
    "!(π > fix(5)) is π <= fix(5): the unbound (predicate) complement.");
static_assert(std::same_as<decltype(!(π >= fix(5_c))), decltype(π < fix(5_c))>,
              "!(π >= fix(5)) is π < fix(5).");
static_assert(
    std::same_as<decltype(ℕ | !(π > fix(5_c))), decltype(ℕ | (π <= fix(5_c)))>,
    "ℕ | !(π > fix(5)) binds to the same halfspace as ℕ | π <= fix(5).");

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

/** @section halfspace__PointFree_Scout_Decidability_848
 *
 * #848 acceptance witness: the point-free comprehension @c ℕ @c | @c pred and
 * the (deprecated) scout spelling @c element<ℕ> @c | @c pred now classify
 * IDENTICALLY on the carrier-axis decidability resolver.  The point-free path
 * reduces to a bare @c Halfspace, whose freshly-threaded @c cardinality_type
 * (see the struct, @c ℵ_0 over the countable @c ℕ) makes @c NaturalLogic read
 * the same @c Boole verdict the scout @c Comprehension inherits from its
 * ambient @c C.  Before the thread @c NaturalLogic<Halfspace> hit its
 * pessimistic primary-template fallback (@c Kleene / @c TernaryLogic). */
namespace detail_848_pointfree_scout {
using PointFree = decltype(ℕ | (π > fix(5_c)));
using Scout = decltype(element<ℕ> | (element<ℕ> > bound<5>));

// The raw comprehensions agree on the NaturalLogic (carrier-axis) verdict.
static_assert(std::same_as<typename NaturalLogic<PointFree>::type,
                           typename NaturalLogic<Scout>::type>,
              "#848: point-free ℕ|pred and scout element<ℕ>|pred yield the "
              "same NaturalLogic verdict.");
static_assert(std::same_as<typename NaturalLogic<PointFree>::type, Boole>,
              "#848: {x∈ℕ | x>5} is carrier-axis countable (ℵ₀), hence Boole "
              "(decidable membership), NOT the Kleene fallback.");

// And the observable symptom 1: the Set-wrapped forms agree on decidable
// membership (the Set CTAD keys the logic species off NaturalLogic<inner>).
static_assert(HasDecidableMembership<decltype(Set{ℕ | (π > fix(5_c))})> ==
                  HasDecidableMembership<decltype(Set{
                      element<ℕ> | (element<ℕ> > bound<5>)})>,
              "#848: Set{ℕ|pred} and Set{element<ℕ>|pred} agree on "
              "HasDecidableMembership.");
static_assert(HasDecidableMembership<decltype(Set{ℕ | (π > fix(5_c))})>,
              "#848: Set{ℕ | x>5} is a decidable (ClassicalLogic) set.");

// The continuum leg (ℝ) stays honestly ternary through the SAME thread: a real
// halfspace is carrier-axis ℶ₁, so NaturalLogic keeps its Kleene verdict --- no
// regression of the uncountable case the pre-fix Kleene fallback covered.
static_assert(
    std::same_as<
        typename NaturalLogic<Halfspace<double, 5.0, Direction::Upward,
                                        Strictness::Strict, Kleene>>::type,
        Kleene>,
    "#848: a real (ℶ₁) halfspace stays Kleene/ternary.");
}  // namespace detail_848_pointfree_scout

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

/** @brief The De Morgan dual of a comparison flavour: @c !(x @c R @c y) is
 *  @c x @c negate(R) @c y.  Drives @c operator! on the relational predicates.
 */
constexpr Rel negate(Rel r) {
  switch (r) {
    case Rel::Lt:
      return Rel::Ge;
    case Rel::Le:
      return Rel::Gt;
    case Rel::Gt:
      return Rel::Le;
    case Rel::Ge:
      return Rel::Lt;
    case Rel::Eq:
      return Rel::Ne;
    case Rel::Ne:
      return Rel::Eq;
  }
  return r;  // unreachable; all six flavours are covered above.
}

/** @brief Nested-typedef marker so the predicate-level @c && / @c || fire only
 *  on relational predicates (and gate the @c 𝔸<pair> @c | @c relpred
 *  comprehension); kept off the class hierarchy so the predicates stay
 *  aggregates.
 */
export template <typename T>
concept IsRelPredicate = requires { typename T::is_rel_predicate; };

/** @brief @f$\pi_I \bowtie \pi_J@f$ --- a strongly-typed predicate on a pair.
 */
export template <IsRingIntegral auto I, Rel R, IsRingIntegral auto J>
struct ProjProj {
  using is_rel_predicate = void;
  template <typename P>
  constexpr bool operator()(const P& p) const {
    return rel_apply<R>(coord<I>(p), coord<J>(p));
  }
};

/** @brief @f$\pi_I \bowtie \mathrm{fix}(V)@f$ --- a strongly-typed pair
 *  predicate. */
export template <IsRingIntegral auto I, Rel R, auto V>
struct ProjBound {
  using is_rel_predicate = void;
  template <typename P>
  constexpr bool operator()(const P& p) const {
    return rel_apply<R>(coord<I>(p), V);
  }
};

// RelAnd (the meet predicate carrier) moved DOWN to dedekind.relational:dyadic
// (#792); halfspace still USES it (the predicate-level meet --- now
// structured_and, reached via the generic &&, #824 --- axis_factor, the >>
// functional trait) as dedekind::relational::RelAnd, imported from :dyadic.
// Its dual RelOr (structured_or, via ||) was re-added (#824); the set-level
// join stays the set-grammar | / Join node (:sets, #365).

// π_I ⋈ π_J  →  ProjProj (projection-vs-projection).
export template <IsRingIntegral auto I, IsRingIntegral auto J>
constexpr ProjProj<I, Rel::Lt, J> operator<(Projection<I>, Projection<J>) {
  return {};
}
export template <IsRingIntegral auto I, IsRingIntegral auto J>
constexpr ProjProj<I, Rel::Le, J> operator<=(Projection<I>, Projection<J>) {
  return {};
}
export template <IsRingIntegral auto I, IsRingIntegral auto J>
constexpr ProjProj<I, Rel::Gt, J> operator>(Projection<I>, Projection<J>) {
  return {};
}
export template <IsRingIntegral auto I, IsRingIntegral auto J>
constexpr ProjProj<I, Rel::Ge, J> operator>=(Projection<I>, Projection<J>) {
  return {};
}
export template <IsRingIntegral auto I, IsRingIntegral auto J>
constexpr ProjProj<I, Rel::Eq, J> operator==(Projection<I>, Projection<J>) {
  return {};
}
export template <IsRingIntegral auto I, IsRingIntegral auto J>
constexpr ProjProj<I, Rel::Ne, J> operator!=(Projection<I>, Projection<J>) {
  return {};
}

// !pred on the relational predicates: negate the comparison flavour (the
// De Morgan dual), the unbound-predicate analogue of set complement.  See the
// UnboundHalfspace overload above and FIXME(#829).
export template <IsRingIntegral auto I, Rel R, IsRingIntegral auto J>
constexpr ProjProj<I, negate(R), J> operator!(const ProjProj<I, R, J>&) {
  return {};
}
export template <IsRingIntegral auto I, Rel R, auto V>
constexpr ProjBound<I, negate(R), V> operator!(const ProjBound<I, R, V>&) {
  return {};
}

// π_I ⋈ fix(V), I >= 1  →  ProjBound (I == 0 is the unary π of §M1 above).
export template <IsRingIntegral auto I, auto V>
  requires(I >= 1)
constexpr ProjBound<I, Rel::Lt, V> operator<(Projection<I>, Bound<V>) {
  return {};
}
export template <IsRingIntegral auto I, auto V>
  requires(I >= 1)
constexpr ProjBound<I, Rel::Le, V> operator<=(Projection<I>, Bound<V>) {
  return {};
}
export template <IsRingIntegral auto I, auto V>
  requires(I >= 1)
constexpr ProjBound<I, Rel::Gt, V> operator>(Projection<I>, Bound<V>) {
  return {};
}
export template <IsRingIntegral auto I, auto V>
  requires(I >= 1)
constexpr ProjBound<I, Rel::Ge, V> operator>=(Projection<I>, Bound<V>) {
  return {};
}
export template <IsRingIntegral auto I, auto V>
  requires(I >= 1)
constexpr ProjBound<I, Rel::Eq, V> operator==(Projection<I>, Bound<V>) {
  return {};
}
export template <IsRingIntegral auto I, auto V>
  requires(I >= 1)
constexpr ProjBound<I, Rel::Ne, V> operator!=(Projection<I>, Bound<V>) {
  return {};
}

// !pred witnesses on the relational predicates (grammar p1): negation flips the
// comparison flavour on both ProjProj and ProjBound, covering every negate(Rel)
// mapping including the Eq/Ne branch.  The set-level dual is ~ (complement);
// see the UnboundHalfspace witnesses above and FIXME(#829).
static_assert(std::same_as<decltype(!(π1 < π2)), decltype(π1 >= π2)>,
              "!(π1 < π2) is π1 >= π2 (Lt->Ge).");
static_assert(std::same_as<decltype(!(π1 <= π2)), decltype(π1 > π2)>,
              "!(π1 <= π2) is π1 > π2 (Le->Gt).");
static_assert(std::same_as<decltype(!(π1 > π2)), decltype(π1 <= π2)>,
              "!(π1 > π2) is π1 <= π2 (Gt->Le).");
static_assert(std::same_as<decltype(!(π1 >= π2)), decltype(π1 < π2)>,
              "!(π1 >= π2) is π1 < π2 (Ge->Lt).");
static_assert(std::same_as<decltype(!(π1 == π2)), decltype(π1 != π2)>,
              "!(π1 == π2) is π1 != π2 (Eq->Ne).");
static_assert(std::same_as<decltype(!(π1 != π2)), decltype(π1 == π2)>,
              "!(π1 != π2) is π1 == π2 (Ne->Eq).");
static_assert(
    std::same_as<decltype(!(π1 > fix(5_c))), decltype(π1 <= fix(5_c))>,
    "!(π1 > fix(5)) is π1 <= fix(5) (ProjBound Gt->Le).");
static_assert(
    std::same_as<decltype(!(π1 == fix(5_c))), decltype(π1 != fix(5_c))>,
    "!(π1 == fix(5)) is π1 != fix(5) (ProjBound Eq->Ne).");

// Meet / join of relational PREDICATES is the pointwise && / || over the
// operands' OWN truth-value carrier (Boolean, or Kleene ∧/∨ over a
// Kleene relation --- RelAnd/RelOr return auto, not bool, to keep
// Unknown), distinct from set intersection/union & / | (the vectorized
// {truth-value}ⁿ ops on Sets).  Rather than defining operator&& / operator||
// here (which would be ambiguous with the generic predicate operator&& /
// operator|| in :sets:expressions), we hook the STRUCTURED forms: the generic
// operators dispatch to structured_and / structured_or via ADL, and these
// return the marker-preserving RelAnd / RelOr (which the generic Meet /
// Join nodes are NOT, so their result could not feed the 𝔸<pair> | relpred
// comprehension)
// (#824).  This is the same mechanism the Halfspace lattice uses above.  RelAnd
// / RelOr live in :dyadic (#792).
//
// FIXME(#824): these hooks live in :order, so ADL reaches them only when an
// operand is :order-native (ProjProj/ProjBound — every current meet).  A pair
// of :relational-native rel-predicates (e.g. two DiagPred) does NOT find them
// and falls back to the marker-less Meet/Join nodes.  The clean fix is
// to unify the projection+equality DSL in ONE namespace (relocate it to
// :relational, where relations live and order is not required) — done in the
// graph/arrow-lift follow-up, not here.
export template <IsRelPredicate A, IsRelPredicate B>
constexpr dedekind::relational::RelAnd<A, B> structured_and(A a, B b) {
  return {a, b};
}
export template <IsRelPredicate A, IsRelPredicate B>
constexpr dedekind::relational::RelOr<A, B> structured_or(A a, B b) {
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
  // NOTE: @c product here is a bool-returning rel-predicate (or the always-true
  // universal), so the boolean cast is safe in the graph DSL.  A ternary-valued
  // product would corrupt via @c static_cast<bool> (Ternary::False's underlying
  // −1 casts to true); a logic-preserving @c L::AND needs @c L threaded through
  // @c ProductRestrict, tracked as a follow-up (not reachable by construction
  // today, since restrictions are decidable comparisons).
  template <typename Pair>
  constexpr bool operator()(const Pair& p) const {
    return static_cast<bool>(product(p)) && rp(p);
  }
};

// 𝔸<A×B> | relPred  →  the relation as an IsSet on A × B.  The @b universal
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
 * @details The product is the universal set of products @c 𝔸<pair> refined by
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
export template <IsRingIntegral auto I, typename T, auto Pivot, Direction D,
                 Strictness S, typename L>
constexpr auto cylinder(const Halfspace<T, Pivot, D, S, L>&) {
  return ProjBound<I, rel_of(D, S), Pivot>{};
}

// restricted × total:  {x ⋈ p} × 𝔸  =  𝔸<pair> | (π1 ⋈ fix(p)).
export template <typename T, auto P, Direction D, Strictness S, typename L,
                 typename T2, typename L2, typename C2>
  requires std::same_as<L, L2>
constexpr auto operator*(const Halfspace<T, P, D, S, L>& a,
                         const UniversalSet<T2, L2, C2>&) {
  return 𝔸<std::pair<T, T2>, L> | cylinder<1>(a);
}

// total × restricted:  𝔸 × {y ⋈ q}  =  𝔸<pair> | (π2 ⋈ fix(q)).
export template <typename T1, typename L1, typename C1, typename T, auto Q,
                 Direction D, Strictness S, typename L>
  requires std::same_as<L1, L>
constexpr auto operator*(const UniversalSet<T1, L1, C1>&,
                         const Halfspace<T, Q, D, S, L>& b) {
  return 𝔸<std::pair<T1, T>, L> | cylinder<2>(b);
}

// restricted × restricted:  𝔸<pair> | (π1 ⋈ fix(p)) && (π2 ⋈ fix(q)).
export template <typename Ta, auto Pa, Direction Da, Strictness Sa, typename La,
                 typename Tb, auto Qb, Direction Db, Strictness Sb, typename Lb>
  requires std::same_as<La, Lb>
constexpr auto operator*(const Halfspace<Ta, Pa, Da, Sa, La>& a,
                         const Halfspace<Tb, Qb, Db, Sb, Lb>& b) {
  return 𝔸<std::pair<Ta, Tb>, La> | (cylinder<1>(a) && cylinder<2>(b));
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
 * @c 𝔸<T_I> (honest exactly when @c R is entire on that side).  This is the
 * @b free case; the @b existential @f$\{a\mid\exists b.R(a,b)\}@f$ that a
 * coupled, non-entire relation needs is the separate Rice-gated operation.
 * The order-layer overloads specialise the sets-layer @c 𝔸<T1> fallback (they
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
 *  no axis-@c I structure, so the declared universe @c 𝔸<TI>. */
export template <IsRingIntegral auto I, typename TI, typename L, typename P>
constexpr auto axis_factor(const P&) {
  return 𝔸<TI, L>;  // preserve the relation's logic species
}

/** @brief A cylinder @c ProjBound on axis @c I: the halfspace it lifted from
 *  (only an order comparison is a halfspace; Eq/Ne fall to the default).
 *  @note The predicate's own slot @c Slot is deduced separately and matched to
 *  the requested axis @c I @b by value (@c Slot @c == @c I), @b not by NTTP
 *  type-identity: a @c ProjBound built from an @c int @c 1 and a query for an
 *  @c unsigned @c 1 name the same axis and must agree, rather than silently
 *  falling through to the universal factor (review #871). */
export template <IsRingIntegral auto I, typename TI, typename L,
                 IsRingIntegral auto Slot, Rel R, auto V>
  requires(is_order_rel(R) && Slot == I)
constexpr auto axis_factor(const ProjBound<Slot, R, V>&) {
  return Halfspace<TI, V, dir_of(R), strict_of(R), L>{};
}

/** @brief A meet of cylinders: the factor on axis @c I is the @b intersection
 *  of both children's factors on that axis, so two bounds on the same axis
 *  (@c π1<=5 @c && @c π1<=3) meet to the tighter one rather than dropping
 * either.
 */
export template <IsRingIntegral auto I, typename TI, typename L, typename A,
                 typename B>
constexpr auto axis_factor(const dedekind::relational::RelAnd<A, B>& r) {
  auto fa = axis_factor<I, TI, L>(r.a);
  auto fb = axis_factor<I, TI, L>(r.b);
  if constexpr (requires { typename decltype(fa)::is_universal_boundary; }) {
    return fb;  // a does not constrain axis I; the factor is b's
  } else if constexpr (requires {
                         typename decltype(fb)::is_universal_boundary;
                       }) {
    return fa;  // b does not constrain axis I; the factor is a's
  } else {
    // BOTH constrain axis I.  fa/fb are recovered Halfspace SETS, so this is
    // the set-level bare-halfspace meet: call structured_and DIRECTLY (the
    // customization point operator& / && both forward to) so it collapses to
    // the tighter bound / interval.  NOT the predicate-level && (a categorical
    // Morphism, dropping the tightening), and not the set-level operator&
    // either (declared below this point, so unreachable by ordinary lookup
    // here).
    //
    // FIXME(#872): axis_factor is not closed over its recursive outputs.  With
    // 3+ same-axis bounds a child reduces to an OrderInterval/Singleton and
    // structured_and(OrderInterval, Halfspace) has no overload, so the relation
    // fails to instantiate (association-dependent).  Two-bound meets work
    // (witnessed below); the meet-lattice closure / RelAnd normalization is
    // #872, out of this PR's meet/join scope.
    return dedekind::order::structured_and(fa, fb);
  }
}

/** @brief A restricted product bounding a graph: the factor lives on the
 *  product (cylinder) side; the graph @c rp couples the axes, so it is
 *  transparent to a single-axis projection. */
export template <IsRingIntegral auto I, typename TI, typename L, typename Pp,
                 typename RP>
constexpr auto axis_factor(const ProductRestrict<Pp, RP>& r) {
  return axis_factor<I, TI, L>(r.product);
}

/** @brief @f$\pi_A(R)@f$ --- the domain factor, recovered structurally
 *  (preserving the relation's logic species @c L). */
export template <typename T1, typename T2, typename L, IsRelPredicate P>
constexpr auto dom(const Set<std::pair<T1, T2>, L, P>& r) {
  return axis_factor<1, T1, L>(r.predicate());
}

/** @brief @f$\pi_B(R)@f$ --- the codomain factor, recovered structurally. */
export template <typename T1, typename T2, typename L, IsRelPredicate P>
constexpr auto cod(const Set<std::pair<T1, T2>, L, P>& r) {
  return axis_factor<2, T2, L>(r.predicate());
}

/** @section halfspace__Formal_Verification (relational surface) */

// less-than on 𝔹×𝔹, a strongly-typed point-free relation, membership-checked.
static_assert((𝔹 * 𝔹 | π1 < π2)(std::pair{false, true}),
              "(false, true) ∈ {(x,y) | x < y}.");
static_assert(!(𝔹 * 𝔹 | π1 < π2)(std::pair{true, true}),
              "(true, true) ∉ {(x,y) | x < y}.");

// a meet of two projection predicates: {(x,y) | x ≤ y ∧ y == true}.
static_assert((𝔹 * 𝔹 | (π1 <= π2 && π2 == fix(true_c)))(std::pair{false, true}),
              "(false, true) satisfies x ≤ y ∧ y = true.");
static_assert(!(𝔹 * 𝔹 |
                (π1 <= π2 && π2 == fix(true_c)))(std::pair{false, false}),
              "(false, false) fails y = true.");

// NESTED meet: BOTH operands of the outer && are themselves RelAnd (relational-
// native, no bare :order operand), so this is the case the FIXME(#824) warns
// about --- yet ADL still reaches structured_and THROUGH RelAnd's projection-
// atom template args, so the marker survives and the comprehension restricts.
// relpred is therefore closed under &&/|| for the projection sub-grammar; the
// fall-through only bites non-projection rel-predicates (e.g. two diag()).
static_assert(
    IsRelPredicate<decltype((π1 <= π2 && π2 == fix(true_c)) &&
                            (π1 <= π2 && π1 < π2))>,
    "nested (RelAnd && RelAnd) stays IsRelPredicate via ADL on the atom args.");
static_assert((𝔹 * 𝔹 | ((π1 <= π2 && π2 == fix(true_c)) &&
                        (π1 <= π2 && π1 < π2)))(std::pair{false, true}),
              "(false, true) satisfies (x ≤ y ∧ y = true) ∧ (x ≤ y ∧ x < y).");

// ── converse and the bracket-free relation query ───────────────────────────
// SwapPred / converse and IsPairLike / is_relation moved DOWN to
// dedekind.relational:dyadic (#792) --- pure Set<pair> algebra, no ordering.
// The ordered witnesses below stay here and reach them by ADL on their
// dedekind::sets::Set arguments (order imports dedekind.relational).

// converse swaps the coordinates; is_relation certifies the product Domain.
static_assert(converse(𝔹* 𝔹 | π1 < π2)(std::pair{true, false}),
              "converse of < contains (true, false): false < true.");
static_assert(is_relation(𝔹* 𝔹 | π1 < π2),
              "𝔹*𝔹 | π1 < π2 is a relation (IsSet on a product).");

// ── Projection arithmetic (for the divides relation, Listing 7) ────────────
/** @brief @f$\pi_I \% \pi_J@f$ --- a value expression on a pair, awaiting a
 *  comparison to a bound. */
export template <IsRingIntegral auto I, IsRingIntegral auto J>
struct ProjMod {};
export template <IsRingIntegral auto I, IsRingIntegral auto J>
constexpr ProjMod<I, J> operator%(Projection<I>, Projection<J>) {
  return {};
}

/** @brief @f$(\pi_I \% \pi_J) \bowtie \mathrm{fix}(V)@f$ --- a strongly-typed
 *  pair predicate (the modular / divisibility shape). */
export template <IsRingIntegral auto I, IsRingIntegral auto J, Rel R, auto V>
struct ProjModBound {
  using is_rel_predicate = void;
  template <typename P>
  constexpr bool operator()(const P& p) const {
    return rel_apply<R>(coord<I>(p) % coord<J>(p), V);
  }
};
export template <IsRingIntegral auto I, IsRingIntegral auto J, auto V>
constexpr ProjModBound<I, J, Rel::Eq, V> operator==(ProjMod<I, J>, Bound<V>) {
  return {};
}

// divides: {(a,b) | b % a == 0 ∧ a != 0} = ℕ*ℕ | (π1 != fix(0_c) && π2 % π1 ==
// fix(0_c)).  The guard is spelled FIRST so RelAnd's && short-circuits it
// before the %, and a == 0 never reaches the division.
static_assert((ℕ * ℕ | (π1 != fix(0_c) && π2 % π1 == fix(0_c)))(std::pair{
                  finite_cardinality(2), finite_cardinality(6)}),
              "6 % 2 == 0: (2,6) ∈ divides.");
static_assert(!(ℕ * ℕ | (π1 != fix(0_c) && π2 % π1 == fix(0_c)))(std::pair{
                  finite_cardinality(4), finite_cardinality(6)}),
              "6 % 4 != 0: (4,6) ∉ divides.");

// ── The arrow lift: f(π_I) into the relpred DSL (#871 / #824) ────────────────
/** @brief @f$f(\pi_I)@f$ --- an @c IsArrow @c f applied to the @c I-th
 *  coordinate: a value expression on a pair awaiting a comparison (the arrow
 *  LIFT into the projection DSL, sibling of @c ProjMod).  Built by
 *  @c ap(f, π_I).
 *
 *  @note The coordinate index is a constrained-auto NTTP gated by
 *  @c IsRingIntegral (a structural integral).  Axis identity is matched @b by
 *  value, not by NTTP type-identity (see @c axis_factor), so two spellings of
 *  the same axis (@c int @c 1 vs @c unsigned @c 1) agree rather than silently
 *  diverging (CP review #871).  @c IsRingIntegral admits neither ℕ/Cardinality
 *  (a @c std::variant, non-structural) nor enums (not @c std::integral); it is
 *  the structural integers.  @b Provisional: this whole projection surface
 *  belongs with @c category::IsProductProjection in @c :limit, not in
 *  @c :halfspace --- relocation (and the attendant deletion) is tracked in
 *  #878. */
export template <IsRingIntegral auto I, typename F>
struct ProjApply {
  F f;
};

/** @brief @c ap(f, π_I) --- lift the arrow @c f into the relpred DSL as
 *  @f$f(\pi_I)@f$.  Gated on @c IsArrow<F> (a pure, terminating map, §2.2).
 *  @c f is taken by value and @c std::move'd into the wrapper, so the @b lift
 *  step itself never copies the arrow.
 *  @note This does @b not promise end-to-end move-only support: the relpred is
 *  a value carried by the comprehension binders (@c operator| and the
 *  restricted-product overloads copy their stored predicate), so the DSL is
 *  value-semantics throughout and arrows used in @c 𝔸<pair> @c | @c (...)
 * should be copyable.  The @c std::move here is a local optimisation, not a
 *  move-only guarantee (CP review #871). */
export template <IsRingIntegral auto I, typename F>
  requires dedekind::category::IsArrow<F>
constexpr ProjApply<I, std::remove_cvref_t<F>> ap(F f, Projection<I>) {
  return {std::move(f)};
}

/** @brief @f$\pi_J = f(\pi_I)@f$ --- the arrow-lift relpred: coordinate @c J
 *  equals @c f of coordinate @c I.  This is the point-free @f$y = f(x)@f$ that
 *  will redefine @c graph (#871).
 *
 *  @details @b Forward membership @f$b = f(a)@f$ is decidable exactly when the
 *  arrow's @c Codomain has decidable equality (@c std::equality_comparable) ---
 *  the corrected gate from the #870 CP review: monicity is neither necessary
 *  nor sufficient for this direction.  The @b pre-image direction (recover
 *  @c a from @c b) is a separate capability gated on @c IsRetractableArrow /
 *  iso (consuming the retract / dagger witness); it is @b not required here and
 *  is the graph/preimage follow-up. */
export template <IsRingIntegral auto J, IsRingIntegral auto I, typename F>
  requires std::equality_comparable<typename std::remove_cvref_t<F>::Codomain>
struct ProjApplyEq {
  F f;
  using is_rel_predicate = void;
  template <typename P>
  constexpr bool operator()(const P& p) const {
    // @c IsArrow only guarantees @c f(x) is @b convertible to @c Codomain, not
    // that its (possibly proxy) result compares directly with the coordinate.
    // Normalise to the declared @c Codomain --- the type the @c
    // equality_comparable gate actually vouches for --- before the compare.
    using Cod = typename std::remove_cvref_t<F>::Codomain;
    return static_cast<Cod>(f(coord<I>(p))) == coord<J>(p);
  }
};

/** @brief @f$\pi_J = \mathrm{ap}(f, \pi_I)@f$ → @c ProjApplyEq (the
 *  @c 𝑦 @c == @c ap(f, @c 𝑥) blackboard spelling). */
export template <IsRingIntegral auto J, IsRingIntegral auto I, typename F>
constexpr ProjApplyEq<J, I, F> operator==(Projection<J>, ProjApply<I, F> a) {
  return {std::move(a.f)};
}
/** @brief The symmetric spelling @f$\mathrm{ap}(f, \pi_I) = \pi_J@f$. */
export template <IsRingIntegral auto I, IsRingIntegral auto J, typename F>
constexpr ProjApplyEq<J, I, F> operator==(ProjApply<I, F> a, Projection<J>) {
  return {std::move(a.f)};
}

// The lift in action: 𝑦 == ap(id, 𝑥) is the diagonal Δ = {(x,x)}.
static_assert((𝔹 * 𝔹 | (𝑦 == ap(dedekind::category::Identity<bool>{},
                                𝑥)))(std::pair{true, true}),
              "(true,true) ∈ {(x,y) | y = id(x)} = Δ.");
static_assert(!(𝔹 * 𝔹 | (𝑦 == ap(dedekind::category::Identity<bool>{},
                                 𝑥)))(std::pair{true, false}),
              "(true,false) ∉ Δ: false ≠ id(true).");

/** @brief @f$\pi_I \% \mathrm{fix}(V)@f$ --- projection mod a @b constant, a
 *  value expression on a pair awaiting a comparison (sibling of @c ProjMod,
 *  whose modulus is the projection @c π_J rather than a fixed @c V). */
export template <IsRingIntegral auto I, auto V>
struct ProjModConst {};
// The modulus must be positive: @c coord % 0 is undefined behaviour (and fails
// constant evaluation), matching the @c Modular<N> requirement @c N>0.
export template <IsRingIntegral auto I, auto V>
  requires(V > 0)
constexpr ProjModConst<I, V> operator%(Projection<I>, Bound<V>) {
  return {};
}

/** @brief @f$(\pi_I \% \mathrm{fix}(V)) \bowtie \pi_J@f$ --- compare a
 *  projection-mod-constant to another projection: the residue-class graph
 *  @f$b = a \bmod V@f$. */
export template <IsRingIntegral auto I, auto V, Rel R, IsRingIntegral auto J>
struct ProjModConstProj {
  using is_rel_predicate = void;
  template <typename P>
  constexpr bool operator()(const P& p) const {
    return rel_apply<R>(coord<I>(p) % V, coord<J>(p));
  }
};
export template <IsRingIntegral auto I, auto V, IsRingIntegral auto J>
constexpr ProjModConstProj<I, V, Rel::Eq, J> operator==(ProjModConst<I, V>,
                                                        Projection<J>) {
  return {};
}

/** @brief @f$(\pi_I \% \mathrm{fix}(V)) \bowtie \mathrm{fix}(W)@f$ --- a
 *  @b congruence @b class predicate @f$\pi_I \equiv W \pmod V@f$ (sibling of
 *  @c ProjModConstProj, whose right side is the projection @c π_J rather than a
 *  fixed residue @c W).  Restricts an axis to a residue class. */
export template <IsRingIntegral auto I, auto V, Rel R, auto W>
struct ProjModConstBound {
  using is_rel_predicate = void;
  template <typename P>
  constexpr bool operator()(const P& p) const {
    // The MATHEMATICAL residue class, not C++ remainder: normalise both sides
    // into [0,V) so signed values agree with the congruence (C++ −2 % 3 == −2,
    // but −2 ≡ 1 (mod 3)), keeping this consistent with argmax's residue
    // normalisation and the finite-residue materialisation.  Add V only to a
    // negative remainder (which is already in (−V,0)), so the intermediate
    // never overflows for a large valid modulus.
    const auto lhs0 = coord<I>(p) % V;
    const auto lhs = lhs0 < 0 ? lhs0 + V : lhs0;
    constexpr auto rhs0 = W % V;
    constexpr auto rhs = rhs0 < 0 ? rhs0 + V : rhs0;
    return rel_apply<R>(lhs, rhs);
  }
};
export template <IsRingIntegral auto I, auto V, auto W>
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
export template <IsRingIntegral auto I, auto V>
struct ProjAddConst {};
export template <IsRingIntegral auto I, auto V>
constexpr ProjAddConst<I, V> operator+(Projection<I>, Bound<V>) {
  return {};
}
export template <IsRingIntegral auto I, auto V>
struct ProjMulConst {};
export template <IsRingIntegral auto I, auto V>
constexpr ProjMulConst<I, V> operator*(Projection<I>, Bound<V>) {
  return {};
}

/** @brief @f$(\pi_I + \mathrm{fix}(V)) \bowtie \pi_J@f$ --- the
 * successor-shaped graph @f$b = a + V@f$. */
export template <IsRingIntegral auto I, auto V, Rel R, IsRingIntegral auto J>
struct ProjAddConstProj {
  using is_rel_predicate = void;
  template <typename P>
  constexpr bool operator()(const P& p) const {
    const auto a = coord<I>(p);
    using C = std::remove_cvref_t<decltype(a)>;
    // Add through @c std::plus<C>, NOT the bare @c +: on a narrow carrier
    // (@c unsigned @c char) bare @c + promotes to @c int, so @c 255+1 becomes
    // @c 256 and the graph would omit @c (255,0); @c std::plus<C> converts back
    // to @c C, keeping the carrier's certified (modular) semantics.
    return rel_apply<R>(std::plus<C>{}(a, static_cast<C>(V)), coord<J>(p));
  }
};
export template <IsRingIntegral auto I, auto V, IsRingIntegral auto J>
constexpr ProjAddConstProj<I, V, Rel::Eq, J> operator==(ProjAddConst<I, V>,
                                                        Projection<J>) {
  return {};
}

/** @brief @f$(\pi_I \cdot \mathrm{fix}(V)) \bowtie \pi_J@f$ --- the scaling
 *  graph @f$b = a \cdot V@f$ (e.g. the doubler @f$b = 2a@f$). */
export template <IsRingIntegral auto I, auto V, Rel R, IsRingIntegral auto J>
struct ProjMulConstProj {
  using is_rel_predicate = void;
  template <typename P>
  constexpr bool operator()(const P& p) const {
    const auto a = coord<I>(p);
    using C = std::remove_cvref_t<decltype(a)>;
    // Multiply through @c std::multiplies<C> (same narrow-promotion reason as
    // @c ProjAddConstProj): keep the carrier's certified semantics.
    return rel_apply<R>(std::multiplies<C>{}(a, static_cast<C>(V)),
                        coord<J>(p));
  }
};
export template <IsRingIntegral auto I, auto V, IsRingIntegral auto J>
constexpr ProjMulConstProj<I, V, Rel::Eq, J> operator==(ProjMulConst<I, V>,
                                                        Projection<J>) {
  return {};
}

// (The monic-gated arrow lift 𝑦 == apply(f, 𝑥) → ProjApply/ProjApplyEq moved to
// its own effort: CP review showed monicity ≠ a computable pre-image (that
// needs IsRetractableArrow), the codomain-== gate was missing, and it
// duplicated the existing GraphPredicate — so it is reworked as the
// graph-redefinition PR, in :relational, alongside the projection-DSL
// relocation.  See #824.)

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

// TWO bounds on the SAME axis: dom recovers the TIGHTER halfspace (their meet),
// not either bound alone.  Guards axis_factor's both-branch: the factors are
// recovered Halfspace SETS, so their meet is the set-level & collapse; the
// predicate-level && would build a categorical Morphism and lose dom/cod.
static_assert(dom(ℕ* ℕ |
                  (π1 <= fix(5_c) && π1 <= fix(3_c)))(finite_cardinality(3)),
              "π_A of {a ≤ 5 ∧ a ≤ 3} recovers the tighter {a ≤ 3}: 3 ≤ 3.");
static_assert(!dom(ℕ * ℕ |
                   (π1 <= fix(5_c) && π1 <= fix(3_c)))(finite_cardinality(4)),
              "π_A recovers the TIGHTER bound: 4 ≤ 5 but 4 ≰ 3, so excluded.");

// relational application: apply(R, a) is the fibre {b | (a,b) ∈ R}.  For the
// residue graph (a function) it is the singleton {a % 17}: apply(R,20) = {3}.
static_assert(
    dedekind::relational::apply(ℕ* ℕ | π1 % fix(17_c) == π2,
                                finite_cardinality(20))(finite_cardinality(3)),
    "apply(R,20) = {3}: (20,3) ∈ R since 20 % 17 == 3.");
static_assert(
    !dedekind::relational::apply(ℕ * ℕ | π1 % fix(17_c) == π2,
                                 finite_cardinality(20))(finite_cardinality(4)),
    "apply(R,20) does not contain 4.");

/** @brief @c upperbounds(S) --- the @f$\forall@f$-projection @f$R/\ni@f$: the
 *  region dominating all of @c S, and the pluggable point of the extremum
 *  (Bird \& de~Moor @cite birddemoor1997aop).
 *
 *  @details @c max is one GENERIC definition, the @f$\forall@f$-projection of
 * the order relation onto its second coordinate:
 *  @f[ \max S \;=\; \{\pi_2 \in S \mid \forall \pi_1 \in S.\; \pi_1 \le \pi_2\}
 *               \;=\; S \cap \mathrm{upperbounds}(S) \;=\; (\in) \cap (R/\ni).
 * @f]
 *  @c upperbounds is the pluggable point (as @f$\forall@f$ / @f$\exists@f$ plug
 *  into @c Ø::operator==), decided SYMBOLICALLY per structure so no candidate
 * is enumerated: a halfspace bounded ABOVE (@c {x≤p}) is dominated exactly by
 *  @c {x≥p}; one unbounded above (@c {x≥p}) has no upper bound (@c Ø).
 *  @c lowerbounds is the dual.  The generic @c max/min then meet @c S with
 * them, and the meet @c structured_and collapses the interval to the attained
 * pivot
 *  (@c {x≤p} ∩ @c {x≥p} = @c {p}) or, when the sup is unattained (strict) or
 *  absent (unbounded), to @c Ø. */
export template <typename T, auto p, Strictness S, typename L>
constexpr auto upperbounds(Halfspace<T, p, Direction::Downward, S, L>) {
  if constexpr (S == Strictness::Strict && strict_lower_cut_empty<T, p>()) {
    // The strict cut {x<p} is EMPTY (p at/below the carrier's least element),
    // so EVERY element is vacuously an upper bound: the ∀-projection is the
    // whole universe 𝔸 (and max = S ∩ 𝔸 = S = ∅).  Honouring the contract, not
    // just the answer.
    return dedekind::sets::UniversalSet<T, L>{};
  } else if constexpr (S == Strictness::Strict &&
                       (std::integral<T> ||
                        dedekind::category::IsSaturating<T>)) {
    // DISCRETE strict {x<p}, non-empty: the sup is the ATTAINED predecessor
    // p−1, so {x<p} ∩ {x≥p−1} = {p−1}.  Boundary-safe: the empty branch already
    // peeled off the floor, so p > the least element and p−1 neither underflows
    // a machine int nor leaves ℕ (a saturating carrier escalates regardless).
    return Halfspace<T, p - 1, Direction::Upward, Strictness::NonStrict, L>{};
  } else {
    // {x≤p}: sup p attained.  Dense {x<p}: sup p unattained (no predecessor),
    // so upper bounds {x≥p} and the meet is Ø (no max).
    return Halfspace<T, p, Direction::Upward, Strictness::NonStrict, L>{};
  }
}
export template <typename T, auto p, Strictness S, typename L>
constexpr auto upperbounds(Halfspace<T, p, Direction::Upward, S, L>) {
  return Ø<T, L>{};  // unbounded above: no upper bound
}
export template <typename T, auto p, Strictness S, typename L>
constexpr auto lowerbounds(Halfspace<T, p, Direction::Upward, S, L>) {
  if constexpr (S == Strictness::Strict && strict_upper_cut_empty<T, p>()) {
    // Machine discrete {x>p} is EMPTY at the ceiling (p at the type's greatest
    // value: true for bool, INT_MAX for int), so every element bounds ∅ → the
    // universe (min = S ∩ 𝔸 = ∅).  No p+1 (which would overflow/wrap).
    return dedekind::sets::UniversalSet<T, L>{};
  } else if constexpr (S == Strictness::Strict && HasZeroFloor<T> &&
                       p + 1 < 0) {
    // Floor-0 carrier (ℕ / unsigned): the successor p+1 falls below the
    // carrier, so the min clamps to the carrier minimum 0.  NOT the signed ℤ
    // proxy, which has no floor and takes the ordinary successor branch below
    // (#837 review).
    return Halfspace<T, 0, Direction::Downward, Strictness::NonStrict, L>{};
  } else if constexpr (S == Strictness::Strict &&
                       (std::integral<T> ||
                        dedekind::category::IsSaturating<T>)) {
    // DISCRETE strict {x>p}, non-empty: the min is the ATTAINED successor p+1.
    // Boundary-safe: the ceiling branch (machine) already peeled off the top, ℕ
    // is unbounded above, and a saturating carrier escalates regardless.
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

/** @brief @c & IS the meet on bare order operands: it forwards to the
 *  @c structured_and customization point, exactly as @c operator& does for
 *  wrapped predicates, so no @c Set{} wrapping is needed.  The complement-pair
 *  @c operator& above (opposite direction AND flipped strictness → @c Ø) is
 * more specialized and still claims its case; every other halfspace pair
 *  (overlapping, same-direction) routes here. */
export template <typename T, auto P1, Direction D1, Strictness S1, auto P2,
                 Direction D2, Strictness S2, typename L>
constexpr auto operator&(Halfspace<T, P1, D1, S1, L> a,
                         Halfspace<T, P2, D2, S2, L> b)
  requires requires { structured_and(a, b); }
{
  return structured_and(a, b);
}
/** @brief @c | IS the join on bare order operands, dual to the @c & meet: it
 *  forwards to @c structured_or, so a same-direction or overlapping halfspace
 *  union collapses.  The complement-pair @c operator| above (→ universe) is
 * more specialized and still claims its case. */
export template <typename T, auto P1, Direction D1, Strictness S1, auto P2,
                 Direction D2, Strictness S2, typename L>
constexpr auto operator|(Halfspace<T, P1, D1, S1, L> a,
                         Halfspace<T, P2, D2, S2, L> b)
  requires requires { structured_or(a, b); }
{
  return structured_or(a, b);
}
/** @brief Fallback join for a genuine GAP that does not collapse to a
 * halfspace: the honest POINT-WISE union, a @c Set whose membership ORs the two
 * operands in the carrier's logic (@c L::OR).  Selected exactly when @c
 * structured_or does not apply, so @c | is total (no hard error) while still
 * collapsing where it can. */
export template <typename T, auto P1, Direction D1, Strictness S1, auto P2,
                 Direction D2, Strictness S2, typename L>
  requires(!requires(Halfspace<T, P1, D1, S1, L> x,
                     Halfspace<T, P2, D2, S2, L> y) { structured_or(x, y); })
constexpr auto operator|(Halfspace<T, P1, D1, S1, L> a,
                         Halfspace<T, P2, D2, S2, L> b) {
  auto pred = [a, b](const T& v) { return L::OR(a(v), b(v)); };
  return dedekind::sets::Set<T, L, decltype(pred)>{pred};
}
/** @brief @c Ø absorbs the meet (no upper bound ⟹ no max), the completion the
 *  @f$\forall@f$-projection needs at the unbounded end. */
export template <typename T, auto p, Direction D, Strictness S, typename L,
                 typename LZ>
constexpr auto operator&(Halfspace<T, p, D, S, L>, Ø<T, LZ>) {
  // Codomain leg (#894): the empty meet is decided → Boolean codomain.
  return dedekind::sets::codomain_reduce_t<Ø<T, L>>{};
}
/** @brief @c 𝔸 is the meet IDENTITY at the other end: @c {x⋈p} ∩ 𝔸 = @c {x⋈p}.
 *  The universe's own @c operator& handles @c 𝔸∩X; this is the halfspace-first
 *  order @c X∩𝔸, which @c max/min hit when @c upperbounds/lowerbounds of an
 *  EMPTY source is the whole universe (the @f$\forall@f$-projection of @c ∅).
 */
export template <typename T, auto p, Direction D, Strictness S, typename L,
                 typename LU, typename C>
constexpr auto operator&(Halfspace<T, p, D, S, L> h,
                         const dedekind::sets::UniversalSet<T, LU, C>&) {
  return h;
}

/** @brief @c max(S) --- the generic extremum: @c S met (@c ∩) with its own
 *  @f$\forall@f$-dominators, @f$S \cap \mathrm{upperbounds}(S)@f$.  @c min is
 * the dual (@c S met with its minorants).
 *
 *  @details One definition for any ordered @c S whose @c upperbounds and meet
 * are defined; the structural collapse lives entirely in @c upperbounds and the
 *  meet, so there is no generic search.  Gated on the SEMANTIC order:
 *  greatest/least element is a @b partial-order notion, so the domain must
 *  certify @c IsPartiallyOrdered (dedekind's reflexive/transitive/antisymmetric
 *  axioms, which subsume @c std::totally_ordered one level up in
 *  @c IsTotallyOrdered).  A carrier that is not an ordered set --- e.g.\
 *  @c SignedCardinality, which carries the unordered @c NaZ like an IEEE NaN
 * --- is honestly rejected: you cannot take the max of a set that may contain a
 *  NaN. */
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
    𝔸<SignedCardinality>;  // local alias (:integer is downstream)
// Exhibit (intensional, infinite case) over ℕ = @c 𝔸<Cardinality>, a registered
// TOTAL order (⊃ partial).  @c ℤ = @c SignedCardinality carries the unordered
// @c NaZ (NaN-like), so it is NOT an ordered set and the @c IsPartiallyOrdered
// gate correctly rejects @c max/min on it; the max/min VALUES are identical on
// ℕ (they are non-negative).
inline constexpr auto ℕ = 𝔸<Cardinality>;
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

/** @brief Two translation graphs are the same relation iff they carry the same
 *  shift: structural equality on the graph, compile-time. */
// Gated on @c IsSaturating: the shift arithmetic is only FAITHFUL on a carrier
// whose @c + is well-behaved (the ℕ/ℤ proxies escalate).  On @c bool the shift
// is cast into the carrier (@c +fix(1_c) and @c +fix(2_c) both become @c +true,
// the SAME graph {false→true}), so a raw @c K1==K2 would wrongly separate them;
// bool is declined rather than compared incorrectly.
export template <typename T, auto K1, auto K2, typename L>
  requires dedekind::category::IsSaturating<T>
constexpr bool operator==(
    const Set<std::pair<T, T>, L, ProjAddConstProj<1, K1, Rel::Eq, 2>>&,
    const Set<std::pair<T, T>, L, ProjAddConstProj<1, K2, Rel::Eq, 2>>&) {
  return K1 == K2;
}

/** @brief @b Symbolic composition of translation graphs: @f$T_a \circ T_b =
 *  T_{a+b}@f$, the shifts added, with @b no @f$\exists@f$ over the intermediate
 *  (contrast the Boolean relative product below).  This is the group law read
 *  off the structure; @c + commutes, so the order of composition is immaterial
 *  --- the abelian translation group, at compile time.  Gated on @c
 *  IsSaturating: the symbolic @c a+b equals the actual composite only where the
 *  carrier's @c + is faithful.  On @c bool the @c +1 graph is @c {false→true}
 *  and composing it with itself is EMPTY, while the symbolic @c +2 graph is
 *  non-empty --- so bool is declined rather than rewritten wrongly. */
export template <typename T, auto A, auto B, typename L>
  requires dedekind::category::IsSaturating<T>
constexpr auto operator>>(
    const Set<std::pair<T, T>, L, ProjAddConstProj<1, A, Rel::Eq, 2>>&,
    const Set<std::pair<T, T>, L, ProjAddConstProj<1, B, Rel::Eq, 2>>&) {
  return Set<std::pair<T, T>, L, ProjAddConstProj<1, A + B, Rel::Eq, 2>>{
      ProjAddConstProj<1, A + B, Rel::Eq, 2>{}};
}

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

/** @brief The general boundary-equality theorems (#832): a @c Halfspace value
 *  is a @b proper cut by construction --- @c make_halfspace collapses an empty
 *  cut to @c Ø and a moot cut to @c 𝔸 --- so it equals neither boundary.
 *  Decided from the carrier bounds (@c halfspace_is_empty / @c
 * halfspace_is_moot) so the answer is sound even for a raw out-of-contract
 * halfspace; for every factory-built value the oracles are @c false and these
 * are simply @c False. This lifts the honest Rice wall (@c Ø / @c UniversalSet
 * expose no general halfspace-equality case) now that emptiness / mootness are
 * decidable, and it unlocks opposite-direction subset: @c {x>5} ⊆ {x<3} reduces
 * to @c (a∩b)==a where the meet is @c EmptyPredicate / @c Ø, and @c 𝔸 ⊆ @c
 * {x≥5} reduces through @c Halfspace @c == @c 𝔸.  The finite-@c bool overloads
 * above are more specialised and still claim @c bool. */
export template <typename T, auto P, Direction D, Strictness S, typename L>
constexpr bool operator==(const Ø<T, L>&, const Halfspace<T, P, D, S, L>&) {
  return halfspace_is_empty<T, P, D, S>();
}
export template <typename T, auto P, Direction D, Strictness S, typename L>
constexpr bool operator==(const Halfspace<T, P, D, S, L>&, const Ø<T, L>&) {
  return halfspace_is_empty<T, P, D, S>();
}
export template <typename T, auto P, Direction D, Strictness S, typename L>
constexpr bool operator==(const dedekind::sets::EmptyPredicate<T>&,
                          const Halfspace<T, P, D, S, L>&) {
  return halfspace_is_empty<T, P, D, S>();
}
export template <typename T, auto P, Direction D, Strictness S, typename L>
constexpr bool operator==(const Halfspace<T, P, D, S, L>&,
                          const dedekind::sets::EmptyPredicate<T>&) {
  return halfspace_is_empty<T, P, D, S>();
}
export template <typename T, auto P, Direction D, Strictness S, typename L,
                 typename C>
constexpr bool operator==(const UniversalSet<T, L, C>&,
                          const Halfspace<T, P, D, S, L>&) {
  return halfspace_is_moot<T, P, D, S>();
}
export template <typename T, auto P, Direction D, Strictness S, typename L,
                 typename C>
constexpr bool operator==(const Halfspace<T, P, D, S, L>&,
                          const UniversalSet<T, L, C>&) {
  return halfspace_is_moot<T, P, D, S>();
}

/** @brief A @c Singleton over @c bool is never all of @c 𝔹 (two elements), so
 *  @c == 𝔸 is @c false: the forall (scheme B) leg for the @c == fragment on 𝔹
 *  (@c 𝔸<bool> | (π == fix(v)) collapses to @c Singleton<v>). */
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
static_assert(max(𝔸<bool>)(true), "max 𝔹 = {true}.");
static_assert(!max(𝔸<bool>)(false), "false is not the greatest element of 𝔹.");
static_assert(min(𝔸<bool>)(false), "min 𝔹 = {false}.");
static_assert(!min(𝔸<bool>)(true), "true is not the least element of 𝔹.");
// Modelling witness ("Theorems for Free", type-checked) with a STRUCTURAL
// IsPredicate --- not an opaque lambda, which cannot feed the collapse.  The
// specific max(𝔹) models the abstract (∈) ∩ (R/∋): at false the dominance
// ∀a∈𝔹. a ≤ false FAILS (true ⋠ false), spelled as the halfspace {a ≤ false},
// so false is correctly NOT the max.
static_assert(max(𝔸<bool>)(false) ==
                  (𝔸<bool>(false) && forall(𝔸<bool>, π <= fix(false_c))),
              "specific max(𝔹) models (∈) ∩ (R/∋), structurally.");

// (The image of a halfspace under a translation --- the pivot shifted by K ---
// is now the affine pushforward on the GRAPH surface: image(graph | π1 ⋈ p) =
// {y ⋈ p+K}, below.  The earlier Translation<T,K>-arrow version is superseded.)

// ── Relative product: composition of relations (Tarski) ────────────────────
// ComposePred and the Boolean-middle relative product operator>> moved DOWN to
// dedekind.relational:dyadic (#792).  The functional-propagation trait for
// ComposePred (is_right_unique_v, §3.2 Table 3) stays here in :halfspace's
// registry block below (it registers a dedekind::category trait), qualified as
// dedekind::sets::ComposePred; the >> witnesses reach the operator by ADL.

// ≤ ∘ ≤ = ≤ (transitivity), decidable because the intermediate is Boolean.
static_assert(static_cast<bool>(((𝔹 * 𝔹 | π1 <= π2) >>
                                 (𝔹 * 𝔹 | π1 <= π2))(std::pair{false, true})),
              "≤ ∘ ≤ contains (false, true).");
static_assert(!static_cast<bool>(((𝔹 * 𝔹 | π1 <= π2) >>
                                  (𝔹 * 𝔹 | π1 <= π2))(std::pair{true, false})),
              "≤ ∘ ≤ excludes (true, false): transitivity recovers ≤.");

// ── Relation algebra: union (the set-grammar |), the diagonal, and the
// reflexive / symmetric closures.  The relation | / operator& (union / meet),
// the
// diagonal Δ (reframed from ProjProj<Eq> to a plain equality DiagPred), and the
// reflexive / symmetric closures moved DOWN to dedekind.relational:dyadic
// (#792) --- pure Set<pair> algebra.  The ordered witnesses below stay and
// reach them by ADL on their dedekind::sets::Set arguments.

// reflexive(<) = < ∪ Δ = ≤ on 𝔹; symmetric(<) = < ∪ <° = ≠ on 𝔹.
static_assert(static_cast<bool>(reflexive(𝔹* 𝔹 |
                                          π1 < π2)(std::pair{false, false})),
              "reflexive(<) adds the diagonal: (false,false) ∈ < ∪ Δ = ≤.");
static_assert(!static_cast<bool>(reflexive(𝔹 * 𝔹 |
                                           π1 < π2)(std::pair{true, false})),
              "reflexive(<) is still ≤: (true,false) ∉ ≤.");
static_assert(static_cast<bool>(symmetric(𝔹* 𝔹 |
                                          π1 < π2)(std::pair{true, false})),
              "symmetric(<) adds the converse: (true,false) ∈ < ∪ <° = ≠.");
static_assert(!static_cast<bool>(symmetric(𝔹 * 𝔹 |
                                           π1 < π2)(std::pair{false, false})),
              "symmetric(<) excludes the diagonal: (false,false) ∉ ≠.");

// The converse is the DAGGER of Rel: an INVOLUTION (R°° = R) that REVERSES
// composition ((R;S)° = S°;R°) --- the two laws that make it a dagger functor,
// and the reason a bijective relation's converse IS its inverse (Component A).
static_assert(static_cast<bool>(converse(converse(𝔹* 𝔹 | π1 < π2))(std::pair{
                  false, true})) ==
                  static_cast<bool>((𝔹 * 𝔹 | π1 < π2)(std::pair{false, true})),
              "R°° = R at (false,true): the converse is an involution.");
static_assert(static_cast<bool>(converse(converse(𝔹* 𝔹 | π1 < π2))(std::pair{
                  true, false})) ==
                  static_cast<bool>((𝔹 * 𝔹 | π1 < π2)(std::pair{true, false})),
              "R°° = R at (true,false): agrees on the excluded pair too.");
static_assert(
    static_cast<bool>(converse((𝔹 * 𝔹 | π1 < π2) >>
                               (𝔹 * 𝔹 | π1 <= π2))(std::pair{true, false})) ==
        static_cast<bool>((converse(𝔹 * 𝔹 | π1 <= π2) >>
                           converse(𝔹 * 𝔹 | π1 < π2))(std::pair{true, false})),
    "(R;S)° = S°;R° at (true,false): the converse reverses composition "
    "(dagger contravariance).");

// Kleene / relation-algebra laws on the DSL, witnessed on 𝔹: Δ is the
// composition unit (R;Δ = R, the algebra's 1); composition distributes over
// union (R;(S∪T) = R;S ∪ R;T); and --- the Schröder property-gated rewrite ---
// a FUNCTIONAL relation's composition distributes over MEET too (R;(S∩T) =
// R;S ∩ R;T), which fails for a non-functional relation.
static_assert(static_cast<bool>(((𝔹 * 𝔹 | π1 < π2) >>
                                 dedekind::relational::diag<bool>())(std::pair{
                  false, true})) ==
                  static_cast<bool>((𝔹 * 𝔹 | π1 < π2)(std::pair{false, true})),
              "R;Δ = R: the diagonal is the composition unit (the 1).");
static_assert(
    static_cast<bool>(((𝔹 * 𝔹 | π1 <= π2) >>
                       ((𝔹 * 𝔹 | π1 < π2) | (𝔹 * 𝔹 | π1 == π2)))(std::pair{
        false, true})) ==
        static_cast<bool>((((𝔹 * 𝔹 | π1 <= π2) >> (𝔹 * 𝔹 | π1 < π2)) |
                           ((𝔹 * 𝔹 | π1 <= π2) >>
                            (𝔹 * 𝔹 | π1 == π2)))(std::pair{false, true})),
    "R;(S∪T) = R;S ∪ R;T: composition distributes over union.");
static_assert(
    static_cast<bool>(((𝔹 * 𝔹 | π1 != π2) >>
                       ((𝔹 * 𝔹 | π1 <= π2) & (𝔹 * 𝔹 | π1 == π2)))(std::pair{
        true, false})) ==
        static_cast<bool>((((𝔹 * 𝔹 | π1 != π2) >> (𝔹 * 𝔹 | π1 <= π2)) &
                           ((𝔹 * 𝔹 | π1 != π2) >>
                            (𝔹 * 𝔹 | π1 == π2)))(std::pair{true, false})),
    "functional R ⟹ R;(S∩T) = R;S ∩ R;T: the property-gated Schröder rewrite.");

// ── FIXME(#786): the reflexive-TRANSITIVE closure (the Kleene star R*) is the
// remaining brick that turns §7's CPM exhibit into a one-liner (R* at a
// tropical semiring).  R* = Δ + R⁺ = Δ + R + R² + ... needs powers Rⁿ = the
// relative product iterated, but @c operator>> above is BOOLEAN-MIDDLE ONLY
// (requires std::same_as<B, bool>), so on 𝔹 the star degenerates to Δ + R (two
// nodes reach in ≤1 step) — no genuine iteration.  Two bricks, in order:
//   1. a FINITE-CARRIER relative product: enumerate a finite middle carrier
//      (the finite-quotient handle, §3.1) so Rⁿ is computable for >2 nodes, and
//      bound the star by the carrier size (Rⁿ stabilises at n = |carrier|).
//   2. a SEMIRING-PARAMETRIC >> and star: (R;S)(a,c) = ⊕_b R(a,b)⊗S(b,c), so
//      R* over (∨,∧) = reachability and over Tropical(max,+) = the critical
//      path (CPM) — the SAME closure, one semiring apart, re-expressing
//      showcase_13 through the DSL star and retiring the net/DSL duality.
// Brick 2's semiring choice + the carrier-enumeration protocol are design
// decisions best made with the author (not guessed); this seed lands the union
// (∪, the set-grammar |), the diagonal (1), and the reflexive/symmetric
// closures (half the Kleene algebra), leaving the star for the next pass.

}  // namespace dedekind::order

// ── Structural inference of the relation properties (Table 3) ───────────────
// The four properties are opt-in traits (trust at the leaves).  Here they are
// INFERRED for the DSL's OWN graph relations, so the concepts are no longer
// restrictive when structure decides.  is_right_unique_v (functional) and
// is_left_total_v (entire) are the pluggable points; IsFunction reads off them.
// This is Table 3's point-free reading realised: a LEAF carries the property by
// its shape, a NODE (a relative product @c >>) inherits it because the property
// composes.
namespace dedekind::category {

// This block carries only FUNCTIONALITY (@c is_right_unique_v), the STRUCTURAL
// half: a graph is single-valued by its shape, decidable in @c order alone.
// ENTIRENESS (@c is_left_total_v) is the ALGEBRAIC half --- a translation is
// total iff the carrier is an ordered additive group --- so those
// specialisations live in @c dedekind.algebra:halfspace_transport, alongside
// the transport operations.  (The @c ProductRestrict entireness is left at its
// primary false there and in the primary: a restriction MAY drop the domain, so
// entireness is conservatively not certified through a joint.)

// LEAF: a translation graph x ↦ x+K is FUNCTIONAL on any carrier (single-valued
// by construction).
template <typename T, auto K, typename L>
inline constexpr bool is_right_unique_v<dedekind::sets::Set<
    std::pair<T, T>, L,
    dedekind::order::ProjAddConstProj<1, K, dedekind::order::Rel::Eq, 2>>> =
    true;

// LEAF: the diagonal π1==π2 (the identity relation) is functional on any
// carrier
// -- a ↦ a, single-valued.
template <typename T, typename L>
inline constexpr bool is_right_unique_v<dedekind::sets::Set<
    std::pair<T, T>, L,
    dedekind::order::ProjProj<1, dedekind::order::Rel::Eq, 2>>> = true;

// RESTRICTION preserves single-valuedness (it removes pairs, never adds), so
// FUNCTIONALITY propagates through ProductRestrict: a restricted graph is still
// functional (a PARTIAL function).
template <typename A, typename B, typename L, typename P, typename RP>
inline constexpr bool is_right_unique_v<dedekind::sets::Set<
    std::pair<A, B>, L, dedekind::order::ProductRestrict<P, RP>>> =
    is_right_unique_v<dedekind::sets::Set<std::pair<A, B>, L, P>>;

// NODE (the compositional closure) for ComposePred's FUNCTIONALITY moved to
// dedekind.relational:dyadic (PR #797, Copilot review) --- it is
// structure-independent relation-algebra (R>>S functional iff both factors
// are), so it lives with ComposePred so a client importing only :relational
// sees it.

}  // namespace dedekind::category

namespace dedekind::order {
// A RESTRICTED translation graph is still FUNCTIONAL (a partial function),
// inferred through ProductRestrict from the underlying translation's
// certificate
// -- but NOT entire (the restriction drops the domain), so it is not a total
// function, matching the paper's partial-function reading.
static_assert(
    dedekind::relational::IsFunctional<decltype((ℤ * ℤ | π1 + fix(3_c) == π2) |
                                                π1 <= fix(5_c))> &&
        !dedekind::relational::IsEntire<decltype((ℤ * ℤ | π1 + fix(3_c) == π2) |
                                                 π1 <= fix(5_c))>,
    "a restricted translation graph is a functional-but-not-entire partial "
    "function, inferred through ProductRestrict.");
}  // namespace dedekind::order
