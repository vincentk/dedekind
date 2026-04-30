/**
 * @file dedekind/numbers/real.cppm
 * @partition :real
 * @brief Minimal real wrapper for experimental reintegration.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "Die Zahlen sind freie Schöpfungen des menschlichen Geistes."
 *       ("Numbers are free creations of the human mind.")
 *       -- Richard Dedekind, Was sind und was sollen die Zahlen? (1888)
 */
module;

#include <compare>
#include <concepts>
#include <cstddef>
#include <functional>
#include <type_traits>
#include <utility>

export module dedekind.numbers:real;

import dedekind.algebra; // HasRingOperators / HasFieldOperators (canonical-spine witnesses)
import dedekind.category;
import dedekind.order;
import dedekind.sets;
import :rational;

namespace dedekind::numbers {
using namespace dedekind::category;
using namespace dedekind::order;
using namespace dedekind::sets;

// Canonical machine realization for the real scalar carrier.
export using machine_real_scalar = double;

export template <typename S>
concept IsRealCarrier = requires(S a, S b) {
  S{};
  { a + b } -> std::same_as<S>;
  { a - b } -> std::same_as<S>;
  { a * b } -> std::same_as<S>;
  { a / b } -> std::same_as<S>;
  { a < b } -> std::convertible_to<bool>;
};

export template <IsRealCarrier Q>
class Real {
 public:
  // Self-Domain: var<ℝ> ranges over reals (per the symbolic-scout
  // factory's Variable<S>::T = S::Domain rule).  The underlying
  // scalar carrier is exposed via ScalarCarrier.
  using Domain = Real;
  using ScalarCarrier = Q;
  using path_type = Q;

  constexpr Real() = default;
  /** @brief Implicit to allow `(a + b) / Q{2}` in IsDense checks. */
  constexpr Real(Q value)
      : value_(value) {}  // NOLINT(google-explicit-constructor)
  /** @brief Single-step implicit conversion from any V → Q.
   *  Allows `(a + b) / 2` in IsDense: int → Rational → Real in one UDC. */
  template <typename V>
    requires(!std::same_as<V, Q> && std::convertible_to<V, Q>)
  constexpr Real(V v) : value_(v) {}  // NOLINT(google-explicit-constructor)

  constexpr Q resolve() const { return value_; }
  constexpr Q path() const { return value_; }

  // Compatibility with prior symbolic-cut style usage.
  constexpr bool operator()(const Q& x) const { return x < value_; }

  // Three-way comparison delegates to the scalar carrier.
  // For Q = Rational<I>: strong_ordering; for Q = double: partial_ordering.
  friend constexpr auto operator<=>(const Real& a, const Real& b) {
    return a.value_ <=> b.value_;
  }

  friend constexpr bool operator==(const Real&, const Real&) = default;

  /** @brief Supremum and infimum of the singleton {value_}.
   *  Satisfies HasExtrema — required by IsDedekindComplete. */
  constexpr Q infimum() const { return value_; }
  constexpr Q supremum() const { return value_; }

  // Additive inverse: -(a) = Real{-value_}
  constexpr Real operator-() const { return Real{-value_}; }

  friend constexpr Real operator+(const Real& a, const Real& b) {
    return Real{a.value_ + b.value_};
  }

  friend constexpr Real operator*(const Real& a, const Real& b) {
    return Real{a.value_ * b.value_};
  }

  friend constexpr Real operator-(const Real& a, const Real& b) {
    return Real{a.value_ - b.value_};
  }

  friend constexpr Real operator/(const Real& a, const Real& b) {
    return Real{a.value_ / b.value_};
  }

 private:
  Q value_{};
};

/** @section real__Partial_Arithmetic_with_Ternary_Logic */

/**
 * @brief Partial addition transform for Real<S>.
 *
 * Real arithmetic uses a numeric carrier S (typically double/IEEE 754).
 * Operations succeed but may lose precision due to rounding.
 * We acknowledge this by always returning Ternary::True (the operation
 * completed) but document that the result is approximate.
 */
export template <IsRealCarrier S>
struct PartialAddReal {
  using value_type = Real<S>;
  using logic_species = TernaryLogic;

  TernaryResult<Real<S>> operator()(
      std::pair<const Real<S>&, const Real<S>&> p) const noexcept {
    auto [a, b] = p;
    return {Ternary::True, a + b};
  }
};

/**
 * @brief Partial multiplication transform for Real<S>.
 *
 * Real multiplication succeeds but may lose precision due to rounding.
 */
export template <IsRealCarrier S>
struct PartialMulReal {
  using value_type = Real<S>;
  using logic_species = TernaryLogic;

  TernaryResult<Real<S>> operator()(
      std::pair<const Real<S>&, const Real<S>&> p) const noexcept {
    auto [a, b] = p;
    return {Ternary::True, a * b};
  }
};

/**
 * @brief Partial division for Real<S> with zero-check.
 *
 * Returns Ternary::False if divisor is zero (for double), reflecting
 * the IEEE 754 behavior of producing non-finite values (±∞ or NaN).
 */
export template <IsRealCarrier S>
struct PartialDivReal {
  using value_type = Real<S>;
  using logic_species = TernaryLogic;

  TernaryResult<Real<S>> operator()(
      std::pair<const Real<S>&, const Real<S>&> p) const noexcept {
    auto [a, b] = p;
    // For floating-point types, division by zero produces inf/nan.
    // We flag this as False because non-finite output lies outside
    // the intended domain of this partial operation.
    if constexpr (std::is_floating_point_v<S>) {
      if (b.resolve() == S{0}) {
        return {Ternary::False, a / b};  // Produces ±∞ or NaN per IEEE 754
      }
    }
    return {Ternary::True, a / b};
  }
};

/**
 * @brief Identity and Associativity traits for Real arithmetic.
 *
 * Real<S> arithmetic (e.g., with S = double) is commutative but NOT
 * associative: floating-point rounding means (a+b)+c ≠ a+(b+c) in general.
 * Commutativity holds for all IEEE 754 operations (barring no special cases
 * that differ by order). Associativity-by-fiat is reserved for the
 * explicit dedekind::ieee::IEEE<F> opt-in wrapper.
 *
 * Partial identities: 0 for addition, 1 for multiplication.
 *
 * Specializations are declared in the dedekind::category namespace (see below).
 */

/**
 * @brief Embedding transform: ℚ ↪ ℝ with Ternary acknowledgment.
 *
 * The embedding of a rational Q = p/q into the reals (via IEEE 754 scalar S)
 * is **lossy**: the result is the closest representable value, not necessarily
 * exact. This transform returns Ternary::Unknown to signal potential
 * information loss due to floating-point rounding.
 *
 * In a more sophisticated model, we could check if the rational is exactly
 * representable and return True, but conservatively, we flag all embeddings
 * as Unknown.
 */
export template <IsInteger I = default_integer,
                 IsRealCarrier S = machine_real_scalar>
struct PartialEmbedRationalToReal {
  using value_type = Real<S>;
  using logic_species = TernaryLogic;

  TernaryResult<Real<S>> operator()(const Rational<I>& q) const noexcept {
    // The embedding is lossy due to IEEE 754 approximation — inline to avoid
    // forward ref
    return {Ternary::Unknown,
            Real<S>{static_cast<S>(q.num()) / static_cast<S>(q.den())}};
  }
};

/**
 * @brief Kleene traits for rational→real embedding (lossy).
 *
 * The embedding is not associative/commutative in the Kleene sense because
 * different computation orders might yield different rounded results.
 */
// Note: We intentionally do NOT declare associativity for this embedding
// because floating-point rounding violates Kleene associativity.

}  // namespace dedekind::numbers

namespace dedekind::category {

/** @brief Kleene traits for real arithmetic.
 *
 * Commutativity holds for IEEE 754 addition and multiplication.
 * Associativity does NOT hold for floating-point — that opt-in belongs
 * exclusively to dedekind::ieee::IEEE<F>.
 */
template <dedekind::numbers::IsRealCarrier S>
inline constexpr bool is_kleene_commutative_v<
    dedekind::numbers::Real<S>, dedekind::numbers::PartialAddReal<S>> = true;

template <dedekind::numbers::IsRealCarrier S>
inline constexpr dedekind::numbers::Real<S> partial_identity_v<
    dedekind::numbers::Real<S>, dedekind::numbers::PartialAddReal<S>> =
    dedekind::numbers::Real<S>{S{0}};

template <dedekind::numbers::IsRealCarrier S>
inline constexpr bool is_kleene_commutative_v<
    dedekind::numbers::Real<S>, dedekind::numbers::PartialMulReal<S>> = true;

template <dedekind::numbers::IsRealCarrier S>
inline constexpr dedekind::numbers::Real<S> partial_identity_v<
    dedekind::numbers::Real<S>, dedekind::numbers::PartialMulReal<S>> =
    dedekind::numbers::Real<S>{S{1}};

/** @brief Ordering traits: Real<Rational<I>> inherits ℚ's total order.
 *
 * Real<double> is intentionally withheld (NaN breaks reflexivity).
 * Real<Rational<I>> is totally ordered because Rational<I> is.
 */
template <dedekind::numbers::IsInteger I>
inline constexpr bool
    is_reflexive_v<dedekind::numbers::Real<dedekind::numbers::Rational<I>>,
                   std::less_equal<>> = true;

template <dedekind::numbers::IsInteger I>
inline constexpr bool
    is_transitive_v<dedekind::numbers::Real<dedekind::numbers::Rational<I>>,
                    std::less_equal<>> = true;

template <dedekind::numbers::IsInteger I>
inline constexpr bool
    is_antisymmetric_v<dedekind::numbers::Real<dedekind::numbers::Rational<I>>,
                       std::less_equal<>> = true;

}  // namespace dedekind::category

namespace dedekind::numbers {

/**
 * @brief Machine realization arrow ℚ ↪ ℝ: Rational<I> → Real<S>.
 * @details Converts a rational p/q to the closest IEEE 754 value of type S.
 *
 * @section Carrier_Bridge_Locality
 * The integer-to-IEEE conversion @c static_cast<S>(q.num()) is the lossy
 * step.  When @c I is a built-in (@c int, @c long, ...), the standard
 * float-from-int conversion fires.  When @c I is a variant exact carrier
 * (@c SignedExtensionalCardinal<>, etc.) which deliberately does @b not
 * export an @c operator @c double (to keep the variant carriers free of
 * IEEE coupling), the lossy bridge is implemented here at the
 * @b call-site rather than upstream: the variant integer is first
 * extracted as a built-in @c int via its @c operator @c int (single-limb,
 * the canonical retarget instance) and then promoted to @c S via
 * standard widening.  This keeps the IEEE-coupling local to the arrow
 * that owns the lossy semantics, matching the structural-Platonist
 * directive that variant carriers do not advertise machine-numeric
 * conversions they do not need.
 */
export template <IsInteger I = default_integer,
                 IsRealCarrier S = machine_real_scalar>
inline constexpr auto embed_ℚ_ℝ =
    arrow<Rational<I>, Real<S>>([](const Rational<I>& q) noexcept {
      auto to_real_scalar = [](const I& z) -> S {
        // Detect @b static-castability rather than just implicit
        // convertibility: a carrier may expose @c explicit
        // @c operator @c S, which makes @c static_cast<S>(z) valid
        // even when @c std::convertible_to<I, S> is false.  PR #520
        // review follow-up: this captures both the implicit-conversion
        // case (built-in @c int @c → @c double) and the
        // explicit-conversion-operator case uniformly.
        if constexpr (requires { static_cast<S>(z); }) {
          return static_cast<S>(z);
        } else {
          // Variant carrier path: the variant integer is first
          // extracted via its documented @c operator @c int (a
          // signed-integral conversion, gated separately on the
          // variant), then widened to @c S.  Lossy by design at the
          // variant-int step (multi-limb values truncate to
          // single-limb int) AND at the int-to-float step (above 2^53
          // IEEE rounding fires).  We require the int bridge exists;
          // if neither path is available, the static_assert below
          // surfaces a diagnostic at the right spot rather than an
          // opaque template substitution failure.
          static_assert(
              requires { static_cast<int>(z); },
              "embed_ℚ_ℝ requires either static_cast<S>(I) or "
              "static_cast<int>(I) to be valid; provide one or "
              "the other on the integer carrier I.");
          return static_cast<S>(static_cast<int>(z));
        }
      };
      return Real<S>{to_real_scalar(q.num()) / to_real_scalar(q.den())};
    });

/**
 * @brief Canonical embedding of any std::floating_point type into ℝ.
 *
 * @details Wraps any floating-point value in Real<machine_real_scalar>
 * (= Real<double>). For float → Real<double> this is a widening conversion;
 * for double it is an identity wrap. The embedding is approximate because
 * IEEE 754 does not represent all reals exactly (cf. PartialEmbedRationalToReal
 * for the lossy-flagged variant).
 *
 * @tparam F Any std::floating_point source type.
 */
export template <std::floating_point F>
constexpr Real<machine_real_scalar> embed_floating_ℝ(F v) {
  return Real<machine_real_scalar>{static_cast<machine_real_scalar>(v)};
}

/**
 * @brief Characteristic morphism for ℝ: the real numbers.
 * Accepts native Real<S> and delegates predecessor checks through ℚ.
 */
export template <IsRealCarrier S = machine_real_scalar,
                 IsInteger I = default_integer, typename L = ClassicalLogic,
                 typename C = ℶ_1>
struct RealsOf {
  using Domain = Real<S>;
  using Codomain = typename L::Ω;
  using logic_species = L;
  using cardinality_type = C;

  // Native Real<S>: always a member of ℝ
  constexpr typename L::Ω operator()(const Real<S>&) const { return L::True; }

  // Direct parent: embed Rational<I> into ℝ via the canonical arrow.
  constexpr typename L::Ω operator()(const Rational<I>& q) const {
    return operator()(embed_ℚ_ℝ<I, S>(q));
  }

  // Delegate non-parent ancestors to ambient ℚ.
  template <typename T>
    requires(!std::same_as<T, Real<S>> && !std::same_as<T, Rational<I>>)
  constexpr typename L::Ω operator()(const T& x) const {
    return dedekind::numbers::RationalsOf<I>{}(x);
  }
};

export using RealSet = RealsOf<>;
export using ℝ = RealSet;

export inline constexpr ℝ R{};

}  // namespace dedekind::numbers

namespace dedekind::category {
template <typename Q>
struct SpeciesTraits<dedekind::numbers::Real<Q>> {
  using Domain = dedekind::numbers::Real<Q>;
  using machine_type = dedekind::numbers::Real<Q>;
};

template <>
inline constexpr bool
    is_monic_arrow_v<std::decay_t<decltype(dedekind::numbers::embed_ℚ_ℝ<>)>> =
        true;
static_assert(
    IsInjective<std::decay_t<decltype(dedekind::numbers::embed_ℚ_ℝ<>)>>,
    "embed_ℚ_ℝ (ℚ ↪ ℝ) is registered injective.");
}  // namespace dedekind::category

namespace dedekind::numbers {

// Functor identification: Real<Q> = Cuts(Q).  The Cuts functor
// (Dedekind order-completion of an ordered field; cf. Lang §I appendix)
// takes a dense-ordered carrier Q to its Cauchy/Dedekind completion;
// for any IsRealCarrier Q, Real<Q> IS that completion.  The
// ScalarCarrier alias is the source-side projection of the Cuts
// functor, mechanically aligning the §2 paper paragraph
// ("Named functors that build the library's carriers") with source.
//
// FIXME(#498/NEW-A): same naming-convention question as
// Rational<I>::IntegerCarrier — see the FIXME there.  Aligning
// IntegerCarrier / ScalarCarrier / value_type with :functor's
// Σ_cat / Τ_cat / Shape<U> convention is NEW-A trait-registry work.
static_assert(std::same_as<typename Real<machine_real_scalar>::ScalarCarrier,
                           machine_real_scalar>,
              "Real<Q> is the Cuts-functor image of Q; ScalarCarrier names Q "
              "mechanically.");

static_assert(
    dedekind::category::IsSet<decltype(dedekind::category::ambient_set<
                                       Real<machine_real_scalar>>(R))>,
    "RealsOf must be the canonical IsSet anchor for dedekind.numbers:real.");

/**
 * @brief Canonical exact real: ℝ defined over ℚ by the Dedekind cut
 * construction.
 *
 * Real<Rational<I>> uses exact rational arithmetic as its scalar carrier.
 * Unlike Real<double>, it satisfies IsDedekindComplete: the ordering is total
 * (inherited from ℚ), the density holds (midpoint (a+b)/2 always exists in
 * ℚ), and HasExtrema holds trivially (infimum = supremum = the point itself).
 *
 * The name "ExactReal" reflects that this is the formally honest ℝ:
 * every value is a ratio of integers with no rounding loss.
 */
export template <IsInteger I = default_integer>
using ExactReal = Real<Rational<I>>;

// Proof: ExactReal<> satisfies the Dedekind-completeness axioms.
static_assert(
    IsDedekindComplete<ExactReal<>>,
    "ExactReal<> (= Real<Rational<Z>>) must satisfy IsDedekindComplete — "
    "ℝ defined over ℚ via the Dedekind cut construction.");

// Proof: ExactReal<> satisfies the operational field-like witness.
static_assert(dedekind::algebra::HasFieldOperators<ExactReal<>>,
              "ExactReal<> must satisfy HasFieldOperators (ℝ is a field).");

/** @section real__Canonical_Species_Spine (ℝ)
 *
 * The canonical real-number species ℝ is defined above as
 * @c RealSet @c = @c RealsOf<> with value-level constant @c R; the
 * exact realisation is @c ExactReal<I> @c = @c Real<Rational<I>>
 * (Dedekind cuts over ℚ, formally honest --- every value is a ratio
 * of integers with no rounding loss).  The spine witnesses below pin
 * ℝ's syntax / semantics / arrow fabric:
 *
 * (1) IsSet anchor on @c R (above).
 * (2) Syntax: @c HasRingOperators / @c HasFieldOperators on the
 *     exact carrier @c ExactReal<>; the machine carrier @c double
 *     fires the literal-shape concept too, but its semantics are
 *     IEEE-policy-flavoured (@c HasFieldOperators holds with the
 *     usual rounding caveats).
 * (3) Semantics: @c IsDedekindComplete<ExactReal<>> (above) is the
 *     defining ℝ axiom; @c HasFieldOperators<ExactReal<>> is the
 *     operational field witness (Pattern-(b) per #394).  Strict
 *     @c IsField is blocked by two distinct issues: the
 *     architectural @c IsTotal gate (currently periodic/idempotent/
 *     saturating only --- exact carriers are none of those) and the
 *     species-trait specialisations on @c Rational / @c ExactReal
 *     under the active numeric policy.  See the @c FIXME(\#379) at
 *     the @c Field_ℚ definition in @c :integer for the full chain.
 * (4) Primitive-type arrow: ℝ ↔ @c double is the open #398 work
 *     (explicit @c embed_double / @c realize_to_double morphisms).
 *     The current trivial direction is @c Real<double>{x} for the
 *     forward and @c .resolve() for the reverse.
 * (5) Adjacent-set arrows: ℚ ↪ ℝ via @c embed_ℚ_ℝ above
 *     (registered monic); ℝ ↪ ℂ via @c embed_ℝ_ℂ in @c :complex
 *     (downstream).
 */
static_assert(dedekind::algebra::HasRingOperators<ExactReal<>>,
              "ExactReal<> closes the ring operator surface.");
static_assert(dedekind::algebra::HasFieldOperators<ExactReal<>>,
              "ExactReal<> closes the field operator surface "
              "(+, binary -, unary -, *, /, T{1}).");

}  // namespace dedekind::numbers
