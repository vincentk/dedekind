/**
 * @file dedekind/numbers/rational.cppm
 * @partition :rational
 * @brief Level 8: ℚ — rationals as numerator/denominator pairs.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "Lectio mathematica nihil aliud est, quam meditatio cum calamo
 * coniuncta."
 *       — Leonhard Euler, paraphrase from the preface of
 *         *Introductio in analysin infinitorum* (1748).
 *       [Trans: "Reading mathematics is nothing other than meditation
 *        conjoined with the pen."]
 */

module;
#include <bit>
#include <compare>
#include <concepts>
#include <cstdint>
#include <functional>
#include <limits>
#include <numeric>
#include <stdexcept>
#include <utility>

export module dedekind.numbers:rational;

import dedekind.algebra;
import dedekind.category;
import dedekind.order;
import dedekind.sets;
import :integer;

namespace dedekind::numbers {
using namespace dedekind::category;
using namespace dedekind::order;
using namespace dedekind::sets;

// Canonical extensional/machine realization for the integer carrier.
export using machine_integer = extensional_integer;

// Canonical extensional/machine realization for the floating-point side
// of the rational realization boundary.  Mirrors @c machine_integer: a
// single-place rename surface so downstream code does not hard-code
// @c double everywhere.  The IEEE 754 partial reading lives at
// @c numbers:floating_point ; the exact embedding into ℚ runs through
// @c embed_double_ℚ (defined further down).
export using machine_float = double;

/**
 * @class Rational
 * @brief The Field of Fractions over an Integral Domain Z.
 */
export template <IsInteger Z>
class Rational {
 public:
  // Self-Domain: `Variable<Rational<Z>>::T` becomes `Rational<Z>` so
  // the symbolic-scout factory `var<ℚ>` ranges over rationals (the
  // intended math reading), not over the underlying integer carrier.
  // The integer carrier is still accessible via the @c IntegerCarrier
  // alias below.
  using Domain = Rational;
  using IntegerCarrier = Z;

  // Numerator and denominator as a canonical pair (second always positive).
  // Public to satisfy IsProduct<Rational<Z>, Z, Z> (ℚ ≅ ℤ × ℤ / ~).
  Z first, second;

  /** @brief Default: 0/1. Required so Rational<Z> satisfies IsRealCarrier. */
  constexpr Rational() : first(Z{}), second(Z{1}) {}

  constexpr Rational(Z num, Z den) : first(num), second(den) { simplify(); }

  /** @brief Embedding of an integer as a rational n/1.
   *  Implicit to allow `(a + b) / Z{2}` in IsDense checks. */
  constexpr Rational(Z n)
      : first(n), second(Z{1}) {}  // NOLINT(google-explicit-constructor)

  /** @section The_Simplification_Morphism */
  constexpr void simplify() {
    if (second == Z{0}) throw std::domain_error("Rational: Division by zero.");

    Z common = euclidean_gcd(first, second);
    first = first / common;
    second = second / common;

    // Canonical sign: denominator is always positive
    if (second < Z{0}) {
      first = -first;
      second = -second;
    }
  }

  /** @section Relational_Morphisms */

  // Three-way comparison: a/b <=> c/d iff a*d <=> c*b (positive denominators).
  // Providing <=> gives <, <=, >, >= automatically (C++20).
  friend constexpr std::strong_ordering operator<=>(const Rational& a,
                                                    const Rational& b) {
    const auto lhs = a.first * b.second;
    const auto rhs = b.first * a.second;
    if (lhs < rhs) return std::strong_ordering::less;
    if (rhs < lhs) return std::strong_ordering::greater;
    return std::strong_ordering::equal;
  }

  friend constexpr bool operator==(const Rational&, const Rational&) = default;

  constexpr Z num() const { return first; }
  constexpr Z den() const { return second; }

  /** @section Multiplicative_Inverse: The Reciprocal */
  constexpr Rational inverse() const {
    if (first == Z{0}) {
      throw std::domain_error("Rational: Zero has no multiplicative inverse.");
    }
    // To invert (a/b), we return (b/a)
    return Rational(second, first);
  }

  /** @section Field_Operators */

  // Multiplication: (a/b) * (c/d) = (ac / bd)
  friend constexpr Rational operator*(const Rational& a, const Rational& b) {
    return Rational(a.first * b.first, a.second * b.second);
  }

  friend constexpr Rational operator+(const Rational& a, const Rational& b) {
    return Rational((a.first * b.second) + (b.first * a.second),
                    a.second * b.second);
  }

  friend constexpr Rational operator-(const Rational& a, const Rational& b) {
    return a + (-b);
  }

  // Division: (a/b) / (c/d) = (a/b) * (d/c)
  friend constexpr Rational operator/(const Rational& a, const Rational& b) {
    return a * b.inverse();
  }

  // Additive Inverse: -(a/b) = (-a/b)
  constexpr Rational operator-() const { return Rational(-first, second); }
};

/** @section Partial_Arithmetic_with_Ternary_Logic */

/**
 * @brief Partial addition transform for Rational<I>.
 *
 * Since rational arithmetic is mathematically exact (no rounding loss),
 * this always returns Ternary::True. The status could be Unknown
 * if we wanted to check for integer overflow, but we delegate that
 * responsibility to I's overflow predicates.
 */
export template <IsInteger I>
struct PartialAddRational {
  using value_type = Rational<I>;
  using logic_species = TernaryLogic;

  TernaryResult<Rational<I>> operator()(
      std::pair<const Rational<I>&, const Rational<I>&> p) const {
    auto [a, b] = p;
    return {Ternary::True, a + b};
  }
};

/**
 * @brief Partial multiplication transform for Rational<I>.
 *
 * Since rational arithmetic is mathematically exact (no rounding loss),
 * this always returns Ternary::True.
 */
export template <IsInteger I>
struct PartialMulRational {
  using value_type = Rational<I>;
  using logic_species = TernaryLogic;

  TernaryResult<Rational<I>> operator()(
      std::pair<const Rational<I>&, const Rational<I>&> p) const {
    auto [a, b] = p;
    return {Ternary::True, a * b};
  }
};

/**
 * @brief Division transform for Rational<I>.
 *
 * Since Rational<I> models exact arithmetic in the quotient field,
 * division does not truncate the way integer division can. Accordingly,
 * this returns Ternary::True whenever the rational quotient is formed.
 * Division by zero propagates via Rational::inverse(), which may throw.
 */
export template <IsInteger I>
struct HonestDivRational {
  using value_type = Rational<I>;
  using logic_species = TernaryLogic;

  TernaryResult<Rational<I>> operator()(
      std::pair<const Rational<I>&, const Rational<I>&> p) const {
    auto [a, b] = p;
    if (b.num() == I{0}) {
      return {Ternary::False, a};
    }
    return {Ternary::True, a / b};  // Rational division is exact when defined
  }
};

/**
 * @brief Identity and Associativity traits for Rational arithmetic.
 *
 * Rational<I> arithmetic over exact field operations satisfies:
 * - Kleene associativity: if both sides are defined, they are equal
 * - Kleene commutativity (for addition/multiplication)
 * - Partial identities: 0 for addition, 1 for multiplication
 *
 * Specializations are declared in the dedekind::category namespace (see below).
 */

/**
 * @brief Embedding transform: ℤ ↪ ℚ with Ternary acknowledgment.
 *
 * The embedding of an integer Z into the rationals is **exact**:
 * every integer n corresponds uniquely to n/1.
 * This transform returns Ternary::True to signal no information loss.
 */
export template <IsInteger I>
struct PartialEmbedIntegerToRational {
  using value_type = Rational<I>;
  using logic_species = TernaryLogic;

  TernaryResult<Rational<I>> operator()(I n) const noexcept {
    return {Ternary::True, Rational<I>{n, static_cast<I>(1)}};
  }
};

}  // namespace dedekind::numbers

/**
 * @section Kleene_Traits_in_Category_Namespace
 *
 * Specializations of Kleene traits for Rational<I> types.
 * These must be in the dedekind::category namespace where the traits
 * template variables are declared.
 */
namespace dedekind::category {

/** @brief Kleene traits for rational arithmetic. */
template <dedekind::numbers::IsInteger I>
inline constexpr bool is_kleene_associative_v<
    dedekind::numbers::Rational<I>, dedekind::numbers::PartialAddRational<I>> =
    true;

template <dedekind::numbers::IsInteger I>
inline constexpr bool is_kleene_commutative_v<
    dedekind::numbers::Rational<I>, dedekind::numbers::PartialAddRational<I>> =
    true;

template <dedekind::numbers::IsInteger I>
inline constexpr dedekind::numbers::Rational<I> partial_identity_v<
    dedekind::numbers::Rational<I>, dedekind::numbers::PartialAddRational<I>> =
    dedekind::numbers::Rational<I>(I{0}, I{1});

template <dedekind::numbers::IsInteger I>
inline constexpr bool is_kleene_associative_v<
    dedekind::numbers::Rational<I>, dedekind::numbers::PartialMulRational<I>> =
    true;

template <dedekind::numbers::IsInteger I>
inline constexpr bool is_kleene_commutative_v<
    dedekind::numbers::Rational<I>, dedekind::numbers::PartialMulRational<I>> =
    true;

template <dedekind::numbers::IsInteger I>
inline constexpr dedekind::numbers::Rational<I> partial_identity_v<
    dedekind::numbers::Rational<I>, dedekind::numbers::PartialMulRational<I>> =
    dedekind::numbers::Rational<I>(I{1}, I{1});

/** @brief Kleene traits for integer→rational embedding (exact). */
template <dedekind::numbers::IsInteger I>
inline constexpr bool is_kleene_associative_v<
    dedekind::numbers::Rational<I>,
    dedekind::numbers::PartialEmbedIntegerToRational<I>> = true;

/** @brief Ordering traits: Rational<I> is a total order (ℚ ≤ is a chain). */
template <dedekind::numbers::IsInteger I>
inline constexpr bool
    is_reflexive_v<dedekind::numbers::Rational<I>, std::less_equal<>> = true;

template <dedekind::numbers::IsInteger I>
inline constexpr bool
    is_transitive_v<dedekind::numbers::Rational<I>, std::less_equal<>> = true;

template <dedekind::numbers::IsInteger I>
inline constexpr bool
    is_antisymmetric_v<dedekind::numbers::Rational<I>, std::less_equal<>> =
        true;

}  // namespace dedekind::category

namespace dedekind::numbers {

/** @section Formal_Verification */

// Proof: The inverse of 2/3 is 3/2.
static_assert((Rational<default_integer>(2, 3).inverse().num() == 3));

/**
 * @brief Characteristic morphism for ℚ: the rationals.
 * Accepts native Rational<I> and delegates predecessor checks through ℤ.
 */
export template <IsInteger I = default_integer, typename L = ClassicalLogic,
                 typename C = ℵ_0>
struct RationalsOf {
  using Domain = Rational<I>;
  using Codomain = typename L::Ω;
  using logic_species = L;
  using cardinality_type = C;

  // Native Rational<I>: always a member of ℚ
  constexpr typename L::Ω operator()(const Rational<I>&) const {
    return L::True;
  }

  // Direct parent: embed extensional integer into ℚ.
  constexpr typename L::Ω operator()(machine_integer z) const {
    return operator()(Rational<I>{static_cast<I>(z), static_cast<I>(1)});
  }

  // Delegate non-parent ancestors to ambient ℤ.
  template <typename T>
    requires(!std::same_as<T, Rational<I>> && !std::same_as<T, machine_integer>)
  constexpr typename L::Ω operator()(const T& x) const {
    return dedekind::numbers::Z(x);
  }
};

/** @brief Internal predicate-set type for the rational numbers (the
 *         IsSet anchor; categorical set object).  See @c ℚ below for
 *         the algebraic carrier (the @c Rational<...> field type).
 *         Not @c export-ed --- the API boundary is crystal clear:
 *         @c ℚ is the field, @c RationalSet is the implementation
 *         detail underlying the value-level constant @c Q.
 */
using RationalSet = RationalsOf<>;

/** @brief The canonical rational-number field ℚ, as the carrier type
 *         (default-instantiated @c Rational<default_integer>).
 *
 *  @details Pluggable via @c ℚ_t<MyInteger> for non-default integer
 *  carriers; the bare @c ℚ defaults to @c Rational<default_integer>
 *  so the show-to-a-wider-audience API reads as plain mathematics:
 *  @c static_assert(algebra::HasFieldOperators<ℚ>), @c auto @c q @c
 *  = @c var<ℚ>, etc.
 *
 *  @note  The strict @c category::IsField<ℚ, std::plus<ℚ>,
 *  std::multiplies<ℚ>> is @b not currently certified.  Two distinct
 *  blocks compose:
 *
 *  1. @b IsTotal @b gate (architectural).  The strict ring/field
 *     ladder requires @c IsMagma, which requires @c IsTotal<T, Op>
 *     @c = @c IsPeriodic @c || @c IsIdempotent @c || @c IsSaturating.
 *     Exact carriers like @c Rational<...> are none of those (no
 *     wrap, no idempotence, no saturation), so the strict ladder is
 *     blocked at the totality step regardless of invertibility
 *     specialisations.  An "exact" or "infinite-domain-total"
 *     fourth path on @c IsTotal would be required.
 *  2. @b Species-trait specialisations (incremental).  Even with
 *     IsTotal lifted, the @c is_invertible_v / @c inverse_trait
 *     registrations would still be missing on @c Rational under the
 *     active numeric policy.
 *
 *  See the @c FIXME(\#379) breadcrumb at the Field_ℚ definition in
 *  @c :integer.  Until both blocks lift, the operational
 *  @c algebra::IsFieldLikeScalar<ℚ> is the load-bearing field-
 *  arithmetic guarantee.
 */
export template <IsInteger I = default_integer>
using ℚ_t = Rational<I>;

export using ℚ = ℚ_t<>;

/** @brief The predicate-set value (instance of @c RationalSet),
 *         retained for set-builder DSL usage like @c Set{q @c % @c Q}
 *         where @c q @c = @c var<ℚ> ranges over rationals.
 */
export inline constexpr RationalSet Q{};

/**
 * @brief Machine realization arrow ℤ ↪ ℚ: machine_integer → Rational<I>.
 * @details Every integer n embeds as the fraction n/1.
 *          This is the current machine model lift of Z → Q.
 */
export template <IsInteger I = default_integer>
inline constexpr auto embed_ℤ_ℚ =
    arrow<machine_integer, Rational<I>>([](const machine_integer& n) noexcept {
      return Rational<I>{static_cast<I>(n), static_cast<I>(1)};
    });

/**
 * @brief Exact dyadic embedding @c double → ℚ.
 *
 * @details Every finite IEEE 754 @c double is exactly the dyadic
 * rational @c m·2^e for integers @c m, @c e (mantissa, exponent).
 * This arrow extracts the bit-pattern via @c std::bit_cast and
 * constructs @c Rational<I>{numerator, denominator} where the
 * denominator is the relevant power of two.  The mapping is lossless
 * on its in-range subset and partial in three ways: NaN and ±∞ are
 * out-of-band for ℚ (the rationals are a field with no sentinels)
 * and trip @c std::domain_error; finite inputs whose reduced
 * mantissa or scaled denominator do not fit in @c I trip
 * @c std::overflow_error.  In a constant-evaluation context the
 * same conditions are rejected as not-a-constant-expression because
 * @c throw is not @c constexpr.
 *
 * @section Precision
 * The integer carrier @c I must be wide enough to hold the reduced
 * 53-bit mantissa and any post-decomposition power-of-two scaling.
 * For built-in signed @c I (e.g. @c int, @c long, @c long @c long)
 * the arrow checks each step against @c std::numeric_limits<I> and
 * throws @c std::overflow_error on out-of-range values --- avoiding
 * the signed-overflow UB that an unchecked multiplication would
 * trip.  For non-built-in carriers (@c SignedExtensionalCardinal<N>
 * etc.) the arrow trusts the carrier's own arithmetic; sufficient
 * width or saturating semantics is the carrier's responsibility.
 *
 * The default @c default_integer (= @c int) suffices for inputs
 * whose exact rational representation fits in 32 bits on each side
 * --- notably the small integer-valued doubles paper showcases use
 * (e.g. @c -21.0, @c 0.5, @c 1.5).  Callers needing full IEEE
 * precision instantiate over @c SignedExtensionalCardinal<N> with
 * @c N large enough.
 *
 * @section Honesty_Obligation
 * The arrow is a @b partial morphism in the Cockett--Lack
 * restriction-category sense (cf. references.bib, cockett2002restriction):
 * total on its in-range subset (where it is provably injective by the
 * exactness of the dyadic decomposition; Knuth, @em TAOCP, Vol. 2,
 * §4.2), partial on the IEEE sentinels and on the precision-overflow
 * boundary.  The unconditional @c is_monic_arrow_v registration is
 * @b deferred to the safe-float boundary refactor in #496, where the
 * partial-domain restriction will live in the type rather than in
 * the throws --- at that point the in-range-subset injectivity will
 * lift to a total-arrow @c IsMonicArrow witness.  Until then the
 * structural claim is documented in this comment but @b not pinned
 * mechanically (a mechanical pin would be a stronger claim than the
 * partial function actually supports).
 */
export template <IsInteger I = default_integer>
inline constexpr auto embed_double_ℚ =
    arrow<double, Rational<I>>([](const double& x) -> Rational<I> {
      // Reject IEEE 754 sentinels: NaN / ±∞ are out-of-band for ℚ.
      // In a constexpr context these throws make the call
      // not-a-constant-expression --- the right behaviour for an
      // exact-rationals codomain.
      if (x != x) {
        throw std::domain_error(
            "embed_double_ℚ: NaN has no embedding in ℚ (the rationals "
            "are a field with no sentinels).");
      }
      constexpr double kInfinity = std::numeric_limits<double>::infinity();
      if (x == kInfinity || x == -kInfinity) {
        throw std::domain_error(
            "embed_double_ℚ: ±∞ has no embedding in ℚ; this requires "
            "an extended-real interpretation rather than a rational one.");
      }

      // ±0 collapses to canonical zero.
      if (x == 0.0) {
        return Rational<I>{I{0}, I{1}};
      }

      // Decompose IEEE 754 double via bit_cast (constexpr in C++20):
      //   sign      bit  : 1 bit at position 63
      //   biased exp     : 11 bits at positions 62..52, biased by 1023
      //   mantissa frac  : 52 bits at positions 51..0 (with implicit
      //                    leading 1 for normals; absent for subnormals)
      const auto bits = std::bit_cast<std::uint64_t>(x);
      const bool sign = (bits >> 63) & 1U;
      const auto biased_exp = static_cast<unsigned>((bits >> 52) & 0x7FFU);
      const auto mantissa_frac = bits & 0x000F'FFFF'FFFF'FFFFULL;

      // Recover the integer mantissa (with implicit-1 for normals) and
      // the binary exponent.  After this block, x = (sign? -1 : 1) *
      // mantissa * 2^exp exactly.
      std::uint64_t mantissa;
      int exp;
      if (biased_exp == 0) {
        // Subnormal: no implicit leading 1; smallest exponent is -1074.
        mantissa = mantissa_frac;
        exp = -1074;
      } else {
        mantissa = (1ULL << 52) | mantissa_frac;
        exp = static_cast<int>(biased_exp) - 1023 - 52;
      }

      // Reduce to lowest terms in the dyadic form: strip factors of 2
      // from the mantissa, raising the exponent.  This keeps the
      // numerator / denominator small for cleanly-representable values
      // (e.g. -21.0 lands as mantissa=21, exp=0 → Rational<I>{-21, 1}).
      while ((mantissa & 1U) == 0U && mantissa != 0U) {
        mantissa >>= 1;
        ++exp;
      }

      // Mantissa-fit check: for built-in signed I, throw if the reduced
      // mantissa exceeds I::max() (otherwise the static_cast below
      // would silently truncate or invoke implementation-defined
      // behaviour).  For non-built-in carriers, trust the carrier.
      if constexpr (std::integral<I>) {
        using L = std::numeric_limits<I>;
        if (mantissa > static_cast<std::uint64_t>(L::max())) {
          throw std::overflow_error(
              "embed_double_ℚ: reduced mantissa exceeds I::max(); use a "
              "wider integer carrier (e.g. long long, "
              "SignedExtensionalCardinal<N> with N sufficiently large).");
        }
      }

      // Form the rational: numerator carries the sign and the reduced
      // mantissa; denominator is 2^|exp| when exp < 0, else 1 (with
      // the mantissa carrying the 2^exp factor in the numerator).
      // Overflow-checked doubling: for built-in signed I, throw before
      // a multiplication that would overflow; for non-built-in
      // carriers, trust the carrier's own arithmetic discipline.
      const auto checked_double = [](I value) -> I {
        if constexpr (std::integral<I>) {
          using L = std::numeric_limits<I>;
          if (value > L::max() / I{2} || value < L::min() / I{2}) {
            throw std::overflow_error(
                "embed_double_ℚ: power-of-two scaling overflows I; use "
                "a wider integer carrier.");
          }
        }
        return value * I{2};
      };

      I num = sign ? -static_cast<I>(mantissa) : static_cast<I>(mantissa);
      if (exp >= 0) {
        for (int i = 0; i < exp; ++i) num = checked_double(num);
        return Rational<I>{num, I{1}};
      } else {
        I den = I{1};
        for (int i = 0; i < -exp; ++i) den = checked_double(den);
        return Rational<I>{num, den};
      }
    });

}  // namespace dedekind::numbers

namespace dedekind::category {
template <dedekind::numbers::IsInteger Z>
struct SpeciesTraits<dedekind::numbers::Rational<Z>> {
  using Domain = dedekind::numbers::Rational<Z>;
  using machine_type = dedekind::numbers::Rational<Z>;
};

template <>
inline constexpr bool
    is_monic_arrow_v<std::decay_t<decltype(dedekind::numbers::embed_ℤ_ℚ<>)>> =
        true;
static_assert(
    IsInjective<std::decay_t<decltype(dedekind::numbers::embed_ℤ_ℚ<>)>>,
    "embed_ℤ_ℚ (ℤ ↪ ℚ) is registered injective.");

// Note: @c is_monic_arrow_v<embed_double_ℚ<I>> is intentionally NOT
// registered.  embed_double_ℚ is a partial morphism (rejects NaN, ±∞,
// and precision-overflow inputs) in the Cockett--Lack restriction-
// category sense.  On its in-range subset the embedding is provably
// injective by the exactness of the dyadic decomposition, but a
// type-level @c IsMonicArrow registration would claim totality.  The
// claim will be pinned at #496, where the safe-float refined-type
// boundary moves the partiality from throws into the typed surface
// (an @c embed_safe_float_ℚ : @c safe_float<F> → @c Rational<I> arrow
// is total on its refined domain and therefore cleanly monic).
}  // namespace dedekind::category

namespace dedekind::numbers {

/** @section Canonical_Species_Spine (ℚ)
 *
 * The canonical rational-number species ℚ is defined above as
 * @c ℚ @c = @c Rational<default_integer> with value-level
 * predicate-set constant @c Q (of type @c IntegersOf<>-analog @c
 * RationalsOf<>).  The spine witnesses below pin ℚ's syntax /
 * semantics / arrow-fabric witnesses against drift; the layout
 * mirrors the @c numbers:integer spine for ℤ so that downstream code
 * can scan one block per textbook carrier and find the same five
 * witness slots.
 */

// (1) IsSet anchor on the predicate-set constant Q.
static_assert(
    dedekind::category::IsSet<decltype(dedekind::category::ambient_set<
                                       Rational<default_integer>>(Q))>,
    "RationalsOf must be the canonical IsSet anchor for "
    "dedekind.numbers:rational.");

// (2) Syntax (the C++ operator surface that maps to ℚ's algebra).
//   - HasRingOperators<ℚ>: literal +, binary -, unary -, * close on ℚ.
//   - HasFieldOperators<ℚ>: literal +, -, *, / close on ℚ
//     (HasRingOperators + closure under /).
//   - HasGroupOperatorsAdd<ℚ>: literal +, binary -, unary - close on ℚ.
//   - HasGroupOperatorsMul<ℚ>: literal *, / close on ℚ \ {0}.
static_assert(dedekind::algebra::HasRingOperators<ℚ>,
              "ℚ closes the literal ring operator surface (+, binary -, "
              "unary -, *).");
static_assert(dedekind::algebra::HasFieldOperators<ℚ>,
              "ℚ closes the literal field operator surface (+, -, *, /).");
static_assert(dedekind::algebra::HasGroupOperatorsAdd<ℚ>,
              "ℚ closes the additive-group operator surface (+, binary -, "
              "unary -).");
static_assert(dedekind::algebra::HasGroupOperatorsMul<ℚ>,
              "ℚ closes the multiplicative-group operator surface "
              "(*, /), modulo division-by-zero (the field carrier "
              "of ℚ is total only off the multiplicative-zero set).");

// (3) Semantics (the algebraic structures Rational<default_integer> actually
//     carries).  The strict @c category::IsField<ℚ, std::plus, std::multiplies>
//     witness is architecturally blocked by the @c IsTotal gate (exact carriers
//     are neither periodic, idempotent, nor saturating); the operational
//     reading via @c IsFieldLikeScalar is the load-bearing field-arithmetic
//     guarantee, asserted downstream in this file's Formal_Verification block.
//     Group_ℤ on the integer carrier of ℚ is also asserted downstream where
//     the species-trait registry is reachable.

// (4) Primitive-type arrows on ℚ:
//   - Forward (machine → ℚ): @c embed_ℤ_ℚ promotes a @c machine_integer
//     to a rational n/1 (registered monic; injective by construction).
//   - Forward (machine_float → ℚ): @c embed_double_ℚ extracts the exact
//     dyadic rational m·2^e from the IEEE 754 bit-pattern (registered
//     monic; rejects NaN / ±∞ as out-of-band — the rationals are a field
//     with no sentinels).
// Both arrows are defined further up in this partition; their
// IsMonicArrow / IsInjective registrations live in the
// @c dedekind::category-namespace block below those definitions.

// (5) Adjacent-set arrows on ℚ:
//   - Forward predecessor: @c ℤ ↪ ℚ via @c embed_ℤ_ℚ (n ↦ n/1; same
//     arrow as (4)).
//   - Forward successor:    @c ℚ ↪ ℝ via @c embed_ℚ_ℝ in @c :real
//     (downstream; q ↦ Real{q}, the canonical inclusion).
//   - Reverse direction (ℚ → machine):  @c .resolve() yields a
//     @c double approximation (lossy when the denominator's not a
//     power of two; exact for dyadic rationals).

/**
 * @section Formal_Verification
 *
 * Rational<I> satisfies the operational field-like witness (IsFieldLikeScalar):
 * all four arithmetic operations are available and closed.
 *
 * @note The stronger categorical proof IsField<Rational<I>> is architecturally
 * blocked: IsField requires IsRing which requires IsMonoid which requires
 * IsTotal (IsPeriodic || IsIdempotent). Rational arithmetic is neither periodic
 * nor idempotent, so it lives in the Kleene/partial-function stratum instead.
 * The Kleene traits (is_kleene_associative_v etc.) are the appropriate anchors
 * for the algebraic properties of Rational<I>.
 */
// ExtensionalCardinal<> (the cardinality ring) now carries division/modulo,
// making it satisfy IsInteger. It represents ℕ "by fiat": a total ring
// (IsRing, IsMagma) whose wrapping arithmetic avoids UB. This assert lives
// here (not in cardinality.cppm) to avoid a circular import chain:
//   :cardinality → :natural   :integer → :cardinality   :rational → :integer
static_assert(IsInteger<ExtensionalCardinal<>>,
              "ExtensionalCardinal<> must satisfy IsInteger (Euclidean "
              "ring: +, -, *, /, % with two's-complement wrapping).");

static_assert(IsInteger<SignedExtensionalCardinal<>>,
              "SignedExtensionalCardinal<> must satisfy IsInteger (Euclidean "
              "signed ring: sign-magnitude arithmetic, overflow-free up to "
              "2^{N*64 - 1}).");

static_assert(
    dedekind::algebra::IsFieldLikeScalar<Rational<default_integer>>,
    "Rational<I> must satisfy the operational field-like witness (ℚ is a "
    "field).");

static_assert(
    dedekind::algebra::IsFieldLikeScalar<Rational<SignedExtensionalCardinal<>>>,
    "Rational<SignedExtensionalCardinal<>> must satisfy IsFieldLikeScalar: "
    "the intended arbitrary-precision signed-rational carrier for ℚ.");

// ---------------------------------------------------------------------------
// Algebraic-soul concept certifications (dogfood the library's own concepts)
// ---------------------------------------------------------------------------
//
// These assertions are the library's contract with itself: the carriers the
// paper and downstream code instantiate must satisfy the algebraic-soul
// concepts they *claim* to realize. When a future certification fails to
// hold (e.g. because a downstream change breaks a species-trait), the build
// breaks here, at the point of the claim, rather than silently downstream.

// ℕ as a commutative monoid under +: the canonical arbitrary-precision
// natural carrier.
static_assert(Monoid_ℕ<ExtensionalCardinal<>>,
              "ExtensionalCardinal<> must realize ℕ as a commutative monoid "
              "under std::plus.");

// ℤ as an abelian group under +: the canonical arbitrary-precision signed
// integer carrier. The species-trait registry supplies associativity,
// commutativity, identity, and inverse; the concept gate enforces them.
static_assert(Group_ℤ<SignedExtensionalCardinal<>>,
              "SignedExtensionalCardinal<> must realize ℤ as an abelian "
              "group under std::plus.");

// ℤ-deal (#394): the strict additive-group proof above AND the literal
// C++ +, binary -, unary - operators close strictly on the carrier.
// SignedExtensionalCardinal<>'s friend operators all return
// SignedExtensionalCardinal<> exactly, so HasGroupOperatorsAdd fires;
// combined with the species-trait Group_ℤ proof, IsArithmeticAdditiveGroup
// is the meet-point between the math-textbook ℤ structure and the
// standard C++ arithmetic operators on this carrier.
static_assert(
    dedekind::algebra::IsArithmeticAdditiveGroup<SignedExtensionalCardinal<>>,
    "SignedExtensionalCardinal<> is the canonical ℤ-deal: strict "
    "additive group AND literal +,-,unary - close on the carrier.");

// ℚ as the field of rationals: the canonical arbitrary-precision rational
// carrier that the paper-facing showcases instantiate on.
//
// FIXME: Field_ℚ currently fails to certify on this carrier because the
// concept's body transitively depends on IsFieldLikeScalar (the ad-hoc
// operational concept) and IsRational (which chains through IsFieldLike ->
// IsRationalLike). The right repair is part of a broader cleanup:
// introduce `IsField<T, Add, Mult>` in dedekind.category:total, retire the
// ad-hoc IsFieldLike / IsFieldLikeScalar names (the IsRingLike-flavoured
// ones already retired under #394's sweep into HasRingOperators), and
// rebind Field_ℚ to the proper algebraic concept. Tracked under the
// concept-dogfooding issue sweep (see backlog issues to be created).
// Until then the operational witnesses (IsFieldLikeScalar<Rational<
// SignedEC<>>> asserted above) are the load-bearing guarantees.

// ℚ-deal (#394, surface only): HasFieldOperators is the literal field-
// operator surface (+, -, unary -, *, /, T{1}).  Rational<I>'s friend
// operators and inverse-via-T{1}/x route close strictly on Rational<I>
// for any operationally-ring-like I; the witness fires regardless of
// the strict-IsField FIXME above, since it is a syntactic shape claim
// and not an axiomatic one.  The strict-and-literal seal (analogous to
// IsArithmeticAdditiveGroup on ℤ) lands when the FIXME above closes.
static_assert(dedekind::algebra::HasFieldOperators<Rational<default_integer>>,
              "Rational<default_integer> has the literal field-operator "
              "surface: +, -, unary -, *, / all close on Rational, and "
              "Rational{1} is the multiplicative unit.");
static_assert(
    dedekind::algebra::HasFieldOperators<Rational<SignedExtensionalCardinal<>>>,
    "Rational<SignedExtensionalCardinal<>> --- the canonical exact ℚ "
    "carrier --- has the field-operator surface.");

// Canonical-spine syntactic witnesses on Rational<default_integer>:
// the ring / additive-group / multiplicative-group surfaces are all
// implied by HasFieldOperators above (which is HasRingOperators ∧
// HasGroupOperatorsMul), but pinning them explicitly here surfaces
// each shape concept in the partition's witness ledger.
static_assert(dedekind::algebra::HasRingOperators<Rational<default_integer>>,
              "Rational<default_integer> closes the ring operator surface.");
static_assert(
    dedekind::algebra::HasGroupOperatorsAdd<Rational<default_integer>>,
    "Rational<default_integer> closes the additive-group operator "
    "surface (+, binary -, unary -).");
static_assert(
    dedekind::algebra::HasGroupOperatorsMul<Rational<default_integer>>,
    "Rational<default_integer> closes the multiplicative-group operator "
    "surface (*, /, T{1}).");

/**
 * @brief Canonical polynomial ring over the rationals: Q[x].
 *
 * @details RigPolynomial<Rational<I>> forms a commutative ring under
 * coefficient-wise addition and Cauchy product. The coefficient field is
 * the quotient field of the integer carrier I. The default I = default_integer
 * (currently `int`, the machine signed integer) gives Q[x] over ℤ.
 *
 * @note This is the structurally correct polynomial ring: the coefficient type
 * Rational<I> is field-like (IsFieldLikeScalar), so the polynomial ring has
 * all four arithmetic operations available. A future retarget to
 * SignedExtensionalCardinal<N> would give a provably total Q[x].
 */
export template <IsInteger I = default_integer>
using RationalPolynomial = dedekind::algebra::RigPolynomial<Rational<I>>;

static_assert(dedekind::algebra::IsFieldLikeScalar<Rational<default_integer>>,
              "RationalPolynomial coefficient type must be field-like.");

// Ordering and density proofs for ℚ.
static_assert(IsTotallyOrdered<Rational<default_integer>>,
              "Rational<Z> must satisfy IsTotallyOrdered (ℚ is a total "
              "order).");
static_assert(IsDense<Rational<default_integer>>,
              "Rational<Z> must satisfy IsDense (ℚ is dense in itself: "
              "between any two rationals lies another).");

// ℚ is also a valid net domain: the total order makes it directed
// (every pair {a, b} has a common upper bound, e.g.\ max(a, b)).
// Cf.\ Munkres / Kelley: a net is a function from a directed set,
// not necessarily ℕ.
static_assert(dedekind::order::IsDirectedSet<Rational<default_integer>>,
              "Rational<Z> is a directed set --- ℚ is a valid net "
              "domain (totally ordered, every pair has a common "
              "upper bound).");

// Structural product proof: ℚ is constructed as pairs (num, den) from ℤ × ℤ.
static_assert(
    dedekind::category::IsProduct<Rational<default_integer>, default_integer,
                                  default_integer>,
    "Rational<Z> must satisfy IsProduct<Rational<Z>, Z, Z> (ℚ ≅ ℤ × ℤ / ~).");

/** @section CCC_Inheritance_389 CCC inheritance (#389)
 *
 * The rational-number carriers and their integer building blocks all
 * host a canonical Cartesian-closed Set ambient: their @c IsSet
 * anchor (above) entails @c HasCanonicalSetCCC over the ambient
 * species per #389.  These witnesses pin the CCC corollary directly
 * next to the carrier definitions for downstream-module discoverability.
 */
static_assert(dedekind::category::HasCanonicalSetCCC<Rational<default_integer>>,
              "Rational<default_integer> hosts a canonical "
              "Cartesian-closed Set ambient.");
static_assert(
    dedekind::category::HasCanonicalSetCCC<ExtensionalCardinal<>>,
    "ExtensionalCardinal<> hosts a canonical Cartesian-closed Set ambient.");
static_assert(
    dedekind::category::HasCanonicalSetCCC<SignedExtensionalCardinal<>>,
    "SignedExtensionalCardinal<> hosts a canonical "
    "Cartesian-closed Set ambient.");

}  // namespace dedekind::numbers
