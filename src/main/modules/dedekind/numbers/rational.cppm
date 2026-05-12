/**
 * @file dedekind/numbers/rational.cppm
 * @partition :rational
 * @brief ℚ — rationals as numerator/denominator pairs.
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
#include <type_traits>  // std::remove_cvref_t for the post-#559 universe-witness static_asserts
#include <utility>

export module dedekind.numbers:rational;

import dedekind.algebra;
import dedekind.category;
import dedekind.order;
import dedekind.sets;
import :integer;
import :sint;  // IsInteger / default_integer (relocated from :integer per #670)

namespace dedekind::numbers {
using namespace dedekind::category;
using namespace dedekind::order;
using namespace dedekind::sets;

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
   *  Implicit to allow @c Rational<Z>{Z_value} to flow into the
   *  @c (a @c + @c b) @c / @c T{2} midpoint expression that the
   *  @c IsDense concept (in @c order:completeness) requires.
   *  When @c T = @c Rational<Z>, @c T{2} resolves through the
   *  @c Rational(std::integral) overload below (@c int → @c Rational<Z>
   *  in one UDC), bypassing the two-UDC chain
   *  @c int @c → @c Z @c → @c Rational<Z> that C++ would otherwise
   *  reject.  This @c Rational(Z) form remains the canonical embedding
   *  when the caller already has a @c Z value in hand. */
  constexpr Rational(Z n)
      : first(n), second(Z{1}) {}  // NOLINT(google-explicit-constructor)

  /** @brief Embedding of any standard integral as a rational n/1, in @b one
   *  user-defined conversion.
   *
   *  @details Without this constructor, building a @c Rational<Z> from an
   *  @c int literal would require two UDCs (@c int → @c Z → @c Rational<Z>),
   *  which C++ does not permit in implicit-conversion chains.  This template
   *  collapses both steps into a single UDC, enabling:
   *
   *    * @c Real<Rational<Z>>{1} for the @c HasGroupOperatorsMul
   *      @c T{1} witness on @c ExactReal<>;
   *    * @c Rational<Z>{2} for the @c (a @c + @c b) @c / @c T{2}
   *      midpoint expression in the @c IsDense concept (the @c T{2}
   *      reformulation, landed under this PR, replaces the previous
   *      bare-int @c 2 so the carrier is in control of its own
   *      value-2);
   *    * generally, any context where an @c int literal lifts into
   *      @c Rational<Z> across one UDC.
   *
   *  Canonical example post-retarget: @c Z @c = @c
   *  SignedExtensionalCardinal<>, whose @c S → @c SEC<> constructor
   *  is itself a one-UDC step.  Required for the
   *  @c HasGroupOperatorsMul / @c IsDense witnesses to fire on
   *  @c Real<Rational<default_integer>> (#499 / default_integer
   *  retarget).
   */
  template <std::integral S>
    requires(!std::same_as<S, Z>)
  constexpr Rational(S n)  // NOLINT(google-explicit-constructor)
      : first(Z{n}), second(Z{1}) {}

  /** @section rational__The_Simplification_Morphism */
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

  /** @section rational__Relational_Morphisms */

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

  /** @section rational__Multiplicative_Inverse: The Reciprocal */
  constexpr Rational inverse() const {
    if (first == Z{0}) {
      throw std::domain_error("Rational: Zero has no multiplicative inverse.");
    }
    // To invert (a/b), we return (b/a)
    return Rational(second, first);
  }

  /** @section rational__Field_Operators */

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

/** @section rational__Partial_Arithmetic_with_Ternary_Logic */

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
 * @section rational__Kleene_Traits_in_Category_Namespace
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

/** @section rational__Formal_Verification */

// Proof: The inverse of 2/3 is 3/2.
static_assert((Rational<default_integer>(default_integer{2}, default_integer{3})
                   .inverse()
                   .num() == default_integer{3}));

// Functor identification: Rational<I> = Frac(I).  The Frac functor
// (field-of-fractions; left adjoint to the inclusion / forgetful functor
// Fields → IntegralDomains; cf. Lang §II.4) takes an integral domain
// I to its quotient field; for any IsInteger I, Rational<I> IS that
// quotient field.  Pinning this mechanically aligns the paper §2
// prose ("Named functors that build the library's carriers") with the
// source: the IntegerCarrier alias is the source-side projection of
// the Frac functor, so any consumer can read off "what does this
// rational sit over?" by inspecting Rational<I>::IntegerCarrier.
//
// FIXME(#498/NEW-A): IntegerCarrier is the ad-hoc source-projection
// alias on Rational<I>; sibling carriers use ScalarCarrier
// (Real<Q>, Complex<R>) and value_type (Dual<F>, IsTangentBundle).
// The dedekind.category:functor partition meanwhile establishes the
// canonical convention --- Σ_cat / Τ_cat for source/target category
// and Shape<U> for the on-objects action (cf. maybe_functor<T> at
// src/main/modules/dedekind/category/functor.cppm).  Unifying the carrier-side
// projections with the :functor convention is NEW-A trait-registry
// work and tracked under #498; this static_assert is the smallest
// honest pinning available today.
static_assert(std::same_as<typename Rational<default_integer>::IntegerCarrier,
                           default_integer>,
              "Rational<I> is the Frac-functor image of I; the "
              "IntegerCarrier alias names I mechanically.");

}  // namespace dedekind::numbers

// ---------------------------------------------------------------------------
// Quotient-algebra registration for Rational<I> (#498/#499 NEW-A).
//
// Rational<I> = Frac(I) is a quotient construction over the integer
// carrier I (the equivalence collapses (a, b) ~ (c, d) iff a*d = c*b).
// The single declaration below — `quotient_algebra_base<Rational<I>>::type
// = I` — fires the structural-trait propagation through
// `dedekind.algebra:quotient`: associativity, commutativity,
// distributivity, and the full IsTotal disjunction (periodic /
// idempotent / saturating) all lift from I to Rational<I> uniformly.
// The carrier-specific bits (additive identity 0/1, additive inverse
// via -q) remain explicit specialisations of identity_trait /
// inverse_trait below.
// ---------------------------------------------------------------------------

namespace dedekind::category {

template <dedekind::numbers::IsInteger I>
struct quotient_algebra_base<dedekind::numbers::Rational<I>> {
  using type = I;
};

// Carrier-specific identity values (0/1 → 0/1 for plus, 1/1 for multiplies):
// these don't propagate trivially because the quotient witness has its
// own constructor shape.
template <dedekind::numbers::IsInteger I>
struct identity_trait<dedekind::numbers::Rational<I>,
                      std::plus<dedekind::numbers::Rational<I>>> {
  using value_type = dedekind::numbers::Rational<I>;
  static constexpr value_type value = value_type{I{0}, I{1}};
};

template <dedekind::numbers::IsInteger I>
struct identity_trait<dedekind::numbers::Rational<I>,
                      std::multiplies<dedekind::numbers::Rational<I>>> {
  using value_type = dedekind::numbers::Rational<I>;
  static constexpr value_type value = value_type{I{1}, I{1}};
};

// Carrier-specific additive inverse: the defining ℚ trait — every
// rational has an additive inverse via numerator-negation.  This too
// is carrier-specific (the construction depends on Rational's layout).
template <dedekind::numbers::IsInteger I>
inline constexpr bool is_invertible_v<
    dedekind::numbers::Rational<I>, std::plus<dedekind::numbers::Rational<I>>> =
    true;

template <dedekind::numbers::IsInteger I>
struct inverse_trait<dedekind::numbers::Rational<I>,
                     std::plus<dedekind::numbers::Rational<I>>> {
  static constexpr bool exists = true;
  using value_type = dedekind::numbers::Rational<I>;
  static constexpr value_type compute(
      const dedekind::numbers::Rational<I>& q) noexcept {
    return -q;
  }
};

// Carrier-specific multiplicative inverse: the field-defining ℚ trait
// --- every @b non-zero rational has a multiplicative inverse via
// numerator/denominator swap (@c q.inverse()).  Zero is excluded by
// the @c is_invertible_v<T, Mult> convention pinned in
// @c total.cppm:486-491 ("the strict mathematical definition --- every
// non-zero element is multiplicatively invertible --- cannot be stated
// in a C++ concept; a carrier opts into the multiplicative-group
// witness by specialising @c is_invertible_v / @c inverse_trait to
// assert the field-level claim; zero is understood excluded").  Same
// mechanism the @c :algebra:field witness for @c bool / 𝔽₂ already
// uses (@c is_invertible_v<bool, std::bit_and> @c = @c true).  With
// this registration in place, @c IsAbelianGroup<Rational<I>,
// std::multiplies> --- and consequently @c category::IsField and
// @c algebra::IsField --- fire on ℚ.
template <dedekind::numbers::IsInteger I>
inline constexpr bool
    is_invertible_v<dedekind::numbers::Rational<I>,
                    std::multiplies<dedekind::numbers::Rational<I>>> = true;

template <dedekind::numbers::IsInteger I>
struct inverse_trait<dedekind::numbers::Rational<I>,
                     std::multiplies<dedekind::numbers::Rational<I>>> {
  static constexpr bool exists = true;
  using value_type = dedekind::numbers::Rational<I>;
  static constexpr value_type compute(
      const dedekind::numbers::Rational<I>& q) noexcept {
    return q.inverse();
  }
};

}  // namespace dedekind::category

namespace dedekind::numbers {

// NEW-A trait registry witness (#498/#499): @c Rational<I> is a
// module over its @c IntegerCarrier @c I.  Textbook reading: ℚ is a
// ℤ-module (and ℚ is the field of fractions @c Frac(ℤ); the Frac
// construction preserves the source-side scalar action).  The trait
// itself is concept-based (in @c dedekind::algebra:modules); the
// witness fires through the species-trait specialisations above.
static_assert(
    dedekind::algebra::is_module_v<Rational<default_integer>, default_integer>,
    "Rational<I> is a module over its IntegerCarrier I.");

/** @brief The canonical rational-number universe ℚ =
 * Ω<Rational<default_integer>> (post-#559).
 *
 *  @details Per #559's chosen direction (option A): the named species
 *  symbols (@c 𝔹 / @c ℕ / @c ℤ / @c ℚ / @c ℝ / @c ℂ / @c 𝔻) denote the
 *  @b universe values (constexpr instances of @c UniversalSet over the
 *  carrier), not carrier @b types.  Carrier types are spelled directly
 *  (@c Rational<default_integer>, or @c Rational<I> for the
 *  parameterised form over an arbitrary @c IsInteger I) in
 *  template-type-parameter positions; @c ℚ alone is the universe
 *  value the set-builder DSL takes as ambient.
 *
 *  This makes @c element<ℚ> the canonical scout spelling — closer to
 *  textbook math notation than the pre-#559 @c element<Ω<ℚ>> form.
 *  Pre-#559 the spelling was @c using @c ℚ @c = @c ℚ_t<> (a carrier-
 *  type alias); the type-context sites in concept gates and
 *  static_asserts were migrated to @c Rational<default_integer>
 *  directly in step 1 of this slice.
 *
 *  @note  Post-ℚ-retarget (#673) the ring ladder closes via the
 *  saturating @c IsTotal path inherited from @c default_integer @c =
 *  @c SignedCardinality, and the multiplicative-inverse trait
 *  registration in this file lifts @c Rational<I> to the textbook
 *  field axiom (Lang, @em Algebra §III.1: a commutative ring whose
 *  @b non-zero elements form a multiplicative group; zero excluded by
 *  the @c is_invertible_v / @c inverse_trait convention pinned in
 *  @c total.cppm).  Both @c category::IsField and the operator-bearing
 *  @c algebra::IsField fire on @c Rational<default_integer> --- see
 *  the axiomatic probes near the bottom of this file.
 */
export inline constexpr UniversalSet<Rational<default_integer>, ClassicalLogic,
                                     ℵ_0>
    ℚ = dedekind::sets::Ω<Rational<default_integer>>;

static_assert(std::same_as<std::remove_cvref_t<decltype(ℚ)>,
                           dedekind::sets::UniversalSet<
                               Rational<default_integer>, ClassicalLogic, ℵ_0>>,
              "ℚ is the universe Ω<Rational<default_integer>> (post-#559).");
static_assert(
    std::same_as<typename std::remove_cvref_t<decltype(ℚ)>::Domain,
                 Rational<default_integer>>,
    "ℚ's underlying carrier IS Rational<default_integer> — the textbook "
    "universe-over-carrier reading.");

// #573 slice 4 pilot witness: ℚ walks SetAsProduct.  The universe value
// is a UniversalSet over Rational<default_integer> --- its Ambient is the
// underlying species, its χ is the always-True classifier.  Names the
// (Underlying, Classifier) product reading of the ambient rational set
// concretely on the canonical pilot carrier.
static_assert(
    dedekind::category::SetAsProduct<std::remove_cvref_t<decltype(ℚ)>,
                                     Rational<default_integer>,
                                     std::remove_cvref_t<decltype(ℚ.χ)>>,
    "#573 slice 4: ℚ must witness SetAsProduct over "
    "(Rational<default_integer>, "
    "χ-type).  The universe value is a UniversalSet whose Ambient matches the "
    "rational carrier and whose χ is recoverable as a typed member.");

// `Q` constant + `RationalSet` alias removed under ℚ-retarget
// chiselling --- callers spell @c element<ℚ> directly off the universe
// value @c ℚ above.

// `embed_ℤ_ℚ` machine-layer arrow removed: its source type
// @c machine_integer (= @c int) was the deprecated machine-layer
// integer carrier.  Callers that need ℤ ↪ ℚ realisation should
// construct @c Rational<I>{n, I{1}} directly at the call site.
//
// Pragmatic alias kept for downstream callers that still reach for it:
export using machine_integer = int;

// `Frac` set-level lift removed under ℚ-retarget chiselling: depended
// transitively on @c embed_ℤ_ℚ (removed above).  The carrier-lattice
// functor zoo (@c Frac / @c Cplx / @c Dual / ...) at the set level
// remains a valid target — the next iteration will restore @c Frac
// on the new @c ℤ carrier (@c SignedCardinality) via @c embed_sint_ℤ
// rather than the deprecated machine-layer arrow.

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
 * @section rational__Precision
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
 * @section rational__Honesty_Obligation
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

/** @section rational__Quotient_Construction (#567)
 *
 * The textbook construction ℚ = (ℤ × ℤ_≠0) / ~ is observable in code
 * via the @c quotient operator from @c sets:quotient (see #567).  The
 * equivalence relation is cross-multiplication: @c (a, @c b) @c ~ @c
 * (c, @c d) iff @c a*d @c == @c b*c.  The carrier inhabiting the
 * quotient is @c Rational<I>; this is registered via a specialisation
 * of @c quotient_carrier in the @c dedekind::sets namespace below.
 *
 * The structural claim the artefact delivers is the @b type identity:
 *   @c decltype(quotient(pairs, CrossMultEquiv<I>{}))::Domain @c ==
 *   @c Rational<I>
 * --- the textbook construction's Domain is the carrier we already use
 * for ℚ.  No new carrier is introduced; the existing @c Rational<I>
 * provides canonical-representative semantics (@c simplify normalises
 * via gcd; equality cross-multiplies --- exactly what the equivalence
 * relation specifies).
 *
 * @section rational__CrossMultEquiv_Total_Closure
 * The textbook relation @c a*d @c == @c b*c is an equivalence relation
 * on @c ℤ @c × @c ℤ_≠0 but @b not on the full @c std::pair<I, I> domain
 * once zero second components are admitted: e.g.\ @c (1, @c 0) @c ~ @c
 * (0, @c 0) and @c (0, @c 0) @c ~ @c (0, @c 1) but @c (1, @c 0) @c ≁
 * @c (0, @c 1).  The trait registry that powers
 * @c IsEquivalenceRelation is keyed on the relation type alone (not on
 * a sub-domain), so for the trait to be honestly assertable on the
 * full pair domain we close the relation to be the identity outside
 * @c ℤ_≠0 on either side: pairs with a zero second component are only
 * equivalent to themselves.  This preserves the textbook semantics on
 * the intended subdomain (the @c quotient operator's @c Pairs argument
 * is the @c cartesian_product with denominators filtered to non-zero,
 * so the additional clause never fires in production use) while making
 * the trait registration totally transitive — the engineer's honesty
 * obligation lands.
 */
export template <IsInteger I = default_integer>
struct CrossMultEquiv {
  template <typename Pair>
  constexpr bool operator()(const Pair& p, const Pair& q) const {
    if (p.second == I{0} || q.second == I{0}) {
      return p.first == q.first && p.second == q.second;
    }
    return p.first * q.second == p.second * q.first;
  }
};

}  // namespace dedekind::numbers

// Register CrossMultEquiv<I> with the upstream @c category:cartesian
// equivalence-relation trait surface.  With the identity-closure on
// zero-second-component pairs (see CrossMultEquiv_Total_Closure
// above), the relation is reflexive (a*b == b*a; identity on zero-
// denom pairs), symmetric (cross-mult is symmetric under pair swap;
// identity is symmetric), and transitive on the full std::pair<I, I>
// domain (on the nonzero-denom subdomain via standard cross-mult
// transitivity; outside it via the identity branch).  Opting into all
// three traits lets @c IsEquivalenceRelation<CrossMultEquiv<I>,
// std::pair<I, I>> fire honestly on the full domain, which is what
// the @c quotient operator's requires-clause consumes.
namespace dedekind::category {
template <dedekind::numbers::IsInteger I>
inline constexpr bool
    is_reflexive_relation_v<dedekind::numbers::CrossMultEquiv<I>> = true;
template <dedekind::numbers::IsInteger I>
inline constexpr bool
    is_symmetric_relation_v<dedekind::numbers::CrossMultEquiv<I>> = true;
template <dedekind::numbers::IsInteger I>
inline constexpr bool
    is_transitive_relation_v<dedekind::numbers::CrossMultEquiv<I>> = true;
}  // namespace dedekind::category

// Register the ℚ quotient_carrier specialisation.  This lives in the
// @c dedekind::sets namespace where the primary template was declared;
// the specialisation references types from @c dedekind::numbers, which
// is fine.
namespace dedekind::sets {
export template <dedekind::numbers::IsInteger I>
struct quotient_carrier<std::pair<I, I>, dedekind::numbers::CrossMultEquiv<I>> {
  using type = dedekind::numbers::Rational<I>;
};
}  // namespace dedekind::sets

// Static-assert exhibits: the trait-registration above lets the upstream
// IsBinaryRelation / IsEquivalenceRelation concepts fire on CrossMultEquiv,
// which is the contract the @c sets:quotient operator's requires-clause
// consumes.
namespace dedekind::numbers {
static_assert(
    dedekind::category::IsBinaryRelation<
        CrossMultEquiv<default_integer>,
        std::pair<default_integer, default_integer>,
        std::pair<default_integer, default_integer>>,
    "CrossMultEquiv is a binary relation (callable on pairs, returns bool).");
static_assert(
    dedekind::category::IsEquivalenceRelation<
        CrossMultEquiv<default_integer>,
        std::pair<default_integer, default_integer>>,
    "CrossMultEquiv is an equivalence relation (reflexive + symmetric + "
    "transitive) on pairs of default_integer — the upstream axiom the "
    "sets:quotient operator's requires-clause consumes.");
}  // namespace dedekind::numbers

namespace dedekind::category {
template <dedekind::numbers::IsInteger Z>
struct SpeciesTraits<dedekind::numbers::Rational<Z>> {
  using Domain = dedekind::numbers::Rational<Z>;
  using machine_type = dedekind::numbers::Rational<Z>;
};

// embed_ℤ_ℚ monicity / IsEmbeddingFunctor / is_homomorphism_v
// registrations removed under ℚ-retarget chiselling — the arrow
// itself was removed (machine-layer ℕ → ℤ scaffolding).  Restoring
// on the new ℤ carrier (SignedCardinality) lands as a follow-up.

}  // namespace dedekind::category

namespace dedekind::category {

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

/** @section rational__Canonical_Species_Spine (ℚ)
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

// (1) IsSet anchor on the predicate-set constant Q --- removed under
// ℚ-retarget chiselling: the @c Q constant + @c RationalsOf were
// removed.  IsSet for ℚ is witnessed via the universe value ℚ above.

// (2) Syntax (the C++ operator surface that maps to ℚ's algebra).  Post-
// #559, ℚ is the universe value Ω<Rational<default_integer>>; the carrier
// in concept-gate type-parameter slots is Rational<default_integer>
// directly.
//   - HasRingOperators<Rational<default_integer>>: literal +, binary -,
//     unary -, * close on the carrier.
//   - HasFieldOperators<Rational<default_integer>>: literal +, -, *, /
//     close on the carrier (HasRingOperators + closure under /).
//   - HasGroupOperatorsAdd<Rational<default_integer>>: literal +, binary -,
//     unary - close on the carrier.
//   - HasGroupOperatorsMul<Rational<default_integer>>: literal *, / close
//     on the carrier \ {0}.
static_assert(dedekind::algebra::HasRingOperators<Rational<default_integer>>,
              "ℚ closes the literal ring operator surface (+, binary -, "
              "unary -, *).");
static_assert(dedekind::algebra::HasFieldOperators<Rational<default_integer>>,
              "ℚ closes the literal field operator surface (+, -, *, /).");
static_assert(
    dedekind::algebra::HasGroupOperatorsAdd<Rational<default_integer>>,
    "ℚ closes the additive-group operator surface (+, binary -, "
    "unary -).");
static_assert(
    dedekind::algebra::HasGroupOperatorsMul<Rational<default_integer>>,
    "ℚ closes the multiplicative-group operator surface "
    "(*, /), modulo division-by-zero (the field carrier "
    "of ℚ is total only off the multiplicative-zero set).");

// (3) Semantics (the algebraic structures Rational<default_integer> actually
//     carries).  The strict @c category::IsField<ℚ, std::plus, std::multiplies>
//     witness is architecturally blocked by the @c IsTotal gate (exact carriers
//     are neither periodic, idempotent, nor saturating); the operational
//     reading via @c HasFieldOperators is the load-bearing
//     field-arithmetic guarantee, asserted downstream in this file's
//     Formal_Verification block. Group_ℤ on the integer carrier of ℚ is also
//     asserted downstream where the species-trait registry is reachable.

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
//   - Forward successor:    @c ℚ ↪ ℝ — the @c embed_ℚ_ℝ arrow was
//     removed under the ℚ retarget cleanup (the @c SignedCardinality
//     variant carrier deliberately does not expose machine-numeric
//     conversions).  Callers construct @c Real<S>{…} directly at the
//     call site for the lossy realisation.
//   - Reverse direction (ℚ → machine):  intentionally not a single
//     primitive on @c Rational<I>: ℚ → @c double is the realisation
//     crossing into IEEE 754 numerics, and the carrier-lattice
//     convention routes that through the ℝ-proxy so the
//     IEEE-754 friction is named at the right partition.

/**
 * @section rational__Formal_Verification_2
 *
 * Rational<I> satisfies both the operational field-like witness
 * (HasFieldOperators --- all four arithmetic operations available and
 * closed) and the axiomatic categorical claim @c IsField<Rational<I>,
 * +, *> after the multiplicative-inverse trait registration earlier
 * in this file: every @b non-zero rational has a multiplicative
 * inverse via @c q.inverse(), with zero excluded by the
 * @c is_invertible_v convention pinned in @c total.cppm.  This is the
 * textbook field factoring (Lang, @em Algebra §III.1: a field is a
 * commutative ring whose non-zero elements form a multiplicative
 * group).  The Kleene partial-function traits remain available for
 * codepaths that need explicit failure handling on division by zero
 * (@c HonestDivRational), but they are no longer the only anchor ---
 * the strict @c IsField witness is now load-bearing.
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
    dedekind::algebra::HasFieldOperators<Rational<default_integer>>,
    "Rational<I> must satisfy the operational field-like witness (ℚ is a "
    "field).");

static_assert(
    dedekind::algebra::HasFieldOperators<Rational<SignedExtensionalCardinal<>>>,
    "Rational<SignedExtensionalCardinal<>> must satisfy "
    "HasFieldOperators: "
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

// `Monoid_ℕ` static_assert removed under ℚ-retarget chiselling: the
// concept was deleted from :natural.

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
// carrier that the paper-facing showcases instantiate on.  Both the
// operational @c HasFieldOperators surface AND the axiomatic
// @c category::IsField / @c algebra::IsField close on
// @c Rational<default_integer> (see the probes below).

// ℚ-deal (#394, surface only): HasFieldOperators is the literal field-
// operator surface (+, -, unary -, *, /, T{1}).  Rational<I>'s friend
// operators and inverse-via-T{1}/x route close strictly on Rational<I>
// for any operationally-ring-like I; the witness fires regardless of
// the strict-IsField FIXME above, since it is a syntactic shape claim
// and not an axiomatic one.
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

// =========================================================================
// Axiomatic-algebra probes on Rational<default_integer> (post-ℚ-retarget).
//
// The static_asserts below pin which strict algebraic concepts fire on
// the post-retarget @c Rational<SignedCardinality> carrier.  Both the
// additive-group and multiplicative-group axioms close --- the latter
// under the @c is_invertible_v / @c inverse_trait convention pinned in
// @c total.cppm:486-491 ("the strict mathematical definition --- every
// non-zero element is multiplicatively invertible --- cannot be stated
// in a C++ concept; a carrier opts into the multiplicative-group
// witness by specialising @c is_invertible_v / @c inverse_trait to
// assert the field-level claim; zero is understood excluded").  This
// matches the textbook factoring (Lang, @em Algebra §III.1): a field
// is a commutative ring whose @b non-zero elements form a
// multiplicative group.
// =========================================================================

// (+, *) abelian group fires on both:
//   • IsAbelianGroup<ℚ, +>  — closed under + with 0/1 identity, every
//     rational has an additive inverse -q.
//   • IsAbelianGroup<ℚ, *>  — closed under * with 1/1 identity, every
//     @b non-zero rational has a multiplicative inverse via q.inverse()
//     (zero excluded by the is_invertible_v convention).
namespace probe {
template <typename Q>
inline constexpr bool ℚ_is_additive_abelian_group_v =
    dedekind::category::IsAbelianGroup<Q, std::plus<Q>>;
template <typename Q>
inline constexpr bool ℚ_is_multiplicative_abelian_group_v =
    dedekind::category::IsAbelianGroup<Q, std::multiplies<Q>>;
}  // namespace probe
static_assert(probe::ℚ_is_additive_abelian_group_v<Rational<default_integer>>,
              "ℚ under + must be an abelian group (associative, "
              "commutative, identity 0/1, inverse -q).");
static_assert(
    probe::ℚ_is_multiplicative_abelian_group_v<Rational<default_integer>>,
    "ℚ under * (with zero excluded by convention) must be an abelian "
    "group: associative, commutative, identity 1/1, inverse q.inverse().");

// IsRing fires on the saturating @c Rational<SignedCardinality>: post-
// #669 (SignedCardinality satisfying std::regular etc.) + #670 (ℤ
// retarget to SignedCardinality) + ℚ retarget, the IsTotal disjunction
// closes via the @c is_saturating path that the saturating carriers
// expose.  This RESOLVES the rational.cppm:1174 FIXME for the new ℚ
// (the FIXME was filed against the cyclic-fragment Rational<SE<>>
// carrier; the saturating retarget cleared the architectural blocker).
static_assert(
    dedekind::category::IsRing<Rational<default_integer>,
                               std::plus<Rational<default_integer>>,
                               std::multiplies<Rational<default_integer>>>,
    "Strict IsRing<ℚ, +, *> CLOSES on the saturating ℚ "
    "(post-#669/#670/ℚ-retarget): the IsTotal disjunction "
    "closes via the saturating-carrier path.");
static_assert(
    dedekind::category::IsCommutativeRing<
        Rational<default_integer>, std::plus<Rational<default_integer>>,
        std::multiplies<Rational<default_integer>>>,
    "ℚ is a commutative ring: multiplication of rationals "
    "commutes (a/b * c/d = c/d * a/b).");

// IsField fires on ℚ via the textbook factoring: (ℚ, +) is an abelian
// group, (ℚ, *) is a commutative monoid, and @b every @b non-zero
// rational has a multiplicative inverse (the @c is_invertible_v<...,
// std::multiplies> trait above, with zero excluded by the convention
// pinned in @c total.cppm:486-491 --- the same convention the @c bool /
// 𝔽₂ field witness already uses).  The axiomatic + operator-bearing
// witnesses both close.
static_assert(
    dedekind::category::IsField<Rational<default_integer>,
                                std::plus<Rational<default_integer>>,
                                std::multiplies<Rational<default_integer>>>,
    "Axiomatic IsField<ℚ, +, *> CLOSES (Lang, Algebra §III.1): "
    "commutative ring with the non-zero elements forming a "
    "multiplicative group, registered via is_invertible_v<Rational<I>, "
    "std::multiplies> = true (zero excluded by convention).");
static_assert(
    dedekind::algebra::IsField<Rational<default_integer>,
                               std::plus<Rational<default_integer>>,
                               std::multiplies<Rational<default_integer>>>,
    "Operator-bearing IsField<ℚ, +, *> CLOSES: the field axioms hold "
    "via traits AND the operator surface (+, -, *, /, .inverse()) is "
    "fully exposed on Rational<I>.");

/**
 * @brief Canonical polynomial ring over the rationals: Q[x].
 *
 * @details RigPolynomial<Rational<I>> forms a commutative ring under
 * coefficient-wise addition and Cauchy product. The coefficient field is
 * the quotient field of the integer carrier I. The default I = default_integer
 * (currently `int`, the machine signed integer) gives Q[x] over ℤ.
 *
 * @note This is the structurally correct polynomial ring: the coefficient type
 * Rational<I> is field-like (HasFieldOperators), so the polynomial ring
 * has all four arithmetic operations available. A future retarget to
 * SignedExtensionalCardinal<N> would give a provably total Q[x].
 */
export template <IsInteger I = default_integer>
using RationalPolynomial = dedekind::algebra::RigPolynomial<Rational<I>>;

static_assert(dedekind::algebra::HasFieldOperators<Rational<default_integer>>,
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

}  // namespace dedekind::numbers
