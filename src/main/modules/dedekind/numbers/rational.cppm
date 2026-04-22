/**
 * @file dedekind/numbers/rational.cppm
 * @partition :rational
 * @brief Module interface in the dedekind hierarchy.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "Etudiez la nature, aimez la nature, approchez-vous de la nature.
 * Elle ne vous trompera jamais."
 *       ("Study nature, love nature, stay close to nature. It will never
 * fail you.")
 *       -- Jean-Baptiste Fourier
 */

module;
#include <compare>
#include <concepts>
#include <functional>
#include <numeric>
#include <stdexcept>
#include <utility>

/**
 * @file dedekind/numbers/rational.cppm
 * @module dedekind.numbers:rational
 * @brief Level 8: The Quotient Field (Q).
 */

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

/**
 * @class Rational
 * @brief The Field of Fractions over an Integral Domain Z.
 */
export template <IsInteger Z>
class Rational {
 public:
  using Domain = Z;

  // Numerator and denominator as a canonical pair (second always positive).
  // Public to satisfy IsProduct<Rational<Z>, Z, Z> (ℚ ≅ ℤ × ℤ / ~).
  Z first, second;

  constexpr Rational(Z num, Z den) : first(num), second(den) { simplify(); }

  /** @brief Embedding of an integer as a rational n/1.
   *  Implicit to allow `(a + b) / Z{2}` in IsDense checks. */
  constexpr Rational(Z n) : first(n), second(Z{1}) {}  // NOLINT(google-explicit-constructor)

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

export using RationalSet = RationalsOf<>;
export using ℚ = RationalSet;

export inline constexpr ℚ Q{};

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
}  // namespace dedekind::category

namespace dedekind::numbers {

static_assert(
    dedekind::category::IsSet<decltype(dedekind::category::ambient_set<
                                       Rational<default_integer>>(Q))>,
    "RationalsOf must be the canonical IsSet anchor for "
    "dedekind.numbers:rational.");

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
//   :cardinality → :naturals   :integer → :cardinality   :rational → :integer
static_assert(IsInteger<ExtensionalCardinal<>>,
              "ExtensionalCardinal<> must satisfy IsInteger (Euclidean "
              "ring: +, -, *, /, % with two's-complement wrapping).");

static_assert(
    dedekind::algebra::IsFieldLikeScalar<Rational<default_integer>>,
    "Rational<I> must satisfy the operational field-like witness (ℚ is a "
    "field).");

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

// Structural product proof: ℚ is constructed as pairs (num, den) from ℤ × ℤ.
static_assert(
    dedekind::category::IsProduct<Rational<default_integer>, default_integer,
                                  default_integer>,
    "Rational<Z> must satisfy IsProduct<Rational<Z>, Z, Z> (ℚ ≅ ℤ × ℤ / ~).");

}  // namespace dedekind::numbers
