/**
 * @file dedekind/numbers/rational.cppm
 * @partition :rational
 * @brief ℚ: one @c Rational, the fractions of ℤ in normal form; ℚ* and ℚ₊ as
 *        types.
 *
 * @details A rational is a pair @c (num, den) with @c num an @c Integer,
 * @c den a @c Numeral at least @c 1, and @f$\gcd(|num|, den) = 1@f$: the
 * normal form, kept by every constructor and operation, so @c == is
 * memberwise.  No value denotes ∞: finite terms over finite atoms stay
 * finite.  Division is total on ℚ × ℚ*, where ℚ* = @c NonZero<Rational>
 * (@c :total) is entered by @c nonzero, the one refusal point; there is no
 * @c Rational @c / @c Rational.  ℚ₊ is @c Positive, a refinement closed under
 * @c +, @c * and @c /, and the squares of ℚ* land in it, so a norm needs no
 * check.  The atoms of ℚ are every integral type but @c bool, through ℤ, and
 * the finite floating values, exactly, through @c from.  Order between atoms
 * of different types is ℚ's, never C++'s arithmetic conversion
 * ([expr.arith.conv]), which loses integers above @f$2^{53}@f$.
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
#include <bit>  // std::bit_cast: a double's bits, exactly
#include <compare>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <limits>
#include <optional>  // the check into ℚ*, ℚ₊, and from(double)
#include <type_traits>
#include <utility>
#include <vector>  // a power of two as limbs

export module dedekind.numbers:rational;

import dedekind.algebra;
import dedekind.category;
import dedekind.morphologies;
import dedekind.order;
import dedekind.sets;
import :integer;

namespace dedekind::numbers {
using namespace dedekind::algebra;
using namespace dedekind::category;
using namespace dedekind::morphologies;
using namespace dedekind::order;
using namespace dedekind::sets;

export class Rational;
export class Positive;
/** @brief ℚ* = ℚ \ {0}, the certified divisors of ℚ (@c :total). */
export using NonZeroRational = dedekind::category::NonZero<Rational>;
export constexpr std::optional<Positive> positive(const Rational& q);
export constexpr Positive square(const NonZeroRational& x);
export constexpr Positive abs(const NonZeroRational& x);
}  // namespace dedekind::numbers

namespace dedekind::category {
/** @brief ℚ divides by ℚ*. */
template <>
struct divisor_of<dedekind::numbers::Rational> {
  using type = NonZero<dedekind::numbers::Rational>;
};
/** @brief ℚ₊ ⊂ ℚ*: declared here, kept by Positive's own constructors. */
template <>
struct is_nonzero_refinement<dedekind::numbers::Positive,
                             dedekind::numbers::Rational> : std::true_type {};
}  // namespace dedekind::category

namespace dedekind::numbers {

/**
 * @class Rational
 * @brief ℚ, the field of fractions of ℤ: @c num / @c den in normal form.
 */
export class Rational {
 public:
  using Domain = Rational;
  /** @brief ℚ is countable; @c :order's halfspaces read this. */
  using cardinality_type = ℵ_0;

  /** @brief @f$0 = 0/1@f$. */
  constexpr Rational() = default;
  /** @brief ℤ ↪ ℚ, @f$n \mapsto n/1@f$.  @param n an integer. */
  constexpr Rational(Integer n) : num_(std::move(n)) {}  // NOLINT
  /** @brief An atom: any integral type but @c bool, through ℤ.
   *  @tparam S the atom's type.  @param n the atom. */
  template <std::integral S>
    requires(!std::same_as<S, bool>)
  constexpr Rational(S n) : num_(atom(n)) {}  // NOLINT
  /** @brief A literal fraction, @f$n/d@f$: a zero @c d is refused by constant
   *  evaluation (the check into ℚ* fails).  Literals only, by @c consteval.
   *  @tparam N the numerator's type.  @tparam D the denominator's type.
   *  @param n the numerator.  @param d the denominator. */
  template <std::integral N, std::integral D>
    requires(!std::same_as<N, bool> && !std::same_as<D, bool>)
  consteval Rational(N n, D d)
      : Rational(Rational{n} / nonzero(Rational{d}).value()) {}

  /** @return the numerator, in ℤ: carries the sign. */
  constexpr const Integer& num() const { return num_; }
  /** @return the denominator, in ℕ, at least @c 1. */
  constexpr const Numeral& den() const { return den_; }

  /** @brief Equality of normal forms. */
  constexpr bool operator==(const Rational&) const = default;
  /** @brief The order: @f$a/b < c/d \iff ad < cb@f$, the denominators
   *  positive. */
  friend constexpr std::strong_ordering operator<=>(const Rational& a,
                                                    const Rational& b) {
    return a.num_ * Integer{b.den_} <=> b.num_ * Integer{a.den_};
  }

  /** @brief @f$-(a/b) = (-a)/b@f$, already reduced. */
  constexpr Rational operator-() const { return {-num_, den_, Reduced{}}; }
  friend constexpr Rational operator+(const Rational& a, const Rational& b) {
    return reduced(a.num_ * Integer{b.den_} + b.num_ * Integer{a.den_},
                   a.den_ * b.den_);
  }
  friend constexpr Rational operator-(const Rational& a, const Rational& b) {
    return a + (-b);
  }
  friend constexpr Rational operator*(const Rational& a, const Rational& b) {
    return reduced(a.num_ * b.num_, a.den_ * b.den_);
  }
  /** @brief Division, total on ℚ × ℚ*: @f$(a/b) / (c/d) = (ad) / (bc)@f$,
   *  the sign of @c c moved to the numerator.  @param a the dividend.
   *  @param d the divisor, certified. */
  friend constexpr Rational operator/(const Rational& a,
                                      const NonZeroRational& d) {
    const Rational& b = d.value();
    return reduced(Integer(a.num_.magnitude() * b.den_,
                           a.num_.negative() != b.num_.negative()),
                   a.den_ * b.num_.magnitude());
  }

 private:
  struct Reduced {};
  constexpr Rational(Integer num, Numeral den, Reduced)
      : num_(std::move(num)), den_(std::move(den)) {}
  /** @return @c num/den in normal form: both divided by their gcd.
   *  @param num the numerator.  @param den the denominator, at least 1. */
  static constexpr Rational reduced(Integer num, Numeral den) {
    const Numeral g = gcd(num.magnitude(), den);
    return {Integer(num.magnitude() / g, num.negative()), den / g, Reduced{}};
  }
  template <std::signed_integral S>
  static constexpr Integer atom(S n) {
    return Integer{n};
  }
  template <std::unsigned_integral U>
  static constexpr Integer atom(U n) {
    return Integer{Numeral{n}};
  }

  Integer num_{};
  Numeral den_{1u};
};

/** @brief The literal @f$2 \in@f$ ℚ*: the midpoint's divisor. */
export inline constexpr NonZeroRational two = NonZeroRational::literal<2>();

/** @brief The midpoint @f$a + (b - a)/2@f$: ℚ is dense (@c IsDense reads
 *  this by argument-dependent lookup).  @param a one end.  @param b the
 *  other. */
export constexpr Rational midpoint(const Rational& a, const Rational& b) {
  return a + (b - a) / two;
}

/**
 * @class Positive
 * @brief ℚ₊ = {q > 0}: a refinement of ℚ closed under @c +, @c * and @c /, and
 *        the home of the squares of ℚ*.  Entered by @c positive, a literal,
 *        or those laws; widens to ℚ* (a declared law, @c
 *        is_nonzero_refinement) and to ℚ.
 */
export class Positive {
 public:
  using Domain = Rational;
  constexpr const Rational& value() const { return q_; }
  constexpr operator const Rational&() const { return q_; }  // NOLINT
  /** @brief A positive literal; zero and the negatives are refused by the
   *  constraint.  @tparam N the literal. */
  template <auto N>
    requires(N > 0)
  static consteval Positive literal() {
    return Positive{Rational{N}};
  }
  constexpr bool operator==(const Positive&) const = default;
  /** @brief Closure: a sum of positives is positive. */
  friend constexpr Positive operator+(const Positive& a, const Positive& b) {
    return Positive{a.q_ + b.q_};
  }
  /** @brief Closure: a product of positives is positive. */
  friend constexpr Positive operator*(const Positive& a, const Positive& b) {
    return Positive{a.q_ * b.q_};
  }
  /** @brief Closure: a quotient of positives is positive. */
  friend constexpr Positive operator/(const Positive& a, const Positive& b) {
    return Positive{a.q_ / NonZeroRational{b}};
  }
  /** @brief @f$1/p > 0@f$. */
  constexpr Positive inverse() const {
    return Positive{Rational{1} / NonZeroRational{*this}};
  }
  /** @return @f$2^k > 0@f$, written as limbs: the dyadic scale of a double.
   *  @param k the exponent. */
  static constexpr Positive pow2(std::size_t k) {
    std::vector<Numeral::limb> limbs(k / 64 + 1, Numeral::limb{});
    limbs.back() = Numeral::limb{1} << (k % 64);
    return Positive{Rational{Integer{Numeral{std::move(limbs)}}}};
  }

 private:
  constexpr explicit Positive(Rational q) : q_(std::move(q)) {}
  friend constexpr std::optional<Positive> positive(const Rational& q);
  friend constexpr Positive square(const NonZeroRational& x);
  friend constexpr Positive abs(const NonZeroRational& x);
  Rational q_;
};

/** @brief The check into ℚ₊: the value, certified, or nothing at zero and
 *  below.  @param q the value. */
export constexpr std::optional<Positive> positive(const Rational& q) {
  if (q <= Rational{}) return std::nullopt;
  return Positive{q};
}
/** @brief @f$x^2 > 0@f$ for @f$x \neq 0@f$: the law that makes a norm a
 *  certified divisor.  @param x a non-zero rational. */
export constexpr Positive square(const NonZeroRational& x) {
  return Positive{x.value() * x.value()};
}
/** @brief @f$|x| > 0@f$ for @f$x \neq 0@f$.  @param x a non-zero rational. */
export constexpr Positive abs(const NonZeroRational& x) {
  return x.value() < Rational{} ? Positive{-x.value()} : Positive{x.value()};
}

/** @brief The canonical projections, ℚ as a quotient of ℤ × ℕ (@c IsProduct,
 *  the hook @c :limit's @c π_1 / @c π_2 read).  @param q a rational. */
export constexpr Integer component(const Rational& q,
                                   dedekind::category::At<1>) {
  return q.num();
}
export constexpr Numeral component(const Rational& q,
                                   dedekind::category::At<2>) {
  return q.den();
}

/** @section rational__Atoms_from_floating_point
 *  Every finite @c double is exactly the dyadic rational @f$m \cdot 2^e@f$
 *  read off its bits (@c std::bit_cast; Knuth, @em TAOCP, Vol. 2, §4.2), so
 *  it embeds into ℚ exactly; NaN and ±∞ are not values of ℚ and the check
 *  refuses them; @f$-0@f$ and @f$+0@f$ are one rational.  A @c float is a
 *  @c double exactly. */
/** @return the rational the double is, or nothing for NaN and ±∞.
 *  @param x the double. */
export constexpr std::optional<Rational> from(double x) {
  const auto bits = std::bit_cast<std::uint64_t>(x);
  const bool sign = (bits >> 63) & 1U;
  const auto biased = static_cast<unsigned>((bits >> 52) & 0x7FFU);
  const std::uint64_t fraction = bits & 0x000F'FFFF'FFFF'FFFFULL;
  if (biased == 0x7FFU) return std::nullopt;  // NaN or ±∞
  // x = ±mantissa · 2^exp exactly; a subnormal has no implicit leading one.
  const std::uint64_t mantissa =
      biased == 0 ? fraction : (std::uint64_t{1} << 52) | fraction;
  const int exp = biased == 0 ? -1074 : static_cast<int>(biased) - 1075;
  if (mantissa == 0) return Rational{};  // ±0
  const Rational m{Integer(Numeral{mantissa}, sign)};
  if (exp >= 0) return m * Positive::pow2(static_cast<std::size_t>(exp));
  return m / Positive::pow2(static_cast<std::size_t>(-exp));
}
/** @overload */
export constexpr std::optional<Rational> from(float x) {
  return from(static_cast<double>(x));
}

/** @brief Order and equality against a floating atom go through ℚ: the
 *  double is read exactly, and C++'s own conversion of the integer side to
 *  @c double ([expr.arith.conv]), which rounds above @f$2^{53}@f$, is never
 *  taken.  NaN is unordered and equal to nothing. */
export template <std::floating_point F>
constexpr bool operator==(const Rational& q, F x) {
  const auto r = from(x);
  return r && *r == q;
}
export template <std::floating_point F>
constexpr std::partial_ordering operator<=>(const Rational& q, F x) {
  if (const auto r = from(x)) return q <=> *r;
  return std::partial_ordering::unordered;
}

/** @concept IsRational
 *  @brief @c Q is ℚ.  Gates the real carriers (@c Cut, @c QuadraticReal):
 *  over ℚ a nonsquare integer @c D has @f$\sqrt D \notin \mathbb{Q}@f$, so
 *  those carriers are genuine. */
export template <typename Q>
concept IsRational = std::same_as<std::remove_cvref_t<Q>, Rational>;

/** @section rational__Partial_Arithmetic_with_Ternary_Logic
 *  The Kleene surface: exact arithmetic answers @c True; division answers
 *  @c False at a zero divisor, where the typed division has no term. */
export struct PartialAddRational {
  using value_type = Rational;
  using logic_species = Kleene;
  constexpr TernaryResult<Rational> operator()(
      std::pair<const Rational&, const Rational&> p) const {
    return {Ternary::True, p.first + p.second};
  }
};
export struct PartialMulRational {
  using value_type = Rational;
  using logic_species = Kleene;
  constexpr TernaryResult<Rational> operator()(
      std::pair<const Rational&, const Rational&> p) const {
    return {Ternary::True, p.first * p.second};
  }
};
export struct HonestDivRational {
  using value_type = Rational;
  using logic_species = Kleene;
  constexpr TernaryResult<Rational> operator()(
      std::pair<const Rational&, const Rational&> p) const {
    if (const auto d = nonzero(p.second)) return {Ternary::True, p.first / *d};
    return {Ternary::False, p.first};
  }
};
/** @brief ℤ ↪ ℚ on the Kleene surface: exact, so always @c True. */
export struct PartialEmbedIntegerToRational {
  using value_type = Rational;
  using logic_species = Kleene;
  constexpr TernaryResult<Rational> operator()(Integer n) const {
    return {Ternary::True, Rational{std::move(n)}};
  }
};

/** @brief The subalgebra embedding ℤ ↪ ℚ, @f$n \mapsto n/1@f$: a monic ring
 *  homomorphism preserving the order, the tower rung below @c embed_ℚ_ℝ. */
export inline constexpr auto embed_ℤ_ℚ_ =
    arrow<Integer, Rational>([](const Integer& n) { return Rational{n}; });

/** @brief The exact dyadic embedding of the finite doubles into ℚ, @c from
 *  on the domain that excludes NaN and ±∞ by type, so total and monic. */
export inline constexpr auto embed_𝕃_ℚ =
    arrow<dedekind::morphologies::𝕃<double>, Rational>(
        [](const dedekind::morphologies::𝕃<double>& f) {
          return from(f.value()).value();  // 𝕃 certifies finiteness
        });

/** @brief The polynomial ring ℚ[x]. */
export using RationalPolynomial = dedekind::algebra::Polynomial<Rational>;

}  // namespace dedekind::numbers

// ---------------------------------------------------------------------------
// Registrations: the laws of ℚ, over an exact ℤ unconditionally.
// ---------------------------------------------------------------------------
namespace dedekind::category {
using Q_ = dedekind::numbers::Rational;

template <>
struct SpeciesTraits<Q_> {
  using Domain = Q_;
  using machine_type = Q_;
};

// Kleene traits of the partial surface.
template <>
inline constexpr bool
    is_kleene_associative_v<Q_, dedekind::numbers::PartialAddRational> = true;
template <>
inline constexpr bool
    is_kleene_commutative_v<Q_, dedekind::numbers::PartialAddRational> = true;
template <>
inline constexpr Q_ partial_unit_v<Q_, dedekind::numbers::PartialAddRational> =
    Q_{};
template <>
inline constexpr bool
    is_kleene_associative_v<Q_, dedekind::numbers::PartialMulRational> = true;
template <>
inline constexpr bool
    is_kleene_commutative_v<Q_, dedekind::numbers::PartialMulRational> = true;
template <>
inline constexpr Q_ partial_unit_v<Q_, dedekind::numbers::PartialMulRational> =
    Q_{1};
template <>
inline constexpr bool is_kleene_associative_v<
    Q_, dedekind::numbers::PartialEmbedIntegerToRational> = true;

// The chain ℚ.
template <>
inline constexpr bool is_reflexive_v<Q_, std::less_equal<>> = true;
template <>
inline constexpr bool is_transitive_v<Q_, std::less_equal<>> = true;
template <>
inline constexpr bool is_antisymmetric_v<Q_, std::less_equal<>> = true;
template <>
inline constexpr bool is_connex_v<Q_, std::less_equal<>> = true;

// The field: Frac(ℤ) is a localisation (Atiyah–Macdonald ch. 3), not a
// homomorphic image, so the laws are registered directly; over ℤ's exact
// arithmetic no cross-product wraps, and the fractions obey them.
template <>
inline constexpr bool is_associative_v<Q_, std::plus<Q_>> = true;
template <>
inline constexpr bool is_associative_v<Q_, std::multiplies<Q_>> = true;
template <>
inline constexpr bool is_commutative_v<Q_, std::plus<Q_>> = true;
template <>
inline constexpr bool is_commutative_v<Q_, std::multiplies<Q_>> = true;
template <>
inline constexpr bool
    is_distributive_v<Q_, std::multiplies<Q_>, std::plus<Q_>> = true;
template <>
struct is_exact_total<Q_, std::plus<Q_>> : std::true_type {};
template <>
struct is_exact_total<Q_, std::multiplies<Q_>> : std::true_type {};
template <>
struct unit_trait<Q_, std::plus<Q_>> {
  using value_type = Q_;
  static constexpr value_type value = Q_{};
};
template <>
struct unit_trait<Q_, std::multiplies<Q_>> {
  using value_type = Q_;
  static constexpr value_type value = Q_{1};
};
// ℚ is a ℤ-module, n (p/q) = (np)/q (Lang, Algebra §III.1).
template <>
struct is_linear_action<Q_, dedekind::sets::Integer, std::plus<Q_>,
                        std::plus<dedekind::sets::Integer>,
                        std::multiplies<dedekind::sets::Integer>,
                        std::multiplies<>> : std::true_type {};
template <>
inline constexpr bool is_invertible_v<Q_, std::plus<Q_>> = true;
template <>
struct inverse_trait<Q_, std::plus<Q_>> {
  static constexpr bool exists = true;
  using value_type = Q_;
  static constexpr value_type compute(const Q_& q) noexcept { return -q; }
};
// Every non-zero rational has an inverse: the convention of IsField, zero
// excluded; the inverse itself is NonZero<Rational>::inverse, on ℚ*.
template <>
inline constexpr bool is_invertible_v<Q_, std::multiplies<Q_>> = true;

/** @brief ℤ ↪ ℚ is monic, monotone, and a ring homomorphism. */
template <>
inline constexpr bool
    is_monic_arrow_v<std::decay_t<decltype(dedekind::numbers::embed_ℤ_ℚ_)>> =
        true;
template <>
inline constexpr bool is_monotone_v<
    std::decay_t<decltype(dedekind::numbers::embed_ℤ_ℚ_)>, std::less_equal<>> =
    true;
template <>
struct is_homomorphism<std::decay_t<decltype(dedekind::numbers::embed_ℤ_ℚ_)>,
                       dedekind::sets::Integer,
                       std::plus<dedekind::sets::Integer>, std::plus<Q_>>
    : std::true_type {};
template <>
struct is_homomorphism<std::decay_t<decltype(dedekind::numbers::embed_ℤ_ℚ_)>,
                       dedekind::sets::Integer,
                       std::multiplies<dedekind::sets::Integer>,
                       std::multiplies<Q_>> : std::true_type {};
/** @brief The finite doubles embed injectively: distinct doubles are distinct
 *  dyadic rationals (−0.0 and +0.0 are one element of 𝕃). */
template <>
inline constexpr bool
    is_monic_arrow_v<std::decay_t<decltype(dedekind::numbers::embed_𝕃_ℚ)>> =
        true;
}  // namespace dedekind::category

namespace dedekind::algebra {
// ℚ is an ORDERED field (Lang, Algebra §III.1): the order is compatible with
// translation (O1) and with scaling by positives (O2).
template <>
struct is_translation_invariant_ordered<dedekind::numbers::Rational>
    : std::true_type {};
template <>
struct is_scaling_invariant_ordered<dedekind::numbers::Rational>
    : std::true_type {};
}  // namespace dedekind::algebra

namespace dedekind::numbers {

/** @brief The universe ℚ = 𝔸<Rational>: the named species symbols denote
 *  universe values, carrier types are spelled directly (#559). */
export inline constexpr 𝔸<Rational, Boole, ℵ_0> ℚ =
    dedekind::sets::𝔸<Rational>{};

/** @section rational__Formal_Verification */
namespace {
using dedekind::sets::Integer;
using dedekind::sets::Numeral;

// Normal forms: reduced, the sign in the numerator, zero as 0/1.
static_assert(Rational{2, 4} == Rational{1, 2} &&
                  Rational{-2, 4}.num() == Integer{-1} &&
                  Rational{2, -4} == Rational{-1, 2} &&
                  Rational{0, 7} == Rational{} &&
                  Rational{3, -3} == Rational{-1},
              "a rational is its normal form");
static_assert(Rational{1, 2} + Rational{1, 3} == Rational{5, 6} &&
                  Rational{1, 2} * Rational{1, 3} == Rational{1, 6} &&
                  Rational{1, 2} - Rational{1, 2} == Rational{} &&
                  -Rational{1, 2} == Rational{-1, 2} &&
                  Rational{1, 3} < Rational{1, 2} &&
                  Rational{-1, 2} < Rational{},
              "the field operations and the order, on literals");
// Division is by ℚ* only: nonzero is the check, and it fails at zero.
static_assert(Rational{1, 2} / nonzero(Rational{1, 3}).value() ==
                      Rational{3, 2} &&
                  !nonzero(Rational{}).has_value() &&
                  nonzero(Rational{3}).has_value() &&
                  Rational{1} / two == Rational{1, 2} &&
                  (two * two).value() == Rational{4} &&
                  two.inverse().value() == Rational{1, 2} &&
                  (two / two).value() == Rational{1} &&
                  (-two).value() == Rational{-2},
              "ℚ* is closed under ·, /, inverse and negation");
static_assert(midpoint(Rational{1}, Rational{2}) == Rational{3, 2} &&
                  midpoint(Rational{-1}, Rational{1}) == Rational{},
              "the midpoint, ℚ dense");
// ℚ₊.
static_assert(positive(Rational{1, 2}).has_value() &&
                  !positive(Rational{}).has_value() &&
                  !positive(Rational{-1}).has_value() &&
                  (Positive::literal<3>() + Positive::literal<4>()).value() ==
                      Rational{7} &&
                  (Positive::literal<3>() / Positive::literal<4>()).value() ==
                      Rational{3, 4} &&
                  square(-two).value() == Rational{4} &&
                  abs(-two).value() == Rational{2} &&
                  Rational{1} / Positive::literal<4>() == Rational{1, 4},
              "ℚ₊ is closed under +, · and /, and holds the squares of ℚ*");
// Atoms past the word, both signs.
constexpr Integer two_64() {
  return Integer{Numeral{std::numeric_limits<std::uint64_t>::max()}} +
         Integer{1};
}
static_assert(Rational{two_64()} / two == Rational{two_64() / Integer{2}} &&
                  Rational{1} / nonzero(Rational{two_64()}).value() *
                          Rational{two_64()} ==
                      Rational{1} &&
                  Rational{-1} / nonzero(Rational{two_64()}).value() <
                      Rational{} &&
                  Rational{std::numeric_limits<std::uint64_t>::max()} +
                          Rational{1} ==
                      Rational{two_64()},
              "fractions with numerators and denominators past 2^64");
// Floating atoms, exactly; NaN and ∞ refused; −0 = +0.
static_assert(
    from(0.5) == Rational{1, 2} && from(-0.75) == Rational{-3, 4} &&
        from(3.0) == Rational{3} && from(-0.0) == Rational{} &&
        from(0.1) ==
            Rational{Integer{3602879701896397LL}} /
                nonzero(Rational{Integer{std::int64_t{1} << 55}}).value() &&
        !from(std::numeric_limits<double>::quiet_NaN()).has_value() &&
        !from(std::numeric_limits<double>::infinity()).has_value() &&
        from(std::numeric_limits<double>::denorm_min()).has_value() &&
        from(0.5f) == Rational{1, 2},
    "a double is the dyadic rational its bits spell");
// C++'s own comparison converts the integer to double and rounds: 2^53 + 1
// reads equal to 2^53.  ℚ's does not.
static_assert(
    (1LL << 53) + 1 == static_cast<double>(1LL << 53),
    "[expr.arith.conv]: the usual arithmetic conversions lose 2^53 + 1");
static_assert(Rational{(1LL << 53) + 1} != static_cast<double>(1LL << 53) &&
                  Rational{(1LL << 53) + 1} > static_cast<double>(1LL << 53) &&
                  Rational{1, 2} == 0.5 &&
                  Rational{1, 3} != 0.3333333333333333 &&
                  (Rational{1} <=> std::numeric_limits<double>::quiet_NaN()) ==
                      std::partial_ordering::unordered,
              "ℚ compares atoms of different types in ℚ, exactly");

// The spine.
static_assert(dedekind::algebra::HasRingOperators<Rational> &&
                  dedekind::algebra::HasGroupOperatorsAdd<Rational> &&
                  dedekind::algebra::HasGroupOperatorsMul<Rational> &&
                  dedekind::algebra::HasFieldOperators<Rational>,
              "ℚ closes the field operator surface, dividing by ℚ*");
static_assert(
    dedekind::category::IsField<Rational, std::plus<Rational>,
                                std::multiplies<Rational>> &&
        dedekind::category::IsRing<Rational, std::plus<Rational>,
                                   std::multiplies<Rational>> &&
        !dedekind::category::is_saturating_v<Rational, std::plus<Rational>>,
    "ℚ is a strict field: exact totality, laws registered");
static_assert(dedekind::algebra::IsDivisionRing<Rational>,
              "ℚ is a division ring: inverse and division on ℚ*");
static_assert(dedekind::algebra::IsOrderedMultiplicativeGroup<Rational>,
              "ℚ is an ordered field (Lang, Algebra §III.1)");
static_assert(dedekind::algebra::is_module_v<Rational, Integer>,
              "ℚ is a ℤ-module");
static_assert(IsTotallyOrdered<Rational> && IsDense<Rational> &&
                  dedekind::order::IsDirectedSet<Rational>,
              "ℚ is a dense chain, a net domain");
static_assert(!dedekind::category::HasNNOStep<Rational>,
              "ℚ has no step: it is not an integer chain");
static_assert(dedekind::category::IsProduct<Rational, Integer, Numeral> &&
                  dedekind::category::π_1(Rational{3, 5}) == Integer{3} &&
                  dedekind::category::π_2(Rational{3, 5}) == Numeral{5u},
              "ℚ projects to ℤ × ℕ: π_1 = num, π_2 = den");
static_assert(
    std::same_as<std::remove_cvref_t<decltype(ℚ)>,
                 dedekind::sets::𝔸<Rational, Boole, ℵ_0>> &&
        std::same_as<std::remove_cvref_t<decltype(ℚ)>::Domain, Rational>,
    "ℚ is the universe 𝔸<Rational>");
static_assert(dedekind::algebra::IsAlgebraOnSet<
                  decltype(ℚ), std::plus<Rational>, std::multiplies<Rational>,
                  std::negate<Rational>>,
              "ℚ is an algebraic set: closed under +, * and unary -");
static_assert(dedekind::category::SetAsProduct<std::remove_cvref_t<decltype(ℚ)>,
                                               Rational, bool>,
              "ℚ walks SetAsProduct over (Rational, Boole::Ω = bool)");
// ℤ ↪ ℚ, computed.
static_assert(
    dedekind::category::IsMonicArrow<std::decay_t<decltype(embed_ℤ_ℚ_)>> &&
        dedekind::algebra::EmbedsAsSubalgebra<
            std::decay_t<decltype(embed_ℤ_ℚ_)>> &&
        embed_ℤ_ℚ_(Integer{2} + Integer{3}) ==
            embed_ℤ_ℚ_(Integer{2}) + embed_ℤ_ℚ_(Integer{3}) &&
        embed_ℤ_ℚ_(Integer{2} * Integer{3}) ==
            embed_ℤ_ℚ_(Integer{2}) * embed_ℤ_ℚ_(Integer{3}) &&
        embed_ℤ_ℚ_(Integer{2}) < embed_ℤ_ℚ_(Integer{3}),
    "ℤ ↪ ℚ is a monic, monotone ring homomorphism");
static_assert(
    dedekind::category::IsMonicArrow<std::decay_t<decltype(embed_𝕃_ℚ)>> &&
        embed_𝕃_ℚ(dedekind::morphologies::𝕃<double>{-21.0}) == Rational{-21},
    "𝕃 ↪ ℚ is monic and exact");
static_assert(std::regular<Rational> && std::copyable<Positive> &&
                  std::copyable<NonZeroRational>,
              "ℚ is a value type; ℚ* and ℚ₊, without a default, are copyable");
}  // namespace
}  // namespace dedekind::numbers
