/**
 * @file dedekind/numbers/rational.cppm
 * @partition :rational
 * @brief ℚ, the field of fractions of ℤ, certified on its terms; ℚ* and ℚ₊ as
 *        types.
 *
 * @details A rational is a term of ℚ (@c sets::RatExpr): atoms of machine
 * words, sums, products, negations and quotients by certified divisors, its
 * equality and order those of its canonical form, the reduced fraction with a
 * positive denominator (@c sets:expr).  Division is total on ℚ × ℚ*, where
 * ℚ* = @c NonZero<RatExpr<>> (@c :total) is entered by @c nonzero, the one
 * refusal point; there is no division by a term of ℚ.  ℚ₊ is @c Positive, a
 * refinement closed under @c +, @c * and @c /, and the squares of ℚ* land in
 * it, so a norm needs no check.  The atoms of ℚ are every integral type but
 * @c bool, through the total embedding, and the finite doubles, exactly,
 * through @c from.  Order between atoms of different types is ℚ's, never
 * C++'s arithmetic conversion ([expr.arith.conv]), which loses integers above
 * @f$2^{53}@f$.
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
#include <compare>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <limits>
#include <optional>  // the checks into ℚ* and ℚ₊
#include <type_traits>
#include <utility>

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

/** @brief The check into ℚ* and its type, re-exported beside ℚ. */
export using dedekind::category::NonZero;
export using dedekind::category::nonzero;
export using dedekind::sets::from;

/** @brief The literal @f$2 \in@f$ ℚ*: the midpoint's divisor. */
export inline constexpr NonZero<RatExpr<>> two_ℚ =
    NonZero<RatExpr<>>::literal<2>();

export class Positive;
export constexpr std::optional<Positive> positive(const RatExpr<>& q);
export constexpr Positive square(const NonZero<RatExpr<>>& x);
export constexpr Positive abs(const NonZero<RatExpr<>>& x);
}  // namespace dedekind::numbers

namespace dedekind::sets {
/** @brief The midpoint @f$a + (b - a)/2@f$, a term: ℚ is dense (@c IsDense
 *  reads this by argument-dependent lookup on @c sets).  @param a one end.
 *  @param b the other. */
export template <std::size_t N>
constexpr RatExpr<N> midpoint(const RatExpr<N>& a, const RatExpr<N>& b) {
  return a + (b - a) /
                 dedekind::category::NonZero<RatExpr<N>>::template literal<2>();
}
}  // namespace dedekind::sets

namespace dedekind::category {
/** @brief ℚ₊ ⊂ ℚ*: declared here, kept by Positive's own constructors. */
template <>
struct is_nonzero_refinement<dedekind::numbers::Positive,
                             dedekind::sets::RatExpr<>> : std::true_type {};
}  // namespace dedekind::category

namespace dedekind::numbers {

/**
 * @class Positive
 * @brief ℚ₊ = {q > 0}: a refinement of ℚ closed under @c +, @c * and @c /, and
 *        the home of the squares of ℚ*.  Entered by @c positive, a literal,
 *        or those laws; widens to ℚ* (a declared law, @c
 *        is_nonzero_refinement) and to ℚ.  Its value is a term; a computation
 *        that iterates calls @c canonical where it stores one.
 */
export class Positive {
 public:
  using Domain = RatExpr<>;
  /** @return the value, a term of ℚ. */
  constexpr RatExpr<> value() const { return q_; }
  /** @brief The inclusion ℚ₊ ⊂ ℚ. */
  constexpr operator RatExpr<>() const { return q_; }  // NOLINT
  /** @brief A positive literal; zero and the negatives are refused by the
   *  constraint.  @tparam N the literal. */
  template <auto N>
    requires(N > 0)
  static consteval Positive literal() {
    return Positive{RatExpr<>{N}};
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
    return Positive{a.q_ / NonZero<RatExpr<>>{b}};
  }
  /** @brief @f$1/p > 0@f$. */
  constexpr Positive inverse() const {
    return Positive{RatExpr<>{1} / NonZero<RatExpr<>>{*this}};
  }
  /** @brief The canonical form of a positive: still positive.  The explicit
   *  normalisation point of a computation that stores one.  @param p the
   *  positive. */
  friend constexpr Positive canonical(const Positive& p) {
    return Positive{canonical(p.q_)};
  }

 private:
  constexpr explicit Positive(RatExpr<> q) : q_(std::move(q)) {}
  friend constexpr std::optional<Positive> positive(const RatExpr<>& q);
  friend constexpr Positive square(const NonZero<RatExpr<>>& x);
  friend constexpr Positive abs(const NonZero<RatExpr<>>& x);
  RatExpr<> q_;
};

/** @brief The check into ℚ₊: the value, certified, or nothing at zero and
 *  below.  @param q the value. */
export constexpr std::optional<Positive> positive(const RatExpr<>& q) {
  if (q <= RatExpr<>{}) return std::nullopt;
  return Positive{q};
}
/** @brief @f$x^2 > 0@f$ for @f$x \neq 0@f$: the law that makes a norm a
 *  certified divisor.  @param x a non-zero rational. */
export constexpr Positive square(const NonZero<RatExpr<>>& x) {
  return Positive{x.value() * x.value()};
}
/** @brief @f$|x| > 0@f$ for @f$x \neq 0@f$.  @param x a non-zero rational. */
export constexpr Positive abs(const NonZero<RatExpr<>>& x) {
  return x.value() < RatExpr<>{} ? Positive{-x.value()} : Positive{x.value()};
}

/** @concept IsRational
 *  @brief @c Q is ℚ.  Gates the real carriers (@c Cut, @c QuadraticReal):
 *  over ℚ a nonsquare integer @c D has @f$\sqrt D \notin \mathbb{Q}@f$, so
 *  those carriers are genuine. */
export template <typename Q>
concept IsRational = std::same_as<std::remove_cvref_t<Q>, RatExpr<>>;

/** @section rational__Partial_Arithmetic_with_Ternary_Logic
 *  The Kleene surface: exact arithmetic answers @c True; division answers
 *  @c False at a zero divisor, where the typed division has no term. */
export struct PartialAddRational {
  using value_type = RatExpr<>;
  using logic_species = Kleene;
  constexpr TernaryResult<RatExpr<>> operator()(
      std::pair<const RatExpr<>&, const RatExpr<>&> p) const {
    return {Ternary::True, p.first + p.second};
  }
};
export struct PartialMulRational {
  using value_type = RatExpr<>;
  using logic_species = Kleene;
  constexpr TernaryResult<RatExpr<>> operator()(
      std::pair<const RatExpr<>&, const RatExpr<>&> p) const {
    return {Ternary::True, p.first * p.second};
  }
};
export struct HonestDivRational {
  using value_type = RatExpr<>;
  using logic_species = Kleene;
  constexpr TernaryResult<RatExpr<>> operator()(
      std::pair<const RatExpr<>&, const RatExpr<>&> p) const {
    if (const auto d = nonzero(p.second)) return {Ternary::True, p.first / *d};
    return {Ternary::False, p.first};
  }
};
/** @brief ℤ ↪ ℚ on the Kleene surface: exact, so always @c True. */
export struct PartialEmbedIntegerToRational {
  using value_type = RatExpr<>;
  using logic_species = Kleene;
  constexpr TernaryResult<RatExpr<>> operator()(const IntExpr<>& n) const {
    return {Ternary::True, RatExpr<>::widened(n)};
  }
};

/** @brief The subalgebra embedding ℤ ↪ ℚ, every node of the integer's term
 *  kept: a monic ring homomorphism preserving the order, the tower rung below
 *  @c embed_ℚ_ℝ. */
export inline constexpr auto embed_ℤ_ℚ_ = arrow<IntExpr<>, RatExpr<>>(
    [](const IntExpr<>& n) { return RatExpr<>::widened(n); });

/** @brief The exact dyadic embedding of the finite doubles into ℚ: @c from on
 *  the domain that excludes NaN and ±∞ by type, so total and monic. */
export inline constexpr auto embed_𝕃_ℚ =
    arrow<dedekind::morphologies::𝕃<double>, RatExpr<>>(
        [](const dedekind::morphologies::𝕃<double>& f) {
          const auto q = from(f.value());  // 𝕃 certifies finiteness
          if (!q) __builtin_trap();
          return *q;
        });

/** @brief The universe ℚ = 𝔸<RatExpr<>>: the named species symbols denote
 *  universe values (#559). */
export inline constexpr 𝔸<RatExpr<>, Boole, ℵ_0> ℚ =
    dedekind::sets::𝔸<RatExpr<>>{};

}  // namespace dedekind::numbers

// ---------------------------------------------------------------------------
// Registrations: ℚ's order with its field, the Kleene surface, the arrows.
// ℚ's field laws are certified on its terms in sets:expr.
// ---------------------------------------------------------------------------
namespace dedekind::algebra {
// ℚ is an ORDERED field (Lang, Algebra §III.1): the order is compatible with
// translation (O1) and with scaling by positives (O2).
template <>
struct is_translation_invariant_ordered<dedekind::sets::RatExpr<>>
    : std::true_type {};
template <>
struct is_scaling_invariant_ordered<dedekind::sets::RatExpr<>>
    : std::true_type {};
}  // namespace dedekind::algebra

namespace dedekind::category {
using Q_ = dedekind::sets::RatExpr<>;
using Z_ = dedekind::sets::IntExpr<>;
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
                       Z_, std::plus<Z_>, std::plus<Q_>> : std::true_type {};
template <>
struct is_homomorphism<std::decay_t<decltype(dedekind::numbers::embed_ℤ_ℚ_)>,
                       Z_, std::multiplies<Z_>, std::multiplies<Q_>>
    : std::true_type {};
/** @brief The finite doubles embed injectively: distinct doubles are distinct
 *  dyadic rationals (−0.0 and +0.0 are one element of 𝕃). */
template <>
inline constexpr bool
    is_monic_arrow_v<std::decay_t<decltype(dedekind::numbers::embed_𝕃_ℚ)>> =
        true;
}  // namespace dedekind::category

namespace dedekind::numbers {
/** @section rational__Formal_Verification  Small values only: the big-value
 *  cases are runtime tests, so constant evaluation stays cheap. */
namespace {
using Q = RatExpr<>;
static_assert(Q{2, 4} == Q{1, 2} && Q{2, -4} == Q{-1, 2} && Q{0, 7} == Q{} &&
                  Q{1, 2} + Q{1, 3} == Q{5, 6} &&
                  Q{1, 2} * Q{1, 3} == Q{1, 6} && Q{1, 3} < Q{1, 2},
              "ℚ's operations and order, through the canonical form");
static_assert(Q{1, 2} / nonzero(Q{1, 3}).value() == Q{3, 2} &&
                  !nonzero(Q{}).has_value() && Q{1} / two_ℚ == Q{1, 2} &&
                  (two_ℚ * two_ℚ).value() == Q{4} &&
                  two_ℚ.inverse().value() == Q{1, 2},
              "ℚ* is closed under · and inverse; division is by ℚ* only");
static_assert(midpoint(Q{1}, Q{2}) == Q{3, 2}, "the midpoint, ℚ dense");
static_assert(positive(Q{1, 2}).has_value() && !positive(Q{}).has_value() &&
                  !positive(Q{-1}).has_value() &&
                  (Positive::literal<3>() + Positive::literal<4>()).value() ==
                      Q{7} &&
                  square(-two_ℚ).value() == Q{4} && abs(-two_ℚ).value() == Q{2},
              "ℚ₊ is closed under + and ·, and holds the squares of ℚ*");
static_assert(from(0.5) == Q{1, 2} && from(-0.75) == Q{-3, 4} &&
                  from(-0.0) == Q{} &&
                  !from(std::numeric_limits<double>::quiet_NaN()).has_value() &&
                  !from(std::numeric_limits<double>::infinity()).has_value(),
              "a double is the dyadic rational its bits spell");
// C++'s own comparison converts the integer to double and rounds: 2^53 + 1
// reads equal to 2^53.  ℚ's does not.
static_assert(
    (1LL << 53) + 1 == static_cast<double>(1LL << 53),
    "[expr.arith.conv]: the usual arithmetic conversions lose 2^53 + 1");
static_assert(Q{(1LL << 53) + 1} != static_cast<double>(1LL << 53) &&
                  Q{(1LL << 53) + 1} > static_cast<double>(1LL << 53) &&
                  Q{1, 2} == 0.5,
              "ℚ compares atoms of different types in ℚ, exactly");
static_assert(dedekind::algebra::HasFieldOperators<Q> &&
                  dedekind::algebra::IsDivisionRing<Q> &&
                  dedekind::algebra::IsOrderedMultiplicativeGroup<Q> &&
                  IsTotallyOrdered<Q> && IsDense<Q> &&
                  !dedekind::category::HasNNOStep<Q>,
              "ℚ is an ordered field and a dense chain");
static_assert(
    std::same_as<std::remove_cvref_t<decltype(ℚ)>::Domain, Q> &&
        dedekind::algebra::IsAlgebraOnSet<decltype(ℚ), std::plus<Q>,
                                          std::multiplies<Q>, std::negate<Q>>,
    "ℚ is the universe 𝔸<RatExpr<>>, an algebraic set");
static_assert(embed_ℤ_ℚ_(IntExpr<>{2} + IntExpr<>{3}) ==
                      embed_ℤ_ℚ_(IntExpr<>{2}) + embed_ℤ_ℚ_(IntExpr<>{3}) &&
                  embed_ℤ_ℚ_(IntExpr<>{2}) < embed_ℤ_ℚ_(IntExpr<>{3}) &&
                  dedekind::algebra::EmbedsAsSubalgebra<
                      std::decay_t<decltype(embed_ℤ_ℚ_)>>,
              "ℤ ↪ ℚ is a monic, monotone ring homomorphism");
}  // namespace
}  // namespace dedekind::numbers
