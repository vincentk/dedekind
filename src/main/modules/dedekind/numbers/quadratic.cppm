/**
 * @file dedekind/numbers/quadratic.cppm
 * @partition :quadratic
 * @brief @f$\mathbb{Q}(\sqrt D)@f$ --- a genuine, @b decidable quadratic real
 *        field.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section quadratic__A_Real_Field_You_Can_Compute
 * @c Cut<Q> (see @c :cut) reifies the @b order of the reals decidably but stops
 * at the leaf: it cannot @b add two irrationals (the sum of two cuts is a
 * Minkowski sum whose membership is @f$\exists@f$-over-@f$\mathbb{Q}@f$, and
 * the order of general sums forces the undecidable equality of reals).  @c
 * QuadraticReal steps just far enough to keep @b everything decidable: it fixes
 * @b one radical extension @f$\mathbb{Q}(\sqrt D)@f$ (@f$D@f$ a non-square
 * positive integer) and represents a real as @f$a+b\sqrt
 * D,\ a,b\in\mathbb{Q}@f$. On this carrier the field operations
 * @f$+,-,\times,\div@f$ close @b exactly (conjugation gives the inverse:
 * @f$1/(a+b\sqrt D)=(a-b\sqrt D)/(a^2-b^2D)@f$),
 * @b and the order stays total and decidable (compare @f$a+b\sqrt D@f$ to 0 by
 * comparing @f$a^2@f$ to @f$b^2D@f$).  So the field laws are @b witnessed by
 * computation --- @f$(1+\sqrt2)(1-\sqrt2)=-1@f$ is a @c static_assert --- not
 * postulated on an uninhabited carrier.  It @b models @f$\mathbb{Q}(\sqrt
 * D)\subset\mathbb{R}@f$, one extension, not all of @f$\mathbb{R}@f$: the
 * honest price of decidability.
 *
 * @note "Wir müssen wissen. Wir werden wissen." — David Hilbert (1930).
 */
module;

#include <cassert>  // nonzero-norm guard on inverse()
#include <compare>
#include <concepts>
#include <functional>
#include <type_traits>

export module dedekind.numbers:quadratic;

import dedekind.algebra; // IsField / HasFieldOperators (the field witnesses)
import dedekind.category;
import dedekind.order;
import dedekind.sets;
import :rational;

namespace dedekind::numbers {
using namespace dedekind::category;

namespace detail_quadratic {
/** @brief Is @p n a perfect square?  Guards @c QuadraticReal against a square
 *  radicand (where @f$\sqrt D@f$ is rational, the a/b representation is not
 *  unique, and the "extension" degenerates to @f$\mathbb{Q}@f$). */
consteval bool is_perfect_square(long n) {
  if (n < 0) return false;
  if (n < 2) return true;  // 0, 1 are squares
  // Binary-search the root comparing @c mid to @c n/mid (never forming
  // @c mid*mid), so every positive @c long is checked without overflow.
  for (long lo = 1, hi = n; lo <= hi;) {
    const long mid = lo + (hi - lo) / 2;
    const long q = n / mid;
    if (q == mid && n % mid == 0) return true;  // mid² == n
    if (mid <= q)
      lo = mid + 1;  // mid² ≤ n → root ≥ mid
    else
      hi = mid - 1;  // mid² > n
  }
  return false;
}
}  // namespace detail_quadratic

/**
 * @class QuadraticReal
 * @brief A real of @f$\mathbb{Q}(\sqrt D)@f$: @f$a+b\sqrt D@f$.
 *
 * @tparam D the (non-square, positive) radicand, carried in the type so
 *         @f$\mathbb{Q}(\sqrt2)@f$ and @f$\mathbb{Q}(\sqrt3)@f$ are distinct
 *         fields.
 * @tparam Q the rational coefficient carrier.
 */
export template <long D, typename Q = Rational<default_integer>>
  requires(D > 1) && std::three_way_comparable<Q, std::strong_ordering>
class QuadraticReal {
  // Q must be STRONGLY ordered (the decidable field order returns
  // std::strong_ordering; a partial carrier like floating-point cannot satisfy
  // it) and exact.  The default Rational is strongly ordered and exact for all
  // practical purposes — its precision boundary is physical/data-type, not a
  // silent wraparound (see is_exact_total).
  static_assert(
      !detail_quadratic::is_perfect_square(D),
      "QuadraticReal<D> requires a NON-square D — else √D is rational "
      "and ℚ(√D) degenerates to ℚ with a non-unique a+b√D form.");

 public:
  using Domain = QuadraticReal;
  using ScalarCarrier = Q;

  /** @brief @f$0@f$. */
  constexpr QuadraticReal() = default;
  /** @brief A rational @b is a real of the field: @f$a+0\sqrt D@f$. */
  constexpr QuadraticReal(Q a)  // NOLINT(google-explicit-constructor)
      : a_(a) {}
  /** @brief Single-step lift @c V @c → @c Q @c → field (e.g.\ an @c int). */
  template <typename V>
    requires(!std::same_as<V, Q> && std::convertible_to<V, Q>)
  constexpr QuadraticReal(V v)  // NOLINT(google-explicit-constructor)
      : a_(Q{v}) {}

  /** @brief The generator @f$\sqrt D@f$ (@f$0+1\sqrt D@f$). */
  static constexpr QuadraticReal root() { return of(Q{}, Q{1}); }
  /** @brief @f$a+b\sqrt D@f$ from its parts. */
  static constexpr QuadraticReal of(Q a, Q b) { return QuadraticReal{a, b}; }

  constexpr Q rational_part() const { return a_; }
  constexpr Q radical_part() const { return b_; }

  /** @name Field operations (closed, exact) @{ */
  constexpr QuadraticReal operator-() const { return of(-a_, -b_); }
  friend constexpr QuadraticReal operator+(const QuadraticReal& x,
                                           const QuadraticReal& y) {
    return of(x.a_ + y.a_, x.b_ + y.b_);
  }
  friend constexpr QuadraticReal operator-(const QuadraticReal& x,
                                           const QuadraticReal& y) {
    return of(x.a_ - y.a_, x.b_ - y.b_);
  }
  friend constexpr QuadraticReal operator*(const QuadraticReal& x,
                                           const QuadraticReal& y) {
    // (a+b√D)(a'+b'√D) = (aa' + bb'D) + (ab' + a'b)√D
    return of(x.a_ * y.a_ + x.b_ * y.b_ * Q{D}, x.a_ * y.b_ + y.a_ * x.b_);
  }
  /** @brief @f$1/(a+b\sqrt D)@f$ via the conjugate; the norm @f$a^2-b^2D@f$ is
   *  nonzero for every nonzero element (@f$\sqrt D@f$ irrational). */
  constexpr QuadraticReal inverse() const {
    const Q norm = a_ * a_ - b_ * b_ * Q{D};
    assert(norm != Q{});  // norm = 0 iff *this = 0, which has no inverse
    return of(a_ / norm, -b_ / norm);
  }
  friend constexpr QuadraticReal operator/(const QuadraticReal& x,
                                           const QuadraticReal& y) {
    return x * y.inverse();
  }
  /** @} */

  /** @brief Extrema of the singleton @f$\{*this\}@f$ (inf = sup = the value).
   *  Supplies @c HasExtrema, the structural prerequisite for
   *  @c IsDedekindComplete (matching @c ExactReal). */
  constexpr QuadraticReal infimum() const { return *this; }
  constexpr QuadraticReal supremum() const { return *this; }

  /** @brief Total, @b decidable order: sign of @f$(a-a')+(b-b')\sqrt D@f$ by
   *  comparing squares.  This is what keeps @f$\mathbb{Q}(\sqrt D)@f$ a genuine
   *  @c IsTotallyOrdered field rather than a Ternary residue. */
  friend constexpr std::strong_ordering operator<=>(const QuadraticReal& x,
                                                    const QuadraticReal& y) {
    return sign_of(x.a_ - y.a_, x.b_ - y.b_);
  }
  friend constexpr bool operator==(const QuadraticReal& x,
                                   const QuadraticReal& y) {
    return x.a_ == y.a_ && x.b_ == y.b_;  // canonical: √D irrational
  }

 private:
  constexpr QuadraticReal(Q a, Q b) : a_(a), b_(b) {}

  /** @brief Sign of @f$a+b\sqrt D@f$ (@f$D>1@f$) as an ordering against 0. */
  static constexpr std::strong_ordering sign_of(const Q& a, const Q& b) {
    const Q zero{};
    if (b == zero) return a <=> zero;
    if (a == zero) return b <=> zero;  // √D > 0
    const bool a_pos = a > zero;
    const bool b_pos = b > zero;
    if (a_pos && b_pos) return std::strong_ordering::greater;
    if (!a_pos && !b_pos) return std::strong_ordering::less;
    // Mixed signs: the sum's sign is decided by a² <=> b²D.
    const std::strong_ordering sq = (a * a) <=> (b * b * Q{D});
    return a_pos ? sq : reverse(sq);  // a>0,b<0 ⇒ sq; a<0,b>0 ⇒ reverse
  }

  Q a_{};  ///< rational part
  Q b_{};  ///< coefficient of √D
};

}  // namespace dedekind::numbers

namespace dedekind::category {

/** @brief Atlas registration: @c QuadraticReal<D,Q> is a first-class species.
 */
template <long D, typename Q>
struct SpeciesTraits<dedekind::numbers::QuadraticReal<D, Q>> {
  using Domain = dedekind::numbers::QuadraticReal<D, Q>;
  using machine_type = dedekind::numbers::QuadraticReal<D, Q>;
};

/** @section quadratic__Order_Fabric
 *  The order is a genuine total order (decided by comparing squares), so the
 *  poset traits are earned; registered parametrically over @c (D,Q). */
template <long D, typename Q>
inline constexpr bool
    is_reflexive_v<dedekind::numbers::QuadraticReal<D, Q>, std::less_equal<>> =
        true;
template <long D, typename Q>
inline constexpr bool
    is_transitive_v<dedekind::numbers::QuadraticReal<D, Q>, std::less_equal<>> =
        true;
template <long D, typename Q>
inline constexpr bool is_antisymmetric_v<dedekind::numbers::QuadraticReal<D, Q>,
                                         std::less_equal<>> = true;

/** @section quadratic__Field_Axioms
 *  ℚ(√D) is a GENUINE field: every axiom below is @b witnessed by computation
 *  in the numbers-namespace static_asserts (√2·√2=2, (1+√2)(1−√2)=−1,
 *  √2·(1/√2)=1, associativity / commutativity / distributivity).  Registered so
 *  @c category::IsField holds --- the honest opposite of a postulated field on
 *  an uninhabited carrier.  @c is_exact_total (Path D) admits @c + and @c * as
 *  total (exact & unbounded), lifting ℚ(√D) to a Magma and thence up the
 *  ring/field chain to @c IsField. */
template <long D, typename Q>
struct is_exact_total<dedekind::numbers::QuadraticReal<D, Q>,
                      std::plus<dedekind::numbers::QuadraticReal<D, Q>>>
    : std::true_type {};
template <long D, typename Q>
struct is_exact_total<dedekind::numbers::QuadraticReal<D, Q>,
                      std::multiplies<dedekind::numbers::QuadraticReal<D, Q>>>
    : std::true_type {};

template <long D, typename Q>
struct is_associative<dedekind::numbers::QuadraticReal<D, Q>,
                      std::plus<dedekind::numbers::QuadraticReal<D, Q>>>
    : std::true_type {};
template <long D, typename Q>
struct is_associative<dedekind::numbers::QuadraticReal<D, Q>,
                      std::multiplies<dedekind::numbers::QuadraticReal<D, Q>>>
    : std::true_type {};
template <long D, typename Q>
struct is_commutative<dedekind::numbers::QuadraticReal<D, Q>,
                      std::plus<dedekind::numbers::QuadraticReal<D, Q>>>
    : std::true_type {};
template <long D, typename Q>
struct is_commutative<dedekind::numbers::QuadraticReal<D, Q>,
                      std::multiplies<dedekind::numbers::QuadraticReal<D, Q>>>
    : std::true_type {};

template <long D, typename Q>
inline constexpr bool
    is_distributive_v<dedekind::numbers::QuadraticReal<D, Q>,
                      std::multiplies<dedekind::numbers::QuadraticReal<D, Q>>,
                      std::plus<dedekind::numbers::QuadraticReal<D, Q>>> = true;

template <long D, typename Q>
inline constexpr bool
    is_invertible_v<dedekind::numbers::QuadraticReal<D, Q>,
                    std::plus<dedekind::numbers::QuadraticReal<D, Q>>> = true;
template <long D, typename Q>
inline constexpr bool
    is_invertible_v<dedekind::numbers::QuadraticReal<D, Q>,
                    std::multiplies<dedekind::numbers::QuadraticReal<D, Q>>> =
        true;

template <long D, typename Q>
struct identity_trait<dedekind::numbers::QuadraticReal<D, Q>,
                      std::plus<dedekind::numbers::QuadraticReal<D, Q>>> {
  using value_type = dedekind::numbers::QuadraticReal<D, Q>;
  static constexpr value_type value{};  // 0
};
template <long D, typename Q>
struct identity_trait<dedekind::numbers::QuadraticReal<D, Q>,
                      std::multiplies<dedekind::numbers::QuadraticReal<D, Q>>> {
  using value_type = dedekind::numbers::QuadraticReal<D, Q>;
  static constexpr value_type value{1};  // 1
};

template <long D, typename Q>
struct inverse_trait<dedekind::numbers::QuadraticReal<D, Q>,
                     std::plus<dedekind::numbers::QuadraticReal<D, Q>>> {
  static constexpr bool exists = true;
  using value_type = dedekind::numbers::QuadraticReal<D, Q>;
  static constexpr value_type compute(const value_type& x) { return -x; }
};

}  // namespace dedekind::category

namespace dedekind::numbers {

/** @section quadratic__Formal_Verification
 *  The field laws are @b witnessed by computation over @f$\mathbb{Q}(\sqrt2)@f$
 *  --- the honest contrast with a postulated field on an uninhabited carrier.
 */
namespace {
using R2 = QuadraticReal<2>;
constexpr R2 root2 = R2::root();  // √2
}  // namespace

static_assert(std::regular<R2>, "ℚ(√2) is a value type.");
static_assert(dedekind::category::IsSpecies<R2>, "ℚ(√2) is a species.");
static_assert(dedekind::order::IsTotallyOrdered<R2>,
              "ℚ(√2) is totally ordered — decidably.");
static_assert(dedekind::algebra::HasFieldOperators<R2>,
              "ℚ(√2) closes the field operator surface (+,−,*,/,T{1}).");

// √2 is a genuine field element with √2·√2 = 2, between 1 and 2.
static_assert(root2 * root2 == R2{2}, "√2·√2 = 2 (exact).");
static_assert(R2{1} < root2 && root2 < R2{2}, "1 < √2 < 2.");
// The conjugate identity — a field-law witness, computed not declared.
static_assert((R2{1} + root2) * (R2{1} - root2) == R2{-1},
              "(1+√2)(1−√2) = −1.");
// Multiplicative inverse closes: √2 · (1/√2) = 1.
static_assert(root2 * root2.inverse() == R2{1}, "√2 · (1/√2) = 1.");
static_assert(R2{1} / root2 == R2::of(Rational<>{}, Rational<>{1, 2}),
              "1/√2 = ½√2.");
// Associativity / commutativity / distributivity, WITNESSED (not postulated).
static_assert((root2 + R2{3}) + R2{5} == root2 + (R2{3} + R2{5}), "+ assoc.");
static_assert(root2 * R2{3} == R2{3} * root2, "× comm.");
static_assert(root2 * (R2{2} + root2) == root2 * R2{2} + root2 * root2,
              "× distributes over +.");

// The concept-level payoff: ℚ(√2) is a bona-fide FIELD — not just field-shaped
// operators, but the axiomatic IsField, now that Path-D (exact) totality lifts
// it to a Magma.  Ω⟨ℚ(√2)⟩ is a real field as a first-class C++ value, exactly
// the way ℚ = Ω⟨Rational⟩ is — the algebraic coat-hanger, on the reals.
static_assert(
    dedekind::category::IsField<R2, std::plus<R2>, std::multiplies<R2>>,
    "ℚ(√2)'s carrier satisfies the axiomatic IsField.");
static_assert(
    dedekind::algebra::IsField<
        std::remove_cvref_t<decltype(dedekind::sets::Ω<R2>)>>,
    "Ω⟨ℚ(√2)⟩ is a field as a first-class value, like ℚ = Ω⟨Rational⟩.");

// The topological half of the coat-hanger, paired with IsField: ℚ(√2) is
// order-complete — totally ordered + dense (midpoint (a+b)/T{2}) + extrema.
// IsDedekindComplete is the library's STRUCTURAL surrogate (totally ordered +
// dense + extrema), which ℚ itself also passes; ℚ(√2) is countable and so not
// genuinely order-complete.  What is honest here is the concept pair holding on
// one real value: a field AND the completeness surrogate — like ℚ =
// Ω⟨Rational⟩.
static_assert(dedekind::order::IsDedekindComplete<R2>,
              "ℚ(√2) satisfies the structural IsDedekindComplete surrogate, "
              "paired with IsField on the same real value.");

}  // namespace dedekind::numbers
