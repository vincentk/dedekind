/**
 * @file dedekind/algebra/polynomial.cppm
 * @partition :polynomial
 * @brief The monoid algebra R[M]: polynomials as finitely supported nets over
 *        the monomials, and the normal form of a rig's, ring's or field's
 *        terms.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section polynomial__The_Monoid_Algebra
 * A polynomial over a commutative rig @c R in the variables @c π, @c π1,
 * @c π2, @c π3 is a finitely supported function from the monomials (the free
 * commutative monoid on the variables, @c Monomial<N>) to @c R: the monoid
 * algebra @c R[M] (Lang, Algebra, §II.3).  Addition
 * is pointwise; multiplication is the convolution Σ_{m·m' = n} a_m b_{m'},
 * the Cauchy product in one variable.  A formal power series @c R[[M]] is any
 * function @c M @c → @c R; a polynomial is one with finite support, and it is
 * the support that is stored.
 *
 * In the DSL's grammar a polynomial is a @b net (@c sequences::IsNet), an
 * arrow from a directed set: the monomials ordered by total degree and then
 * lexicographically (the graded lexicographic order; Cox, Little, O'Shea,
 * §2.2), a chain compatible with multiplication, along which the support is
 * stored sorted.  The arrow's graph restricted to the support is a finite
 * functional graph: the finite function the normal form is compared and
 * printed by.
 *
 * @section polynomial__The_Normal_Form_Of_Terms
 * The reading @c Poly<C> is an algebra for @c cata over the term grammar
 * (@c sets:terms, @c terms__Signature_And_Free_Algebra): a projection is its
 * variable, a constant a coefficient, @c + @c - @c * the polynomial's, and a
 * division by a nonzero constant on a field a multiplication by its inverse.
 * Every operator must be admitted on the carrier (@c admits: the laws decide
 * the syntax), and @c //, @c % and a division by a term have no free algebra
 * here: they are refused, never rewritten.  @c normal_form<C>(t) is the
 * fold; two terms are equal in the free algebra iff their normal forms are
 * equal, which is equality of two finite functions, monomial to coefficient.
 *
 * Over a finite carrier that is not equality of the polynomial @b functions:
 * on 𝔽ₚ, @c πᵖ and @c π agree at every point, and on ℤ/2⁶⁴ so do
 * @c 2⁶³·π·(π+1) and @c 0.  That quotient (by the vanishing ideal) is a
 * further reading, not this one.
 *
 * The exponents are machine words, so @c Monomial<N> is the type
 * (ℤ/2³²)ᴺ, not the set ℕᴺ: its product is certified total because it wraps
 * (path A of @c category:total), the same posture as @c u64 itself.  It is the
 * free commutative monoid on the slots below total degree 2³², which is where
 * every term written by hand lives.
 *
 * Wikipedia: Monoid ring, Polynomial ring, Monomial order
 *
 * @note "Die ganzen Zahlen hat der liebe Gott gemacht, alles andere ist
 * Menschenwerk."
 * [Trans: "God made the integers; all else is the work of man."]
 * Leopold Kronecker, as reported by Heinrich Weber, "Leopold Kronecker",
 * Jahresbericht der DMV 2 (1893), p. 19.
 */
module;

#include <algorithm>    // std::sort
#include <array>        // Monomial's exponents
#include <compare>      // std::strong_ordering
#include <concepts>     // std::equality_comparable
#include <cstddef>      // std::size_t
#include <functional>   // std::plus, std::minus, std::multiplies, std::divides
#include <stdexcept>    // std::domain_error, std::out_of_range
#include <string>       // the refusals' text
#include <type_traits>  // std::true_type
#include <utility>      // std::pair
#include <vector>       // the support

export module dedekind.algebra:polynomial;

import dedekind.category;
import dedekind.sets;      // the term grammar: Projection, Const, BinOp, Term
import dedekind.order;     // IsTotallyOrdered: the monomials are a chain
import dedekind.sequences; // IsNet: a polynomial is a net over the monomials
import :field;             // FieldDivision's rule, for a term's / by a constant
import :free;              // free_algebra_base: R[M] is free over R

namespace dedekind::algebra {

using namespace dedekind::category;

// ── Monomials ───────────────────────────────────────────────────────────────

/** @brief A monomial in @c N variables: its exponents, one per slot (@c π is
 *  the slot 0; @c π1, @c π2, @c π3 the slots 1, 2, 3).  The free commutative
 *  monoid on @c N generators, written multiplicatively (the exponents, machine
 *  words, add and wrap with the word), and a chain under the graded
 *  lexicographic order: by total degree, then by exponents.
 *  @tparam N the number of variables. */
export template <std::size_t N>
struct Monomial {
  std::array<unsigned, N> e{};

  /** @brief The empty product, the monoid's unit. */
  static constexpr Monomial one() { return {}; }
  /** @brief The variable of @c slot.  @throws std::out_of_range beyond @c N. */
  static constexpr Monomial variable(std::size_t slot) {
    if (slot >= N) throw std::out_of_range("a variable the monomials have not");
    Monomial m;
    m.e[slot] = 1;
    return m;
  }
  /** @brief The total degree. */
  constexpr unsigned degree() const {
    unsigned d = 0;
    for (unsigned k : e) d += k;
    return d;
  }
  /** @brief The exponent of @c slot.  @throws std::out_of_range beyond
   *  @c N. */
  constexpr unsigned exponent(std::size_t slot) const { return e.at(slot); }
  /** @brief This monomial with one factor of @c slot removed; defined where
   *  @c exponent(slot) is positive (the derivative's use). */
  constexpr Monomial without(std::size_t slot) const {
    Monomial m = *this;
    --m.e.at(slot);
    return m;
  }
  friend constexpr Monomial operator*(const Monomial& a, const Monomial& b) {
    Monomial m;
    for (std::size_t i = 0; i < N; ++i) m.e[i] = a.e[i] + b.e[i];
    return m;
  }
  friend constexpr bool operator==(const Monomial&, const Monomial&) = default;
  /** @brief The graded lexicographic order. */
  friend constexpr std::strong_ordering operator<=>(const Monomial& a,
                                                    const Monomial& b) {
    if (const auto by_degree = a.degree() <=> b.degree(); by_degree != 0)
      return by_degree;
    return a.e <=> b.e;
  }
};

// ── Polynomials ─────────────────────────────────────────────────────────────

namespace detail_polynomial {
/** @brief The support's order: by monomial. */
struct ByMonomial {
  template <typename M, typename R>
  constexpr bool operator()(const std::pair<M, R>& a,
                            const std::pair<M, R>& b) const {
    return a.first < b.first;
  }
};
}  // namespace detail_polynomial

/** @brief The monoid algebra @c R[M]: a finitely supported function from the
 *  monomials to the coefficients, stored as its support sorted along the
 *  monomials' chain, no coefficient zero.  A net (@c Domain the monomials,
 *  @c Codomain the coefficients, @c p(m) the coefficient of @c m), a
 *  commutative rig over a commutative rig, a ring over a ring (@c - where
 *  @c R has additive inverses).  @tparam R the coefficients.  @tparam M the
 *  monomials: a commutative monoid that is a chain. */
export template <typename R, typename M = Monomial<1>>
  requires std::equality_comparable<R> &&
           dedekind::category::IsCommutativeMonoid<M, std::multiplies<M>> &&
           dedekind::order::IsTotallyOrdered<M>
class Polynomial {
 public:
  using coefficient_type = R;
  using monomial_type = M;
  using Domain = M;
  using Codomain = R;
  using term_type = std::pair<M, R>;

  static constexpr Polynomial zero() { return {}; }
  static constexpr Polynomial one() { return Polynomial(unit()); }
  /** @brief The variable of @c slot, as a polynomial. */
  static constexpr Polynomial variable(std::size_t slot) {
    return Polynomial({{M::variable(slot), unit()}});
  }

  /** @brief The zero polynomial. */
  constexpr Polynomial() = default;
  /** @brief A constant. */
  constexpr explicit Polynomial(R scalar) {
    if (scalar != zero_coefficient())
      terms_.emplace_back(M::one(), std::move(scalar));
  }
  /** @brief The sum of the given terms, in any order: combined, sorted, zero
   *  coefficients dropped.  @param terms monomial and coefficient pairs. */
  constexpr explicit Polynomial(std::vector<term_type> terms)
      : terms_(std::move(terms)) {
    canonicalize();
  }

  /** @brief The net: the coefficient of @c m, zero off the support. */
  constexpr R operator()(const M& m) const {
    const auto at = std::lower_bound(terms_.begin(), terms_.end(),
                                     term_type{m, zero_coefficient()},
                                     detail_polynomial::ByMonomial{});
    return at != terms_.end() && at->first == m ? at->second
                                                : zero_coefficient();
  }
  /** @brief The support with its coefficients: the finite function this
   *  polynomial is, sorted along the monomials' chain. */
  constexpr const std::vector<term_type>& support() const noexcept {
    return terms_;
  }
  constexpr bool is_zero() const { return terms_.empty(); }
  /** @brief The total degree of the leading monomial; 0 for the zero
   *  polynomial. */
  constexpr unsigned degree() const {
    return terms_.empty() ? 0 : terms_.back().first.degree();
  }
  /** @throws std::domain_error on the zero polynomial. */
  constexpr const R& leading_coefficient() const {
    if (terms_.empty())
      throw std::domain_error("the zero polynomial has no leading coefficient");
    return terms_.back().second;
  }

  friend constexpr bool operator==(const Polynomial&,
                                   const Polynomial&) = default;

  /** @brief Pointwise: the merge of two sorted supports. */
  friend constexpr Polynomial operator+(const Polynomial& a,
                                        const Polynomial& b) {
    std::vector<term_type> sum;
    sum.reserve(a.terms_.size() + b.terms_.size());
    sum.insert(sum.end(), a.terms_.begin(), a.terms_.end());
    sum.insert(sum.end(), b.terms_.begin(), b.terms_.end());
    return Polynomial(std::move(sum));
  }
  /** @brief Coefficientwise additive inverse, where @c R has one. */
  friend constexpr Polynomial operator-(const Polynomial& a)
    requires is_invertible_v<R, std::plus<R>>
  {
    std::vector<term_type> neg = a.terms_;
    for (term_type& t : neg) t.second = negated(t.second);
    return Polynomial(std::move(neg));
  }
  friend constexpr Polynomial operator-(const Polynomial& a,
                                        const Polynomial& b)
    requires is_invertible_v<R, std::plus<R>>
  {
    return a + (-b);
  }
  /** @brief The additive inverse under the category's @c inverse_v bridge, so
   *  @c is_invertible_v<Polynomial, std::plus<Polynomial>> follows from
   *  @c R's. */
  friend constexpr Polynomial inverse(const Polynomial& a,
                                      std::plus<Polynomial>)
    requires is_invertible_v<R, std::plus<R>>
  {
    return -a;
  }
  /** @brief The convolution: every pair of terms multiplied, then combined. */
  friend constexpr Polynomial operator*(const Polynomial& a,
                                        const Polynomial& b) {
    std::vector<term_type> product;
    product.reserve(a.terms_.size() * b.terms_.size());
    for (const term_type& x : a.terms_)
      for (const term_type& y : b.terms_)
        product.emplace_back(x.first * y.first, x.second * y.second);
    return Polynomial(std::move(product));
  }
  /** @brief The formal partial derivative by the variable of @c slot:
   *  @c ∂(c·m) @c = @c e·c·m/π, the factor @c e by repeated doubling, so no
   *  more than @c R's addition is asked (a rig suffices). */
  constexpr Polynomial derive(std::size_t slot = 0) const {
    std::vector<term_type> d;
    for (const term_type& t : terms_) {
      const unsigned e = t.first.exponent(slot);
      if (e == 0) continue;
      d.emplace_back(t.first.without(slot), times_natural(t.second, e));
    }
    return Polynomial(std::move(d));
  }

 private:
  std::vector<term_type> terms_;

  static constexpr R zero_coefficient() { return identity_v<R, std::plus<R>>; }
  static constexpr R unit() { return identity_v<R, std::multiplies<R>>; }
  /** @brief The additive inverse of a coefficient, where @c R certifies one:
   *  the category's @c inverse_v bridge where @c R registers an @c inverse,
   *  else @c 0 @c - @c c (a carrier whose certificate is the trait alone). */
  static constexpr R negated(const R& c) {
    if constexpr (requires { inverse_v<R, std::plus<R>>(c); })
      return inverse_v<R, std::plus<R>>(c);
    else
      return zero_coefficient() - c;
  }
  /** @brief @c n·c by repeated doubling: O(log n) additions. */
  static constexpr R times_natural(R c, unsigned n) {
    R acc = zero_coefficient();
    for (; n > 0; n >>= 1) {
      if (n & 1u) acc = acc + c;
      c = c + c;
    }
    return acc;
  }
  /** @brief Sort by monomial, add equal monomials' coefficients, drop zeros. */
  constexpr void canonicalize() {
    std::sort(terms_.begin(), terms_.end(), detail_polynomial::ByMonomial{});
    std::vector<term_type> out;
    out.reserve(terms_.size());
    for (term_type& t : terms_) {
      if (!out.empty() && out.back().first == t.first)
        out.back().second = out.back().second + t.second;
      else
        out.push_back(std::move(t));
      if (out.back().second == zero_coefficient()) out.pop_back();
    }
    terms_ = std::move(out);
  }
};

// ── The normal form of terms ────────────────────────────────────────────────

/** @brief The polynomial reading of a term on the carrier @c C, an algebra for
 *  @c cata: a projection is its variable, a constant a coefficient, @c + @c -
 *  @c * the polynomial's where the carrier admits them, a division by a
 *  nonzero constant on a field the multiplication by its inverse.  Any other
 *  node (@c //, @c %, a division by a term, an applied arrow, a conditional)
 *  has no free algebra catalogued here and is refused.  @tparam C the
 *  carrier.  @tparam N the variables: the slots @c π, @c π1, @c π2, @c π3. */
export template <typename C, std::size_t N = 4>
struct Poly {
  using P = Polynomial<C, Monomial<N>>;

  template <dedekind::sets::IsRingIntegral auto I>
  constexpr P operator()(dedekind::sets::Projection<I>) const {
    return P::variable(static_cast<std::size_t>(I));
  }
  template <typename K>
  constexpr P operator()(const dedekind::sets::Const<K>& c) const {
    return P(dedekind::sets::carrier_cast<C>(c.value));
  }
  template <typename Op, typename A, typename B>
  constexpr P operator()(const dedekind::sets::BinOp<Op, A, B>&, const P& x,
                         const P& y) const {
    static_assert(std::same_as<Op, std::plus<>> ||
                      std::same_as<Op, std::minus<>> ||
                      std::same_as<Op, std::multiplies<>>,
                  "no free algebra catalogued for this term: a polynomial "
                  "reads + - * and a field's division by a nonzero constant");
    static_assert(dedekind::sets::admits(dedekind::sets::On<C>{}, Op{}, false),
                  "the operator has no rule on the carrier: not in the "
                  "signature of its certified algebras");
    return Op{}(x, y);
  }
  template <typename A, typename K>
  constexpr P operator()(const dedekind::sets::BinOp<
                             std::divides<>, A, dedekind::sets::Const<K>>& t,
                         const P& x, const P&) const {
    static_assert(dedekind::sets::admits(dedekind::sets::On<C>{},
                                         dedekind::sets::FieldDivision{}, true),
                  "a division by a constant has a polynomial on a field only: "
                  "the inverse is the coefficient (import algebra)");
    return x * P(reciprocal(dedekind::sets::carrier_cast<C>(t.b.value)));
  }
  template <typename Node, typename... Legs>
  constexpr P operator()(const Node&, const Legs&...) const {
    static_assert(no_free_algebra<Node>,
                  "no free algebra catalogued for this term: a polynomial "
                  "reads + - * and a field's division by a nonzero constant");
    return {};
  }

  /** @brief A term built at run time: each node checked against the carrier's
   *  rules as it is read, the refusal @c not_admitted. */
  template <typename K>
  P steer(const dedekind::sets::Term<K>& t) const {
    using T = dedekind::sets::Term<K>;
    using dedekind::sets::admits;
    using dedekind::sets::Arith;
    using dedekind::sets::On;
    switch (t.kind) {
      case T::Kind::Proj:
        return P::variable(t.slot);
      case T::Kind::Const:
        return P(dedekind::sets::carrier_cast<C>(t.value));
      case T::Kind::Bin: {
        const P x = steer(*t.a);
        switch (t.op) {
          case Arith::Add:
            if (admits(On<C>{}, std::plus<>{}, false)) return x + steer(*t.b);
            break;
          case Arith::Sub:
            if constexpr (requires(const P& a, const P& b) { a - b; }) {
              if (admits(On<C>{}, std::minus<>{}, false))
                return x - steer(*t.b);
            }
            break;
          case Arith::Mul:
            if (admits(On<C>{}, std::multiplies<>{}, false))
              return x * steer(*t.b);
            break;
          case Arith::Div:
            if constexpr (requires(const C& a, const C& b) { a / b; }) {
              if (t.b->kind == T::Kind::Const &&
                  admits(On<C>{}, dedekind::sets::FieldDivision{}, true))
                return x * P(reciprocal(
                               dedekind::sets::carrier_cast<C>(t.b->value)));
            }
            break;
          case Arith::FloorDiv:
          case Arith::Mod:
            break;
        }
        throw dedekind::sets::not_admitted(
            std::string("no free algebra catalogued for ") +
            dedekind::sets::spelling(t.op) +
            " here: a polynomial reads + - * and a field's division by a "
            "nonzero constant");
      }
    }
    throw dedekind::sets::not_admitted("an unknown term");
  }

 private:
  template <typename>
  static constexpr bool no_free_algebra = false;
  /** @brief @c 1/k on the field; a @c k that is 0 on the carrier (p on 𝔽ₚ)
   *  is refused. */
  static constexpr C reciprocal(const C& k) {
    if (k == identity_v<C, std::plus<C>>)
      throw std::domain_error(
          "a division by a constant that is 0 on the carrier");
    return identity_v<C, std::multiplies<C>> / k;
  }
};

/** @brief The normal form of the term @c t in the polynomials over @c C: the
 *  fold of @c Poly<C>.  Two terms are equal in the free algebra iff their
 *  normal forms are.  @tparam C the carrier.  @param t a typed term, or one
 *  built at run time. */
export template <typename C, typename T>
constexpr auto normal_form(const T& t) {
  return cata(Poly<C>{}, t);
}

}  // namespace dedekind::algebra

// ── Registrations ───────────────────────────────────────────────────────────
namespace dedekind::category {

template <std::size_t N>
struct SpeciesTraits<dedekind::algebra::Monomial<N>> {
  using Domain = dedekind::algebra::Monomial<N>;
  using machine_type = dedekind::algebra::Monomial<N>;
};
// The graded lexicographic order is a chain (the three partial-order laws;
// connexity is the defaulted <=>).
template <std::size_t N>
inline constexpr bool
    is_reflexive_v<dedekind::algebra::Monomial<N>, std::less_equal<>> = true;
template <std::size_t N>
inline constexpr bool
    is_transitive_v<dedekind::algebra::Monomial<N>, std::less_equal<>> = true;
template <std::size_t N>
inline constexpr bool
    is_antisymmetric_v<dedekind::algebra::Monomial<N>, std::less_equal<>> =
        true;
// The free commutative monoid: exponents add.
template <std::size_t N>
inline constexpr bool
    is_associative_v<dedekind::algebra::Monomial<N>,
                     std::multiplies<dedekind::algebra::Monomial<N>>> = true;
template <std::size_t N>
inline constexpr bool
    is_commutative_v<dedekind::algebra::Monomial<N>,
                     std::multiplies<dedekind::algebra::Monomial<N>>> = true;
template <std::size_t N>
struct identity_trait<dedekind::algebra::Monomial<N>,
                      std::multiplies<dedekind::algebra::Monomial<N>>> {
  static constexpr dedekind::algebra::Monomial<N> value{};
};
// Totality: the exponents are unsigned words adding componentwise, so the
// product wraps with them (path A of category:total, as the word's own +).
template <std::size_t N>
struct is_periodic<dedekind::algebra::Monomial<N>,
                   std::multiplies<dedekind::algebra::Monomial<N>>>
    : std::true_type {};

// R[M] is the free commutative R-algebra on M's generators (Lang §II.3):
// its laws and totality are read off R by construction (category:total; + is
// coefficientwise, × multiplies out).
template <typename R, typename M>
struct free_algebra_base<dedekind::algebra::Polynomial<R, M>> {
  using type = R;
};
template <typename R, typename M>
struct identity_trait<dedekind::algebra::Polynomial<R, M>,
                      std::plus<dedekind::algebra::Polynomial<R, M>>> {
  static constexpr dedekind::algebra::Polynomial<R, M> value{};
};
template <typename R, typename M>
struct identity_trait<dedekind::algebra::Polynomial<R, M>,
                      std::multiplies<dedekind::algebra::Polynomial<R, M>>> {
  static constexpr dedekind::algebra::Polynomial<R, M> value =
      dedekind::algebra::Polynomial<R, M>::one();
};
// Periodicity is coefficientwise: if R's operation wraps, so does R[M]'s.
template <typename R, typename M>
  requires is_periodic_v<R, std::plus<R>>
struct is_periodic<dedekind::algebra::Polynomial<R, M>,
                   std::plus<dedekind::algebra::Polynomial<R, M>>>
    : std::true_type {};
template <typename R, typename M>
  requires is_periodic_v<R, std::multiplies<R>>
struct is_periodic<dedekind::algebra::Polynomial<R, M>,
                   std::multiplies<dedekind::algebra::Polynomial<R, M>>>
    : std::true_type {};

}  // namespace dedekind::category

namespace dedekind::algebra {

/** @section polynomial__Formal_Verification */
using M4 = Monomial<4>;
static_assert(dedekind::category::IsCommutativeMonoid<M4, std::multiplies<M4>>,
              "the monomials are the free commutative monoid on the slots");
static_assert(dedekind::order::IsTotallyOrdered<M4> &&
                  dedekind::order::IsDirectedSet<M4>,
              "the graded lexicographic order is a chain, so a directed set");
static_assert(M4::variable(1) * M4::variable(2) ==
                      M4::variable(2) * M4::variable(1) &&
                  M4::variable(0) * M4::variable(0) > M4::variable(3),
              "π1·π2 = π2·π1; π² is above π3: degree first, then the slots");

using PolyWord = Polynomial<unsigned int, M4>;
static_assert(dedekind::sequences::IsNet<PolyWord>,
              "a polynomial is a net over the monomials: an arrow from a "
              "directed set");
static_assert(dedekind::category::IsCommutativeRing<
                  PolyWord, std::plus<PolyWord>, std::multiplies<PolyWord>>,
              "ℤ/2³²[π, π1, π2, π3] is a commutative ring: the coefficients' "
              "laws by construction");
static_assert(dedekind::category::IsFreeAlgebra<PolyWord>,
              "R[M] is free over R");

// The normal form decides equality in the free algebra, at compile time.
using dedekind::sets::π;
using dedekind::sets::π1;
using dedekind::sets::π2;
static_assert(normal_form<unsigned>(π + 1u - 1u) == normal_form<unsigned>(π),
              "π + 1 - 1 ≡ π in ℤ/2³²[π]");
static_assert(normal_form<unsigned>((π1 + π2) * (π1 + π2)) ==
                  normal_form<unsigned>(π1 * π1 + 2u * π1 * π2 + π2 * π2),
              "(π1 + π2)² ≡ π1² + 2π1π2 + π2²");
static_assert(normal_form<unsigned>(π * 4294967295u + π).is_zero(),
              "the coefficients wrap with the word: (2³² − 1)π + π ≡ 0");
static_assert(normal_form<unsigned>(π1* π2).degree() == 2 &&
                  normal_form<unsigned>(π1 * π2).support().size() == 1,
              "π1·π2 is one monomial of degree 2");

}  // namespace dedekind::algebra
