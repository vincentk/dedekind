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
 * A polynomial over a commutative rig @c R in the variables @c π, @c π_1,
 * @c π_2, @c π_3 is a finitely supported function from the monomials (the free
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
 * here: they are refused, never rewritten.  @c canonical<C>(t) is the
 * fold; two terms are equal in the free algebra iff their normal forms are
 * equal, which is equality of two finite functions, monomial to coefficient.
 *
 * A set expression is normalised through its predicate, atom by atom
 * (@c canonical(pred, lift)): an equation @c a @c = @c b on a ring becomes
 * @c a @c - @c b @c = @c 0 with the difference a polynomial, the unit part of
 * its leading coefficient divided out (monic on a field), written back as a
 * term (@c as_term).  Equal normal forms cut out equal sets;
 * the converse fails (the next paragraph), so a comparison of sets by normal
 * form may answer only "equal" or "not decided".
 *
 * Over a finite carrier that is not equality of the polynomial @b functions:
 * on 𝔽ₚ, @c πᵖ and @c π agree at every point, and on ℤ/2⁶⁴ so do
 * @c 2⁶³·π·(π+1) and @c 0.  That quotient (by the vanishing ideal) is a
 * further reading, not this one.
 *
 * The exponents are natural numbers, held in machine words: a product whose
 * exponent would leave the word raises @c std::overflow_error rather than
 * wrap.  A wrapping coefficient is the carrier's own arithmetic; a wrapping
 * exponent would change the term (@c π^(2³²−1)·π is not the constant 1), so
 * the monoid is ℕᴺ, certified total on the exact path with the overflow a
 * tripwire, the posture of @c Rational's out-of-memory sentinel.
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

#include <algorithm>   // std::sort
#include <array>       // Monomial's exponents
#include <bit>         // std::countr_zero, std::bit_width
#include <compare>     // std::strong_ordering
#include <concepts>    // std::equality_comparable
#include <cstddef>     // std::size_t
#include <functional>  // std::plus, std::minus, std::multiplies, std::divides
#include <limits>      // an exponent's bound
#include <optional>    // a definition found through a block of quantifiers
#include <stdexcept>  // std::domain_error, std::out_of_range, std::overflow_error
#include <string>     // the refusals' text
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
 *  the slot 0; @c π_1, @c π_2, @c π_3 the slots 1, 2, 3).  The free commutative
 *  monoid on @c N generators, written multiplicatively (the exponents add, as
 *  natural numbers: leaving the word raises), and a chain under the graded
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
  /** @brief The total degree, in a wider word than the exponents. */
  constexpr unsigned long long degree() const {
    unsigned long long d = 0;
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
  /** @brief The product: exponents add.  @throws std::overflow_error where
   *  an exponent would leave the word. */
  friend constexpr Monomial operator*(const Monomial& a, const Monomial& b) {
    Monomial m;
    for (std::size_t i = 0; i < N; ++i) {
      if (b.e[i] > std::numeric_limits<unsigned>::max() - a.e[i])
        throw std::overflow_error("an exponent beyond the word");
      m.e[i] = a.e[i] + b.e[i];
    }
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
  static constexpr Polynomial variable(std::size_t slot)
    requires requires { M::variable(slot); }
  {
    return Polynomial({{M::variable(slot), unit()}});
  }

  /** @brief The zero polynomial. */
  constexpr Polynomial() = default;
  /** @brief A constant. */
  constexpr explicit Polynomial(R scalar) {
    if (scalar != zero_coefficient())
      terms_.emplace_back(identity_v<M, std::multiplies<M>>, std::move(scalar));
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
  constexpr auto degree() const {
    return terms_.empty() ? decltype(terms_.back().first.degree()){0}
                          : terms_.back().first.degree();
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
 *  has no free algebra catalogued here and is refused.  Expanding into
 *  polynomials uses commutativity of @c * and its distributivity over @c +,
 *  so the carrier must be a certified commutative rig.  @tparam C the
 *  carrier.  @tparam N the variables: the slots @c π, @c π_1, @c π_2, @c π_3.
 */
export template <typename C, std::size_t N = 4>
  requires dedekind::category::IsRig<C, std::plus<C>, std::multiplies<C>> &&
           dedekind::category::IsCommutative<C, std::multiplies<C>>
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
      case T::Kind::Cond:
        throw dedekind::sets::not_admitted(
            "no free algebra catalogued for a conditional: cond is read by "
            "its branches, never rewritten");
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
constexpr auto canonical(const T& t) {
  return cata(Poly<C>{}, t);
}

/** @brief A polynomial written back as a term of the grammar: its terms from
 *  the leading monomial down, each the coefficient times the product of its
 *  variables (a coefficient 1 omitted), summed; the zero polynomial is the
 *  constant 0.  @tparam K the constants' type.  @param p the polynomial.
 *  @param lift a coefficient into a constant. */
export template <typename K, typename C, std::size_t N, typename Lift>
dedekind::sets::Term<K> as_term(const Polynomial<C, Monomial<N>>& p,
                                const Lift& lift) {
  using T = dedekind::sets::Term<K>;
  using dedekind::sets::Arith;
  if (p.is_zero()) return T::constant(lift(identity_v<C, std::plus<C>>));
  T sum;
  bool first = true;
  for (auto it = p.support().rbegin(); it != p.support().rend(); ++it) {
    const auto& [m, c] = *it;
    T product;
    bool constant = true;
    for (std::size_t slot = 0; slot < N; ++slot) {
      // π^e by repeated squaring: the square shares its operand, so the term
      // is logarithmic in e in size and in depth.
      T base = T::proj(static_cast<unsigned>(slot));
      for (unsigned e = m.exponent(slot); e > 0; e >>= 1) {
        if (e & 1u) {
          product = constant ? base : T::bin(Arith::Mul, product, base);
          constant = false;
        }
        if (e > 1u) base = T::bin(Arith::Mul, base, base);
      }
    }
    const T term = constant ? T::constant(lift(c))
                   : c == identity_v<C, std::multiplies<C>>
                       ? product
                       : T::bin(Arith::Mul, T::constant(lift(c)), product);
    sum = first ? term : T::bin(Arith::Add, sum, term);
    first = false;
  }
  return sum;
}

/** @brief A unit @c u of the carrier with @c u·c the chosen representative of
 *  @c c's associates (the elements @c c times a unit): the hook the normal form
 *  of an equation divides by.  1 by default, nothing divided out; a carrier
 *  with more units states its own overload, found by ADL.  @param c the
 *  leading coefficient. */
export template <typename C>
constexpr C unit_normaliser(const C&) {
  return identity_v<C, std::multiplies<C>>;
}
/** @brief On a word, ℤ/2ⁿ: @c c @c = @c 2ᵏ·o with @c o odd, and the odd
 *  elements are the units, so @c o⁻¹ (Newton's iteration, 2-adic: each step
 *  doubles the correct low bits) makes @c u·c @c = @c 2ᵏ.  0 has no unit part.
 *  @param c the leading coefficient. */
export template <std::unsigned_integral W>
  requires(!std::same_as<W, bool>) && (sizeof(W) >= sizeof(unsigned))
constexpr W unit_normaliser(const W& c) {
  if (c == W{0}) return W{1};
  const W odd = static_cast<W>(c >> std::countr_zero(c));
  W y = odd;  // o·o ≡ 1 (mod 8): three correct bits to start
  while (static_cast<W>(odd * y) != W{1})
    y = static_cast<W>(y * static_cast<W>(W{2} - static_cast<W>(odd * y)));
  return y;
}
static_assert(unit_normaliser(3u) * 3u == 1u &&
                  unit_normaliser(6u) * 6u == 2u &&
                  unit_normaliser(~0u) == ~0u && unit_normaliser(8u) == 1u,
              "the odd part's inverse: 3 → 1, 6 → 2, -1 → 1, 8 stays 8");

/** @brief The normal form of a comparison atom read in the sort @c C: an
 *  equation or its negation @c a @c ⋈ @c b on a ring moves to @c a @c - @c b
 *  @c ⋈ @c 0 with the difference a polynomial, the unit part of its leading
 *  coefficient divided out (monic on a field; @c unit_normaliser elsewhere),
 *  so an equation and its multiple by any unit meet; any other atom has its
 *  sides normalised and its comparison kept.  @tparam C the sort. */
template <typename C, std::size_t N, typename K, typename Lift>
dedekind::sets::Pred<K> canonical_atom_in(const dedekind::sets::Pred<K>& p,
                                          const Lift& lift) {
  using Pr = dedekind::sets::Pred<K>;
  using P = Polynomial<C, Monomial<N>>;
  using dedekind::sets::On;
  using dedekind::sets::Rel;
  const P x = cata(Poly<C, N>{}, *p.a);
  const P y = cata(Poly<C, N>{}, *p.b);
  // Both sides constants: the atom is decided, and written back as ⊤ or ⊥,
  // the Boolean algebra's constants, free of a sort no constant could name
  // again.
  if (x.degree() == 0 && y.degree() == 0) {
    if (!dedekind::sets::rel_admitted<C>(p.rel))
      throw dedekind::sets::not_admitted(
          "the comparison is not in the carrier's signature");
    return dedekind::sets::compare_rel(p.rel, x(Monomial<N>::one()),
                                       y(Monomial<N>::one()))
               ? Pr::top()
               : Pr::bottom();
  }
  if constexpr (requires(const P& u, const P& v) { u - v; }) {
    if ((p.rel == Rel::Eq || p.rel == Rel::Ne) &&
        dedekind::sets::admits(On<C>{}, std::minus<>{}, false)) {
      P d = x - y;
      // The sides differ by a constant: decided, ⊤ or ⊥ (0 == 0 is ⊤).
      if (d.degree() == 0)
        return (p.rel == Rel::Eq) == d.is_zero() ? Pr::top() : Pr::bottom();
      // An equation keeps its zero set under a unit factor, so the unit part
      // of the leading coefficient is divided out: on a field all of it
      // (monic), elsewhere what the carrier's unit_normaliser names.
      if (!d.is_zero()) {
        bool field = false;
        if constexpr (requires(const C& a, const C& b) { a / b; }) {
          if (dedekind::sets::admits(On<C>{}, dedekind::sets::FieldDivision{},
                                     true)) {
            d = d *
                P(identity_v<C, std::multiplies<C>> / d.leading_coefficient());
            field = true;
          }
        }
        if (!field) d = d * P(unit_normaliser(d.leading_coefficient()));
      }
      return Pr::cmp(
          p.rel, as_term<K>(d, lift),
          dedekind::sets::Term<K>::constant(lift(identity_v<C, std::plus<C>>)));
    }
  }
  return Pr::cmp(p.rel, as_term<K>(x, lift), as_term<K>(y, lift));
}
/** @brief An atom's normal form with as many variables as it reads: the
 *  monomials' width chosen by the atom's highest slot (@c π and up to
 *  @c π_7, or up to @c π_31, the width of the slot mask).  A slot past the
 *  mask is refused; a sparse monomial would lift the bound (#1023).
 *  @tparam C the sort. */
template <typename C, typename K, typename Lift>
dedekind::sets::Pred<K> canonical_atom(const dedekind::sets::Pred<K>& p,
                                       const Lift& lift) {
  const unsigned mask =
      dedekind::sets::slots(*p.a) | dedekind::sets::slots(*p.b);
  if (mask == ~0u)
    throw dedekind::sets::not_admitted(
        "an atom past π_31, the width of the slot mask (#1023)");
  if (std::bit_width(mask) <= 8) return canonical_atom_in<C, 8>(p, lift);
  return canonical_atom_in<C, 32>(p, lift);
}

namespace detail_canonical {
/** @brief The dispatch of an atom's normal form to its sort: a comparison of
 *  two constants decided in it, any other atom in the polynomials over a
 *  carrier of the points where + and · are total on it, and on a carrier
 *  without them (𝔹, K₃) kept as written: equal forms still cut out equal
 *  sets, and different ones decide nothing.  @tparam P the point type. */
template <typename P, typename K, typename Lift>
struct AtomAt {
  const dedekind::sets::Pred<K>& p;
  const Lift& lift;
  template <typename C>
  dedekind::sets::Pred<K> operator()(std::type_identity<C>) const {
    using Pr = dedekind::sets::Pred<K>;
    using T = dedekind::sets::Term<K>;
    if (p.a->kind == T::Kind::Const && p.b->kind == T::Kind::Const) {
      if (!dedekind::sets::rel_admitted<C>(p.rel))
        throw dedekind::sets::not_admitted(
            "the comparison is not in the carrier's signature");
      return dedekind::sets::compare_rel(
                 p.rel, dedekind::sets::constant_as<P, C>(p.a->value),
                 dedekind::sets::constant_as<P, C>(p.b->value))
                 ? Pr::top()
                 : Pr::bottom();
    }
    if constexpr (dedekind::sets::is_sort_of_v<
                      C, dedekind::sets::point_sorts_t<P>> &&
                  dedekind::sets::admits(dedekind::sets::On<C>{}, std::plus<>{},
                                         false) &&
                  dedekind::sets::admits(dedekind::sets::On<C>{},
                                         std::multiplies<>{}, false))
      return canonical_atom<C>(p, lift);
    else
      return p;  // a chain without a rig's + · (𝔹, K₃): its own form
  }
};
/** @brief The values of a finite chain as constants, through @c lift: the
 *  cases of Shannon's expansion; none for any other carrier. */
template <typename K, typename Lift>
struct ChainValues {
  const Lift& lift;
  template <typename C>
  std::vector<dedekind::sets::Term<K>> operator()(std::type_identity<C>) const {
    std::vector<dedekind::sets::Term<K>> out;
    if constexpr (dedekind::category::IsPst<C> && requires(const C& c) {
                    { lift(c) } -> std::convertible_to<K>;
                  }) {
      for (C v = dedekind::sets::chain_bottom<C>();;
           v = dedekind::category::Successor<C>{}(v)) {
        out.push_back(dedekind::sets::Term<K>::constant(lift(v)));
        if (v == dedekind::sets::chain_top<C>()) break;
      }
    }
    return out;
  }
};
template <typename P, typename K, typename Lift>
dedekind::sets::Pred<K> canonical_lifted(const dedekind::sets::Pred<K>& p,
                                         const Lift& lift, const P& rep);
/** @brief The operands of a chain of the connective @c k, each in normal form;
 *  a normal form that is itself a chain of @c k contributes its operands
 *  (associativity).  @param out the operands, appended. */
template <typename P, typename K, typename Lift>
void operands_of(const dedekind::sets::Pred<K>& p,
                 typename dedekind::sets::Pred<K>::Kind k, const Lift& lift,
                 const P& rep, std::vector<dedekind::sets::Pred<K>>& out) {
  if (p.kind == k) {
    operands_of<P>(*p.p, k, lift, rep, out);
    operands_of<P>(*p.q, k, lift, rep, out);
    return;
  }
  const dedekind::sets::Pred<K> c = canonical_lifted<P>(p, lift, rep);
  if (c.kind != k) {
    out.push_back(c);
    return;
  }
  // A normal form is a left-folded chain whose operands are normal already.
  std::vector<const dedekind::sets::Pred<K>*> stack{&c};
  std::vector<dedekind::sets::Pred<K>> leaves;
  while (!stack.empty()) {
    const auto* q = stack.back();
    stack.pop_back();
    if (q->kind == k) {
      stack.push_back(q->q.get());
      stack.push_back(q->p.get());
    } else {
      leaves.push_back(*q);
    }
  }
  out.insert(out.end(), leaves.begin(), leaves.end());
}
/** @brief The normal form of a predicate without conditionals: atom by atom,
 *  the bounds ⊤ / ⊥ folded into the connectives (the lattice's units and
 *  absorbers), the operands of each connective sorted and kept once. */
template <typename P, typename K, typename Lift>
dedekind::sets::Pred<K> canonical_lifted(const dedekind::sets::Pred<K>& p,
                                         const Lift& lift, const P& rep) {
  using Pr = dedekind::sets::Pred<K>;
  switch (p.kind) {
    case Pr::Kind::Top:
    case Pr::Kind::Bottom:
      return p;
    case Pr::Kind::And:
    case Pr::Kind::Or: {
      // The operands of the whole chain of one connective, each in normal
      // form, joined with the order, grouping and repetition forgotten
      // (order::connective, #884).
      std::vector<Pr> ops;
      operands_of<P>(p, p.kind, lift, rep, ops);
      return dedekind::order::connective(p.kind, std::move(ops));
    }
    case Pr::Kind::Exists:
    case Pr::Kind::Forall:
      // A quantifier that survived elimination (@c eliminate_quantifiers):
      // its body in normal form at the extended row, the node kept.
      return Pr::bound_like(
          p, canonical_lifted<P>(*p.p, lift,
                                 dedekind::sets::with_bound(rep, p.sort)));
    case Pr::Kind::Cmp:
      break;
  }
  return dedekind::sets::with_sort<P>(
      dedekind::sets::default_sort<P>(
          dedekind::sets::join_sorts<P>(dedekind::sets::sort_of<P>(*p.a, rep),
                                        dedekind::sets::sort_of<P>(*p.b, rep)),
          rep),
      AtomAt<P, K, Lift>{p, lift});
}
}  // namespace detail_canonical

/** @brief The normal form of a predicate at points of @c P: its conditionals
 *  lifted out of the atoms (@c lift_conditionals, so an applied set
 *  @c ap(S, t) reads as @c S's predicate at @c t), then atom by atom, each
 *  atom in its operands' sort (a point of one carrier has one; a pair of two
 *  carriers two, each axis its own), a decided atom ⊤ or ⊥ and folded into its
 *  connective.  Equal normal forms cut out equal sets; different ones need not
 *  (@c πᵖ @c = @c π everywhere on 𝔽ₚ).  An atom outside the catalogue is
 *  refused (@c not_admitted).  @tparam P the point type.  @param p the
 *  predicate.  @param lift a coefficient into a constant of the predicate's
 *  type.  @param rep a point of @c P (a row is its own schema). */
/** @brief The slots above an eliminated bound slot, moved down by one. */
template <typename K>
struct ShiftDown {
  unsigned above;
  dedekind::sets::Term<K> operator()(unsigned s) const {
    return dedekind::sets::Term<K>::proj(s > above ? s - 1 : s);
  }
};
/** @brief A definition of the slot found through a block of quantifiers of
 *  the same kind (which commute): the defining atom's term, free of the
 *  slots those quantifiers bind, and the block with that atom removed. */
template <typename K>
std::optional<dedekind::sets::Definition<K>> defined_through(
    const dedekind::sets::Pred<K>& q, unsigned slot, bool universal,
    unsigned bound) {
  using Pr = dedekind::sets::Pred<K>;
  if (q.quantifier() && (q.kind == Pr::Kind::Forall) == universal) {
    const unsigned bit =
        q.slot < std::numeric_limits<unsigned>::digits ? 1u << q.slot : 0u;
    if (auto d = defined_through(*q.p, slot, universal, bound | bit))
      return dedekind::sets::Definition<K>{d->term, Pr::bound_like(q, d->rest)};
    return std::nullopt;
  }
  auto d = dedekind::sets::defining_term(q, slot, universal);
  if (d && (dedekind::sets::slots(d->term) & bound) == 0u) return d;
  return std::nullopt;
}
/** @brief The predicate with every quantifier eliminated that can be, the
 *  atoms left as written: innermost first, by substitution where an atom
 *  defines the bound column (a conjunct @c π_h @c == @c t under @c ∃, a
 *  disjunct @c π_h @c != @c t under @c ∀: the fusion law), else by Shannon's
 *  expansion over a finite chain (@c ∨ under @c ∃, @c ∧ under @c ∀).  A
 *  quantifier over an infinite carrier no atom defines is kept.  Read on the
 *  predicate as written (pinned, conditionals not lifted): normalising an
 *  atom first would turn the defining shape @c π_h @c == @c t into
 *  @c π_h @c − @c t @c == @c 0.  @tparam P the point type.  @param p the
 *  predicate.  @param lift a value into a constant that names its sort.
 *  @param rep a point of @c P. */
export template <typename P, typename K, typename Lift>
dedekind::sets::Pred<K> eliminate_quantifiers(const dedekind::sets::Pred<K>& p,
                                              const Lift& lift,
                                              const P& rep = P{}) {
  using Pr = dedekind::sets::Pred<K>;
  switch (p.kind) {
    case Pr::Kind::Top:
    case Pr::Kind::Bottom:
    case Pr::Kind::Cmp:
      return p;
    case Pr::Kind::And:
      return Pr::conj(eliminate_quantifiers<P>(*p.p, lift, rep),
                      eliminate_quantifiers<P>(*p.q, lift, rep));
    case Pr::Kind::Or:
      return Pr::disj(eliminate_quantifiers<P>(*p.p, lift, rep),
                      eliminate_quantifiers<P>(*p.q, lift, rep));
    case Pr::Kind::Exists:
    case Pr::Kind::Forall: {
      const bool universal = p.kind == Pr::Kind::Forall;
      // Once the bound slot is gone, the quantifiers inside it bind one slot
      // lower: every slot above it moves down (the next-slot invariant).
      const auto unbound_raw = [&](const Pr& q) {
        return q.map_slots(ShiftDown<K>{p.slot});
      };
      // A definition first, looked for through the inner quantifiers of the
      // same kind (they commute): substituting before an inner one is
      // expanded keeps the defining atom at the top of its connective.
      // A definition substitutes only a term that lands in the bound column's
      // carrier: ∃b ∈ 𝔹. b == t has a witness only where t is a value of 𝔹
      // (a K₃ term may be U); 𝔹 ↪ K₃ the other way is fine.
      const auto lands = [&](const dedekind::sets::Term<K>& t) {
        const std::size_t s = dedekind::sets::sort_of<P>(t, rep);
        return s == dedekind::sets::no_sort ||
               dedekind::sets::sort_embeds<P>(s, p.sort);
      };
      if (auto d = defined_through(*p.p, p.slot, universal, 0u);
          d && lands(d->term))
        return eliminate_quantifiers<P>(
            unbound_raw(d->rest.subst(p.slot, d->term)), lift, rep);
      const Pr body = eliminate_quantifiers<P>(
          *p.p, lift, dedekind::sets::with_bound(rep, p.sort));
      // Once the bound slot is gone, the quantifiers inside it bind one slot
      // lower: every slot above it moves down (the next-slot invariant).
      const auto unbound = [&](const Pr& q) {
        return q.map_slots(ShiftDown<K>{p.slot});
      };
      if (auto d = dedekind::sets::defining_term(body, p.slot, universal);
          d && lands(d->term))
        return eliminate_quantifiers<P>(unbound(d->rest.subst(p.slot, d->term)),
                                        lift, rep);
      const auto cases = dedekind::sets::with_sort<P>(
          p.sort, detail_canonical::ChainValues<K, Lift>{lift});
      if (!cases.empty()) {
        Pr all = body.subst(p.slot, cases.front());
        for (std::size_t i = 1; i < cases.size(); ++i) {
          const Pr next = body.subst(p.slot, cases[i]);
          all = universal ? Pr::conj(all, next) : Pr::disj(all, next);
        }
        return eliminate_quantifiers<P>(unbound(all), lift, rep);
      }
      return Pr::bound_like(p, body);
    }
  }
  return p;
}

/** @brief The normal form of a predicate: its quantifiers eliminated where
 *  they can be (@c eliminate_quantifiers), its conditionals lifted, its atoms
 *  normalised and the bounds folded (@c canonical_lifted). */
export template <typename P, typename K, typename Lift>
dedekind::sets::Pred<K> canonical(const dedekind::sets::Pred<K>& p,
                                  const Lift& lift, const P& rep = P{}) {
  return detail_canonical::canonical_lifted<P>(
      dedekind::sets::lift_conditionals(eliminate_quantifiers<P>(p, lift, rep)),
      lift, rep);
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
// Totality on the exact path (D of category:total): the exponents add as
// natural numbers, and leaving the word is a tripwire that raises, never a
// wrapped (wrong) exponent.
template <std::size_t N>
struct is_exact_total<dedekind::algebra::Monomial<N>,
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
static_assert(
    M4::variable(1) * M4::variable(2) == M4::variable(2) * M4::variable(1) &&
        M4::variable(0) * M4::variable(0) > M4::variable(3),
    "π_1·π_2 = π_2·π_1; π² is above π_3: degree first, then the slots");

using PolyWord = Polynomial<unsigned int, M4>;
static_assert(dedekind::sequences::IsNet<PolyWord>,
              "a polynomial is a net over the monomials: an arrow from a "
              "directed set");
static_assert(
    dedekind::category::IsCommutativeRing<PolyWord, std::plus<PolyWord>,
                                          std::multiplies<PolyWord>>,
    "ℤ/2³²[π, π_1, π_2, π_3] is a commutative ring: the coefficients' "
    "laws by construction");
static_assert(dedekind::category::IsFreeAlgebra<PolyWord>,
              "R[M] is free over R");

// The normal form decides equality in the free algebra, at compile time.
using dedekind::sets::π;
using dedekind::sets::π_1;
using dedekind::sets::π_2;
static_assert(canonical<unsigned>(π + 1u - 1u) == canonical<unsigned>(π),
              "π + 1 - 1 ≡ π in ℤ/2³²[π]");
static_assert(canonical<unsigned>((π_1 + π_2) * (π_1 + π_2)) ==
                  canonical<unsigned>(π_1 * π_1 + 2u * π_1 * π_2 + π_2 * π_2),
              "(π_1 + π_2)² ≡ π_1² + 2π_1π_2 + π_2²");
static_assert(canonical<unsigned>(π * 4294967295u + π).is_zero(),
              "the coefficients wrap with the word: (2³² − 1)π + π ≡ 0");
static_assert(canonical<unsigned>(π_1* π_2).degree() == 2 &&
                  canonical<unsigned>(π_1 * π_2).support().size() == 1,
              "π_1·π_2 is one monomial of degree 2");

}  // namespace dedekind::algebra
