/**
 * @file dedekind/sets/terms.cppm
 * @partition :terms
 * @brief The term grammar of comprehensions: projections, constants carried as
 * values, arithmetic, arrows applied to terms, conditionals; the comparison
 * atom over two terms; the pointwise connectives of relational predicates.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section terms__Grammar
 *
 *     term ::= π_i | const(v) | term op term | ap(arrow, term)
 *            | cond(pred, term, term)
 *     op   ::= '+' | '-' | '*' | '/' | '%'
 *     atom ::= term rel term
 *     rel  ::= '==' | '!=' | '<' | '<=' | '>' | '>='
 *
 * A term's @b structure is its type: the slots it mentions (@c slots_v, a bit
 * mask), its operators, the comparison an atom makes.  Its @b constants are
 * values, spelled as the literals themselves (@c π_1 @c + @c 1 @c == @c π_2),
 * so a term built at run time --- from Python --- is the same term.  A constant
 * meets the sort of the term it is combined with: on a pair of @c Cardinality
 * the @c 1 above is the cardinal 1, converted when the term is evaluated and
 * checked to be representable.  Facts about a constant's value (a divisor is
 * not zero) are checked when the term is built: in @c constexpr code a
 * violation is a compile error.
 *
 * Certificates read the structure: an atom @c π_2 @c == @c T with
 * @c slots_v<T> @c ⊆ @c {1} is the graph of a function of the first
 * coordinate, whatever @c T is.
 *
 * Every reading of a term is an @b algebra folded by @c category::cata, the
 * one engine: each node states its @c legs once, and an algebra states only
 * the nodes it reads specially (@c Slots, @c Total, @c Bind, @c Value below).
 * A reading whose results are @c std::integral_constant is a type-level
 * certificate; the same fold with values is the evaluation.
 *
 * A term built at @b run @b time (from Python) is the same grammar with its
 * shape a value: @c Term<K> and @c Pred<K> below, read by the same algebras
 * through their @c steer rule, and checked against the carrier when the
 * former binds them (@c bind_check): an operator admitted there or refused
 * (@c not_admitted), a constant meeting the carrier or refused.
 *
 * @section terms__Signature_And_Free_Algebra
 * The laws decide the syntax.  A carrier's @b certified @b varieties supply
 * the @b signature a term may use (@c admits below: @c + on a commutative
 * monoid, @c - on an abelian group, @c * on a monoid, @c / on a field,
 * @c // and @c % on a dividable chain, the order comparisons on a poset,
 * @c == and @c != on any set) and the @b free @b algebra on the variables its
 * normal form lives in; @b equality @b of @b terms is equality in that free
 * algebra.  The catalogue of free algebras with a canonical representative:
 *   - a chain: the Boolean algebra the intervals generate, read as run lists
 *     (@c order:runs, @c steps);
 *   - a commutative rig, ring or field: polynomials over the carrier
 *     (@c algebra:polynomial, @c canonical).
 * A term whose operators are in the signature but outside the catalogue (a
 * remainder, a division by a term) is well-formed and evaluates, and has no
 * equality: it is refused where a normal form is asked, never rewritten.
 * Type-checking is at the former: at compile time for a typed term bound to
 * a @c constexpr universe, at construction for a term built at run time.
 *
 * The carrier and its algebras are different entities (the paper's Figure 1),
 * and a carrier bears several: @c u64 is a chain, a ring and a Euclidean
 * division at once.  The @b signature is the union over the carrier's
 * registered algebras, each rule speaking for one, each import bringing one
 * more into view.  A @b free @b algebra, and so a normal form, is @b per
 * @b algebra, not per carrier: over one carrier the chain's is read as run
 * lists and the ring's as polynomials, and "equal terms" means a different
 * thing in each.  A term spanning two algebras (@c π @c + @c 1 @c < @c 5)
 * belongs to their @b combined theory (the ordered ring, whose laws relate
 * @c <= and @c +) and has a normal form only where that theory's is
 * catalogued; the translate of an atom @c π @c ± @c c @c ⋈ @c k in
 * @c order:halfspace is the first piece of it, not a licence to glue run lists
 * and polynomials without laws.  A field adds no free algebra: division by a
 * nonzero constant factors through the ring (@c π @c / @c 3 on 𝔽₇ is
 * @c π @c * @c 5), so the field certificate only guarantees the constant a
 * unit.
 *
 * Wikipedia: Term (logic), Term algebra, Atomic formula, Relational algebra
 *
 * @note "Die Zahlen sind freie Schöpfungen des menschlichen Geistes, sie
 *       dienen als ein Mittel, um die Verschiedenheit der Dinge leichter und
 *       schärfer aufzufassen."
 *       -- Richard Dedekind, Was sind und was sollen die Zahlen? (1888),
 *       Vorwort.
 *       [Trans: "Numbers are free creations of the human mind; they serve as
 *       a means of grasping the difference of things more easily and more
 *       sharply."]
 */
module;

#include <algorithm>  // fold_left / any_of / all_of / transform over a chain
#include <concepts>
#include <cstddef>
#include <cstdint>  // the search budget of a quantifier
#include <functional>
#include <iterator>  // std::back_inserter
#include <limits>
#include <memory>  // std::shared_ptr: the legs of a term built at run time
#include <optional>
#include <ranges>  // chain_view is a range
#include <stdexcept>
#include <string>  // the spelling of a refusal
#include <tuple>
#include <type_traits>
#include <utility>
#include <variant>  // the value of a term over two carriers
#include <vector>   // a row: the cells of an n-ary point

export module dedekind.sets:terms;

import dedekind.category;
import :boundaries;   // the set objects
import :cardinality;  // IsRingIntegral; Cardinality, the ℕ carrier constants
                      // meet

namespace dedekind::sets {
using namespace dedekind::category;

// ── The point-free variables ───────────────────────────────────────────────
/**
 * @brief The variables of the grammar are the product projections of
 *        @c category:limit, one entity each: @c π (the unary projection, the
 *        identity), @c π_1, @c π_2, @c π_3, and the family @c π_<k>.
 *
 * @details @c π leaves the carrier open: @c π @c > @c 5 is a cut datum, @c π
 * @c == @c v a point, and the former @c carrier @c & @c ... binds it, the
 * constant meeting the carrier exactly.  On a product the variable of slot
 * @c k is the projection @c π_k: called on a point it reads the coordinate, and
 * a comparison of terms over it builds a strongly-typed predicate on the pair
 * (no lambda); @c && conjoins them (@c || joins), distinct from the set-level
 * @c & / @c | on whole relations; and @c product @c | @c predicate restricts
 * the product to the relation.  @c && binds looser than @c |, so the
 * comprehension parenthesises the meet: @c ℕ*ℕ @c & @c (π_1 @c < @c π_2 @c &&
 * @c π_1 @c > @c 5) is the relation @f$\{(x,y) \mid x<y \wedge x>5\}@f$ as an
 * @c IsSet on @c ℕ×ℕ.
 * @see dedekind::category::Projection
 */
export using dedekind::category::Projection;
export using dedekind::category::π;
export using dedekind::category::π_;
export using dedekind::category::π_1;
export using dedekind::category::π_2;
export using dedekind::category::π_3;

// (Plain @c x is deliberately not offered: it shadows the pervasive local
// element parameter named @c x and would trip @c -Wshadow.  @c χ is not an
// alias of π either: it names the classifier under definition, :recursion.)

// Blackboard coordinate aliases so a pair/triple predicate reads as
// @f$z = f(x,y)@f$: the mathematical-italic @c 𝑥/𝑦/𝑧 (U+1D465..7) are
// letter-category identifiers (like @c 𝔸/ℕ), NOT ASCII @c x/y/z, so they carry
// the blackboard look without colliding with ordinary variable names.
export inline constexpr Projection<1> 𝑥{};
export inline constexpr Projection<2> 𝑦{};
export inline constexpr Projection<3> 𝑧{};

// ── Relational predicates and their pointwise connectives ───────────────────

/** @brief A relational predicate: a predicate on the points of a product (or
 *  of a carrier, for an atom on @c π), tagged @c is_rel_predicate; it meets its
 *  universe only at the former @c &.  @tparam T the candidate. */
export template <typename T>
concept IsRelPredicate =
    requires { typename std::remove_cvref_t<T>::is_rel_predicate; };

/** @brief Meet (conjunction) of two relational predicates, the result of
 *  @c && on them.  @tparam A the left operand.  @tparam B the right operand. */
export template <typename A, typename B>
struct RelAnd {
  using is_rel_predicate = void;
  A a;
  B b;
  // @c auto (not @c bool): the result inherits the operands' own logic, so over
  // a @c Kleene relation this is the Kleene @c ∧.  FIXME(#780): mixed
  // Boolean/Ternary operands still need a lift on the bool side.
  template <typename P>
  constexpr auto operator()(const P& p) const {
    return term_value(*this, p);
  }
};

/** @brief Join (disjunction) of two relational predicates, the result of
 *  @c || on them, dual to @c RelAnd.  @tparam A the left operand.  @tparam B
 *  the right operand. */
export template <typename A, typename B>
struct RelOr {
  using is_rel_predicate = void;
  A a;
  B b;
  template <typename P>
  constexpr auto operator()(const P& p) const {
    return term_value(*this, p);
  }
};

// ── Comparisons ─────────────────────────────────────────────────────────────

/** @brief The comparison an atom makes. */
export enum class Rel { Lt, Le, Gt, Ge, Eq, Ne };

/** @brief @c x @c R @c y.  @tparam R the comparison.  @param x the left
 *  value.  @param y the right value. */
export template <Rel R, typename X, typename Y>
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

/** @brief The De Morgan dual of a comparison: @c !(x @c R @c y) is
 *  @c x @c negate(R) @c y.  @param r the comparison. */
export constexpr Rel negate(Rel r) {
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
  return r;  // unreachable; all six are covered above.
}

// ── Terms ───────────────────────────────────────────────────────────────────

/** @brief A constant, carried as a value.  @tparam K its type. */
export template <typename K>
struct Const {
  K value;
};

/** @brief @c a @c op @c b, the operator a standard functor (@c std::plus<>,
 *  @c minus, @c multiplies, @c divides, @c modulus) applied in one carrier.
 *  @tparam Op the operator.  @tparam A,B the terms. */
export template <typename Op, typename A, typename B>
struct BinOp {
  A a;
  B b;
};
/** @brief An arrow applied to a term, @c f(a).  @tparam F the arrow.
 *  @tparam A the term. */
export template <typename F, typename A>
struct Ap {
  F f;
  A a;
};
/** @brief McCarthy's conditional on terms: @c a where @c p holds, else @c b.
 *  @tparam P a relational predicate.  @tparam A,B the branches. */
export template <typename P, typename A, typename B>
struct CondT {
  P p;
  A a;
  B b;
};
export template <Rel R, typename A, typename B>
struct Cmp;
/** @brief A term built at run time, the grammar with its shape a value;
 *  defined after the binding below.  @tparam K the constants' type. */
export template <typename K>
struct Term;
/** @brief A relational predicate built at run time: comparisons of terms and
 *  their connectives; defined after the binding below.  @tparam K the
 *  constants' type. */
export template <typename K>
struct Pred;
template <typename T>
inline constexpr bool is_runtime_term_v = false;
template <typename K>
inline constexpr bool is_runtime_term_v<Term<K>> = true;

/** @name The legs of a node, for @c cata (@c category::f_algebra)
 *  @brief The term functor stated once per node: the sub-terms, by reference.
 *  A projection, a constant, and a predicate the grammar does not know are
 *  leaves.  An arrow applied to a term has one leg, the term (the arrow is a
 *  datum of the node).
 *  @{ */
export template <typename Op, typename A, typename B>
constexpr auto legs(const BinOp<Op, A, B>& t) {
  return std::tie(t.a, t.b);
}
export template <typename F, typename A>
constexpr auto legs(const Ap<F, A>& t) {
  return std::tie(t.a);
}
export template <typename P, typename A, typename B>
constexpr auto legs(const CondT<P, A, B>& t) {
  return std::tie(t.p, t.a, t.b);
}
export template <Rel R, typename A, typename B>
constexpr auto legs(const Cmp<R, A, B>& t) {
  return std::tie(t.a, t.b);
}
export template <typename A, typename B>
constexpr auto legs(const RelAnd<A, B>& t) {
  return std::tie(t.a, t.b);
}
export template <typename A, typename B>
constexpr auto legs(const RelOr<A, B>& t) {
  return std::tie(t.a, t.b);
}
/** @} */

template <typename T>
inline constexpr bool is_term_v = false;
template <IsRingIntegral auto I>
inline constexpr bool is_term_v<Projection<I>> = true;
template <typename K>
inline constexpr bool is_term_v<Const<K>> = true;
template <typename Op, typename A, typename B>
inline constexpr bool is_term_v<BinOp<Op, A, B>> = true;
template <typename F, typename A>
inline constexpr bool is_term_v<Ap<F, A>> = true;
template <typename P, typename A, typename B>
inline constexpr bool is_term_v<CondT<P, A, B>> = true;

/** @brief A term of the grammar.  @tparam T the candidate. */
export template <typename T>
concept IsTerm = is_term_v<std::remove_cvref_t<T>>;

/** @brief A value that can stand as a constant beside a term: copyable, with
 *  state, and not itself part of the grammar (a term, a predicate, an arrow,
 *  or an empty tag).  The term test comes first, so asking this of a carrier
 *  never asks for the carrier's own equality.  @tparam T the candidate. */
export template <typename T>
concept IsConstant =
    !IsTerm<T> && !IsRelPredicate<T> &&
    !is_runtime_term_v<std::remove_cvref_t<T>> &&
    !std::is_empty_v<std::remove_cvref_t<T>> &&
    !IsArrow<std::remove_cvref_t<T>> && std::copyable<std::remove_cvref_t<T>>;

/** @brief The operands of an operator of the grammar: at least one term, the
 *  other a term or a constant. */
template <typename A, typename B>
concept TermOperands =
    (IsTerm<A> && (IsTerm<B> || IsConstant<B>)) || (IsTerm<B> && IsConstant<A>);

/** @brief A term as itself, a constant as @c Const.  @param x the operand. */
export template <typename X>
constexpr auto term_of(X x) {
  if constexpr (IsTerm<X>)
    return x;
  else
    return Const<X>{std::move(x)};
}

template <typename Op, typename A, typename B>
constexpr auto make_binop(A a, B b) {
  auto ta = term_of(std::move(a));
  auto tb = term_of(std::move(b));
  if constexpr (std::same_as<Op, std::divides<>> ||
                std::same_as<Op, std::modulus<>>) {
    if constexpr (requires { tb.value; }) {
      if (tb.value == decltype(tb.value){})
        throw std::domain_error("a division by the constant 0");
    }
  }
  return BinOp<Op, decltype(ta), decltype(tb)>{std::move(ta), std::move(tb)};
}

/** @brief The arithmetic of terms.  @tparam A,B the operands. */
export template <typename A, typename B>
  requires TermOperands<A, B>
constexpr auto operator+(A a, B b) {
  return make_binop<std::plus<>>(std::move(a), std::move(b));
}
export template <typename A, typename B>
  requires TermOperands<A, B>
constexpr auto operator-(A a, B b) {
  return make_binop<std::minus<>>(std::move(a), std::move(b));
}
export template <typename A, typename B>
  requires TermOperands<A, B>
constexpr auto operator*(A a, B b) {
  return make_binop<std::multiplies<>>(std::move(a), std::move(b));
}
export template <typename A, typename B>
  requires TermOperands<A, B>
constexpr auto operator/(A a, B b) {
  return make_binop<std::divides<>>(std::move(a), std::move(b));
}
export template <typename A, typename B>
  requires TermOperands<A, B>
constexpr auto operator%(A a, B b) {
  return make_binop<std::modulus<>>(std::move(a), std::move(b));
}

/** @brief @c ap(f, a): the arrow @c f applied to the term @c a.
 *  @tparam F the arrow.  @tparam A the term. */
export template <typename F, typename A>
  requires IsArrow<std::remove_cvref_t<F>> && IsTerm<A>
constexpr Ap<std::remove_cvref_t<F>, A> ap(F f, A a) {
  return {std::move(f), std::move(a)};
}

/** @brief @c cond(p, a, b): @c a where @c p holds, else @c b.
 *  @tparam P a relational predicate.  @tparam A,B the branches. */
export template <typename P, typename A, typename B>
  requires IsRelPredicate<P> && TermOperands<A, B>
constexpr auto cond(P p, A a, B b) {
  auto ta = term_of(std::move(a));
  auto tb = term_of(std::move(b));
  return CondT<P, decltype(ta), decltype(tb)>{std::move(p), std::move(ta),
                                              std::move(tb)};
}

// ── The slots a term mentions ───────────────────────────────────────────────

/** @brief The slot algebra: the slots a term or a relational predicate
 *  mentions, as a bit mask (bit @c i for @c π_i, bit 0 for the unary @c π).  A
 *  node mentions the union of its legs' slots; a predicate the grammar does not
 *  know mentions every slot, so no certificate reads more into it than it says.
 */
struct Slots {
  template <IsRingIntegral auto I>
  constexpr std::integral_constant<unsigned, (1u << static_cast<unsigned>(I))>
  operator()(Projection<I>) const {
    return {};
  }
  template <typename K>
  constexpr std::integral_constant<unsigned, 0u> operator()(
      const Const<K>&) const {
    return {};
  }
  template <typename Leaf>
  constexpr std::integral_constant<unsigned, ~0u> operator()(
      const Leaf&) const {
    return {};
  }
  template <typename Node, typename... R>
  constexpr std::integral_constant<unsigned, (R::value | ... | 0u)> operator()(
      const Node&, R...) const {
    return {};
  }
};
/** @brief The slots @c T mentions, through any cv-ref (a @c constexpr
 *  variable's @c decltype is @c const).  @tparam T the term or predicate. */
export template <typename T>
inline constexpr unsigned slots_v = decltype(cata(
    Slots{}, std::declval<const std::remove_cvref_t<T>&>()))::value;

/** @brief Whether a mask names at most the given slot.  @param mask the slots.
 *  @param slot the one allowed. */
export constexpr bool only_slot(unsigned mask, unsigned slot) {
  return (mask & ~(1u << slot)) == 0u;
}

// ── Evaluation ──────────────────────────────────────────────────────────────

template <std::integral T, std::floating_point F>
constexpr bool in_integer_range(F x) {
  return x >= static_cast<F>(std::numeric_limits<T>::min()) &&
         x < static_cast<F>(std::numeric_limits<T>::max() / 2 + 1) * 2;
}

/** @brief A constant converted into the carrier @c C it meets: exactly, or not
 *  at all.  Between built-in types: in range, then a round trip that keeps the
 *  sign.  A carrier with its own rule states it beside its definition as
 *  @c carrier_cast(std::type_identity<C>, k), found by ADL (ℕ refuses a
 *  negative constant); any other class carrier converts by its constructor.
 *  @tparam C the carrier.  @tparam K the constant's type.  @param k the
 *  constant. */
export template <typename C, typename K>
constexpr C carrier_cast(const K& k) {
  if constexpr (std::is_arithmetic_v<C> && std::is_arithmetic_v<K>) {
    if constexpr (std::integral<C> && std::floating_point<K>) {
      if (!in_integer_range<C>(k))
        throw std::domain_error("a constant outside the carrier's range");
    } else if constexpr (std::floating_point<C> && std::floating_point<K>) {
      if (k > std::numeric_limits<C>::max() ||
          k < std::numeric_limits<C>::lowest())
        throw std::domain_error("a constant outside the carrier's range");
    }
    const C c = static_cast<C>(k);
    if constexpr (std::floating_point<C> && std::integral<K>) {
      if (!in_integer_range<K>(c))
        throw std::domain_error("a constant the carrier cannot hold exactly");
    }
    if (static_cast<K>(c) != k || ((k < K{}) != (c < C{})))
      throw std::domain_error("a constant the carrier cannot hold exactly");
    return c;
  } else if constexpr (requires { carrier_cast(std::type_identity<C>{}, k); }) {
    return carrier_cast(std::type_identity<C>{}, k);
  } else if constexpr (std::constructible_from<C, const K&>) {
    return C(k);
  } else {
    // A reading dispatched on another sort instantiates this branch as well,
    // so a constant the carrier has no conversion from is an error only where
    // it is actually read.
    throw std::domain_error(
        "a constant of a type the carrier has no conversion from");
  }
}

template <typename T>
inline constexpr bool is_const_v = false;
template <typename K>
inline constexpr bool is_const_v<Const<K>> = true;

/** @brief A projection read at a point: its coordinate, or the point itself
 *  for the unary @c π.  An overload for another kind of point (an environment)
 *  is found by ADL.  @param p the point. */
export template <IsRingIntegral auto I, typename P>
constexpr decltype(auto) term_value(Projection<I>, const P& p) {
  if constexpr (I == 0)
    return p;
  else
    return Projection<I>{}(p);
}
/** @brief The carrier two operands meet in: a constant takes the other's, two
 *  terms their common type.  @tparam A,B the operand terms.  @tparam RA,RB the
 *  types of their values. */
template <typename A, typename B, typename RA, typename RB>
constexpr auto meet_type() {
  if constexpr (is_const_v<A> && !is_const_v<B>)
    return std::type_identity<RB>{};
  else if constexpr (is_const_v<B> && !is_const_v<A>)
    return std::type_identity<RA>{};
  else
    return std::type_identity<std::common_type_t<RA, RB>>{};
}
template <typename A, typename B, typename RA, typename RB>
using meet_t = typename decltype(meet_type<A, B, RA, RB>())::type;
/** @brief The value @c x of the operand @c T placed in the carrier @c V: a
 *  constant exactly or not at all (@c carrier_cast), a term's value by
 *  conversion.  @tparam V the carrier.  @tparam T the operand term. */
template <typename V, typename T, typename R>
constexpr V into(const R& x) {
  if constexpr (is_const_v<T>)
    return carrier_cast<V>(x);
  else
    return static_cast<V>(x);
}
/** @brief The transparent operator @c Op applied at the carrier @c C, as the
 *  typed @c std::plus<C> would apply it: an arithmetic result is converted
 *  back to @c C, so a narrow carrier wraps instead of promoting, and a
 *  comparison keeps its @c bool.  No type constructor is taken as an
 *  argument.  @tparam Op the transparent operator.  @tparam C the carrier. */
template <typename Op, typename C>
  requires requires { typename Op::is_transparent; }
constexpr auto apply_at(const C& a, const C& b) {
  using R = decltype(Op{}(a, b));
  if constexpr (std::same_as<R, bool>)
    return Op{}(a, b);
  else
    return static_cast<C>(Op{}(a, b));
}

// ── The rules: admission by ADL, each beside its certificate ────────────────

/** @brief The carrier a rule is stated for, as a tag.  A rule
 *  @c admits(On<C>, key, divisor_is_constant) says that the operator @c key is
 *  in the @b signature of @c C: a symbol one of the carrier's certified
 *  varieties has.  It is declared in this namespace beside the certificate
 *  and found by ADL, so it is in force where that module is imported: the
 *  comparisons beside the partial orders (@c order:poset), @c // and @c %
 *  beside @c IsDividableChain (@c order:completeness), a field's @c / beside
 *  @c IsField (@c algebra:field); @c + @c - @c * and @c == @c != here, whose
 *  certificates are @c category's.  Without a rule an operator is not in the
 *  signature.  @tparam C the carrier. */
export template <typename C>
struct On {};
/** @brief The two divisions Python tells apart, as keys: a field's, and the
 *  Euclidean one of an ordered carrier with remainder.  The typed grammar's
 *  @c std::divides<> is the carrier's own, admitted where either is. */
export struct FieldDivision {};
export struct EuclideanDivision {};

/** @brief No rule: not admitted.  @tparam C the carrier.  @tparam Key the
 *  operator. */
export template <typename C, typename Key>
constexpr bool admits(On<C>, Key, bool) {
  return false;
}
/** @brief @c + @c - @c * where the carrier's certified laws have the symbol:
 *  @c + on a commutative monoid, @c - on an abelian group (@c a @c - @c b is
 *  @c a @c + @c (-b)), @c * on a monoid.  A magma is total and closed, so the
 *  Pst total operator surface is inside the laws; ℕ's monus is not, until its
 *  variety is named.  @tparam C the carrier. */
export template <typename C>
  requires IsCommutativeMonoid<C, std::plus<C>>
constexpr bool admits(On<C>, std::plus<>, bool) {
  return true;
}
export template <typename C>
  requires IsAbelianGroup<C, std::plus<C>> && IsClosedUnder<C, std::minus<>>
constexpr bool admits(On<C>, std::minus<>, bool) {
  return true;
}
export template <typename C>
  requires IsMonoid<C, std::multiplies<C>>
constexpr bool admits(On<C>, std::multiplies<>, bool) {
  return true;
}
/** @brief The carrier's own division, by a nonzero constant: a field's or the
 *  Euclidean one, whichever rule the carrier has.  @tparam C the carrier. */
export template <typename C>
constexpr bool admits(On<C> c, std::divides<>, bool divisor_is_constant) {
  return admits(c, FieldDivision{}, divisor_is_constant) ||
         admits(c, EuclideanDivision{}, divisor_is_constant);
}
/** @brief @c == and @c != where the carrier compares.  @tparam C the
 *  carrier. */
export template <std::equality_comparable C>
constexpr bool admits(On<C>, std::equal_to<>, bool) {
  return true;
}
export template <std::equality_comparable C>
constexpr bool admits(On<C>, std::not_equal_to<>, bool) {
  return true;
}
/** @brief A comparison's key: the standard functor it is.  @tparam R the
 *  comparison. */
template <Rel R>
struct rel_key;
template <>
struct rel_key<Rel::Lt> {
  using type = std::less<>;
};
template <>
struct rel_key<Rel::Le> {
  using type = std::less_equal<>;
};
template <>
struct rel_key<Rel::Gt> {
  using type = std::greater<>;
};
template <>
struct rel_key<Rel::Ge> {
  using type = std::greater_equal<>;
};
template <>
struct rel_key<Rel::Eq> {
  using type = std::equal_to<>;
};
template <>
struct rel_key<Rel::Ne> {
  using type = std::not_equal_to<>;
};
template <Rel R>
using rel_key_t = typename rel_key<R>::type;

// ── A term whose shape is a value: its readings ─────────────────────────────

/** @brief An operator of a term built at run time.  Python's @c / and @c //
 *  are both the carrier's division; what differs is where each is admitted
 *  (a field, an ordered word). */
export enum class Arith { Add, Sub, Mul, Div, FloorDiv, Mod };
/** @brief The spelling of an operator, for a refusal. */
export constexpr const char* spelling(Arith op) {
  switch (op) {
    case Arith::Add:
      return "+";
    case Arith::Sub:
      return "-";
    case Arith::Mul:
      return "*";
    case Arith::Div:
      return "/";
    case Arith::FloorDiv:
      return "//";
    case Arith::Mod:
      return "%";
  }
  return "?";
}
/** @brief Refused at the carrier: an operator not total there, a division by
 *  a term, an order comparison on an unordered carrier, a slot the points
 *  have not.  Raised when a term built at run time is bound. */
export struct not_admitted : std::logic_error {
  using std::logic_error::logic_error;
};

/** @brief The sorts of a point type: the carriers of its coordinates, each
 *  once.  A carrier value has one sort; a pair of two carriers two (one where
 *  they agree).  A term built at run time names no carrier until it is bound,
 *  and every node then has a sort: a projection its slot's, a constant its
 *  context's, an operator its operands' common one, a comparison 𝔹.
 *  @tparam P the point type. */
template <typename P>
struct point_sorts {
  using type = std::tuple<P>;
};
template <typename A, typename B>
struct point_sorts<std::pair<A, B>> {
  using type =
      std::conditional_t<std::same_as<A, B>, std::tuple<A>, std::tuple<A, B>>;
};
/** @brief A row: a point of an n-ary product whose arity and columns' sorts
 *  are data.  Each cell is a value of one of the carriers @c Cs, and the cell's
 *  alternative is its column's sort, so a row is its own schema.  Slots are
 *  @c π_1 … @c π_n.  @tparam Cs the carriers a column may have. */
export template <typename... Cs>
struct Row {
  using Cell = std::variant<Cs...>;
  std::vector<Cell> cells;
  friend bool operator==(const Row&, const Row&) = default;
};
template <typename... Cs>
struct point_sorts<Row<Cs...>> {
  using type = std::tuple<Cs...>;
};
template <typename P>
inline constexpr bool is_row_v = false;
template <typename... Cs>
inline constexpr bool is_row_v<Row<Cs...>> = true;
export template <typename P>
using point_sorts_t = typename point_sorts<P>::type;
/** @brief Whether @c C is one of the sorts @c S. */
export template <typename C, typename S>
inline constexpr bool is_sort_of_v = false;
template <typename C, typename... Cs>
inline constexpr bool is_sort_of_v<C, std::tuple<Cs...>> =
    (std::same_as<C, Cs> || ...);
/** @brief The tuple @c S with @c C appended, unless @c C is in it already. */
template <typename S, typename C>
struct with_carrier;
template <typename... Cs, typename C>
struct with_carrier<std::tuple<Cs...>, C> {
  using type = std::conditional_t<is_sort_of_v<C, std::tuple<Cs...>>,
                                  std::tuple<Cs...>, std::tuple<Cs..., C>>;
};
/** @brief The sorts a term at points of @c P takes: the points' carriers
 *  first (a slot's sort is its index there), then the truth chains a set
 *  applied to a term lands in, @c ap(S, t) of sort @c S's codomain.
 *  @tparam P the point type. */
export template <typename P>
using term_sorts_t =
    typename with_carrier<typename with_carrier<point_sorts_t<P>, bool>::type,
                          dedekind::category::Ternary>::type;
/** @brief The value of a term built at run time at a point: the one sort's
 *  carrier, or the sum of the sorts where there are more. */
template <typename Sorts>
struct value_sum;
template <typename A>
struct value_sum<std::tuple<A>> {
  using type = A;
};
template <typename A, typename B, typename... Cs>
struct value_sum<std::tuple<A, B, Cs...>> {
  using type = std::variant<A, B, Cs...>;
};
export template <typename P>
using rt_value_t = typename value_sum<point_sorts_t<P>>::type;

/** @brief No sort yet: a tree of constants alone, which takes the sort of its
 *  context. */
export inline constexpr std::size_t no_sort = static_cast<std::size_t>(-1);
/** @brief The sort of the slot @c slot at points of @c P, an index into
 *  @c point_sorts_t<P>: read off the type, or off a representative row, whose
 *  cells are its schema.  @throws not_admitted for a slot the points have not.
 *  @param rep a point of @c P (any, for a typed point). */
export template <typename P>
constexpr std::size_t slot_sort(unsigned slot, const P& rep = P{}) {
  if constexpr (is_row_v<P>) {
    if (slot == 0)
      throw not_admitted("π names an element; the points are rows: π_1 … π_n");
    if (slot > rep.cells.size())
      throw not_admitted("a slot past the row's arity");
    return rep.cells[slot - 1].index();
  } else if constexpr (requires { typename P::first_type; }) {
    using A = typename P::first_type;
    using B = typename P::second_type;
    if (slot == 1) return 0;
    if (slot == 2) return std::same_as<A, B> ? 0 : 1;
    throw not_admitted(slot == 0 ? "π names an element; the points are pairs"
                                 : "binary products only: π_1 and π_2");
  } else {
    if (slot == 0) return 0;
    throw not_admitted(
        "π_1 / π_2 name a pair's coordinates; the points are elements");
  }
}
/** @brief The points' one sort, where every slot has the same; @c no_sort on
 *  a product of several.  @param rep a point of @c P. */
export template <typename P>
constexpr std::size_t one_sort(const P& rep = P{}) {
  if constexpr (is_row_v<P>) {
    if (rep.cells.empty()) return no_sort;
    for (const auto& c : rep.cells)
      if (c.index() != rep.cells.front().index()) return no_sort;
    return rep.cells.front().index();
  } else {
    return std::tuple_size_v<point_sorts_t<P>> == 1 ? 0 : no_sort;
  }
}
/** @brief The sort of a tree of constants without a context: the points' one
 *  sort where they have one.  @throws not_admitted on a product of several
 *  carriers, where a constant alone names none.  @param rep a point of @c P.
 */
export template <typename P>
constexpr std::size_t default_sort(std::size_t s, const P& rep = P{}) {
  if (s != no_sort) return s;
  if (const std::size_t one = one_sort<P>(rep); one != no_sort) return one;
  throw not_admitted(
      "a term of constants alone names no carrier on a product of several");
}
/** @brief Apply @c f to the sort @c s of @c P as a type:
 *  @c f(type_identity<C>).  @c s is a valid index (the caller joined the
 *  sorts).  @tparam P the point type.  @tparam I the first candidate. */
export template <typename P, std::size_t I = 0, typename F>
constexpr auto with_sort(std::size_t s, const F& f) {
  using Sorts = term_sorts_t<P>;
  if constexpr (I + 1 < std::tuple_size_v<Sorts>) {
    if (s == I) return f(std::type_identity<std::tuple_element_t<I, Sorts>>{});
    return with_sort<P, I + 1>(s, f);
  } else {
    return f(std::type_identity<std::tuple_element_t<I, Sorts>>{});
  }
}
/** @brief A value of the sort @c D is read in the sort @c C: the same sort,
 *  or a truth chain along the dominance into @c C's species (𝔹 ↪ K₃, the
 *  registered @c lift_logic).  The one coercion of the grammar; any other
 *  pair of carriers meets through an arrow applied, never implied.
 *  @tparam D the value's sort.  @tparam C the sort it is read in. */
export template <typename D, typename C>
concept EmbedsInto = std::same_as<D, C> || requires {
  typename dedekind::category::classifier_logic_t<C>;
  requires std::same_as<typename dedekind::category::classifier_logic_t<C>::Ω,
                        C>;
  requires dedekind::category::LiftableInto<
      D, dedekind::category::classifier_logic_t<C>>;
};
/** @brief @c d read in the sort @c C.  @tparam C the sort.  @param d the
 *  value. */
export template <typename C, typename D>
  requires EmbedsInto<D, C>
constexpr C embed_into(const D& d) {
  if constexpr (std::same_as<D, C>)
    return d;
  else
    return dedekind::category::lift_logic<
        dedekind::category::classifier_logic_t<C>>(d);
}
/** @brief @c d read in the sort @c C where it embeds, refused elsewhere. */
template <typename C, typename D>
constexpr C read_as(const D& d) {
  if constexpr (EmbedsInto<D, C>)
    return embed_into<C>(d);
  else
    throw not_admitted("a slot of another carrier than the term's sort");
}
/** @brief The coordinate @c slot of the point @c p, read in the sort @c C the
 *  dispatch chose; the point itself for the unary @c π.  @tparam C the sort.
 *  @param p the point. */
/** @brief A cell read in the sort @c C, whichever alternative it holds. */
template <typename C>
struct ReadAs {
  template <typename D>
  constexpr C operator()(const D& d) const {
    return read_as<C>(d);
  }
};
template <typename C, typename P>
constexpr C coordinate_as(unsigned slot, const P& p) {
  if constexpr (is_row_v<P>) {
    if (slot == 0 || slot > p.cells.size())
      throw not_admitted("a slot the row has not");
    return std::visit(ReadAs<C>{}, p.cells[slot - 1]);
  } else if constexpr (requires { typename P::first_type; }) {
    if (slot == 1) return read_as<C>(p.first);
    if (slot == 2) return read_as<C>(p.second);
    throw not_admitted("binary products only: π_1 and π_2");
  } else {
    if (slot != 0)
      throw not_admitted(
          "π_1 / π_2 name a pair's coordinates; the points are elements");
    return read_as<C>(p);
  }
}

/** @brief Whether @c op is admitted on the carrier @c C, with the divisor a
 *  constant (nonzero, checked when the term was built) or a term: the rules
 *  above and beside their certificates, read at run time.  @tparam C the
 *  carrier. */
template <typename C>
constexpr bool op_admitted(Arith op, bool divisor_is_constant) {
  switch (op) {
    case Arith::Add:
      return admits(On<C>{}, std::plus<>{}, divisor_is_constant);
    case Arith::Sub:
      return admits(On<C>{}, std::minus<>{}, divisor_is_constant);
    case Arith::Mul:
      return admits(On<C>{}, std::multiplies<>{}, divisor_is_constant);
    case Arith::Div:
      return admits(On<C>{}, FieldDivision{}, divisor_is_constant);
    case Arith::FloorDiv:
      return admits(On<C>{}, EuclideanDivision{}, divisor_is_constant);
    case Arith::Mod:
      return admits(On<C>{}, std::modulus<>{}, divisor_is_constant);
  }
  return false;
}
/** @brief Whether the comparison @c r is admitted on @c C: @c == and @c !=
 *  where it compares, the order comparisons where the order's rule is in
 *  force (@c order:poset).  @tparam C the carrier. */
export template <typename C>
constexpr bool rel_admitted(Rel r) {
  switch (r) {
    case Rel::Lt:
      return admits(On<C>{}, std::less<>{}, false);
    case Rel::Le:
      return admits(On<C>{}, std::less_equal<>{}, false);
    case Rel::Gt:
      return admits(On<C>{}, std::greater<>{}, false);
    case Rel::Ge:
      return admits(On<C>{}, std::greater_equal<>{}, false);
    case Rel::Eq:
      return admits(On<C>{}, std::equal_to<>{}, false);
    case Rel::Ne:
      return admits(On<C>{}, std::not_equal_to<>{}, false);
  }
  return false;
}
/** @brief @c x @c op @c y in the carrier @c C, for an admitted operator,
 *  through the typed functor; a branch whose expression the carrier has not is
 *  not instantiated (the typed functor's body would be, so the guard asks the
 *  expression). */
template <typename C>
constexpr C apply_op(Arith op, const C& x, const C& y) {
  switch (op) {
    case Arith::Add:
      if constexpr (requires { x + y; }) return std::plus<C>{}(x, y);
      break;
    case Arith::Sub:
      if constexpr (requires { x - y; }) return std::minus<C>{}(x, y);
      break;
    case Arith::Mul:
      if constexpr (requires { x * y; }) return std::multiplies<C>{}(x, y);
      break;
    case Arith::Div:
    case Arith::FloorDiv:
      if constexpr (requires { x / y; }) return std::divides<C>{}(x, y);
      break;
    case Arith::Mod:
      if constexpr (requires { x % y; }) return std::modulus<C>{}(x, y);
      break;
  }
  throw not_admitted(std::string("the operator ") + spelling(op) +
                     " is not admitted on the carrier");
}
/** @brief @c x @c r @c y for a comparison known at run time, admitted when
 *  the term was bound; the branches a carrier has not are not instantiated. */
export template <typename C>
constexpr bool compare_rel(Rel r, const C& x, const C& y) {
  switch (r) {
    case Rel::Lt:
      if constexpr (requires { x < y; }) return x < y;
      break;
    case Rel::Le:
      if constexpr (requires { x <= y; }) return x <= y;
      break;
    case Rel::Gt:
      if constexpr (requires { x > y; }) return x > y;
      break;
    case Rel::Ge:
      if constexpr (requires { x >= y; }) return x >= y;
      break;
    case Rel::Eq:
      return x == y;
    case Rel::Ne:
      return x != y;
  }
  throw not_admitted("the comparison is not admitted on the carrier");
}
/** @brief The sort a constant names by itself, at points of @c P: the first
 *  sort @c C with @c constant_of(type_identity<C>, k) (found by ADL) true,
 *  for a constant written as a value of @c C (a bool, a Ternary) rather than
 *  a numeral; @c no_sort otherwise, a numeral taking its context's sort.
 *  @tparam P the point type.  @param k the constant. */
export template <typename P, std::size_t I = 0, typename K>
constexpr std::size_t constant_sort(const K& k) {
  using Sorts = term_sorts_t<P>;
  if constexpr (I < std::tuple_size_v<Sorts>) {
    using C = std::tuple_element_t<I, Sorts>;
    if constexpr (requires { constant_of(std::type_identity<C>{}, k); }) {
      if (constant_of(std::type_identity<C>{}, k)) return I;
    }
    return constant_sort<P, I + 1>(k);
  } else {
    return no_sort;
  }
}
/** @brief Whether the sort @c b embeds into the sort @c a, at points of
 *  @c P. */
template <typename P>
struct EmbedsAt {
  std::size_t b;
  template <typename A>
  struct From {
    template <typename B>
    constexpr bool operator()(std::type_identity<B>) const {
      return EmbedsInto<B, A>;
    }
  };
  template <typename A>
  constexpr bool operator()(std::type_identity<A>) const {
    return with_sort<P>(b, From<A>{});
  }
};
/** @brief The common sort of two operands at points of @c P: a constant takes
 *  the other's, and a truth chain meets its dominance in the larger (𝔹 and
 *  K₃ in K₃).  @throws not_admitted where two carriers meet. */
export template <typename P>
constexpr std::size_t join_sorts(std::size_t a, std::size_t b) {
  if (a == no_sort) return b;
  if (b == no_sort) return a;
  if (a == b) return a;
  if (with_sort<P>(a, EmbedsAt<P>{b})) return a;
  if (with_sort<P>(b, EmbedsAt<P>{a})) return b;
  throw not_admitted(
      "the operands are of two carriers: a term lives in one sort, and an "
      "arrow between the carriers is applied, not implied");
}
/** @brief Whether a value of the sort @c from is read in the sort @c into at
 *  points of @c P (@c EmbedsInto): the same sort, or along the dominance.
 *  @param from,into indices into @c term_sorts_t<P>. */
export template <typename P>
constexpr bool sort_embeds(std::size_t from, std::size_t into) {
  return with_sort<P>(into, EmbedsAt<P>{from});
}
/** @brief A cell of the sort @c sort, its carrier's default value: a row
 *  built from a schema is a representative of its points. */
template <std::size_t I, typename... Cs>
constexpr std::variant<Cs...> default_cell(std::size_t sort) {
  if constexpr (I < sizeof...(Cs)) {
    if (sort == I) return std::variant<Cs...>{std::in_place_index<I>};
    return default_cell<I + 1, Cs...>(sort);
  } else {
    throw not_admitted("a sort the row's carriers have not");
  }
}
/** @brief The row @c rep extended by a column of the sort @c sort: the point
 *  a quantifier's body is read at.  Any other point has no column to bind.
 *  @param rep the row.  @param sort the bound column's sort. */
export template <typename... Cs>
constexpr Row<Cs...> with_bound(const Row<Cs...>& rep, std::size_t sort) {
  Row<Cs...> out = rep;
  out.cells.push_back(default_cell<0, Cs...>(sort));
  return out;
}
export template <typename P>
constexpr P with_bound(const P&, std::size_t) {
  throw not_admitted(
      "a quantifier binds a column of a row; the points are elements or pairs");
}
/** @brief The last cell of @c row set to @c v, of the sort @c C: the bound
 *  column's value under a quantifier.  @param row the extended row.  @param v
 *  the value. */
export template <typename C, typename... Cs>
constexpr void set_bound(Row<Cs...>& row, const C& v) {
  row.cells.back() = typename Row<Cs...>::Cell{std::in_place_type<C>, v};
}
export template <typename C, typename P>
constexpr void set_bound(P&, const C&) {
  throw not_admitted("a quantifier binds a column of a row");
}
/** @brief The @c i-th value of a carrier for a bounded search: a finite chain
 *  cycles through its values from ⊥, an unsigned word counts up, and any other
 *  carrier states its enumeration as @c nth_value(type_identity<C>, i), found
 *  by ADL beside its definition (@c Modular: 0, 1, −1, 2, −2, …).  @tparam C
 *  the carrier.  @param i the index. */
export template <typename C>
constexpr C search_value(std::uint64_t i) {
  if constexpr (dedekind::category::IsPst<C>) {
    std::uint64_t n = 1;
    for (C v = unit<C>(Sup{}); v != unit<C>(Inf{});
         v = dedekind::category::Successor<C>{}(v))
      ++n;
    C v = unit<C>(Sup{});
    for (std::uint64_t k = i % n; k > 0; --k)
      v = dedekind::category::Successor<C>{}(v);
    return v;
  } else if constexpr (std::unsigned_integral<C>) {
    return static_cast<C>(i);
  } else if constexpr (requires { nth_value(std::type_identity<C>{}, i); }) {
    return nth_value(std::type_identity<C>{}, i);
  } else {
    throw not_admitted("a carrier the bounded search cannot enumerate");
  }
}
/** @brief The row whose columns have the sorts @c sorts: a representative of
 *  the points of that schema, for binding and normal forms.
 *  @tparam Cs the carriers.  @param sorts an index into @c Cs per column. */
export template <typename... Cs>
constexpr Row<Cs...> row_of_sorts(const std::vector<std::size_t>& sorts) {
  Row<Cs...> r;
  r.cells.reserve(sorts.size());
  for (const std::size_t s : sorts)
    r.cells.push_back(default_cell<0, Cs...>(s));
  return r;
}
/** @brief A constant read in the sort @c C: a numeral cast into it, a
 *  constant of a sort (a bool, a Ternary) read in its own and embedded.
 *  @tparam P the point type.  @tparam C the sort.  @param k the constant. */
template <typename P, typename C, typename K>
struct ConstantAs {
  const K& k;
  template <typename D>
  constexpr C operator()(std::type_identity<D>) const {
    if constexpr (EmbedsInto<D, C>)
      return embed_into<C>(carrier_cast<D>(k));
    else
      throw not_admitted(
          "a constant of another carrier than the term's sort: an arrow "
          "between the carriers is applied, not implied");
  }
};
export template <typename P, typename C, typename K>
constexpr C constant_as(const K& k) {
  const std::size_t s = constant_sort<P>(k);
  if (s == no_sort) return carrier_cast<C>(k);
  return with_sort<P>(s, ConstantAs<P, C, K>{k});
}
/** @brief The sort of a term built at run time at points of @c P, from its
 *  projections and its constants of a sort (@c constant_sort): @c no_sort for
 *  a tree of numerals, which takes its context's.  Defined with @c Term.
 *  @tparam P the point type. */
export template <typename P, typename K>
constexpr std::size_t sort_of(const Term<K>& t, const P& rep = P{});

/** @brief The value algebra: a term or a predicate folded at the point @c p.
 *  A projection reads its coordinate, a constant is its value, a predicate the
 *  grammar does not know is evaluated as itself; an operator or a comparison
 *  combines its operands in the carrier of the non-constant one; an applied
 *  arrow converts its argument into its domain.  @tparam P the point's type. */
template <typename P>
struct Value {
  const P& p;

  template <IsRingIntegral auto I>
  constexpr decltype(auto) operator()(Projection<I>) const {
    return term_value(Projection<I>{}, p);
  }
  template <typename K>
  constexpr K operator()(const Const<K>& c) const {
    return c.value;
  }
  template <typename Q>
    requires requires(const Q& q, const P& x) { q(x); }
  constexpr auto operator()(const Q& q) const {
    return q(p);
  }
  template <typename Op, typename A, typename B, typename X, typename Y>
  constexpr auto operator()(const BinOp<Op, A, B>&, const X& x,
                            const Y& y) const {
    using C = meet_t<A, B, X, Y>;
    static_assert(admits(On<C>{}, Op{}, is_const_v<B>),
                  "the operator has no rule on the carrier: not total there, "
                  "a division by a term, or its module is not imported (order "
                  "for // and %, algebra for a field's /)");
    return apply_at<Op, C>(into<C, A>(x), into<C, B>(y));
  }
  template <typename F, typename A, typename X>
  constexpr auto operator()(const Ap<F, A>& t, const X& x) const {
    return t.f(carrier_cast<typename F::Domain>(x));
  }
  template <Rel R, typename A, typename B, typename X, typename Y>
  constexpr bool operator()(const Cmp<R, A, B>&, const X& x, const Y& y) const {
    using C = meet_t<A, B, X, Y>;
    static_assert(admits(On<C>{}, rel_key_t<R>{}, false),
                  "the comparison has no rule on the carrier: an order "
                  "comparison needs the carrier's order (import order)");
    return rel_apply<R>(into<C, A>(x), into<C, B>(y));
  }
  /** @brief The connectives short-circuit as the built-in @c && / @c || do,
   *  and McCarthy's conditional evaluates only the branch it selects: these
   *  nodes steer the recursion into their legs themselves (see @c cata), so a
   *  leg the operator does not reach is never evaluated. */
  template <typename A, typename B>
  constexpr auto steer(const RelAnd<A, B>& t) const {
    return cata(*this, t.a) && cata(*this, t.b);
  }
  template <typename A, typename B>
  constexpr auto steer(const RelOr<A, B>& t) const {
    return cata(*this, t.a) || cata(*this, t.b);
  }
  template <typename Pr, typename A, typename B>
  constexpr auto steer(const CondT<Pr, A, B>& t) const {
    using C =
        meet_t<A, B, decltype(cata(*this, t.a)), decltype(cata(*this, t.b))>;
    return static_cast<bool>(cata(*this, t.p)) ? into<C, A>(cata(*this, t.a))
                                               : into<C, B>(cata(*this, t.b));
  }
  /** @brief A term built at run time, read at the point: its sort is inferred
   *  from its projections (a constant takes its context's), and it is read in
   *  that sort.  The value is the point's one carrier, or the sum of its two.
   */
  template <typename K>
  constexpr rt_value_t<P> steer(const Term<K>& t) const {
    return with_sort<P>(default_sort<P>(sort_of<P>(t, p), p),
                        EvalTerm<K>{*this, t});
  }
  /** @brief A predicate built at run time: a comparison in its operands'
   *  common sort, the connectives in 𝔹. */
  template <typename K>
  constexpr bool steer(const Pred<K>& t) const {
    switch (t.kind) {
      case Pred<K>::Kind::Cmp:
        return with_sort<P>(
            default_sort<P>(
                join_sorts<P>(sort_of<P>(*t.a, p), sort_of<P>(*t.b, p)), p),
            Compare<K>{*this, t});
      case Pred<K>::Kind::And:
        return steer(*t.p) && steer(*t.q);
      case Pred<K>::Kind::Or:
        return steer(*t.p) || steer(*t.q);
      case Pred<K>::Kind::Top:
        return true;
      case Pred<K>::Kind::Bottom:
        return false;
      case Pred<K>::Kind::Exists:
      case Pred<K>::Kind::Forall:
        // Decided by enumeration over a finite chain; over an infinite
        // carrier the answer is a Σ / Π verdict, read by @c verdict.
        return with_sort<P>(t.sort, Quantify<K>{*this, t});
    }
    throw not_admitted("an unknown predicate");
  }
  /** @brief The term read in the sort @c C: a projection its coordinate, a
   *  constant cast, an operator applied where admitted, a conditional's
   *  chosen branch.  Each node visited checks its own admission, so a
   *  predicate called directly is at par with one the former bound. */
  template <typename C, typename K>
  constexpr C eval_as(const Term<K>& t) const {
    switch (t.kind) {
      case Term<K>::Kind::Proj:
        return coordinate_as<C>(t.slot, p);
      case Term<K>::Kind::Const:
        return constant_as<P, C>(t.value);
      case Term<K>::Kind::Bin:
        if (!op_admitted<C>(t.op, t.b->kind == Term<K>::Kind::Const))
          throw not_admitted(std::string("the operator ") + spelling(t.op) +
                             " is not in the carrier's signature");
        return apply_op<C>(t.op, eval_as<C>(*t.a), eval_as<C>(*t.b));
      case Term<K>::Kind::Cond:
        return steer(*t.p) ? eval_as<C>(*t.a) : eval_as<C>(*t.b);
    }
    throw not_admitted("an unknown term");
  }

 private:
  /** @brief The dispatch of a term's reading to its sort. */
  template <typename K>
  struct EvalTerm {
    const Value& v;
    const Term<K>& t;
    template <typename C>
    constexpr rt_value_t<P> operator()(std::type_identity<C>) const {
      if constexpr (is_sort_of_v<C, point_sorts_t<P>>)
        return rt_value_t<P>{v.template eval_as<C>(t)};
      else
        throw not_admitted(
            "a truth value is read by a comparison or a former, not as the "
            "point's value");
    }
  };
  /** @brief The dispatch of a comparison to its operands' sort. */
  template <typename K>
  struct Compare {
    const Value& v;
    const Pred<K>& t;
    template <typename C>
    constexpr bool operator()(std::type_identity<C>) const {
      if (!rel_admitted<C>(t.rel))
        throw not_admitted("the comparison is not in the carrier's signature");
      return compare_rel(t.rel, v.template eval_as<C>(*t.a),
                         v.template eval_as<C>(*t.b));
    }
  };
  /** @brief A quantifier over the sort @c C, decided where @c C is a finite
   *  chain: every value of the bound column tried, @c ∃ any, @c ∀ all. */
  template <typename K>
  struct Quantify {
    const Value& v;
    const Pred<K>& t;
    template <typename C>
    constexpr bool operator()(std::type_identity<C>) const {
      // A bound column has one of the row's carriers: the truth chains the
      // term sorts add for ap(S, t) are not among them unless the row has them.
      if constexpr (!is_sort_of_v<C, point_sorts_t<P>>) {
        throw not_admitted(
            "a quantifier over a sort the row's carriers have not");
      } else if constexpr (dedekind::category::IsPst<C>) {
        const bool universal = t.kind == Pred<K>::Kind::Forall;
        P row = with_bound(v.p, t.sort);
        // A fold over the whole chain, ⊥ to ⊤, with the early exit a
        // quantifier has: any_of / all_of over the interval, a range whose
        // length is fixed before the walk (the Jlt paper, Rule 3).
        const auto holds_at = [&](C c) {
          set_bound(row, c);
          return Value<P>{row}.steer(*t.p);
        };
        const dedekind::category::chain_view<C> whole{unit<C>(Sup{}),
                                                      unit<C>(Inf{})};
        return universal ? std::ranges::all_of(whole, holds_at)
                         : std::ranges::any_of(whole, holds_at);
      } else {
        throw not_admitted(
            "∃ / ∀ over an infinite carrier is a Σ / Π verdict: read it with "
            "verdict()");
      }
    }
  };
};

/** @brief The verdict of a predicate at a point, in K₃: a comparison or a
 *  quantifier over a finite chain is decided (⊤ or ⊥); a quantifier over an
 *  infinite carrier searches its first @c budget values (@c search_value): a
 *  witness of @c ∃ is ⊤, a counterexample to @c ∀ is ⊥, and an exhausted
 *  budget is U, never the other verdict (Σ, respectively Π: the run-time
 *  reading of @c Semidecided).  Same-kind quantifiers over infinite
 *  carriers nested below (a middle object of several columns) share the
 *  budget, @c b values each with @c b^k @c ≤ @c budget; an opposite
 *  quantifier over an infinite carrier inside answers U.
 *  The connectives are Kleene's.  @tparam P the point type.  @param t the
 *  predicate.  @param point the point.  @param budget the search's values. */
export template <typename P, typename K>
constexpr dedekind::category::Ternary verdict(
    const Pred<K>& t, const P& point,
    std::uint64_t budget = std::uint64_t{1} << 16);
/** @brief Whether @c b^k @c ≤ @c n, without overflow. */
constexpr bool power_within(std::uint64_t b, unsigned k, std::uint64_t n) {
  std::uint64_t power = 1;
  for (unsigned j = 0; j < k; ++j) {
    if (b != 0 && power > n / b) return false;
    power *= b;
  }
  return power <= n;
}
/** @brief The largest @c b with @c b^k @c ≤ @c n (@c n @c ≥ 1): the values
 *  each of @c k nested searches may try within a budget of @c n, found by
 *  bisection, logarithmic in @c n. */
constexpr std::uint64_t root_within(std::uint64_t n, unsigned k) {
  std::uint64_t lo = 1, hi = n;
  while (lo < hi) {
    const std::uint64_t mid = lo + (hi - lo + 1) / 2;
    if (power_within(mid, k, n))
      lo = mid;
    else
      hi = mid - 1;
  }
  return lo;
}
template <typename P, typename K>
struct Verdict {
  using Ternary = dedekind::category::Ternary;
  using Kleene = dedekind::category::Kleene;
  const P& point;
  std::uint64_t budget;
  constexpr Ternary operator()(const Pred<K>& t) const {
    switch (t.kind) {
      case Pred<K>::Kind::Cmp:
        return Value<P>{point}.steer(t) ? Ternary::True : Ternary::False;
      case Pred<K>::Kind::And:
        return Kleene::AND((*this)(*t.p), (*this)(*t.q));
      case Pred<K>::Kind::Or:
        return Kleene::OR((*this)(*t.p), (*this)(*t.q));
      case Pred<K>::Kind::Top:
        return Ternary::True;
      case Pred<K>::Kind::Bottom:
        return Ternary::False;
      case Pred<K>::Kind::Exists:
      case Pred<K>::Kind::Forall:
        return with_sort<P>(t.sort, Search{*this, t});
    }
    throw not_admitted("an unknown predicate");
  }
  /** @brief Whether a sort is a finite chain, enumerated exactly. */
  struct FiniteSort {
    template <typename C>
    constexpr bool operator()(std::type_identity<C>) const {
      return dedekind::category::IsPst<C>;
    }
  };
  /** @brief The quantifiers over infinite carriers in @c p, anywhere: those
   *  of the kind @c universal counted, an opposite one flagged. */
  constexpr unsigned infinite_quantifiers(const Pred<K>& p, bool universal,
                                          bool& alternation) const {
    switch (p.kind) {
      case Pred<K>::Kind::Cmp:
      case Pred<K>::Kind::Top:
      case Pred<K>::Kind::Bottom:
        return 0;
      case Pred<K>::Kind::And:
      case Pred<K>::Kind::Or:
        return infinite_quantifiers(*p.p, universal, alternation) +
               infinite_quantifiers(*p.q, universal, alternation);
      case Pred<K>::Kind::Exists:
      case Pred<K>::Kind::Forall: {
        unsigned here = 0;
        if (!with_sort<P>(p.sort, FiniteSort{})) {
          if ((p.kind == Pred<K>::Kind::Forall) != universal)
            alternation = true;
          else
            here = 1;
        }
        return here + infinite_quantifiers(*p.p, universal, alternation);
      }
    }
    return 0;
  }
  struct Search {
    const Verdict& v;
    const Pred<K>& t;
    template <typename C>
    constexpr Ternary operator()(std::type_identity<C>) const {
      if constexpr (!is_sort_of_v<C, point_sorts_t<P>>) {
        throw not_admitted(
            "a quantifier over a sort the row's carriers have not");
      } else {
        return search(std::type_identity<C>{});
      }
    }
    template <typename C>
    constexpr Ternary search(std::type_identity<C>) const {
      const bool universal = t.kind == Pred<K>::Kind::Forall;
      P row = with_bound(v.point, t.sort);
      if constexpr (dedekind::category::IsPst<C>) {
        // The same fold, in K₃ and without an early exit: fold_left over the
        // interval with Kleene's AND / OR.
        const auto verdict_at = [&](Ternary all, C c) {
          set_bound(row, c);
          const Ternary here = Verdict{row, v.budget}(*t.p);
          return universal ? Kleene::AND(all, here) : Kleene::OR(all, here);
        };
        return std::ranges::fold_left(
            dedekind::category::chain_view<C>{unit<C>(Sup{}), unit<C>(Inf{})},
            universal ? Ternary::True : Ternary::False, verdict_at);
      } else {
        // An opposite quantifier over an infinite carrier inside: a search
        // cannot witness ∃ through a ∀ it cannot decide, nor refute a ∀
        // through an ∃; U.  Same-kind quantifiers over infinite carriers
        // below (a middle object of several columns) share the budget: each
        // of the k levels tries b values, b^k ≤ budget.
        bool alternation = false;
        const unsigned k =
            1 + v.infinite_quantifiers(*t.p, universal, alternation);
        if (alternation || v.budget == 0) return Ternary::Unknown;
        const std::uint64_t b = root_within(v.budget, k);
        std::uint64_t inner = 1;
        for (unsigned j = 1; j < k; ++j) inner *= b;
        for (std::uint64_t i = 0; i < b; ++i) {
          set_bound(row, search_value<C>(i));
          const Ternary here = Verdict{row, inner}(*t.p);
          if (!universal && here == Ternary::True) return Ternary::True;
          if (universal && here == Ternary::False) return Ternary::False;
        }
        return Ternary::Unknown;
      }
    }
  };
};
template <typename P, typename K>
constexpr dedekind::category::Ternary verdict(const Pred<K>& t, const P& point,
                                              std::uint64_t budget) {
  return Verdict<P, K>{point, budget}(t);
}

/** @brief The value of a term or predicate at a point.  @param t the term.
 *  @param p the point. */
export template <typename T, typename P>
constexpr auto term_value(const T& t, const P& p) {
  return cata(Value<P>{p}, t);
}
/** @brief A term built at run time read in the sort @c C its context names
 *  (the other side of a graph atom, say), at the point @c p.  @tparam C the
 *  sort.  @param t the term.  @param p the point. */
export template <typename C, typename K, typename P>
constexpr C term_value_as(const Term<K>& t, const P& p) {
  return Value<P>{p}.template eval_as<C>(t);
}

// ── Atoms ───────────────────────────────────────────────────────────────────

/** @brief The atom @c a @c R @c b over two terms, a relational predicate.
 *  @tparam R the comparison.  @tparam A,B the terms. */
export template <Rel R, typename A, typename B>
struct Cmp {
  using is_rel_predicate = void;
  A a;
  B b;
  /** @param p the point (a pair, or a carrier value for an atom on @c π). */
  template <typename P>
  constexpr bool operator()(const P& p) const {
    return term_value(*this, p);
  }
};

template <Rel R, typename A, typename B>
constexpr auto make_cmp(A a, B b) {
  auto ta = term_of(std::move(a));
  auto tb = term_of(std::move(b));
  return Cmp<R, decltype(ta), decltype(tb)>{std::move(ta), std::move(tb)};
}

/** @brief The comparisons of terms.  The term operand is tested first, so a
 *  candidate never asks a carrier for its own equality.  @tparam A,B the
 *  operands. */
export template <typename A, typename B>
  requires TermOperands<A, B>
constexpr auto operator==(A a, B b) {
  return make_cmp<Rel::Eq>(std::move(a), std::move(b));
}
export template <typename A, typename B>
  requires TermOperands<A, B>
constexpr auto operator!=(A a, B b) {
  return make_cmp<Rel::Ne>(std::move(a), std::move(b));
}
export template <typename A, typename B>
  requires TermOperands<A, B>
constexpr auto operator<(A a, B b) {
  return make_cmp<Rel::Lt>(std::move(a), std::move(b));
}
export template <typename A, typename B>
  requires TermOperands<A, B>
constexpr auto operator<=(A a, B b) {
  return make_cmp<Rel::Le>(std::move(a), std::move(b));
}
export template <typename A, typename B>
  requires TermOperands<A, B>
constexpr auto operator>(A a, B b) {
  return make_cmp<Rel::Gt>(std::move(a), std::move(b));
}
export template <typename A, typename B>
  requires TermOperands<A, B>
constexpr auto operator>=(A a, B b) {
  return make_cmp<Rel::Ge>(std::move(a), std::move(b));
}

/** @brief The meet and join of two relational predicates under the reducer's
 *  customization-point names, so the set-level @c & / @c | on two relations
 *  over one product collapse to one comprehension.  @tparam A,B the operands.
 */
export template <IsRelPredicate A, IsRelPredicate B>
constexpr RelAnd<A, B> structured_and(A a, B b) {
  return {std::move(a), std::move(b)};
}
export template <IsRelPredicate A, IsRelPredicate B>
constexpr RelOr<A, B> structured_or(A a, B b) {
  return {std::move(a), std::move(b)};
}

/** @brief @c !atom flips the comparison.  @param c the atom. */
export template <Rel R, typename A, typename B>
constexpr Cmp<negate(R), A, B> operator!(const Cmp<R, A, B>& c) {
  return {c.a, c.b};
}
/** @brief De Morgan on the connectives: @c !(p && q) is @c !p || !q, and
 *  dually.  @param p the conjunction or disjunction. */
export template <typename A, typename B>
constexpr auto operator!(const RelAnd<A, B>& p) {
  return RelOr<decltype(!p.a), decltype(!p.b)>{!p.a, !p.b};
}
export template <typename A, typename B>
constexpr auto operator!(const RelOr<A, B>& p) {
  return RelAnd<decltype(!p.a), decltype(!p.b)>{!p.a, !p.b};
}

// ── Binding ─────────────────────────────────────────────────────────────────

/** @brief The value type a term takes at a point of type @c X. */
export template <typename X, typename T>
using value_at_t = std::remove_cvref_t<decltype(term_value(
    std::declval<const T&>(), std::declval<const X&>()))>;

/** @brief The result of a binding: nothing but the checks that ran. */
struct Bound {};
/** @brief The binding algebra at the points @c X: a constant beside a term
 *  (@c a, @c b of an operator, a comparison or a conditional) meets the term's
 *  carrier now; a constant under an arrow meets the arrow's domain.  A leaf
 *  binds trivially.  @tparam X the point type. */
template <typename X>
struct Bind {
  X rep{};  // a point of X: a row is its own schema
  template <typename Leaf>
  constexpr Bound operator()(const Leaf&) const {
    return {};
  }
  template <typename Node, typename... R>
    requires requires(const Node& n) {
      n.a;
      n.b;
    }
  constexpr Bound operator()(const Node& n, R...) const {
    using A = decltype(n.a);
    using B = decltype(n.b);
    if constexpr (is_const_v<A> && !is_const_v<B>)
      (void)carrier_cast<value_at_t<X, B>>(n.a.value);
    if constexpr (is_const_v<B> && !is_const_v<A>)
      (void)carrier_cast<value_at_t<X, A>>(n.b.value);
    return {};
  }
  template <typename F, typename A, typename R>
  constexpr Bound operator()(const Ap<F, A>& t, R) const {
    if constexpr (is_const_v<A>)
      (void)carrier_cast<typename F::Domain>(t.a.value);
    return {};
  }
  /** @brief A term built at run time meets the points now: every slot one the
   *  points have, every constant cast into its sort, every operator admitted
   *  in it, and no two carriers in one term. */
  template <typename K>
  constexpr Bound steer(const Term<K>& t) const {
    return with_sort<X>(default_sort<X>(sort_of<X>(t, rep), rep),
                        BindTerm<K>{*this, t});
  }
  template <typename K>
  constexpr Bound steer(const Pred<K>& t) const {
    switch (t.kind) {
      case Pred<K>::Kind::Cmp:
        return with_sort<X>(
            default_sort<X>(
                join_sorts<X>(sort_of<X>(*t.a, rep), sort_of<X>(*t.b, rep)),
                rep),
            BindCmp<K>{*this, t});
      case Pred<K>::Kind::And:
      case Pred<K>::Kind::Or:
        steer(*t.p);
        steer(*t.q);
        break;
      case Pred<K>::Kind::Top:
      case Pred<K>::Kind::Bottom:
        break;
      case Pred<K>::Kind::Exists:
      case Pred<K>::Kind::Forall: {
        // The bound slot is the next past the points' width; the body binds
        // at the row extended by the bound column.
        if constexpr (is_row_v<X>) {
          if (t.slot != rep.cells.size() + 1)
            throw not_admitted("a quantifier binds the next slot of the row");
        }
        Bind<X>{with_bound(rep, t.sort)}.steer(*t.p);
        break;
      }
    }
    return {};
  }
  /** @brief The term bound in the sort @c C. */
  template <typename C, typename K>
  constexpr void bind_as(const Term<K>& t) const {
    switch (t.kind) {
      case Term<K>::Kind::Proj:
        (void)coordinate_as<C>(t.slot, rep);
        break;
      case Term<K>::Kind::Const:
        (void)constant_as<X, C>(t.value);
        break;
      case Term<K>::Kind::Bin: {
        bind_as<C>(*t.a);
        bind_as<C>(*t.b);
        const bool division =
            t.op == Arith::Div || t.op == Arith::FloorDiv || t.op == Arith::Mod;
        const bool by_constant = t.b->kind == Term<K>::Kind::Const;
        if (!op_admitted<C>(t.op, by_constant))
          throw not_admitted(
              std::string("the operator ") + spelling(t.op) +
              (division ? " is admitted only by a nonzero constant, where the "
                          "carrier has it"
                        : " is not total on the carrier"));
        // Nonzero as written, but 0 on the carrier (p in 𝔽ₚ, by the quotient
        // map): refused here, as a 0 written is refused when built.
        if (division && by_constant && carrier_cast<C>(t.b->value) == C{})
          throw std::domain_error(
              "a division by a constant that is 0 on the "
              "carrier");
        break;
      }
      case Term<K>::Kind::Cond:
        steer(*t.p);
        bind_as<C>(*t.a);
        bind_as<C>(*t.b);
        break;
    }
  }

 private:
  template <typename K>
  struct BindTerm {
    const Bind& b;
    const Term<K>& t;
    template <typename C>
    constexpr Bound operator()(std::type_identity<C>) const {
      b.template bind_as<C>(t);
      return {};
    }
  };
  template <typename K>
  struct BindCmp {
    const Bind& b;
    const Pred<K>& t;
    template <typename C>
    constexpr Bound operator()(std::type_identity<C>) const {
      b.template bind_as<C>(*t.a);
      b.template bind_as<C>(*t.b);
      if (!rel_admitted<C>(t.rel))
        throw not_admitted(
            "the comparison has no rule on the carrier: an order comparison "
            "needs the carrier's order");
      return {};
    }
  };
};

/** @brief Bind a term or atom to the points @c X it is evaluated at: every
 *  constant meets the carrier of the term beside it now, so a constant the
 *  carrier cannot hold raises at construction, not at the first evaluation
 *  (a compile error in @c constexpr code).
 *  @tparam X the point type.  @tparam T the term. */
export template <typename X, typename T>
constexpr void bind_check(const T& t, const X& rep = X{}) {
  (void)cata(Bind<X>{rep}, t);
}

// ── Terms whose shape is a value ────────────────────────────────────────────

/** @brief A term built at run time: the grammar of @c IsTerm with its shape a
 *  value and its constants of type @c K, which meets the carrier through
 *  @c carrier_cast when the term is bound; McCarthy's conditional
 *  @c cond(p, a, b) chooses a branch by a predicate.  Its legs are shared,
 *  so a term is cheap to copy and to combine.  A division by the
 * constant 0 is refused when the term is built, as for the typed grammar.
 * @tparam K the constants' type. */
export template <typename K>
struct Term {
  enum class Kind { Proj, Const, Bin, Cond };
  Kind kind{Kind::Const};
  unsigned slot{};
  K value{};
  Arith op{};
  std::shared_ptr<const Term> a, b;
  std::shared_ptr<const Pred<K>> p;  // the conditional's predicate

  /** @brief @c π (slot 0), @c π_1, @c π_2. */
  static constexpr Term proj(unsigned slot) {
    Term t;
    t.kind = Kind::Proj;
    t.slot = slot;
    return t;
  }
  /** @brief A constant. */
  static constexpr Term constant(K k) {
    Term t;
    t.value = std::move(k);
    return t;
  }
  /** @brief The term with the variable @c variable replaced by @c with: the
   *  substitution a structural arrow's image is (@c π ↦ @c π @c - @c 1 for
   *  the successor's).  @param variable the slot.  @param with its
   *  replacement.  @return the substituted term. */
  constexpr Term subst(unsigned variable, const Term& with) const {
    return map_slots(SubstOne{variable, with});
  }
  /** @brief Every slot replaced at once by the term @c f gives it: a
   *  simultaneous substitution, so a renaming (a product's shift, a
   *  permutation) never captures.  @param f a slot to its replacement. */
  template <typename F>
  constexpr Term map_slots(const F& f) const {
    switch (kind) {
      case Kind::Proj:
        return f(slot);
      case Kind::Const:
        return *this;
      case Kind::Bin: {
        Term t = *this;
        t.a = std::make_shared<const Term>(a->map_slots(f));
        t.b = std::make_shared<const Term>(b->map_slots(f));
        return t;
      }
      case Kind::Cond: {
        Term t = *this;
        t.p = std::make_shared<const Pred<K>>(p->map_slots(f));
        t.a = std::make_shared<const Term>(a->map_slots(f));
        t.b = std::make_shared<const Term>(b->map_slots(f));
        return t;
      }
    }
    return *this;
  }
  /** @brief The one-slot substitution as a slot map. */
  struct SubstOne {
    unsigned variable;
    const Term& with;
    constexpr Term operator()(unsigned s) const {
      return s == variable ? with : proj(s);
    }
  };
  /** @brief McCarthy's conditional: @c a where @c q holds, else @c b; its
   *  body is instantiated where it is used, after @c Pred is complete.
   *  @param q the predicate.  @param a,b the branches. */
  static constexpr Term cond(Pred<K> q, Term a, Term b) {
    Term t;
    t.kind = Kind::Cond;
    t.p = std::make_shared<const Pred<K>>(std::move(q));
    t.a = std::make_shared<const Term>(std::move(a));
    t.b = std::make_shared<const Term>(std::move(b));
    return t;
  }
  /** @brief @c x @c op @c y.  @throws std::domain_error on a division by the
   *  constant 0. */
  static constexpr Term bin(Arith op, Term x, Term y) {
    if ((op == Arith::Div || op == Arith::FloorDiv || op == Arith::Mod) &&
        y.kind == Kind::Const && y.value == K{})
      throw std::domain_error("a division by the constant 0");
    Term t;
    t.kind = Kind::Bin;
    t.op = op;
    t.a = std::make_shared<const Term>(std::move(x));
    t.b = std::make_shared<const Term>(std::move(y));
    return t;
  }
};
/** @brief A relational predicate built at run time: a comparison of two
 *  terms, their conjunctions and disjunctions, and the quantifiers @c ∃ / @c ∀
 *  over a column of a row.  Negation is pushed to the comparisons as it is
 *  built (De Morgan; @c ∃ and @c ∀ swap), so there is no @c Not node.
 *
 *  A quantifier binds the @b next slot: at points of width @c n it binds
 *  @c π_{n+1}, of the sort it names, and its body is read at the row extended
 *  by that column.  Sibling quantifiers therefore bind the same slot in
 *  separate scopes and never need renaming apart; a nested one binds
 *  @c π_{n+2}.  This is the run-time twin of @c sets:quantifier's @c exists /
 *  @c forall, and the ∃ of a relational composition (@c relational:rows).
 *  @tparam K the constants' type. */
export template <typename K>
struct Pred {
  using is_rel_predicate = void;
  enum class Kind { Cmp, And, Or, Top, Bottom, Exists, Forall };
  Kind kind{Kind::Cmp};
  Rel rel{};
  std::shared_ptr<const Term<K>> a, b;
  std::shared_ptr<const Pred> p, q;
  std::size_t sort{};  // a quantifier's bound sort, an index into the sorts
  unsigned slot{};     // a quantifier's bound slot: the next past the width

  /** @brief Whether this node is @c ∃ or @c ∀. */
  constexpr bool quantifier() const {
    return kind == Kind::Exists || kind == Kind::Forall;
  }
  /** @brief @c ∃π_slot ∈ sort. body.  @param sort the bound column's sort.
   *  @param slot the bound slot, the next past the points' width.  @param
   *  body the predicate, on the row extended by that column. */
  static constexpr Pred exists(std::size_t sort, unsigned slot, Pred body) {
    return bound(Kind::Exists, sort, slot, std::move(body));
  }
  /** @brief @c ∀π_slot ∈ sort. body, dual to @c exists. */
  static constexpr Pred forall(std::size_t sort, unsigned slot, Pred body) {
    return bound(Kind::Forall, sort, slot, std::move(body));
  }
  /** @brief The quantifier of @c q over a new body.  @param q a quantifier
   *  node.  @param body its new body. */
  static constexpr Pred bound_like(const Pred& q, Pred body) {
    return bound(q.kind, q.sort, q.slot, std::move(body));
  }

  /** @brief The atom @c x @c r @c y.  @param r the comparison.  @param x,y
   *  the terms.  @return the predicate. */
  static constexpr Pred cmp(Rel r, Term<K> x, Term<K> y) {
    Pred t;
    t.rel = r;
    t.a = std::make_shared<const Term<K>>(std::move(x));
    t.b = std::make_shared<const Term<K>>(std::move(y));
    return t;
  }
  /** @brief @c x @c ∧ @c y.  @param x,y the predicates.  @return the meet. */
  static constexpr Pred conj(Pred x, Pred y) { return node(Kind::And, x, y); }
  /** @brief @c x @c ∨ @c y.  @param x,y the predicates.  @return the join. */
  static constexpr Pred disj(Pred x, Pred y) { return node(Kind::Or, x, y); }
  /** @brief ⊤ and ⊥, the Boolean algebra's constants: a decided atom written
   *  back (@c 0 @c == @c 0 is ⊤), free of any sort. */
  static constexpr Pred top() {
    Pred t;
    t.kind = Kind::Top;
    return t;
  }
  static constexpr Pred bottom() {
    Pred t;
    t.kind = Kind::Bottom;
    return t;
  }
  /** @brief The predicate with the variable @c variable replaced by the term
   *  @c with in every atom.  @param variable the slot.  @param with its
   *  replacement.  @return the substituted predicate. */
  constexpr Pred subst(unsigned variable, const Term<K>& with) const {
    return map_slots(typename Term<K>::SubstOne{variable, with});
  }
  /** @brief Every slot of every atom replaced at once (@c Term::map_slots).
   *  @param f a slot to its replacement. */
  template <typename F>
  constexpr Pred map_slots(const F& f) const {
    if (kind == Kind::Top || kind == Kind::Bottom) return *this;
    Pred t = *this;
    if (kind == Kind::Cmp) {
      t.a = std::make_shared<const Term<K>>(a->map_slots(f));
      t.b = std::make_shared<const Term<K>>(b->map_slots(f));
    } else if (quantifier()) {
      // The bound slot is renumbered with the rest (a product shifts every
      // slot); it stays a slot.
      const Term<K> to = f(slot);
      if (to.kind != Term<K>::Kind::Proj)
        throw not_admitted("a bound slot is renamed, never replaced by a term");
      t.slot = to.slot;
      t.p = std::make_shared<const Pred>(p->map_slots(f));
    } else {
      t.p = std::make_shared<const Pred>(p->map_slots(f));
      t.q = std::make_shared<const Pred>(q->map_slots(f));
    }
    return t;
  }
  /** @brief @c !p, in negation normal form. */
  constexpr Pred negated() const {
    switch (kind) {
      case Kind::Cmp: {
        Pred t = *this;
        t.rel = negate(rel);
        return t;
      }
      case Kind::And:
        return node(Kind::Or, p->negated(), q->negated());
      case Kind::Or:
        return node(Kind::And, p->negated(), q->negated());
      case Kind::Top:
        return bottom();
      case Kind::Bottom:
        return top();
      case Kind::Exists:
        return forall(sort, slot, p->negated());
      case Kind::Forall:
        return exists(sort, slot, p->negated());
    }
    return *this;
  }
  /** @param x the point (a pair, or a carrier value for an atom on @c π). */
  template <typename P>
  constexpr bool operator()(const P& x) const {
    return term_value(*this, x);
  }

 private:
  // Built through the factories only, so every leg is set: a default
  // predicate would be a comparison of nothing.
  Pred() = default;
  static constexpr Pred node(Kind k, Pred x, Pred y) {
    Pred t;
    t.kind = k;
    t.p = std::make_shared<const Pred>(std::move(x));
    t.q = std::make_shared<const Pred>(std::move(y));
    return t;
  }
  static constexpr Pred bound(Kind k, std::size_t sort, unsigned slot,
                              Pred body) {
    Pred t;
    t.kind = k;
    t.sort = sort;
    t.slot = slot;
    t.p = std::make_shared<const Pred>(std::move(body));
    return t;
  }
};
export template <typename K>
constexpr bool has_quantifier(const Pred<K>& p);
/** @brief Whether a term has a quantifier in a conditional's guard. */
template <typename K>
constexpr bool has_quantifier(const Term<K>& t) {
  switch (t.kind) {
    case Term<K>::Kind::Proj:
    case Term<K>::Kind::Const:
      return false;
    case Term<K>::Kind::Bin:
      return has_quantifier(*t.a) || has_quantifier(*t.b);
    case Term<K>::Kind::Cond:
      return has_quantifier(*t.p) || has_quantifier(*t.a) ||
             has_quantifier(*t.b);
  }
  return false;
}
/** @brief Whether a predicate has a quantifier anywhere, a conditional's
 *  guard in a comparison's operands included.  @param p the predicate. */
export template <typename K>
constexpr bool has_quantifier(const Pred<K>& p) {
  switch (p.kind) {
    case Pred<K>::Kind::Cmp:
      return has_quantifier(*p.a) || has_quantifier(*p.b);
    case Pred<K>::Kind::Top:
    case Pred<K>::Kind::Bottom:
      return false;
    case Pred<K>::Kind::And:
    case Pred<K>::Kind::Or:
      return has_quantifier(*p.p) || has_quantifier(*p.q);
    case Pred<K>::Kind::Exists:
    case Pred<K>::Kind::Forall:
      return true;
  }
  return false;
}

template <typename P, typename K>
constexpr std::size_t sort_of(const Term<K>& t, const P& rep) {
  switch (t.kind) {
    case Term<K>::Kind::Proj:
      return slot_sort<P>(t.slot, rep);
    case Term<K>::Kind::Const:
      return constant_sort<P>(t.value);
    case Term<K>::Kind::Bin:
    case Term<K>::Kind::Cond:
      return join_sorts<P>(sort_of<P>(*t.a, rep), sort_of<P>(*t.b, rep));
  }
  return no_sort;
}

/** @brief The slots a term built at run time mentions, as a mask: the run-time
 *  twin of @c slots_v, read with the same @c only_slot.  @param t the term. */
export template <typename K>
constexpr unsigned slots(const Term<K>& t);
export template <typename K>
constexpr unsigned slots(const Pred<K>& p) {
  switch (p.kind) {
    case Pred<K>::Kind::Cmp:
      return slots(*p.a) | slots(*p.b);
    case Pred<K>::Kind::And:
    case Pred<K>::Kind::Or:
      return slots(*p.p) | slots(*p.q);
    case Pred<K>::Kind::Top:
    case Pred<K>::Kind::Bottom:
      return 0u;
    case Pred<K>::Kind::Exists:
    case Pred<K>::Kind::Forall:
      // The bound slot is not free in the quantifier.
      return slots(*p.p) &
             ~(p.slot < std::numeric_limits<unsigned>::digits ? 1u << p.slot
                                                              : 0u);
  }
  return ~0u;
}
export template <typename K>
constexpr unsigned slots(const Term<K>& t) {
  switch (t.kind) {
    case Term<K>::Kind::Proj:
      // A slot past the mask's width mentions, conservatively, every slot.
      return t.slot < std::numeric_limits<unsigned>::digits ? 1u << t.slot
                                                            : ~0u;
    case Term<K>::Kind::Const:
      return 0u;
    case Term<K>::Kind::Bin:
      return slots(*t.a) | slots(*t.b);
    case Term<K>::Kind::Cond:
      return slots(*t.p) | slots(*t.a) | slots(*t.b);
  }
  return ~0u;
}

// ── Pinning the numerals ────────────────────────────────────────────────────

/** @brief A numeral read in the sort @c C and written back as a constant of
 *  that sort, through @c lift; a carrier @c lift has no constant for keeps
 *  the numeral. */
template <typename K, typename Lift>
struct PinConstant {
  const K& k;
  const Lift& lift;
  template <typename C>
  constexpr K operator()(std::type_identity<C>) const {
    if constexpr (requires(const C& c) {
                    { lift(c) } -> std::convertible_to<K>;
                  })
      return lift(carrier_cast<C>(k));
    else
      return k;
  }
};
export template <typename P, typename K, typename Lift>
constexpr Pred<K> pin_constants(const Pred<K>& p, const Lift& lift,
                                const P& rep = P{});
/** @brief The term @c t, of the sort @c s at points of @c P, with every
 *  numeral pinned to it.  @param t the term.  @param s its sort.  @param lift
 *  a value of a sort into a constant that names it.  @param rep a point of
 *  @c P. */
template <typename P, typename K, typename Lift>
constexpr Term<K> pin_term(const Term<K>& t, std::size_t s, const Lift& lift,
                           const P& rep) {
  using T = Term<K>;
  switch (t.kind) {
    case T::Kind::Proj:
      return t;
    case T::Kind::Const:
      if (s == no_sort || constant_sort<P>(t.value) != no_sort) return t;
      return T::constant(with_sort<P>(s, PinConstant<K, Lift>{t.value, lift}));
    case T::Kind::Bin:
      return T::bin(t.op, pin_term<P>(*t.a, s, lift, rep),
                    pin_term<P>(*t.b, s, lift, rep));
    case T::Kind::Cond:
      return T::cond(pin_constants<P>(*t.p, lift, rep),
                     pin_term<P>(*t.a, s, lift, rep),
                     pin_term<P>(*t.b, s, lift, rep));
  }
  return t;
}
/** @brief The predicate with every numeral pinned to the sort its context
 *  gives it at points of @c P: the constant then names its sort itself, so
 *  the term keeps its meaning when it moves (substituted into another
 *  context by @c ap, or split by @c lift_conditionals into an atom with no
 *  slot left).  @param p the predicate.  @param lift a value of a sort into a
 *  constant that names it (@c constant_of). */
export template <typename P, typename K, typename Lift>
constexpr Pred<K> pin_constants(const Pred<K>& p, const Lift& lift,
                                const P& rep) {
  using Pr = Pred<K>;
  switch (p.kind) {
    case Pr::Kind::Top:
    case Pr::Kind::Bottom:
      return p;
    case Pr::Kind::And:
      return Pr::conj(pin_constants<P>(*p.p, lift, rep),
                      pin_constants<P>(*p.q, lift, rep));
    case Pr::Kind::Or:
      return Pr::disj(pin_constants<P>(*p.p, lift, rep),
                      pin_constants<P>(*p.q, lift, rep));
    case Pr::Kind::Exists:
    case Pr::Kind::Forall:
      return Pr::bound_like(
          p, pin_constants<P>(*p.p, lift, with_bound(rep, p.sort)));
    case Pr::Kind::Cmp: {
      // A comparison of numerals alone takes the points' one sort, as binding
      // reads it; on a product of several it names none and is left as
      // written.
      std::size_t s =
          join_sorts<P>(sort_of<P>(*p.a, rep), sort_of<P>(*p.b, rep));
      if (s == no_sort) s = one_sort<P>(rep);
      return Pr::cmp(p.rel, pin_term<P>(*p.a, s, lift, rep),
                     pin_term<P>(*p.b, s, lift, rep));
    }
  }
  return p;
}
/** @brief A slot's definition: the term @c t of a conjunct @c π_slot @c == @c t
 *  (or @c t @c == @c π_slot) with @c t free of the slot, and the conjunction
 *  without it.  @c ∃π_slot. P is then the rest with @c t for @c π_slot: the
 *  existential is eliminated by substitution.  Dually, under @c ∀ a disjunct
 *  @c π_slot @c != @c t defines it: @c ∀g. (g ≠ t ∨ Q) ≡ Q[t/g].
 *  @tparam K the constants' type. */
export template <typename K>
struct Definition {
  Term<K> term;
  Pred<K> rest;
};
/** @brief The first conjunct of @c p, leftmost, that defines the slot (the
 *  first disjunct, for @c universal); nothing where none does.  @param p the
 *  predicate.  @param slot the slot.  @param universal whether the slot is
 *  bound by @c ∀ rather than @c ∃. */
export template <typename K>
constexpr std::optional<Definition<K>> defining_term(const Pred<K>& p,
                                                     unsigned slot,
                                                     bool universal = false) {
  using Pr = Pred<K>;
  const Rel atom = universal ? Rel::Ne : Rel::Eq;
  const auto connective = universal ? Pr::Kind::Or : Pr::Kind::And;
  // The unit of the connective: what is left when the defining atom was all.
  const Pr unit = universal ? Pr::bottom() : Pr::top();
  const auto free_of = [slot](const Term<K>& t) {
    return (slots(t) &
            (slot < std::numeric_limits<unsigned>::digits ? 1u << slot
                                                          : ~0u)) == 0u;
  };
  const auto is_slot = [slot](const Term<K>& t) {
    return t.kind == Term<K>::Kind::Proj && t.slot == slot;
  };
  const auto join = [&](const Pr& x, const Pr& y) {
    if (x.kind == unit.kind) return y;
    if (y.kind == unit.kind) return x;
    return universal ? Pr::disj(x, y) : Pr::conj(x, y);
  };
  if (p.kind == Pr::Kind::Cmp) {
    if (p.rel != atom) return std::nullopt;
    if (is_slot(*p.a) && free_of(*p.b)) return Definition<K>{*p.b, unit};
    if (is_slot(*p.b) && free_of(*p.a)) return Definition<K>{*p.a, unit};
    return std::nullopt;
  }
  if (p.kind != connective) return std::nullopt;
  if (auto d = defining_term(*p.p, slot, universal))
    return Definition<K>{d->term, join(d->rest, *p.q)};
  if (auto d = defining_term(*p.q, slot, universal))
    return Definition<K>{d->term, join(*p.p, d->rest)};
  return std::nullopt;
}

/** @brief No carrier value as a constant: the lift that names nothing, so
 *  @c eliminate_quantifiers eliminates by definition alone and leaves every
 *  quantifier over a chain to the search. */
export struct NoConstants {};
/** @brief The values of a finite chain as constants, through @c lift: the
 *  cases of Shannon's expansion; none for any other carrier. */
template <typename K, typename Lift>
struct ChainValues {
  const Lift& lift;
  template <typename C>
  constexpr std::vector<dedekind::sets::Term<K>> operator()(
      std::type_identity<C>) const {
    std::vector<dedekind::sets::Term<K>> out;
    if constexpr (dedekind::category::IsPst<C> && requires(const C& c) {
                    { lift(c) } -> std::convertible_to<K>;
                  }) {
      const auto constant_at = [&](C v) {
        return dedekind::sets::Term<K>::constant(lift(v));
      };
      std::ranges::transform(
          dedekind::category::chain_view<C>{unit<C>(Sup{}), unit<C>(Inf{})},
          std::back_inserter(out), constant_at);
    }
    return out;
  }
};
/** @brief The slots above an eliminated bound slot, moved down by one. */
template <typename K>
struct ShiftDown {
  unsigned above;
  constexpr dedekind::sets::Term<K> operator()(unsigned s) const {
    return dedekind::sets::Term<K>::proj(s > above ? s - 1 : s);
  }
};
/** @brief A definition of the slot found through a block of quantifiers of
 *  the same kind (which commute): the defining atom's term, free of the
 *  slots those quantifiers bind, and the block with that atom removed. */
template <typename K>
constexpr std::optional<dedekind::sets::Definition<K>> defined_through(
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
 *  quantifier over an infinite carrier no atom defines is kept, as is one
 *  over a chain whose values @c lift cannot name (@c NoConstants: by
 *  definition only).  Read on the
 *  predicate as written (pinned, conditionals not lifted): normalising an
 *  atom first would turn the defining shape @c π_h @c == @c t into
 *  @c π_h @c − @c t @c == @c 0.  @tparam P the point type.  @param p the
 *  predicate.  @param lift a value into a constant that names its sort.
 *  @param rep a point of @c P. */
export template <typename P, typename K, typename Lift>
constexpr dedekind::sets::Pred<K> eliminate_quantifiers(
    const dedekind::sets::Pred<K>& p, const Lift& lift, const P& rep = P{}) {
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
      const auto unbound = [&](const Pr& q) {
        return q.map_slots(ShiftDown<K>{p.slot});
      };
      // A definition first, looked for through the inner quantifiers of the
      // same kind (they commute): substituting before an inner one is
      // expanded keeps the defining atom at the top of its connective.
      // A definition substitutes only a term that lands in the bound column's
      // carrier: ∃b ∈ 𝔹. b == t has a witness only where t is a value of 𝔹
      // (a K₃ term may be U); 𝔹 ↪ K₃ the other way is fine.
      // A term of no sort (a bare numeral) lands only where the lift names
      // its carrier: moved into the bound column it would lose the sort its
      // context gave it (1 == 1 on a mixed row).  Under NoConstants the
      // quantifier stays, for the search.
      const auto lands = [&](const dedekind::sets::Term<K>& t) {
        const std::size_t s = dedekind::sets::sort_of<P>(t, rep);
        if (s == dedekind::sets::no_sort)
          return !std::is_same_v<Lift, NoConstants>;
        return dedekind::sets::sort_embeds<P>(s, p.sort);
      };
      if (auto d = defined_through(*p.p, p.slot, universal, 0u);
          d && lands(d->term))
        return eliminate_quantifiers<P>(unbound(d->rest.subst(p.slot, d->term)),
                                        lift, rep);
      const Pr body = eliminate_quantifiers<P>(
          *p.p, lift, dedekind::sets::with_bound(rep, p.sort));
      if (auto d = dedekind::sets::defining_term(body, p.slot, universal);
          d && lands(d->term))
        return eliminate_quantifiers<P>(unbound(d->rest.subst(p.slot, d->term)),
                                        lift, rep);
      const auto cases =
          dedekind::sets::with_sort<P>(p.sort, ChainValues<K, Lift>{lift});
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

/** @brief Whether a mask names only slots among @c π_1 … @c π_{n−1}, the
 *  arguments of a graph whose result is @c π_n.  @param mask the slots.
 *  @param n the result slot. */
export constexpr bool only_arguments(unsigned mask, unsigned n) {
  const unsigned args =
      n < std::numeric_limits<unsigned>::digits ? ((1u << n) - 1u) & ~1u : ~1u;
  return (mask & ~args) == 0u;
}
/** @brief The term of a graph atom built at run time, @c π_n @c == @c t or
 *  @c t @c == @c π_n with @c t mentioning only @c π_1 … @c π_{n−1};
 *  @c nullptr for any other predicate.  The run-time twin of @c IsGraphAtom:
 *  such a relation is functional by its shape, and entire once bound, since
 *  binding admits every operator only where it is total on the carrier (or
 *  divides by a nonzero constant).  @param p the predicate.  @param n the
 *  result slot, the last (2 on a pair). */
export template <typename K>
constexpr const Term<K>* graph_term(const Pred<K>& p, unsigned n = 2) {
  if (p.kind != Pred<K>::Kind::Cmp || p.rel != Rel::Eq) return nullptr;
  const auto is_result = [n](const Term<K>& t) {
    return t.kind == Term<K>::Kind::Proj && t.slot == n;
  };
  if (is_result(*p.a) && only_arguments(slots(*p.b), n)) return p.b.get();
  if (is_result(*p.b) && only_arguments(slots(*p.a), n)) return p.a.get();
  return nullptr;
}

/** @brief The same tree: the same nodes, slots, operators and comparisons, and
 *  equal constants.  Syntactic, so sound for equality of what two normal forms
 *  denote and silent on two different trees.  @param x,y the terms. */
export template <typename K>
constexpr bool same_shape(const Term<K>& x, const Term<K>& y) {
  if (x.kind != y.kind) return false;
  switch (x.kind) {
    case Term<K>::Kind::Proj:
      return x.slot == y.slot;
    case Term<K>::Kind::Const:
      return x.value == y.value;
    case Term<K>::Kind::Bin:
      return x.op == y.op && same_shape(*x.a, *y.a) && same_shape(*x.b, *y.b);
    case Term<K>::Kind::Cond:
      return same_shape(*x.p, *y.p) && same_shape(*x.a, *y.a) &&
             same_shape(*x.b, *y.b);
  }
  return false;
}
/** @brief The same predicate tree, atom by atom.  @param x,y the predicates. */
export template <typename K>
constexpr bool same_shape(const Pred<K>& x, const Pred<K>& y) {
  if (x.kind != y.kind) return false;
  if (x.kind == Pred<K>::Kind::Top || x.kind == Pred<K>::Kind::Bottom)
    return true;
  if (x.kind == Pred<K>::Kind::Cmp)
    return x.rel == y.rel && same_shape(*x.a, *y.a) && same_shape(*x.b, *y.b);
  if (x.quantifier())
    return x.sort == y.sort && x.slot == y.slot && same_shape(*x.p, *y.p);
  return same_shape(*x.p, *y.p) && same_shape(*x.q, *y.q);
}

/** @brief A term with its first conditional taken out: the condition, and
 *  the term with the conditional replaced by each branch.  @tparam K the
 *  constants' type. */
template <typename K>
struct CondSplit {
  Pred<K> q;
  Term<K> then_term, else_term;
};
/** @brief The first conditional of @c t, leftmost, split out; @c nullopt for a
 *  term without one.  @param t the term. */
template <typename K>
constexpr std::optional<CondSplit<K>> split_cond(const Term<K>& t) {
  using T = Term<K>;
  switch (t.kind) {
    case T::Kind::Proj:
    case T::Kind::Const:
      return std::nullopt;
    case T::Kind::Cond:
      return CondSplit<K>{*t.p, *t.a, *t.b};
    case T::Kind::Bin:
      if (auto l = split_cond(*t.a))
        return CondSplit<K>{l->q, T::bin(t.op, l->then_term, *t.b),
                            T::bin(t.op, l->else_term, *t.b)};
      if (auto r = split_cond(*t.b))
        return CondSplit<K>{r->q, T::bin(t.op, *t.a, r->then_term),
                            T::bin(t.op, *t.a, r->else_term)};
      return std::nullopt;
  }
  return std::nullopt;
}
export template <typename K>
constexpr Pred<K> lift_conditionals(const Pred<K>& p);
/** @brief @c (q ∧ x) ∨ (¬q ∧ y), each part lifted.  @param q the condition.
 *  @param x where it holds.  @param y where it fails. */
template <typename K>
constexpr Pred<K> by_cases(const Pred<K>& q, const Pred<K>& x,
                           const Pred<K>& y) {
  return Pred<K>::disj(
      Pred<K>::conj(lift_conditionals(q), lift_conditionals(x)),
      Pred<K>::conj(lift_conditionals(q.negated()), lift_conditionals(y)));
}
/** @brief The predicate with every conditional lifted out of its atoms:
 *  @c cond(q, a, b) @c R @c c is @c (q ∧ a R c) ∨ (¬q ∧ b R c), the law of
 *  McCarthy's conditional, until no atom has one.  The same set at every
 *  point; an applied set @c ap(S, t) @c == @c ⊤, the conditional
 *  @c cond(χ_S[π:=t], ⊤, ⊥) compared, becomes @c χ_S[π:=t] once the decided
 *  atoms are folded (@c algebra:polynomial's @c canonical).  @param p the
 *  predicate. */
export template <typename K>
constexpr Pred<K> lift_conditionals(const Pred<K>& p) {
  using Pr = Pred<K>;
  switch (p.kind) {
    case Pr::Kind::Top:
    case Pr::Kind::Bottom:
      return p;
    case Pr::Kind::And:
      return Pr::conj(lift_conditionals(*p.p), lift_conditionals(*p.q));
    case Pr::Kind::Or:
      return Pr::disj(lift_conditionals(*p.p), lift_conditionals(*p.q));
    case Pr::Kind::Exists:
    case Pr::Kind::Forall:
      return Pr::bound_like(p, lift_conditionals(*p.p));
    case Pr::Kind::Cmp:
      if (auto l = split_cond(*p.a))
        return by_cases(l->q, Pr::cmp(p.rel, l->then_term, *p.b),
                        Pr::cmp(p.rel, l->else_term, *p.b));
      if (auto r = split_cond(*p.b))
        return by_cases(r->q, Pr::cmp(p.rel, *p.a, r->then_term),
                        Pr::cmp(p.rel, *p.a, r->else_term));
      return p;
  }
  return p;
}

/** @section terms__Formal_Verification */
// ── Certificates ────────────────────────────────────────────────────────────
// Rules over a term's structure, read by the relation traits (functional,
// entire) where the relation is built.  Each is sufficient, not necessary:
// false means not certified.

/** @brief The totality algebra at the points @c X: a node is total when its
 *  legs are, and an operator when it is admitted on its carrier (@c admits,
 *  the gate the value algebra enforces: total there, or dividing by a nonzero
 *  constant), an applied arrow when the arrow is total and its argument already
 *  has the arrow's domain (a conversion may fail).  A predicate the grammar
 *  does not know is not certified.  @tparam X the point type. */
template <typename X>
struct Total {
  template <typename Leaf>
  constexpr std::bool_constant<IsTerm<Leaf>> operator()(const Leaf&) const {
    return {};
  }
  // A leg that is not total ends the question before this node's carrier is
  // computed: value_at_t of a non-admitted leg would hit the value algebra's
  // refusal, so it is formed only behind the legs' verdict.
  template <typename Op, typename A, typename B, typename RA, typename RB>
  constexpr auto operator()(const BinOp<Op, A, B>&, RA, RB) const {
    if constexpr (RA::value && RB::value)
      return std::bool_constant<admits(
          On<meet_t<A, B, value_at_t<X, A>, value_at_t<X, B>>>{}, Op{},
          is_const_v<B>)>{};
    else
      return std::false_type{};
  }
  template <typename F, typename A, typename R>
  constexpr auto operator()(const Ap<F, A>&, R) const {
    if constexpr (R::value)
      return std::bool_constant<
          (is_const_v<A> ||
           std::same_as<value_at_t<X, A>, typename F::Domain>) &&
          dedekind::category::IsTotalArrow<F>>{};
    else
      return std::false_type{};
  }
  template <typename Node, typename... R>
  constexpr std::bool_constant<(R::value && ...)> operator()(const Node&,
                                                             R...) const {
    return {};
  }
};
/** @brief A term total at every point of type @c X.  @tparam X the point type.
 *  @tparam T the term. */
export template <typename X, typename T>
inline constexpr bool is_total_term_v =
    decltype(cata(Total<X>{}, std::declval<const T&>()))::value;

/** @brief @c π_2 @c == @c t, or @c t @c == @c π_2, with @c t mentioning only
 * the first slot: the graph of the function @c x ↦ t(x).  @c term is @c t.
 *  @tparam P the atom. */
template <typename P>
struct graph_atom : std::false_type {};
template <typename T>
  requires(only_slot(slots_v<T>, 1))
struct graph_atom<Cmp<Rel::Eq, T, Projection<2>>> : std::true_type {
  using term = T;
};
template <typename T>
  requires(only_slot(slots_v<T>, 1))
struct graph_atom<Cmp<Rel::Eq, Projection<2>, T>> : std::true_type {
  using term = T;
};
/** @brief Whether a leaf is the set under definition, a @c χ node;
 *  @c :recursion registers it.  @tparam T the leaf. */
export template <typename T>
inline constexpr bool is_self_v = false;
/** @brief The self algebra: a term or predicate mentions the set under
 *  definition when one of its leaves is a @c χ node; every node passes it up.
 */
struct MentionsSelf {
  template <typename Leaf>
  constexpr std::bool_constant<is_self_v<Leaf>> operator()(const Leaf&) const {
    return {};
  }
  template <typename Node, typename... R>
  constexpr std::bool_constant<(R::value || ...)> operator()(const Node&,
                                                             R...) const {
    return {};
  }
};
/** @brief Whether @c T mentions the set under definition.  @tparam T the term
 *  or predicate. */
export template <typename T>
inline constexpr bool contains_self_v = decltype(cata(
    MentionsSelf{}, std::declval<const std::remove_cvref_t<T>&>()))::value;
/** @brief An atom that is the graph of a function of the first slot. */
export template <typename P>
concept IsGraphAtom = graph_atom<P>::value;
/** @brief The term of a graph atom.  @tparam P the atom. */
export template <IsGraphAtom P>
using graph_term_t = typename graph_atom<P>::term;
/** @brief The term of a graph atom, the side that is not @c π_2.
 *  @param p the atom. */
export template <IsGraphAtom P>
constexpr graph_term_t<P> graph_term(const P& p) {
  if constexpr (std::same_as<decltype(p.a), graph_term_t<P>>)
    return p.a;
  else
    return p.b;
}

namespace detail_terms_witness {
using P = std::pair<unsigned, unsigned>;
inline constexpr auto succ_graph = π_1 + 1 == π_2;
static_assert(IsRelPredicate<decltype(succ_graph)> &&
                  slots_v<decltype(succ_graph)> == 0b110u,
              "π_1 + 1 == π_2 is an atom on slots 1 and 2");
static_assert(succ_graph(P{4, 5}) && !succ_graph(P{4, 6}), "4 + 1 == 5, not 6");
inline constexpr auto half_after_shift = (π_1 + 1) * 3 == π_2;
static_assert(half_after_shift(P{1, 6}), "(1 + 1)·3 == 6: any nesting");
static_assert(only_slot(slots_v<decltype(π_1 * 3 + 1)>, 1),
              "3·π_1 + 1 mentions only the first slot");
inline constexpr auto on_naturals = π_1 + 1 == π_2;
static_assert(on_naturals(std::pair{finite_cardinality(4),
                                    finite_cardinality(5)}),
              "the constant 1 meets ℕ as the cardinal 1");
using N2 = std::pair<Cardinality, Cardinality>;
static_assert(is_total_term_v<N2, decltype(π_1 * 3 + 1)>,
              "3·x + 1 is total on ℕ: + and · saturate");
static_assert(IsGraphAtom<decltype(π_1 * 3 + 1 == π_2)> &&
                  !IsGraphAtom<decltype(π_2 * π_2 == π_1 * π_1)>,
              "π_2 == t(π_1) is a graph; y² == x² (y = ±x) is not certified");
// A predicate the grammar does not know is a leaf the certificates refuse.
struct Foreign {
  using is_rel_predicate = void;
  constexpr bool operator()(const P&) const { return true; }
};
static_assert(slots_v<RelAnd<decltype(succ_graph), Foreign>> == ~0u &&
                  !is_total_term_v<P, RelAnd<decltype(succ_graph), Foreign>> &&
                  RelAnd<decltype(succ_graph), Foreign>{succ_graph,
                                                        {}}(P{4, 5}),
              "a foreign predicate mentions every slot, is not certified "
              "total, and is evaluated as itself");
}  // namespace detail_terms_witness

}  // namespace dedekind::sets

// The variables are category's projections and their term operators are the
// grammar's, defined above.  Declared into the variables' namespace as well, so
// argument-dependent lookup on a bare projection (π_1 < π_2, π + 1) finds them
// wherever this module is imported; each is constrained to term operands.
export namespace dedekind::category {
using dedekind::sets::operator+;
using dedekind::sets::operator-;
using dedekind::sets::operator*;
using dedekind::sets::operator/;
using dedekind::sets::operator%;
using dedekind::sets::operator==;
using dedekind::sets::operator!=;
using dedekind::sets::operator<;
using dedekind::sets::operator<=;
using dedekind::sets::operator>;
using dedekind::sets::operator>=;
}  // namespace dedekind::category
