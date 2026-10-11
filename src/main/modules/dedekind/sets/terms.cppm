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
 * values, spelled as the literals themselves (@c π1 @c + @c 1 @c == @c π2), so
 * a term built at run time --- from Python --- is the same term.  A constant
 * meets the sort of the term it is combined with: on a pair of @c Cardinality
 * the @c 1 above is the cardinal 1, converted when the term is evaluated and
 * checked to be representable.  Facts about a constant's value (a divisor is
 * not zero) are checked when the term is built: in @c constexpr code a
 * violation is a compile error.
 *
 * Certificates read the structure: an atom @c π2 @c == @c T with
 * @c slots_v<T> @c ⊆ @c {1} is the graph of a function of the first
 * coordinate, whatever @c T is.
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

#include <concepts>
#include <cstddef>
#include <functional>
#include <limits>
#include <stdexcept>
#include <type_traits>
#include <utility>
#include <variant>

export module dedekind.sets:terms;

import dedekind.category;
import :cardinality;  // IsRingIntegral; Cardinality, the ℕ carrier constants
                      // meet

namespace dedekind::sets {
using namespace dedekind::category;

// ── The point-free variable ────────────────────────────────────────────────
/**
 * @brief @c π --- the point-free variable, naming no carrier.
 *
 * @details @c π leaves the carrier open: @c π @c > @c 5 is a cut datum, @c π
 * @c == @c v a point, and the former @c carrier @c | @c ... binds it, the
 * constant meeting the carrier exactly.  @c π is the unary projection; the
 * product coordinates @c π1 / @c π2 follow.
 *
 * @note @c Projection itself is an @b empty symbolic tag (not callable), so it
 * does @b not satisfy @c category::IsProductProjection --- it is the DSL
 * @b spelling of a coordinate, consumed by the comparison operators.  The
 * actual product-projection is the @b accessor @c coord below (@c coord<1> /
 * @c coord<2>), which @b delegates to @c category::π_1 / @c category::π_2 (the
 * canonical projections in @c :limit) and IS a certified
 * @c IsProductProjection --- the static_assert after @c coord pins that
 * agreement.  So @c π1 / @c π2 (tags) and @c category::π_1 / @c π_2 (accessors)
 * are two spellings of one notion, bridged by @c coord.
 * @see dedekind::category::π_1, dedekind::category::π_2
 * @see dedekind::category::IsProductProjection
 */
export template <IsRingIntegral auto Slot>
struct Projection {};

export inline constexpr Projection<0> π{};
// Cosmetic alias for the sole projection π (the bound element of a
// single-carrier comprehension): a plain element also reads as @c χ,
// e.g. @c ℕ @c | @c χ @c <= @c 5.  Same type and behaviour as π (a
// distinct inline object, so a distinct address), just a second spelling.
// (Plain @c x is deliberately not offered: it shadows the pervasive local
// element parameter named @c x and would trip @c -Wshadow.)
export inline constexpr Projection<0> χ{};

// ── Product projections: the relational (point-free) variables ─────────────
/**
 * @brief @c π1 / @c π2 --- the coordinate projections of a pair, the positional
 *        variables of the point-free relational surface.
 *
 * @details A comparison of terms over @c π_I builds a
 * @b strongly-typed predicate on a pair (no lambda); @c && conjoins them
 * (@c || joins), distinct from the set-level @c & / @c | on whole relations;
 * and @c product @c | @c predicate restricts the product to the relation.
 * @c && binds looser than @c |, so the comprehension parenthesises the meet:
 * @c ℕ*ℕ @c | @c (π1 @c < @c π2 @c && @c π1 @c > @c 5) is the relation
 * @f$\{(x,y) \mid x<y \wedge x>5\}@f$ as an @c IsSet on @c ℕ×ℕ.
 */
export inline constexpr Projection<1> π1{};
export inline constexpr Projection<2> π2{};
export inline constexpr Projection<3> π3{};

// Blackboard coordinate aliases so a pair/triple predicate reads as
// @f$z = f(x,y)@f$: the mathematical-italic @c 𝑥/𝑦/𝑧 (U+1D465..7) are
// letter-category identifiers (like @c 𝔸/ℕ), NOT ASCII @c x/y/z, so they carry
// the blackboard look without colliding with ordinary variable names.
export inline constexpr Projection<1> 𝑥{};
export inline constexpr Projection<2> 𝑦{};
export inline constexpr Projection<3> 𝑧{};

/** @brief The @c I-th component of a pair (1 = @c first, 2 = @c second).
 *  Binary products only, so an out-of-range slot (the unary @c π, or @c π3) is
 *  a hard error rather than a silent alias for @c .second.
 *  @note The index @b type is a call-site free variable (@c IsRingIntegral);
 *  the @c requires guard pins the @b value domain to @c {1,2} (binary
 *  products), independent of that type.  Extending @c coord to higher arities
 *  (@c π3 / @c 𝑧 already exist as tags) is the arity-aware follow-up (#824). */
export template <IsRingIntegral auto I, typename P>
  requires(I == 1 || I == 2)
constexpr decltype(auto) coord(const P& p) {
  // Reuse @c :limit's canonical product projections rather than re-deriving
  // @c .first / @c .second here: halfspace's @c coord and @c category::π_1 /
  // @c π_2 are the @b same projection (@c IsProductProjection).  Letting the
  // two surfaces share one accessor stops halfspace shadowing @c :limit.
  if constexpr (I == 1)
    return dedekind::category::π_1(p);
  else
    return dedekind::category::π_2(p);
}

// The two wheels know each other: @c :limit's @c π_1 / @c π_2 are exactly the
// @c coord accessors above, and they are certified @c IsProductProjection there
// (@c limit.cppm).  Pin the agreement so the surfaces cannot silently diverge.
static_assert(
    dedekind::category::IsProductProjection<
        decltype([](const std::pair<int, bool>& p) {
          return dedekind::category::π_1(p);
        }),
        std::pair<int, bool>, int>,
    "coord<1> = category::π_1 is a certified product projection (left).");
static_assert(coord<1>(std::pair{7, false}) ==
                      dedekind::category::π_1(std::pair{7, false}) &&
                  coord<2>(std::pair{7, false}) ==
                      dedekind::category::π_2(std::pair{7, false}),
              "coord IS category::π_1 / π_2 (one projection, two call sites).");

// ── Relational predicates and their pointwise connectives ───────────────────

/** @brief A relational predicate: a predicate on the points of a product (or
 *  of a carrier, for an atom on @c π), tagged @c is_rel_predicate; it meets its
 *  universe only at the former @c |.  @tparam T the candidate. */
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
    return a(p) && b(p);
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
    return a(p) || b(p);
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

/** @brief The slots a term or a relational predicate mentions, as a bit mask
 *  (bit @c i for @c π_i, bit 0 for the unary @c π).  A predicate the grammar
 *  does not know mentions every slot, so no certificate reads more into it
 *  than it says.  @tparam T the term or predicate. */
template <typename T>
inline constexpr unsigned slots_of = ~0u;
/** @brief The public reading, through any cv-ref (a @c constexpr variable's
 *  @c decltype is @c const).  @tparam T the term or predicate. */
export template <typename T>
inline constexpr unsigned slots_v = slots_of<std::remove_cvref_t<T>>;

template <IsRingIntegral auto I>
inline constexpr unsigned slots_of<Projection<I>> =
    1u << static_cast<unsigned>(I);
template <typename K>
inline constexpr unsigned slots_of<Const<K>> = 0u;
template <typename Op, typename A, typename B>
inline constexpr unsigned slots_of<BinOp<Op, A, B>> = slots_v<A> | slots_v<B>;
template <typename F, typename A>
inline constexpr unsigned slots_of<Ap<F, A>> = slots_v<A>;
template <typename P, typename A, typename B>
inline constexpr unsigned slots_of<CondT<P, A, B>> =
    slots_v<P> | slots_v<A> | slots_v<B>;
template <typename A, typename B>
inline constexpr unsigned slots_of<RelAnd<A, B>> = slots_v<A> | slots_v<B>;
template <typename A, typename B>
inline constexpr unsigned slots_of<RelOr<A, B>> = slots_v<A> | slots_v<B>;

/** @brief Whether a mask names at most the given slot.  @param mask the slots.
 *  @param slot the one allowed. */
export constexpr bool only_slot(unsigned mask, unsigned slot) {
  return (mask & ~(1u << slot)) == 0u;
}

// ── Evaluation ──────────────────────────────────────────────────────────────

/** @brief A constant converted into the carrier @c C it meets: exactly, or not
 *  at all (a negative constant meeting ℕ, an out-of-range one meeting a
 *  machine word, a fraction meeting an integer, raise).  Between built-in
 *  types the check is range and round trip; a class carrier converts by its
 *  own constructor, which owns the check.  @tparam C the carrier.
 *  @tparam K the constant's type.  @param k the constant. */
export template <typename C, typename K>
constexpr C carrier_cast(const K& k) {
  if constexpr (std::same_as<C, K>) {
    return k;
  } else if constexpr (std::same_as<C, Cardinality> && std::integral<K>) {
    if (k < K{}) throw std::domain_error("a negative constant meets ℕ");
    return finite_cardinality(static_cast<std::size_t>(k));
  } else if constexpr (std::same_as<C, SignedCardinality> && std::integral<K>) {
    return finite_signed_cardinality(k);
  } else if constexpr (std::same_as<C, bool> && std::integral<K>) {
    if (k != K{0} && k != K{1})
      throw std::domain_error("a constant other than 0 or 1 meets 𝔹");
    return k != K{0};
  } else if constexpr (std::integral<C> && std::integral<K>) {
    if (!std::in_range<C>(k))
      throw std::domain_error("a constant outside the carrier's range");
    return static_cast<C>(k);
  } else if constexpr (std::integral<C> && std::floating_point<K>) {
    constexpr K lo = static_cast<K>(std::numeric_limits<C>::min());
    constexpr K hi = static_cast<K>(std::numeric_limits<C>::max() / 2 + 1) * 2;
    if (!(k >= lo && k < hi))
      throw std::domain_error("a constant outside the carrier's range");
    const C c = static_cast<C>(k);
    if (static_cast<K>(c) != k)
      throw std::domain_error("a fraction meets an integer carrier");
    return c;
  } else if constexpr (std::floating_point<C> && std::integral<K>) {
    if constexpr (std::numeric_limits<K>::digits >
                  std::numeric_limits<C>::digits) {
      constexpr K lim = K{1} << std::numeric_limits<C>::digits;
      if (k > lim || (std::is_signed_v<K> && k < K{0} - lim))
        throw std::domain_error("a constant the carrier cannot hold exactly");
    }
    return static_cast<C>(k);
  } else if constexpr (std::floating_point<C> && std::floating_point<K>) {
    const C c = static_cast<C>(k);
    if (static_cast<K>(c) != k)
      throw std::domain_error("a constant the carrier cannot hold exactly");
    return c;
  } else {
    static_assert(std::constructible_from<C, const K&>,
                  "a constant must convert into the carrier it meets");
    return C(k);
  }
}

template <typename T>
inline constexpr bool is_const_v = false;
template <typename K>
inline constexpr bool is_const_v<Const<K>> = true;

/** @brief The value of a term at a point: a projection reads its coordinate
 *  (the point itself for the unary @c π), a constant is its value, an operator
 *  combines its operands in the carrier of the non-constant one.
 *  @param t the term.  @param p the point. */
export template <IsRingIntegral auto I, typename P>
constexpr decltype(auto) term_value(Projection<I>, const P& p) {
  if constexpr (I == 0)
    return p;
  else
    return coord<I>(p);
}
export template <typename K, typename P>
constexpr K term_value(const Const<K>& c, const P&) {
  return c.value;
}
/** @brief Two operand values in one carrier: a constant takes the other's. */
template <typename A, typename B, typename P>
constexpr auto operand_values(const A& a, const B& b, const P& p) {
  if constexpr (is_const_v<A> && !is_const_v<B>) {
    using C = std::remove_cvref_t<decltype(term_value(b, p))>;
    return std::pair<C, C>{carrier_cast<C>(a.value), term_value(b, p)};
  } else if constexpr (is_const_v<B> && !is_const_v<A>) {
    using C = std::remove_cvref_t<decltype(term_value(a, p))>;
    return std::pair<C, C>{term_value(a, p), carrier_cast<C>(b.value)};
  } else {
    using C =
        std::common_type_t<std::remove_cvref_t<decltype(term_value(a, p))>,
                           std::remove_cvref_t<decltype(term_value(b, p))>>;
    return std::pair<C, C>{static_cast<C>(term_value(a, p)),
                           static_cast<C>(term_value(b, p))};
  }
}
export template <typename Op, typename A, typename B, typename P>
constexpr auto term_value(const BinOp<Op, A, B>& t, const P& p) {
  const auto [x, y] = operand_values(t.a, t.b, p);
  static_assert(
      dedekind::category::IsClosedUnder<std::remove_cvref_t<decltype(x)>, Op>,
      "the carrier is not closed under this operator");
  return Op{}(x, y);
}
export template <typename F, typename A, typename P>
constexpr auto term_value(const Ap<F, A>& t, const P& p) {
  using X = typename std::remove_cvref_t<F>::Domain;
  if constexpr (is_const_v<A>)
    return t.f(carrier_cast<X>(t.a.value));
  else
    return t.f(static_cast<X>(term_value(t.a, p)));
}
export template <typename Pr, typename A, typename B, typename P>
constexpr auto term_value(const CondT<Pr, A, B>& t, const P& p) {
  const auto [x, y] = operand_values(t.a, t.b, p);
  return static_cast<bool>(t.p(p)) ? x : y;
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
    const auto [x, y] = operand_values(a, b, p);
    return rel_apply<R>(x, y);
  }
};
template <Rel R, typename A, typename B>
inline constexpr unsigned slots_of<Cmp<R, A, B>> = slots_v<A> | slots_v<B>;

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

template <typename X, typename A, typename B>
constexpr void bind_operands(const A& a, const B& b);

/** @brief Bind a term or atom to the points @c X it is evaluated at: every
 *  constant meets the carrier of the term beside it now, so a constant the
 *  carrier cannot hold raises at construction, not at the first evaluation
 *  (a compile error in @c constexpr code).  A leaf binds trivially.
 *  @tparam X the point type.  @tparam T the term. */
export template <typename X, typename T>
constexpr void bind_check(const T&) {}
export template <typename X, typename Op, typename A, typename B>
constexpr void bind_check(const BinOp<Op, A, B>& t) {
  bind_operands<X>(t.a, t.b);
}
export template <typename X, Rel R, typename A, typename B>
constexpr void bind_check(const Cmp<R, A, B>& t) {
  bind_operands<X>(t.a, t.b);
}
export template <typename X, typename Pr, typename A, typename B>
constexpr void bind_check(const CondT<Pr, A, B>& t) {
  bind_check<X>(t.p);
  bind_operands<X>(t.a, t.b);
}
export template <typename X, typename F, typename A>
constexpr void bind_check(const Ap<F, A>& t) {
  if constexpr (is_const_v<A>)
    (void)carrier_cast<typename std::remove_cvref_t<F>::Domain>(t.a.value);
  else
    bind_check<X>(t.a);
}
export template <typename X, typename A, typename B>
constexpr void bind_check(const RelAnd<A, B>& t) {
  bind_check<X>(t.a);
  bind_check<X>(t.b);
}
export template <typename X, typename A, typename B>
constexpr void bind_check(const RelOr<A, B>& t) {
  bind_check<X>(t.a);
  bind_check<X>(t.b);
}
template <typename X, typename A, typename B>
constexpr void bind_operands(const A& a, const B& b) {
  if constexpr (is_const_v<A> && !is_const_v<B>)
    (void)carrier_cast<value_at_t<X, B>>(a.value);
  else
    bind_check<X>(a);
  if constexpr (is_const_v<B> && !is_const_v<A>)
    (void)carrier_cast<value_at_t<X, A>>(b.value);
  else
    bind_check<X>(b);
}

/** @section terms__Formal_Verification */
// ── Certificates ────────────────────────────────────────────────────────────
// Rules over a term's structure, read by the relation traits (functional,
// entire) where the relation is built.  Each is sufficient, not necessary:
// false means not certified.

/** @brief The operator @c Op typed at the carrier @c C, the key totality is
 *  registered under: @c std::plus<> at @c C is @c std::plus<C>. */
template <typename Op, typename C>
struct at_carrier {
  using type = Op;
};
template <template <typename> class F, typename C>
struct at_carrier<F<void>, C> {
  using type = F<C>;
};

/** @brief A term total at every point of type @c X: slots and constants are;
 *  @c a @c op @c b is when both operands are and @c op is total on their
 *  carrier (@c category::IsTotal); a conditional, a comparison or a connective
 *  when every part is; an applied arrow when the arrow is total.
 *  @tparam X the point type.  @tparam T the term. */
export template <typename X, typename T>
inline constexpr bool is_total_term_v = false;
template <typename X, IsRingIntegral auto I>
inline constexpr bool is_total_term_v<X, Projection<I>> = true;
template <typename X, typename K>
inline constexpr bool is_total_term_v<X, Const<K>> = true;
template <typename X, typename Op, typename A, typename B>
inline constexpr bool is_total_term_v<X, BinOp<Op, A, B>> =
    is_total_term_v<X, A> && is_total_term_v<X, B> &&
    dedekind::category::IsTotal<
        value_at_t<X, BinOp<Op, A, B>>,
        typename at_carrier<Op, value_at_t<X, BinOp<Op, A, B>>>::type>;
template <typename X, Rel R, typename A, typename B>
inline constexpr bool is_total_term_v<X, Cmp<R, A, B>> =
    is_total_term_v<X, A> && is_total_term_v<X, B>;
template <typename X, typename Pr, typename A, typename B>
inline constexpr bool is_total_term_v<X, CondT<Pr, A, B>> =
    is_total_term_v<X, Pr> && is_total_term_v<X, A> && is_total_term_v<X, B>;
template <typename X, typename A, typename B>
inline constexpr bool is_total_term_v<X, RelAnd<A, B>> =
    is_total_term_v<X, A> && is_total_term_v<X, B>;
template <typename X, typename A, typename B>
inline constexpr bool is_total_term_v<X, RelOr<A, B>> =
    is_total_term_v<X, A> && is_total_term_v<X, B>;
template <typename X, typename F, typename A>
inline constexpr bool is_total_term_v<X, Ap<F, A>> =
    is_total_term_v<X, A> &&
    dedekind::category::IsTotalArrow<std::remove_cvref_t<F>>;

/** @brief @c π2 @c == @c t, or @c t @c == @c π2, with @c t mentioning only the
 *  first slot: the graph of the function @c x ↦ t(x).  @c term is @c t.
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
/** @brief An atom that is the graph of a function of the first slot. */
export template <typename P>
concept IsGraphAtom = graph_atom<P>::value;
/** @brief The term of a graph atom.  @tparam P the atom. */
export template <IsGraphAtom P>
using graph_term_t = typename graph_atom<P>::term;

namespace detail_terms_witness {
using P = std::pair<int, int>;
inline constexpr auto succ_graph = π1 + 1 == π2;
static_assert(IsRelPredicate<decltype(succ_graph)> &&
                  slots_v<decltype(succ_graph)> == 0b110u,
              "π1 + 1 == π2 is an atom on slots 1 and 2");
static_assert(succ_graph(P{4, 5}) && !succ_graph(P{4, 6}), "4 + 1 == 5, not 6");
inline constexpr auto half_after_shift = (π1 + 1) * 3 == π2;
static_assert(half_after_shift(P{1, 6}), "(1 + 1)·3 == 6: any nesting");
static_assert(only_slot(slots_v<decltype(π1 * 3 + 1)>, 1),
              "3·π1 + 1 mentions only the first slot");
static_assert(!(π1 < 5)(P{5, 0}) && (!(π1 < 5))(P{5, 0}),
              "!atom flips the comparison");
static_assert((π % 3 == 0)(9) && !(π % 3 == 0)(10),
              "an atom on the unary π evaluates at a carrier value");
static_assert((cond(π1 % 2 == 0, π1 / 2, π1 * 3 + 1) == π2)(P{6, 3}) &&
                  (cond(π1 % 2 == 0, π1 / 2, π1 * 3 + 1) == π2)(P{7, 22}),
              "cond on terms: the Collatz step as one atom");
inline constexpr auto on_naturals = π1 + 1 == π2;
static_assert(on_naturals(std::pair{finite_cardinality(4),
                                    finite_cardinality(5)}),
              "the constant 1 meets ℕ as the cardinal 1");
using N2 = std::pair<Cardinality, Cardinality>;
static_assert(is_total_term_v<N2, decltype(π1 * 3 + 1)>,
              "3·x + 1 is total on ℕ: + and · saturate");
static_assert(!is_total_term_v<P, decltype(π1 + 1)>,
              "x + 1 on int is closed but not total: signed overflow");
static_assert(IsGraphAtom<decltype(π1 * 3 + 1 == π2)> &&
                  !IsGraphAtom<decltype(π2 * π2 == π1 * π1)>,
              "π2 == t(π1) is a graph; y² == x² (y = ±x) is not certified");
}  // namespace detail_terms_witness

}  // namespace dedekind::sets
