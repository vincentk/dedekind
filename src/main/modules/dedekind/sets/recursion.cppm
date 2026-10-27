/**
 * @file dedekind/sets/recursion.cppm
 * @partition :recursion
 * @brief Inductive sets: χ, the classifier under definition, inside its own
 * comprehension; the former closes the recursion and admits only a descent.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section recursion__Overview
 * An inductive definition names the set it defines: the multiples of three are
 * @f$\{n \mid n = 0 \vee \chi(n - 3)\}@f$ with @f$\chi@f$ the characteristic
 * arrow of that very set.  Here @c χ is a tag whose application to terms builds
 * a @c Self node, a relational predicate standing for the set under definition
 * pulled back along the terms.  The former @c S @c & @c body, seeing a @c Self
 * in @c body, closes the recursion into @c Rec: a predicate that evaluates the
 * body on an @c Env the term nodes read as the point (a projection at an
 * @c Env is read at its point) and @c Self reads as the set.
 *
 * The recursion is admitted only when it is @b structural, with one
 * @b measure for the whole body: a coordinate @c i such that @b every @c Self
 * has @f$\pi_i - c@f$, @f$c > 0@f$, in position @c i, on a well-founded
 * carrier. That coordinate then strictly decreases on every call, so an
 * unfolding takes at most its value in steps (primitive recursion on the
 * coordinate; the parametrised NNO).  A subtraction somewhere is not enough: @c
 * χ(π_2 − 1, π_2) calls itself on the same point, and @c χ(π_1 − 1, π_2 + 1) @c
 * || @c χ(π_1 + 1, π_2 − 1) cycles.  A body without such a measure is general
 * recursion, and no former is viable for it: the Rice boundary, refused where
 * it would be written.  The former binds the body and every argument of every
 * @c χ when the set is built, as the ordinary formers do, and admits only a
 * decidable base (a @c bool-valued χ).  A descent below the carrier's bottom is
 * no point of the universe and the pulled-back membership is
 * @c false there, so a base case needs no guard against it.
 *
 * Wikipedia: Recursive definition, Primitive recursive function, Least fixed
 * point, Datalog
 *
 * @note Richard Dedekind, Was sind und was sollen die Zahlen? (1888), §126,
 *       "Satz der Definition durch Induktion": for a map θ of a system Ω into
 *       itself and an element ω of Ω there is one and only one map ψ of the
 *       number series with ψ(1) = ω and ψ(n') = θψ(n).  (Paraphrased.)  The
 *       descent admitted here is that theorem read backwards along a
 * coordinate.
 */
module;

#include <concepts>
#include <cstddef>
#include <optional>
#include <stdexcept>
#include <tuple>
#include <type_traits>
#include <utility>

export module dedekind.sets:recursion;

import dedekind.category;
import :cardinality;  // IsRingIntegral
import :boundaries;   // IsSetObject
import :terms;        // the term grammar: projections, term_value, the descent
import :expressions;  // 𝔸, Comprehension
import :singleton;    // π == v, for the witnesses

namespace dedekind::sets {

/** @brief A carrier a descent terminates on: the unsigned words, ℕ's shadows,
 *  with 0 at the bottom.  (Cardinality joins once its monus is a term.) */
export template <typename T>
concept WellFounded = std::unsigned_integral<T>;

/** @brief A descent: @f$\pi_i - c@f$, a slot stepped down by a constant, and
 *  the slot it steps on.  @c c @c > @c 0 is checked when the @c χ node is
 *  built.  @tparam T the term. */
template <typename T>
struct descent : std::false_type {};
template <IsRingIntegral auto I, typename K>
struct descent<BinOp<std::minus<>, Projection<I>, Const<K>>> : std::true_type {
  static constexpr std::size_t slot = static_cast<std::size_t>(I);
};
template <typename T>
inline constexpr bool is_descent_v = descent<T>::value;
/** @brief The legs of a descent @f$\pi_i - c@f$: the slot it steps on and
 *  the step @c c.  Read through these on the descent's own shape, so a body
 *  generic in the term names no member of it (Rule 2).  @tparam K the step's
 *  type.  @param t the descent. */
template <IsRingIntegral auto I, typename K>
  requires std::copyable<K>
constexpr Projection<I> descent_slot(
    const BinOp<std::minus<>, Projection<I>, Const<K>>& t) {
  return t.a;
}
template <IsRingIntegral auto I, typename K>
  requires std::copyable<K>
constexpr K descent_step(
    const BinOp<std::minus<>, Projection<I>, Const<K>>& t) {
  return t.b.value;
}
/** @brief A term with a value at the points @c X: @c term_value(t, x) is
 *  formed.  @tparam T the term.  @tparam X the point type. */
template <typename T, typename X>
concept HasTermValueAt = requires(const T& t, const X& x) { term_value(t, x); };

/** @brief A @c χ argument's value at a point: the term's value, or @c nullopt
 *  for a descent below the carrier's bottom (no point of the universe, so the
 *  pulled-back membership is @c false there).  @tparam X the point type. */
template <typename T, typename X>
  requires HasTermValueAt<T, X>
constexpr std::optional<value_at_t<X, T>> self_arg(const T& t, const X& x) {
  if constexpr (is_descent_v<T>) {
    using V = value_at_t<X, T>;
    if (term_value(descent_slot(t), x) < carrier_cast<V>(descent_step(t)))
      return std::nullopt;
  }
  return term_value(t, x);
}

/** @brief The point and the set under definition, threaded through a body: the
 *  term nodes read it as the point (conversion, and @c term_value below),
 *  @c Self reads it as the set.  @tparam T the point's type.  @tparam R the
 * closed recursion. */
export template <typename T, typename R>
struct Env {
  const T& point;
  const R* rec;
  constexpr operator T() const { return point; }
};
/** @brief A projection read at an @c Env is read at its point. */
export template <IsRingIntegral auto I, typename T, typename R>
constexpr decltype(auto) term_value(Projection<I> p, const Env<T, R>& e) {
  return term_value(p, e.point);
}

/** @brief A point of @c T from the terms' values: the scalar, or the pair.
 *  @tparam T the point's type.  @tparam Vs the values. */
template <typename T, typename... Vs>
constexpr T point_of(const Vs&... vs) {
  return T{vs...};
}

/** @brief The set under definition pulled back along the terms: @c χ(t₁, …) is
 *  @f$x \mapsto \chi(t_1(x), \ldots)@f$ with @f$\chi@f$ the comprehension's
 *  own classifier, unknown until the former closes it.  A relational predicate
 *  (an operand of @c && / @c ||), evaluable only through an @c Env.
 *  @tparam Terms the terms, one per coordinate of the carrier. */
export template <typename... Terms>
struct Self {
  using is_rel_predicate = void;
  std::tuple<Terms...> terms;
  template <typename T, typename R>
  constexpr bool operator()(const Env<T, R>& e) const {
    return unfold(e, std::index_sequence_for<Terms...>{});
  }

 private:
  template <typename T, typename R, std::size_t... Is>
  constexpr bool unfold(const Env<T, R>& e, std::index_sequence<Is...>) const {
    const auto values = std::tuple{self_arg(std::get<Is>(terms), e.point)...};
    if (!(std::get<Is>(values).has_value() && ...))
      return false;  // off the carrier
    return static_cast<bool>((*e.rec)(point_of<T>(*std::get<Is>(values)...)));
  }
};

/** @brief The classifier under definition: @c χ(terms…) in a former's body is
 *  the set being formed, applied to the terms. */
export struct ChiTag {
  template <typename... Args>
  constexpr auto operator()(Args... args) const {
    // A constant argument becomes a term node, as for the operators.
    return make(term_of(std::move(args))...);
  }

 private:
  template <IsTerm... Terms>
  static constexpr Self<Terms...> make(Terms... ts) {
    (check_descent(ts), ...);
    return Self<Terms...>{std::tuple<Terms...>{std::move(ts)...}};
  }
  template <IsTerm T>
  static constexpr void check_descent(const T& t) {
    if constexpr (is_descent_v<T>) {
      const auto step = descent_step(t);
      if (!(step > decltype(step){}))
        throw std::invalid_argument("a descent by 0 descends on nothing");
    }
  }
};
export inline constexpr ChiTag χ{};

template <typename... Ts>
inline constexpr bool is_self_v<Self<Ts...>> = true;

/** @brief The coordinate a position of @c χ's arguments stands for at points
 *  of @c X: the point itself (slot 0) for a scalar, slot @c J @c + @c 1 of a
 *  pair.  @tparam X the point type.  @tparam J the position, from 0. */
template <typename X, std::size_t J>
inline constexpr std::size_t slot_at =
    requires { typename X::first_type; } ? J + 1 : 0;
/** @brief The number of coordinates of a point of @c X. */
template <typename X>
inline constexpr std::size_t arity_v =
    requires { typename X::first_type; } ? 2 : 1;

/** @brief Whether @c Term is a descent on slot @c S at points of @c X: the
 *  shape @c descent names, on that slot, whose carrier is well-founded. */
template <typename Term, typename X, std::size_t S>
consteval bool descends_on() {
  if constexpr (descent<Term>::value)
    return descent<Term>::slot == S &&
           WellFounded<value_at_t<X, Projection<S>>>;
  else
    return false;
}
template <typename Term, typename X, std::size_t S>
inline constexpr bool descends_on_v = descends_on<Term, X, S>();

/** @brief The measure algebra at the points @c X for position @c J: a @c χ
 *  node passes when its argument in position @c J descends on the coordinate
 *  @c J stands for; every node when all its legs pass; any other leaf
 *  vacuously.  @tparam X the point type.  @tparam J the position. */
template <typename X, std::size_t J>
struct DescendsAt {
  template <typename Leaf>
  constexpr std::true_type operator()(const Leaf&) const {
    return {};
  }
  template <typename... Ts>
  constexpr auto operator()(const Self<Ts...>&) const {
    if constexpr (J < sizeof...(Ts))
      return std::bool_constant<descends_on_v<
          std::tuple_element_t<J, std::tuple<Ts...>>, X, slot_at<X, J>>>{};
    else
      return std::false_type{};
  }
  template <typename Node, typename... R>
  constexpr std::bool_constant<(R::value && ...)> operator()(const Node&,
                                                             R...) const {
    return {};
  }
};
template <typename Node, typename X, std::size_t... Js>
consteval bool some_measure(std::index_sequence<Js...>) {
  return (decltype(dedekind::category::cata(
              DescendsAt<X, Js>{}, std::declval<const Node&>()))::value ||
          ...);
}
/** @brief The structural-recursion gate: one coordinate on which every @c Self
 *  in the body descends.  Vacuous without a @c Self.  @tparam Node the body.
 *  @tparam T the point type. */
export template <typename Node, typename T>
inline constexpr bool self_descends_v =
    some_measure<Node, T>(std::make_index_sequence<arity_v<T>>{});

/** @brief The value of a constant term, read on its own shape.  @tparam K
 *  the constant's type.  @param c the constant. */
template <typename K>
  requires std::copyable<K>
constexpr K constant_value(const Const<K>& c) {
  return c.value;
}
/** @brief A term that binds at the points @c X: @c bind_check<X>(t) is formed.
 *  @tparam T the term.  @tparam X the point type. */
template <typename T, typename X>
concept BindsAt = requires(const T& t) { bind_check<X>(t); };

/** @brief The binding of the arguments of every @c χ in a body at the points
 *  @c X: each a term bound there, a constant cast into its coordinate's
 *  carrier.  The body's own binding treats a @c χ as a leaf, so its arguments
 *  are bound here.  @tparam X the point type. */
template <typename X>
struct BindArgs {
  template <typename Leaf>
  constexpr int operator()(const Leaf&) const {
    return 0;
  }
  template <typename... Ts>
  constexpr int operator()(const Self<Ts...>& s) const {
    bind_all(s, std::index_sequence_for<Ts...>{});
    return 0;
  }
  template <typename Node, typename... R>
  constexpr int operator()(const Node&, R...) const {
    return 0;
  }

 private:
  template <typename... Ts, std::size_t... Js>
  static constexpr void bind_all(const Self<Ts...>& s,
                                 std::index_sequence<Js...>) {
    (bind_one<Js>(std::get<Js>(s.terms)), ...);
  }
  template <std::size_t J, typename T>
    requires BindsAt<T, X>
  static constexpr void bind_one(const T& t) {
    if constexpr (is_const_v<T>)
      (void)carrier_cast<value_at_t<X, Projection<slot_at<X, J>>>>(
          constant_value(t));
    else
      bind_check<X>(t);
  }
};

/** @brief The recursion, closed: the base's membership, then the body evaluated
 *  with this very predicate in scope for its @c Self nodes.  @tparam S the base
 *  set object.  @tparam Body the body. */
export template <IsSetObject S, typename Body>
struct Rec {
  using Domain = typename S::Domain;
  using Codomain = bool;
  S base;
  Body body;
  constexpr bool operator()(const Domain& x) const {
    if (!static_cast<bool>(base(x))) return false;
    return static_cast<bool>(body(Env<Domain, Rec>{x, this}));
  }
};

/** @brief The former that closes a recursion: @c S @c & @c body with a @c χ in
 *  the body is @f$\{x \in S \mid body(x)\}@f$ where @c χ is that set.  Viable
 *  only for a structural recursion over a decidable base; general recursion
 *  finds no former.  @tparam S the base.  @tparam Body the body. */
export template <IsSetObject S, typename Body>
  requires contains_self_v<Body> && self_descends_v<Body, typename S::Domain> &&
           std::same_as<
               std::invoke_result_t<const S&, const typename S::Domain&>, bool>
constexpr auto operator&(const S& base, Body body) {
  using D = typename S::Domain;
  bind_check<D>(body);                                  // the body, χ a leaf
  (void)dedekind::category::cata(BindArgs<D>{}, body);  // χ's arguments
  // The base stays the universe, so its bounds (a finite base) are kept.
  return Comprehension<S, Rec<S, Body>>{base,
                                        Rec<S, Body>{base, std::move(body)}};
}

/** @section recursion__Formal_Verification */
namespace detail_recursion_witness {
using N = std::size_t;
// The multiples of three, inductively: 0, and whatever is three above one.
inline constexpr auto multiples_of_3 = 𝔸<N>{} & ((π == N{0}) || χ(π - N{3}));
static_assert(IsSetObject<decltype(multiples_of_3)>,
              "an inductive set is a set object");
static_assert(multiples_of_3(N{0}) && multiples_of_3(N{9}) &&
                  !multiples_of_3(N{10}) && !multiples_of_3(N{1}),
              "{n | n = 0 ∨ χ(n − 3)}: 0 and 9 are in, 1 and 10 are not");
// ℕ itself, inductively: 0, and the successor of a natural.
inline constexpr auto naturals = 𝔸<N>{} & ((π == N{0}) || χ(π - N{1}));
static_assert(naturals(N{0}) && naturals(N{30}),
              "the inductive ℕ reaches every natural (30 steps for 30; deeper "
              "unfoldings run at runtime, each level costing several calls of "
              "the constant-evaluation depth)");
// General recursion finds no former: a χ that descends on nothing.  (A
// requires-expression refuses SFINAE-style only where it is dependent, hence
// the concepts.)
template <typename T>
concept FormsNonDescendingRecursion =
    requires { 𝔸<T>{} & ((π == T{}) || χ(π)); };
template <typename T>
concept FormsDescendingRecursion =
    requires { 𝔸<T>{} & ((π == T{}) || χ(π - T{1})); };
static_assert(!FormsNonDescendingRecursion<N>,
              "a recursion that descends on nothing is refused at the former");
static_assert(FormsDescendingRecursion<N> && !FormsDescendingRecursion<int>,
              "a descent needs a well-founded carrier: int has no bottom");
// One measure for the whole body: a subtraction somewhere is not enough.
using NN = std::pair<N, N>;
template <typename T>
concept FormsSamePointRecursion = requires { 𝔸<T>{} & χ(π_2 - N{1}, π_2); };
template <typename T>
concept FormsCyclingRecursion = requires {
  𝔸<T>{} & (χ(π_1 - N{1}, π_2 + N{1}) || χ(π_1 + N{1}, π_2 - N{1}));
};
template <typename T>
concept FormsMeasuredRecursion =
    requires { 𝔸<T>{} & ((π_1 == N{0}) || χ(π_1 - N{1}, π_2 + N{1})); };
static_assert(!FormsSamePointRecursion<NN> && !FormsCyclingRecursion<NN> &&
                  FormsMeasuredRecursion<NN>,
              "χ(π_2 − 1, π_2) and the two-call cycle have no common measure; "
              "π_1 − 1 in the first position is one");
// A constant argument is a term, and the measure still holds on π_1:
// (x, y) is in when y = 0, or when (x − 1, 0) is, i.e. when x ≥ 1.
inline constexpr auto y_zero_or_x_positive =
    𝔸<NN>{} & ((π_2 == N{0}) || χ(π_1 - N{1}, N{0}));
static_assert(y_zero_or_x_positive(NN{0, 0}) &&
                  y_zero_or_x_positive(NN{3, 5}) &&
                  !y_zero_or_x_positive(NN{0, 5}),
              "{(x, y) | y = 0 ∨ χ(x − 1, 0)}: y = 0 or x ≥ 1");
// A finite base keeps its bound.
using FiniteN = 𝔸<unsigned, dedekind::category::Boole, Finite>;
inline constexpr auto finite_evens = FiniteN{} & ((π == 0u) || χ(π - 2u));
static_assert(std::same_as<decltype(finite_evens)::cardinality_type,
                           FiniteN::cardinality_type> &&
                  finite_evens(4u) && !finite_evens(5u),
              "a recursion over a finite base is still bounded by it");
}  // namespace detail_recursion_witness

}  // namespace dedekind::sets
