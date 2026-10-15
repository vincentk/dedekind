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
 * pulled back along the terms.  The former @c S @c | @c body, seeing a @c Self
 * in @c body, closes the recursion into @c Rec: a predicate that evaluates the
 * body on an @c Env the term nodes read as the point (a projection at an
 * @c Env is read at its point) and @c Self reads as the set.
 *
 * The recursion is admitted only when it is @b structural: every @c Self must
 * descend on some coordinate, @f$\pi - c@f$ with @f$c > 0@f$ on a well-founded
 * carrier, so an unfolding takes at most that coordinate's value in steps
 * (primitive recursion on the coordinate; the parametrised NNO).  A body whose
 * @c Self descends on nothing is general recursion, and no former is viable for
 * it: the Rice boundary, refused where it would be written.  A descent below
 * the carrier's bottom is no point of the universe and the pulled-back
 * membership is
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
import :setobject;    // IsSetObject
import :terms;        // the term grammar: projections, term_value, the descent
import :expressions;  // 𝔸, Comprehension
import :singleton;    // π == v, for the witnesses

namespace dedekind::sets {

/** @brief A carrier a descent terminates on: the unsigned words, ℕ's shadows,
 *  with 0 at the bottom.  (Cardinality joins once its monus is a term.) */
export template <typename T>
concept WellFounded = std::unsigned_integral<T>;

/** @brief A descent: @f$\pi_i - c@f$, a slot stepped down by a constant.  It
 *  descends on @c X when that slot's carrier is well-founded; @c c > 0 is
 *  checked when the @c χ node is built.  @tparam T the term. */
template <typename T>
inline constexpr bool is_descent_v = false;
template <IsRingIntegral auto I, typename K>
inline constexpr bool
    is_descent_v<BinOp<std::minus<>, Projection<I>, Const<K>>> = true;

/** @brief A @c χ argument's value at a point: the term's value, or @c nullopt
 *  for a descent below the carrier's bottom (no point of the universe, so the
 *  pulled-back membership is @c false there).  @tparam X the point type. */
template <typename T, typename X>
constexpr std::optional<value_at_t<X, T>> self_arg(const T& t, const X& x) {
  if constexpr (is_descent_v<T>) {
    using V = value_at_t<X, T>;
    if (term_value(t.a, x) < carrier_cast<V>(t.b.value)) return std::nullopt;
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
  constexpr operator const T&() const { return point; }
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
  template <typename... Terms>
  constexpr Self<std::remove_cvref_t<Terms>...> operator()(
      Terms&&... ts) const {
    (check_descent(ts), ...);
    return Self<std::remove_cvref_t<Terms>...>{
        std::tuple<std::remove_cvref_t<Terms>...>{std::forward<Terms>(ts)...}};
  }

 private:
  template <typename T>
  static constexpr void check_descent(const T& t) {
    if constexpr (is_descent_v<std::remove_cvref_t<T>>) {
      if (!(t.b.value > decltype(t.b.value){}))
        throw std::invalid_argument("a descent by 0 descends on nothing");
    }
  }
};
export inline constexpr ChiTag χ{};

template <typename... Ts>
inline constexpr bool is_self_v<Self<Ts...>> = true;

/** @brief Whether a term descends at points of type @c X: a descent on a
 *  well-founded slot.  @tparam Term the term.  @tparam X the point type. */
export template <typename Term, typename X>
inline constexpr bool descends_v = false;
template <IsRingIntegral auto I, typename K, typename X>
  requires WellFounded<value_at_t<X, Projection<I>>>
inline constexpr bool
    descends_v<BinOp<std::minus<>, Projection<I>, Const<K>>, X> = true;

/** @brief The structural-recursion algebra at the points @c X: a @c χ node
 *  descends when one of its terms does, a node when all its legs do, any other
 *  leaf vacuously.  @tparam X the point type. */
template <typename X>
struct SelfDescends {
  template <typename Leaf>
  constexpr std::true_type operator()(const Leaf&) const {
    return {};
  }
  template <typename... Ts>
  constexpr std::bool_constant<(descends_v<Ts, X> || ...)> operator()(
      const Self<Ts...>&) const {
    return {};
  }
  template <typename Node, typename... R>
  constexpr std::bool_constant<(R::value && ...)> operator()(const Node&,
                                                             R...) const {
    return {};
  }
};
/** @brief The structural-recursion gate: every @c Self in a body descends on
 *  some coordinate.  Vacuous without a @c Self.  @tparam Node the body.
 *  @tparam T the point type. */
export template <typename Node, typename T>
inline constexpr bool self_descends_v = decltype(dedekind::category::cata(
    SelfDescends<T>{}, std::declval<const Node&>()))::value;

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

/** @brief The former that closes a recursion: @c S @c | @c body with a @c χ in
 *  the body is @f$\{x \in S \mid body(x)\}@f$ where @c χ is that set.  Viable
 *  only for a structural recursion over a decidable base; general recursion
 *  finds no former.  @tparam S the base.  @tparam Body the body. */
export template <IsSetObject S, typename Body>
  requires contains_self_v<Body> && self_descends_v<Body, typename S::Domain> &&
           std::convertible_to<
               std::invoke_result_t<const S&, const typename S::Domain&>, bool>
constexpr auto operator|(const S& base, Body body) {
  using D = typename S::Domain;
  using L = typename S::logic_species;
  return Comprehension<𝔸<D, L>, Rec<S, Body>>{
      𝔸<D, L>{}, Rec<S, Body>{base, std::move(body)}};
}

/** @section recursion__Formal_Verification */
namespace detail_recursion_witness {
using N = std::size_t;
// The multiples of three, inductively: 0, and whatever is three above one.
inline constexpr auto multiples_of_3 = 𝔸<N>{} | ((π == N{0}) || χ(π - N{3}));
static_assert(IsSetObject<decltype(multiples_of_3)>,
              "an inductive set is a set object");
static_assert(multiples_of_3(N{0}) && multiples_of_3(N{9}) &&
                  !multiples_of_3(N{10}) && !multiples_of_3(N{1}),
              "{n | n = 0 ∨ χ(n − 3)}: 0 and 9 are in, 1 and 10 are not");
// ℕ itself, inductively: 0, and the successor of a natural.
inline constexpr auto naturals = 𝔸<N>{} | ((π == N{0}) || χ(π - N{1}));
static_assert(naturals(N{0}) && naturals(N{30}),
              "the inductive ℕ reaches every natural (30 steps for 30; deeper "
              "unfoldings run at runtime, each level costing several calls of "
              "the constant-evaluation depth)");
// General recursion finds no former: a χ that descends on nothing.  (A
// requires-expression refuses SFINAE-style only where it is dependent, hence
// the concepts.)
template <typename T>
concept FormsNonDescendingRecursion =
    requires { 𝔸<T>{} | ((π == T{}) || χ(π)); };
template <typename T>
concept FormsDescendingRecursion =
    requires { 𝔸<T>{} | ((π == T{}) || χ(π - T{1})); };
static_assert(!FormsNonDescendingRecursion<N>,
              "a recursion that descends on nothing is refused at the former");
static_assert(FormsDescendingRecursion<N> && !FormsDescendingRecursion<int>,
              "a descent needs a well-founded carrier: int has no bottom");
}  // namespace detail_recursion_witness

}  // namespace dedekind::sets
