module;

#include <utility>

export module dedekind.order:nodes;
import dedekind.sets; // For RelativeExpression and UniversalSet
import :algebra;      // For IsPartiallyOrdered

namespace dedekind::order {

/** @brief Symbolic Tag for (a, b) or (a, ∞). */
export struct Open {};
/** @brief Symbolic Tag for [a, b] or [a, ∞). */
export struct Closed {};

/** @brief Symbolic Lower Bound: {x | x [op] Bound}. */
export template <typename T, auto Value, typename Boundary>
struct LowerBound {
  static constexpr T bound = Value;
  using boundary_type = Boundary;

  constexpr bool operator()(const T& v) const {
    if constexpr (std::is_same_v<Boundary, Closed>)
      return !(v < Value);
    else
      return Value < v;
  }
};

/** @brief Symbolic Upper Bound: {x | x [op] Bound}. */
export template <typename T, auto Value, typename Boundary>
struct UpperBound {
  static constexpr T bound = Value;
  using boundary_type = Boundary;

  constexpr bool operator()(const T& v) const {
    if constexpr (std::is_same_v<Boundary, Closed>)
      return !(Value < v);
    else
      return v < Value;
  }
};

export template <typename P>
concept IsSymbolicBound = requires {
  typename P::boundary_type;
  { P::bound };
};

export template <typename S>
concept IsRay = sets::IsSet<S, typename S::element_type> &&
                IsSymbolicBound<typename S::predicate_type>;

export template <IsRay L, IsRay R>
  requires(
      requires { typename L::predicate_type::is_lower; } &&
      requires { typename R::predicate_type::is_upper; })
auto operator&(const L& l, const R& r) {
  using T = typename L::element_type;
  constexpr auto min_v = L::predicate_type::bound;
  constexpr auto max_v = R::predicate_type::bound;

  // Theorem: Symbolic Pruning for Disjoint Rays
  if constexpr (max_v < min_v) {
    return sets::ø<T, L>(l.base_set());
  } else {
    auto new_card = l.cardinality() & r.cardinality();
    return sets::IntersectionNode<T, L, R, decltype(new_card)>(l, r, new_card);
  }
}

/**
 * @brief Theorem: LowerBound ∩ UpperBound = Interval.
 *
 * Includes the Symbolic Disjointness Proof: if Max < Min, result is ø.
 * Note: We use IsTotallyOrdered to ensure the comparison < is mathematically
 * sound.
 */
export template <typename T, typename S, auto V1, typename B1, typename C1,
                 auto V2, typename B2, typename C2>
  requires IsTotallyOrdered<T>
auto operator&(const sets::PredicateNode<T, S, LowerBound<T, V1, B1>, C1>& l,
               const sets::PredicateNode<T, S, UpperBound<T, V2, B2>, C2>& r) {
  // 1. Symbolic Pruning: If the rays don't reach each other, it's Empty.
  if constexpr (V2 < V1) {
    return sets::ø<T, S>(l.base_set());
  }
  // 2. Boundary Case: [x, x] is {x}, but (x, x) or [x, x) is ø.
  else if constexpr (V1 == V2) {
    if constexpr (std::is_same_v<B1, Closed> && std::is_same_v<B2, Closed>) {
      // This is a single-element set {V1}.
      // For now, we return a general IntersectionNode.
      return sets::IntersectionNode<T, decltype(l), decltype(r),
                                    sets::Extensional>(l, r,
                                                       sets::Extensional(1));
    } else {
      return sets::ø<T, S>(l.base_set());
    }
  }
  // 3. General Case: Create the symbolic IntersectionNode
  else {
    auto new_card = l.cardinality() & r.cardinality();
    return sets::IntersectionNode<T, decltype(l), decltype(r),
                                  decltype(new_card)>(l, r, new_card);
  }
}

/**
 * @brief Theorem: Commutativity of Bound Intersection.
 *
 * UpperBound ∩ LowerBound = LowerBound ∩ UpperBound.
 */
export template <typename T, typename S, auto V1, typename B1, typename C1,
                 auto V2, typename B2, typename C2>
  requires IsTotallyOrdered<T>
auto operator&(const sets::PredicateNode<T, S, UpperBound<T, V1, B1>, C1>& u,
               const sets::PredicateNode<T, S, LowerBound<T, V2, B2>, C2>& l) {
  // Simply flip the arguments to hit the primary Lower & Upper overload
  return l & u;
}

/**
 * @brief Theorem: ¬LowerBound<V, B> -> UpperBound<V, Flip(B)>
 */
export template <typename T, typename S, auto V, typename B, typename Card>
auto operator!(const sets::PredicateNode<T, S, LowerBound<T, V, B>, Card>& s) {
  using NewBoundary =
      std::conditional_t<std::is_same_v<B, Closed>, Open, Closed>;
  using NewPredicate = UpperBound<T, V, NewBoundary>;

  // Pass NewPredicate{} as the second argument
  return sets::PredicateNode<T, S, NewPredicate, Card>(
      s.base_set(), NewPredicate{}, s.cardinality());
}

/**
 * @brief Theorem: ¬UpperBound<V, B> -> LowerBound<V, Flip(B)>
 */
export template <typename T, typename S, auto V, typename B, typename Card>
auto operator!(const sets::PredicateNode<T, S, UpperBound<T, V, B>, Card>& s) {
  using NewBoundary =
      std::conditional_t<std::is_same_v<B, Closed>, Open, Closed>;
  using NewPredicate = LowerBound<T, V, NewBoundary>;

  // Pass NewPredicate{} here as well
  return sets::PredicateNode<T, S, NewPredicate, Card>(
      s.base_set(), NewPredicate{}, s.cardinality());
}

/** @brief Factory for Lower Closed Rays. */
export template <typename T, typename S, auto V>
auto lower_closed_ray(S s) {
  using P = LowerBound<T, V, Closed>;
  // Pass the predicate instance P{} as the second argument
  return sets::PredicateNode<T, S, P, typename S::cardinality_type>(
      std::move(s), P{}, s.cardinality());
}

/** @brief Factory for Upper Closed Rays. */
export template <typename T, typename S, auto V>
auto upper_closed_ray(S s) {
  using P = UpperBound<T, V, Closed>;
  // Pass P{} here as well
  return sets::PredicateNode<T, S, P, typename S::cardinality_type>(
      std::move(s), P{}, s.cardinality());
}

/**
 * @brief Theorem: A Closed Interval [Min, Max] is the intersection of two
 * closed rays.
 *
 * [Min, Max] = [Min, ∞) ∩ (-∞, Max]
 *
 * @tparam T The element type.
 * @tparam S The base set (Universe).
 * @tparam Min The lower bound value.
 * @tparam Max The upper bound value.
 * @param s The universe/base instance.
 */
export template <typename T, typename S, auto Min, auto Max>
auto closed_interval(S s) {
  // 1. Create the Symbolic Rays
  auto lower = lower_closed_ray<T, S, Min>(s);
  auto upper = upper_closed_ray<T, S, Max>(std::move(s));

  // 2. Intersect them.
  // If Max < Min, this symbolically returns sets::ø at compile-time!
  return lower & upper;
}

}  // namespace dedekind::order
