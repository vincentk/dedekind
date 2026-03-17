module;
#include <functional>
#include <type_traits>
#include <utility>

export module dedekind.algebra:nodes;

import dedekind.sets;
import dedekind.order;
import :abstract;

namespace dedekind::algebra {

/**
 * @brief Symbolic Minkowski Sum Node (A ⊕ B).
 *
 * Theorem: z ∈ (A ⊕ B) iff ∃x ∈ A, y ∈ B such that Op(x, y) == z.
 */
export template <typename T, typename L, typename R, typename Op, auto Card>
struct MinkowskiSumNode
    : public sets::RelativeExpression<T, typename L::base_set_type,
                                      decltype(Card),
                                      MinkowskiSumNode<T, L, R, Op, Card>> {
  using Universe = typename L::base_set_type;
  using CardType = decltype(Card);
  using Parent = sets::RelativeExpression<T, Universe, CardType,
                                          MinkowskiSumNode<T, L, R, Op, Card>>;

  L left;
  R right;

  /**
   * @brief Construct the sum node using the common base set.
   */
  constexpr MinkowskiSumNode(L l, R r, CardType c)
      : Parent(l.base_set(), std::move(c)),
        left(std::move(l)),
        right(std::move(r)) {}

  /** @brief Membership: x + y = z (Symbolic Placeholder) */
  constexpr bool contains(const T& /*v*/) const { return false; }
};

export template <typename L, typename R, typename Op = std::plus<typename L::element_type>>
  requires IsMonoid<typename L::element_type, Op>
auto operator+(L l, R r) {
    using T = typename L::element_type;
    auto new_card = l.cardinality() | r.cardinality();
    return MinkowskiSumNode<T, L, R, Op, new_card>(std::move(l), std::move(r), new_card);
}

/**
 * @brief Theorem: [a, b] ⊕ [c, d] = [a + c, b + d] (Symbolic Collapse)
 *
 * This specialization matches any IntersectionNode (Interval) and peels
 * away references to perform boundary arithmetic at compile-time.
 */
export template <typename T, typename S, typename L1, typename R1, typename C1,
                 typename L2, typename R2, typename C2>
  requires IsAdditiveGroup<T>
auto operator+(const sets::IntersectionNode<T, L1, R1, C1>& a,
               const sets::IntersectionNode<T, L2, R2, C2>& b) {
  using PureL1 = std::remove_cvref_t<L1>;
  using PureR1 = std::remove_cvref_t<R1>;
  using PureL2 = std::remove_cvref_t<L2>;
  using PureR2 = std::remove_cvref_t<R2>;

  // If both intersections are made of symbolic bounds, collapse them.
  if constexpr (requires {
                  PureL1::predicate_type::bound;
                  PureL2::predicate_type::bound;
                }) {
    return order::closed_interval<
        T, S, PureL1::predicate_type::bound + PureL2::predicate_type::bound,
        PureR1::predicate_type::bound + PureR2::predicate_type::bound>(
        a.base_set());
  } else {
    // Fallback for intersections that aren't simple intervals
    auto new_card = a.cardinality() + b.cardinality();
    return MinkowskiSumNode<T, decltype(a), decltype(b), std::plus<T>,
                            new_card>(a, b, new_card);
  }
}

};  // namespace dedekind::algebra
