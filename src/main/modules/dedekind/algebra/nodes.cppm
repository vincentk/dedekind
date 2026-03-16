module;
#include <utility>

export module dedekind.algebra:nodes;

import dedekind.sets;
import :abstract;

namespace dedekind::algebra {

/**
 * @brief Symbolic Addition Node (Minkowski Sum).
 *
 * Represents the set of all sums of elements from two sets.
 * Note: For arbitrary predicates, this is a search problem;
 * for intervals, it collapses into a new interval.
 */
export template <typename T, typename L, typename R, auto Card>
struct AdditionNode
    : public sets::RelativeExpression<T, typename L::base_set_type,
                                      decltype(Card),
                                      AdditionNode<T, L, R, Card>> {
  using Universe = typename L::base_set_type;
  using Parent = sets::RelativeExpression<T, Universe, decltype(Card),
                                          AdditionNode<T, L, R, Card>>;

  const L left;
  const R right;

  constexpr AdditionNode(L l, R r, decltype(Card) c)
      : Parent(l.base_set(), std::move(c)),
        left(std::move(l)),
        right(std::move(r)) {}

  /**
   * @brief Membership Axiom: z ∈ (A + B) iff ∃x ∈ A, y ∈ B such that z = x + y.
   *
   * @note For general sets, this requires a solver. For Intervals, we
   * will provide a specialized 'operator+' that returns a simple IntervalNode
   * instead.
   */
  constexpr bool contains(const T& /*v*/) const {
    // Fallback: This remains a symbolic "Promise" until specialized.
    return false;
  }
};
};  // namespace dedekind::algebra
