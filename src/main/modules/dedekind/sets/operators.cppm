module;

#include <memory>       // For std::make_shared
#include <type_traits>  // For std::is_base_of_v checks in cardinality algebra
#include <utility>      // For std::forward and std::move

export module dedekind.sets:operators;

import :traits;
import :expressions;
// import std;

namespace dedekind::sets {

// 1. Membership Test via operator[]
// We add this to the Handle to make it "Mathy"
export template <typename T>
bool operator[](const SetHandle<T>& s, const T& value) {
  return s.contains(value);
}

// 2. Symbolic Intersection (A & B)

template <typename T, typename L, typename R>
auto operator&(const SetExpression<T, L>& lhs, const SetExpression<T, R>& rhs) {
  // 1. Calculate the resulting Card type from the L and R types
  using ResultCard =
      typename IntersectionCardinality<typename L::cardinality_type,
                                       typename R::cardinality_type>::type;

  // 2. Return the specific "Rich" Node
  return std::make_shared<IntersectionNode<T, ResultCard, L, R>>(
      lhs.self_ptr(), rhs.self_ptr());
}

export template <typename T, typename L, typename R>
auto operator&(const SetHandle<T>& l, const SetHandle<T>& r) {
  // In the MVP, we create a new Intersection node wrapped in a Handle
  auto node = std::make_shared<Intersection<T, L, R>>(l.get_ptr(), r.get_ptr());
  return SetHandle<T>(node);
}

// 3. Symbolic Union (A | B)
export template <typename T, typename L, typename R>
auto operator|(const SetHandle<T>& l, const SetHandle<T>& r) {
  // Similar logic for UnionNode (to be implemented in expressions)
  return ...;
}

// Such-That (A ^ pred)
template <typename T, typename Expr, typename P>
auto operator^(const SetExpression<T, Expr>& lhs, P&& pred) {
  // A filter preserves the parent's cardinality type
  using Card = typename Expr::cardinality_type;

  return std::make_shared<SetBuilderNode<T, Card, Expr, P>>(
      lhs.self_ptr(), std::forward<P>(pred));
}

export template <typename T, typename Expr, typename P>
auto operator^(const SetExpression<T, Expr>& lhs, P&& pred) {
  return std::make_shared<SetBuilderNode<T, Expr, P>>(lhs.self_ptr(),
                                                      std::forward<P>(pred));
}

// Set Difference: A \ B = A & (!B)
export template <typename T, typename C1, typename C2>
auto operator-(const SetHandle<T, C1>& a, const SetHandle<T, C2>& b) {
  // Logic: The cardinality of A \ B is at most the cardinality of A
  // Because a & (!b) uses our new operator& on cardinalities,
  // it will correctly infer that the result is at most C1.
  return a & (!b);
}

// Set-theoretic Complement (A^c)
export template <typename T>
auto operator!(const SetHandle<T>& s) {
  auto node = std::make_shared<ComplementNode<T>>(s.get_ptr());
  return SetHandle<T>(node);
}
}  // namespace dedekind::sets
