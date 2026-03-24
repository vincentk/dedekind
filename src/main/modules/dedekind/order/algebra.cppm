module;

#include <concepts>

export module dedekind.order:algebra;

namespace dedekind::order {

/**
 * @brief Axiom of Partial Order (Reflexive, Antisymmetric, Transitive)
 * Wikipedia: "Partially ordered set"
 */
export template <typename T>
concept IsPartiallyOrdered = requires(T a, T b) {
  { a <= b } -> std::convertible_to<bool>;
};

/**
 * @brief Axiom of Total Order (Linear Order)
 * Wikipedia: "Total order" - Every pair is comparable.
 */
export template <typename T>
concept IsTotallyOrdered = IsPartiallyOrdered<T> && requires(T a, T b) {
  { a < b } -> std::convertible_to<bool>;
};

}  // namespace dedekind::order
