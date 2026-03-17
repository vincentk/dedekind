export module dedekind.ontology:density;

import :mereology;

/** 
 * @section Density: The study of "In-Betweenness" and limits.
 */
namespace dedekind::ontology {

export template <typename T>
inline constexpr bool is_dense_v = false;

/** @brief Theorem: Rational numbers are dense; Integers are not. */
template <> inline constexpr bool is_dense_v<float> = true;
template <> inline constexpr bool is_dense_v<double> = true;
// Our future 'Rational' type will also be registered here.

/** 
 * @concept IsDense
 * @brief A property of the underlying scalar type T. 
 *        Between any two distinct elements, there exists a third.
 * Wikipedia: Dense set, Order density
 */
export template <typename T>
concept IsDense = is_dense_v<T> && requires(T a, T b) {
    // Theorem: If a < b, then there exists c such that a < c < b.
    { (a + b) / 2 } -> std::convertible_to<T>;
};

/**
 * @concept IsArchimedean
 * @brief Requires a Total Order to ensure "exceeding" y is well-defined.
 * Wikipedia: Archimedean property
 */
export template <typename T>
concept IsArchimedean = IsTotallyOrdered<T> && requires(T x, T y) {
    { x + x } -> std::same_as<T>;
};

/**
 * @concept IsDedekindComplete
 * @brief The "Destination": Every non-empty set with an upper bound 
 *        has a least upper bound (Supremum).
 * Wikipedia: Completeness of the real numbers, Dedekind completeness
 */
export template <typename T>
concept IsDedekindComplete = IsDense<T> && IsArchimedean<T> && requires {
    // This is what our Dedekind Cut will eventually provide to the Rationals.
    typename T::supremum_type;
};

} // namespace dedekind::ontology
