export module dedekind.ontology:mereology;

import <concepts>;

/** 
 * @section Mereology: The study of parts and wholes.
 * @section Mereology: The Hierarchy of Order.
 */
namespace dedekind::ontology {

/**
 * @section Mereology: The study of parts and wholes.
 * @concept IsSet
 * @brief The "Naked" entry point. An object S is a Set if it can 
 *        answer the membership question for an element T.
 * Wikipedia: Set (mathematics), Indicator function
 */
export template <typename S, typename T>
concept IsSet = requires(const S s, const T x) {
    { s.contains(x) } -> std::convertible_to<bool>;
};

/**
 * @concept IsMeetSemiLattice
 * @brief A refinement of IsSet that supports the algebraic 
 *        structure of Meet (&).
 * Wikipedia: SemiLattice (order)
 */
export template <typename S>
concept IsMeetSemilattice = requires(S a, S b) {
    { a & b } -> std::same_as<S>; // The Great Lower Bound
};

/**
 * @concept IsJoinSemiLattice
 * @brief A refinement of IsSet that supports the algebraic 
 *        structure of Join (|).
 * Wikipedia: SemiLattice (order)
 */
export template <typename S>
concept IsJoinSemilattice = requires(S a, S b) {
    { a | b } -> std::same_as<S>; // The Least Upper Bound
};

/**
 * @concept IsLattice
 * @brief A refinement of IsSet that supports the algebraic 
 *        structure of Meet (&) and Join (|).
 * Wikipedia: Lattice (order), Absorption law
 */
export template <typename S>
concept IsLattice = IsMeetSemilattice<S> && IsJoinSemilattice<S>;

/**
 * @concept IsPartiallyOrdered
 * @brief Elements that can be compared, but some may be "parallel."
 * Wikipedia: Partial order
 */
export template <typename T>
concept IsPartiallyOrdered = requires(const T a, const T b) {
    { a <= b } -> std::convertible_to<bool>;
    { a == b } -> std::convertible_to<bool>;
};

/**
 * @concept IsTotallyOrdered
 * @brief A Partial Order where every pair is comparable. No "Parallel" elements.
 * Wikipedia: Total order
 */
export template <typename T>
concept IsTotallyOrdered = IsPartiallyOrdered<T> && requires(const T a, const T b) {
    { a < b || a > b || a == b } -> std::convertible_to<bool>;
    // In C++20, this is elegantly captured by:
    requires std::three_way_comparable<T>;
};

/**
 * @concept IsLinearOrder
 * @brief Synonym for Total Order in our 1D road trip.
 */
export template <typename T>
concept IsLinearOrder = IsTotallyOrdered<T>;

} // namespace dedekind::ontology
