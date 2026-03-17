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
 * @concept IsExtensional
 * @brief A set defined solely by the enumeration of its members.
 * @details In our silicon reality, this implies a finite "Size Upper Bound."
 * Wikipedia: Extensionality, Axiom of extensionality
 */
export template <typename S>
concept IsExtensional =
    IsSet<S, typename S::element_type> && requires(const S s) {
      typename S::is_extensional_tag;  // Structural proof
      { s.size_upper_bound() } -> std::integral;
    };

/**
 * @section Mereology: Pointed Species.
 * @concept IsPointed
 * @brief A species that defines its own structural origin.
 * Wikipedia: Pointed space, Origin (mathematics)
 */
export template <typename T>
concept IsPointed = requires {
  { T::origin() } -> std::same_as<T>;
};

/**
 * @concept IsPointedSet
 * @brief A Set that has a designated "Origin" or "Identity" element.
 * Wikipedia: Pointed set
 */
export template <typename S, typename T>
concept IsPointedSet = IsSet<S, T> && IsPointed{};

/**
 * @concept IsMeetSemiLattice
 * @brief A refinement of IsSet that supports the algebraic
 *        structure of Meet (&).
 * Wikipedia: SemiLattice (order)
 */
export template <typename S>
concept IsMeetSemilattice = requires(S a, S b) {
  { a & b } -> std::same_as<S>;  // The Great Lower Bound
};

/**
 * @concept IsJoinSemiLattice
 * @brief A refinement of IsSet that supports the algebraic
 *        structure of Join (|).
 * Wikipedia: SemiLattice (order)
 */
export template <typename S>
concept IsJoinSemilattice = requires(S a, S b) {
  { a | b } -> std::same_as<S>;  // The Least Upper Bound
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
 * @concept IsBoundedLattice
 * @brief A Lattice with a unique "Top" (1) and "Bottom" (0).
 * @details
 * 1. Meet(a, Bottom) = Bottom
 * 2. Join(a, Top) = Top
 * Wikipedia: Bounded lattice
 */
export template <typename S>
concept IsBoundedLattice = IsLattice<S> && requires(S s) {
  { s.lower_bound() } -> std::same_as<typename S::element_type>;  // The Bottom
  { s.upper_bound() } -> std::same_as<typename S::element_type>;  // The Top
};

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
 * @brief A Partial Order where every pair is comparable.
 * @details This is the "Ruler" for our 1D road trip.
 */
export template <typename T>
concept IsTotallyOrdered =
    IsPartiallyOrdered<T> && requires(const T a, const T b) {
      { a < b } -> std::convertible_to<bool>;
      { a <=> b } -> std::same_as<std::strong_ordering>;
      requires std::three_way_comparable<T>;  // The C++20 "Naked" Proof
    };

/**
 * @concept IsLinearOrder
 * @brief Synonym for Total Order in our 1D road trip.
 */
export template <typename T>
concept IsLinearOrder = IsTotallyOrdered<T>;

}  // namespace dedekind::ontology
