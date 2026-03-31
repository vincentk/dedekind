module;

#include <concepts>
#include <cstdint>
#include <functional>
#include <type_traits>
#include <utility>

export module dedekind.sets:logic;

import :mereology;

namespace dedekind::sets {

/**
 * @brief The Dedekind Cut: Representing a Real number as a partition of
 * Rationals.
 * @tparam Q An Ordered Field (typically the Rational species).
 */
template <ontology::IsOrderedField Q>
  requires ontology::IsDense<Q> &&
           std::same_as<typename Q::cardinality_type, ontology::ℵ_0>
struct DedekindCut {
  using Predicate = std::function<bool(Q)>;

  // The "Rule": A Real is defined by its Lower and Upper sets.
  // In practice, we only need the Lower set (the "cut").
  Predicate lower_set;

  explicit constexpr DedekindCut(Predicate L) : lower_set(std::move(L)) {}

  /** @brief Membership test: Is a rational 'q' in the lower set? */
  constexpr bool operator[](Q q) const { return lower_set(q); }

  /** @brief The result of the construction is a species of the Continuum. */
  static constexpr auto cardinality() {
    return power(typename Q::cardinality_type{});  // 2^ℵ₀ = ℶ₁
  }
};

/** @brief Comparison: A < B if A's lower set is a proper subset of B's. */
template <typename Q>
constexpr bool operator<(const DedekindCut<Q>& a, const DedekindCut<Q>& b) {
  // This is symbolic: there must exist a rational q such that q is in B but not
  // A. In a "Best Effort" library, this triggers a search/proof.
  return exists_between(a, b);
}

/** @brief Symbolic Constant: Square Root of 2 */
export template <typename Q>
constexpr auto Sqrt2() {
  return DedekindCut<Q>([](Q q) { return q < 0 || (q * q < Q(2)); });
}

}  // namespace dedekind::sets
