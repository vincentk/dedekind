/**
 * @file etcs.cppm
 * @partition :etcs
 * @brief Level 0c: Elementary Theory of the Category of Sets (ETCS).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 *
 * @section ETCS: The Categorical Axiomatization of Set Theory
 * "In the mathematical development of recent decades, the notion of
 *  set has not only played a fundamental role, but it has itself
 *  passed through a long process of refinement."
 *  — F. William Lawvere, "An Elementary Theory of the Category of Sets"
 *
 * @details
 * This partition provides the axiomatic foundations of ETCS, allowing
 * the Dedekind Universe to treat "Sets" as objects within a category
 * rather than collections defined by membership (ZF).
 *
 * We utilize the 'Ontology Bridge' (SpeciesTraits) to ensure that
 * primitive types (int, bool, size_t) can be treated as first-class
 * categorical objects without violating their C++ nature.
 *
 * @build_order 0
 * @dependency :species, :logic
 *
 * Wikipedia: Elementary Theory of the Category of Sets
 */
module;

#include <concepts>
#include <functional>
#include <type_traits>

export module dedekind.category:etcs;

import :morphism;
import :logic;
import :species;
import :topoi;

namespace dedekind::category {

/**
 * @concept IsSetObject
 * @brief ETCS-facing alias for a subobject S ↣ A.
 */
export template <typename S, typename A>
concept IsSetObject = IsSubobject<S, A>;

/**
 * @concept HasTernarySupport
 * @brief Subobject with χ: A -> Ω_K3 (Ternary support classifier).
 */
export template <typename S>
concept HasTernarySupport = requires(S s) {
	typename S::Ambient;
	requires IsSubobject<S, typename S::Ambient>;
	requires std::same_as<Cod<decltype(s.χ)>, Ternary>;
};

/**
 * @concept IsCompatibleSetPair
 * @brief Two subobjects that share ambient species and Ω codomain.
 */
export template <typename S1, typename S2>
concept IsCompatibleSetPair =
		requires(S1 s1, S2 s2) {
			typename S1::Ambient;
			typename S2::Ambient;
			requires IsSubobject<S1, typename S1::Ambient>;
			requires IsSubobject<S2, typename S2::Ambient>;
			requires std::same_as<typename S1::Ambient, typename S2::Ambient>;
			requires std::same_as<Cod<decltype(s1.χ)>, Cod<decltype(s2.χ)>>;
		};

/** @brief ETCS intersection: materialize A ∩ B from χ_A ∧ χ_B. */
export template <typename S1, typename S2>
	requires IsCompatibleSetPair<S1, S2>
constexpr auto set_intersection(const S1& lhs, const S2& rhs) {
	using A = typename S1::Ambient;
	return classify<A>(lhs.χ && rhs.χ);
}

/** @brief ETCS union: materialize A ∪ B from χ_A ∨ χ_B. */
export template <typename S1, typename S2>
	requires IsCompatibleSetPair<S1, S2>
constexpr auto set_union(const S1& lhs, const S2& rhs) {
	using A = typename S1::Ambient;
	return classify<A>(lhs.χ || rhs.χ);
}

/** @brief ETCS complement: materialize A^c from ¬χ_A. */
export template <typename S>
	requires IsSubobject<S, typename S::Ambient>
constexpr auto set_complement(const S& s) {
	using A = typename S::Ambient;
	return classify<A>(!s.χ);
}

/** @brief Lattice alias: meet = intersection. */
export template <typename S1, typename S2>
	requires IsCompatibleSetPair<S1, S2>
constexpr auto meet(const S1& lhs, const S2& rhs) {
	return set_intersection(lhs, rhs);
}

/** @brief Lattice alias: join = union. */
export template <typename S1, typename S2>
	requires IsCompatibleSetPair<S1, S2>
constexpr auto join(const S1& lhs, const S2& rhs) {
	return set_union(lhs, rhs);
}

}  // namespace dedekind::category