/**
 * @file ontology:cardinalities.cppm
 * @brief The Ontological Foundation of Magnitude.
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */
module;

#include <concepts>
#include <functional>

export module dedekind.ontology:cardinalities;

namespace dedekind::ontology {

/** @concept IsCardinality */
export template <typename C>
concept IsCardinality = requires(C a, C b) {
    { C::is_countable } -> std::convertible_to<bool>;
    { C::is_finite } -> std::convertible_to<bool>;

    /** @brief The Cantor Jump: 2^C is always defined. */
    typename C::power_type;
    requires IsCardinality<typename C::power_type>;

        /** 
     * @brief Weakening the law to Partial Order.
     * This allows us to be ZF-compliant while still supporting 
     * the numerical hierarchy (N < Z < Q < R).
     */
    { a <=> b } -> std::same_as<std::partial_ordering>;
};

/** @concept IsCountable */
export template <typename C>
concept IsCountable = IsCardinality<C> && (C::is_countable == true);

export template <typename C>
concept IsUncountable = IsCardinality<C> && (C::is_countable == false);

/** @brief The Axiomatic Order of Regions.
    Theorem: Any Uncountable species is strictly greater than any Countable
   species. */
export template <IsCardinality L, IsCardinality R>
  requires(IsCountable<L> != IsCountable<R> && IsUncountable<L> != IsUncountable<R>)
constexpr std::strong_ordering operator<=>(const L&, const R&) {
  if constexpr (IsUncountable<L>)
    return std::strong_ordering::greater;
  else
    return std::strong_ordering::less;
}

/** @concept IsTransfinite */
export template <typename C>
concept IsTransfinite = IsCardinality<C> && (C::is_finite == false);

/** @concept IsFinite */
export template <typename C>
concept IsFinite = IsCountable<C> && (C::is_countable == true) && (C::is_finite == true);


/** @brief The Axiomatic Order of Finitude.
    Theorem: Any Transfinite species is strictly greater than any Finite
   species. */
export template <IsCardinality L, IsCardinality R>
  requires(IsFinite<L> != IsFinite<R> && IsTransfinite<L> != IsTransfinite<R>)
constexpr std::strong_ordering operator<=>(const L&, const R&) {
  if constexpr (IsTransfinite<L>)
    return std::strong_ordering::greater;
  else
    return std::strong_ordering::less;
}

/**
 * @struct ℵ
 * @brief The Universal Generator for the Transfinite Hierarchy.
 * 
 * @tparam N The index in the series (0 for Countable, >0 for Uncountable).
 * 
 * @details ℵ is a "Naked" symbolic tag. It carries no data, only its 
 *          position in the hierarchy. Under GCH, the Power Set morphism 
 *          is simply the successor in the index.
 */
export template <std::size_t N>
struct ℵ {
    /** @brief Identity: The rung on the transfinite ladder. */
    static constexpr std::size_t index = N;
    
    /** @brief Requirement: All ℵ are transfinite (not finite). */
    static constexpr bool is_finite = false;

    /** @brief The Switch: ℵ₀ is Countable (true), ℵ₁₊ is Uncountable (false). */
    static constexpr bool is_countable = (N == 0);

    /** @brief The Cantor Animation: 2^ℵₙ ↣ ℵₙ₊₁ */
    using power_type = ℵ<N + 1>;

    /** @brief Comparison: ℵₙ is equivalent only to itself. */
    friend constexpr std::partial_ordering operator<=>(ℵ, ℵ) {
        return std::partial_ordering::equivalent;
    }
};

/** @brief Cross-Index Comparison: ℵₙ < ℵₘ if n < m. */
export template <std::size_t N, std::size_t M>
constexpr std::partial_ordering operator<=>(ℵ<N>, ℵ<M>) {
    return N <=> M;
}

/** 
 * @section The Canonical Symbolic Aliases 
 * These aliases provide the standard mathematical notation for our 
 * numerical foundations.
 */

/** @brief ℵ₀: The Cardinality of the Naturals, Integers, and Rationals. */
export using ℵ_0 = ℵ<0>; 

/** @brief ℵ₁: The Cardinality of the Real Continuum (under GCH). */
export using ℵ_1 = ℵ<1>;

/** @brief ℶ₁: Alias for the Beth-1 Continuum. */
export using ℶ_1 = ℵ_1;

} // namespace dedekind::ontology