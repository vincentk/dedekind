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
concept IsCardinality = requires {
    { C::is_transfinite } -> std::convertible_to<bool>;
};

/** @concept IsTransfinite */
export template <typename C>
concept IsTransfinite = IsCardinality<C> && C::is_transfinite;

/** @concept IsCountable */
export template <typename C>
concept IsCountable = IsCardinality<C>; // Simplify for now

/** @concept IsFinite */
export template <typename C>
concept IsFinite = IsCountable<C> && !IsTransfinite<C>;

/** @concept IsAleph0 */
export template <typename C>
concept IsAleph0 = IsCountable<C> && IsTransfinite<C>;

} // namespace dedekind::ontology