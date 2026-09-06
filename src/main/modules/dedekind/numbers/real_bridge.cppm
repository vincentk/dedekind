/**
 * @file dedekind/numbers/real_bridge.cppm
 * @partition :real_bridge
 * @brief The ℝ bridge as first-class @b Trsk @b relations: subalgebra
 *        inclusions of the reified carriers into the (modelled, uninhabited) ℝ.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @details A subalgebra inclusion is a @b relation --- the graph of the
 * inclusion map --- and relations are first-class here, so the bridge is
 * exposed as reusable relation values, not as one-off arrows.  The relational
 * @c graph builder lives in @c dedekind.relational, so this dependency is
 * isolated in its own partition rather than pinned on all consumers of
 * @c :real.
 */
module;

export module dedekind.numbers:real_bridge;

import dedekind.algebra;    // EmbedsAsSubalgebra
import dedekind.category;   // IsSet
import dedekind.relational; // graph(f) — the graph of an arrow as a relation
import :real;               // embed_ℚ_ℝ / EmbedQtoR / PlatonicReal

namespace dedekind::numbers {

/**
 * @brief @f$\mathbb{Q}\hookrightarrow\mathbb{R}@f$ as a first-class Trsk
 *        relation: the graph of the inclusion @f$\iota@f$,
 *        @f$\{(q,\iota(q))\} \subseteq \mathbb{Q}\times\mathbb{R}@f$.
 *
 * @details A reusable component --- the @b relational form of the subalgebra
 * embedding @c EmbedsAsSubalgebra<EmbedQtoR<>>.  Since a function @b is its
 * graph, this is the honest expression of "ℚ is a subfield of ℝ": the relation
 * @f$y = \iota(x)@f$ on the product @f$\mathbb{Q}\times\mathbb{R}@f$.  It is
 * @b postulated / intensional --- ℝ is uninhabited, so the relation is
 * @b modelled, not @b materialised (its pairs are never enumerated).
 */
export inline constexpr auto ι_ℚ_ℝ = dedekind::sets::graph(embed_ℚ_ℝ<>);

// ι_ℚ_ℝ is a Trsk relation (an ETCS Set of pairs on ℚ × ℝ)...
static_assert(dedekind::category::IsSet<decltype(ι_ℚ_ℝ)>,
              "ι_ℚ_ℝ is a Trsk relation: a Set of pairs on ℚ × ℝ.");
// ...and it is the graph of a faithful subalgebra embedding (ℚ ↪ ℝ).
static_assert(
    dedekind::algebra::EmbedsAsSubalgebra<EmbedQtoR<>>,
    "ι_ℚ_ℝ is the graph of ℚ ↪ ℝ, a faithful (postulated) embedding.");

}  // namespace dedekind::numbers
