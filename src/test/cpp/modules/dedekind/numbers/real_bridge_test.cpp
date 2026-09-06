/** @file dedekind/numbers/real_bridge_test.cpp
 *
 * @brief The worked bridge example: @f$\mathbb{Q}\hookrightarrow\mathbb{R}@f$
 * as a faithful embedding (@c EmbedsAsSubalgebra).
 *
 * ℚ (@c Rational) is @b not a subobject of ℝ's carrier (the uninhabited
 * @c PlatonicReal) --- they are unrelated types.  The relation is the
 * categorical reading of "ℚ ⊆ ℝ": a structure-preserving @b monomorphism whose
 * image is a subfield.  Because ℝ is uninhabited the embedding is @b postulated
 * (declared with the right domain/codomain, never executed), exactly the opt-in
 * @c is_homomorphism_v / @c is_monic_arrow_v tier.  This nails the bridge by
 * example: ℝ is modelled (a field), reached only through certified embeddings.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */
#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <type_traits>
#include <utility>

import dedekind.algebra;
import dedekind.category;
import dedekind.numbers;

using namespace dedekind::numbers;

TEST_CASE("ℚ embeds as a subfield of ℝ (faithful, postulated)",
          "[numbers][real][subalgebra][bridge]") {
  SECTION("faithful embedding: EmbedsAsSubalgebra (a monic homomorphism)") {
    STATIC_CHECK(dedekind::category::IsArrow<EmbedQtoR<>>);
    STATIC_CHECK(dedekind::algebra::IsHomomorphism<EmbedQtoR<>>);
    STATIC_CHECK(dedekind::category::IsMonicArrow<EmbedQtoR<>>);
    STATIC_CHECK(dedekind::algebra::EmbedsAsSubalgebra<EmbedQtoR<>>);
  }

  SECTION("domain is ℚ; codomain is ℝ's (uninhabited) carrier") {
    STATIC_CHECK(std::same_as<typename EmbedQtoR<>::Domain, Rational<>>);
    STATIC_CHECK(std::same_as<typename EmbedQtoR<>::Codomain, PlatonicReal>);
  }

  SECTION("the reusable relation component: ι_ℚ_ℝ = graph(ι)") {
    // A function IS its graph, so the inclusion is a first-class Trsk relation
    // {(q, ι(q))} ⊆ ℚ × ℝ — modelled, not materialised (ℝ uninhabited).
    STATIC_CHECK(dedekind::category::IsSet<decltype(ι_ℚ_ℝ)>);
    STATIC_CHECK(
        std::same_as<typename std::remove_cvref_t<decltype(ι_ℚ_ℝ)>::Domain,
                     std::pair<Rational<>, PlatonicReal>>);
  }

  SECTION("the postulated ι is callable (codecov); its image is not asserted") {
    // ℝ is uninhabited, so the image of a rational is a real no finite carrier
    // can exhibit; we exercise ι without asserting its placeholder value.
    const auto img = embed_ℚ_ℝ<>(Rational<>{});
    (void)img;
  }
}
