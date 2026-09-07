/** @file dedekind/numbers/real_bridge_test.cpp
 *
 * @brief Runtime coverage for the Birkhoff @b S leg
 *        @f$\mathbb{Q}\hookrightarrow\mathbb{R}=\mathbb{Q}(\sqrt2)@f$.
 *
 * The defining laws (homomorphism, injectivity, subfield image) are already
 * @b witnessed at compile time inside @c real_bridge.cppm --- the honest
 * contrast with the retired @c PlatonicReal, whose subalgebra legs were
 * postulated on an uninhabited stub.  Those static_asserts are invisible to
 * coverage, so here we re-run the same laws at runtime over a spread of
 * rationals and exercise the arrow's graph @f$\Gamma=\operatorname{graph}
 * (\text{embed})\subseteq\mathbb{Q}\times\mathbb{R}@f$.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */
#include <array>
#include <catch2/catch_test_macros.hpp>
#include <utility>

import dedekind.algebra;
import dedekind.category;
import dedekind.numbers;
import dedekind.relational;

using namespace dedekind::numbers;

namespace {
using Q = Rational<>;
using R2 = QuadraticReal<2>;

// A spread of rationals to drive the element-level laws (num/den, signs).
constexpr std::array<Q, 5> kSamples{Q{}, Q{1}, Q{2, 3}, Q{-3, 5}, Q{7, 2}};
}  // namespace

TEST_CASE("S-leg: ℚ ↪ ℝ classifies as EmbedsAsSubalgebra",
          "[numbers][hsp][subalgebra]") {
  STATIC_REQUIRE(dedekind::algebra::IsHomomorphism<EmbedRationalToReal<>>);
  STATIC_REQUIRE(dedekind::category::IsMonicArrow<EmbedRationalToReal<>>);
  STATIC_REQUIRE(dedekind::algebra::EmbedsAsSubalgebra<EmbedRationalToReal<>>);
}

TEST_CASE("S-leg: embed preserves the field operations (+, ×, 0, 1)",
          "[numbers][hsp][subalgebra]") {
  for (const Q& x : kSamples) {
    for (const Q& y : kSamples) {
      CHECK(embed_ℚ_ℝ(x + y) == embed_ℚ_ℝ(x) + embed_ℚ_ℝ(y));
      CHECK(embed_ℚ_ℝ(x * y) == embed_ℚ_ℝ(x) * embed_ℚ_ℝ(y));
    }
  }
  CHECK(embed_ℚ_ℝ(Q{}) == R2{});
  CHECK(embed_ℚ_ℝ(Q{1}) == R2{1});
}

TEST_CASE("S-leg: embed is injective and lands in the ℚ-subfield {b = 0}",
          "[numbers][hsp][subalgebra]") {
  for (std::size_t i = 0; i < kSamples.size(); ++i) {
    // Image carries no √2 component: a genuine copy of ℚ inside ℚ(√2).
    CHECK(embed_ℚ_ℝ(kSamples[i]).radical_part() == Q{});
    for (std::size_t j = 0; j < kSamples.size(); ++j) {
      // Monic: distinct rationals map to distinct reals (and vice versa).
      CHECK((kSamples[i] == kSamples[j]) ==
            (embed_ℚ_ℝ(kSamples[i]) == embed_ℚ_ℝ(kSamples[j])));
    }
  }
}

TEST_CASE("S-leg: Γ = graph(embed) is the arrow's graph on the product ℚ×ℝ",
          "[numbers][hsp][subalgebra][relational]") {
  // The inclusion arrow is embed_ℚ_ℝ; its graph Γ ⊆ ℚ×ℝ reifies it
  // relationally.
  const auto Γ = dedekind::sets::graph(embed_ℚ_ℝ);
  for (const Q& x : kSamples) {
    // (q, q+0√2) lies on Γ; (q, √2) does not — √2 ∉ image(ℚ).
    CHECK(Γ(std::pair{x, embed_ℚ_ℝ(x)}));
    CHECK_FALSE(Γ(std::pair{x, R2::root()}));
  }
}
