/** @file dedekind/analysis/hsp_complex_dual_test.cpp
 *
 * @brief Runtime coverage for the Birkhoff HSP legs of the two 2nd-order
 *        quotient functors over the coat-hanger ℝ = ℚ(√2):
 *        @f$\mathbb{C}=\mathbb{R}[i]/(i^2+1)@f$ and
 *        @f$\mathbb{D}=\mathbb{R}[\varepsilon]/(\varepsilon^2)@f$.
 *
 * The defining laws are @b witnessed at compile time inside @c complex.cppm and
 * @c analysis/dual.cppm (the S-leg homomorphism/injectivity, the P-leg product
 * iso, the H-leg quotient, and the functor-composability D<C<·>> / C<D<·>>).
 * Those static_asserts are invisible to coverage, so here we re-run the same
 * laws at runtime over a spread of reals and exercise the nested carriers.
 *
 * Mirrors @c numbers/real_bridge_test.cpp (the S-leg ℚ ↪ ℝ), one order up.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */
#include <array>
#include <catch2/catch_test_macros.hpp>
#include <functional>  // std::plus / std::multiplies for the ring/field witnesses
#include <utility>

import dedekind.algebra;
import dedekind.analysis;
import dedekind.category;
import dedekind.numbers;
import dedekind.relational; // graph(f), the functional relative product >>

using namespace dedekind::numbers;
using dedekind::analysis::Dual;
using dedekind::analysis::embed_ℝ_𝔻;

namespace {
using Q = Rational<>;
using R2 = QuadraticReal<2>;
using Cx = Complex<R2>;  // the canonical ℂ = C<R>
using Du = Dual<R2>;     // the canonical 𝔻 = D<R>

// A spread of exact reals to drive the element-level laws.
constexpr std::array<R2, 5> kReals{R2{}, R2{1}, R2{Q{2, 3}}, R2{Q{-3, 5}},
                                   R2::root()};

using EmbC = std::decay_t<decltype(embed_ℝ_ℂ)>;
using EmbD = std::decay_t<decltype(embed_ℝ_𝔻)>;

// The canonical projections: the pair-like default reaches ℂ (re/im, via
// .first/.second) by ordinary lookup; 𝔻's val/der overload is found by ADL.
using dedekind::category::π_1;
using dedekind::category::π_2;

// The product-level maps ℂ ↔ 𝔻: both are ℝ×ℝ via π_1/π_2, so reconstructing one
// from the other's projections is the canonical rank-2 ℝ-module iso.  NOT a
// ring hom (ℂ is a field, 𝔻 has nilpotents) --- it exhibits the shared P-leg,
// and gives the relative product a ℂ↬𝔻 leg to compose through.
constexpr auto cx_to_du = dedekind::category::arrow<Cx, Du>(
    [](const Cx& z) { return Du{π_1(z), π_2(z)}; });
constexpr auto du_to_cx = dedekind::category::arrow<Du, Cx>(
    [](const Du& d) { return Cx{π_1(d), π_2(d)}; });
}  // namespace

// ── S-leg ────────────────────────────────────────────────────────────────────
TEST_CASE("HSP-S: ℝ ↪ ℂ and ℝ ↪ 𝔻 classify as EmbedsAsSubalgebra",
          "[analysis][numbers][hsp][subalgebra]") {
  STATIC_REQUIRE(dedekind::algebra::IsHomomorphism<EmbC>);
  STATIC_REQUIRE(dedekind::category::IsMonicArrow<EmbC>);
  STATIC_REQUIRE(dedekind::algebra::EmbedsAsSubalgebra<EmbC>);

  STATIC_REQUIRE(dedekind::algebra::IsHomomorphism<EmbD>);
  STATIC_REQUIRE(dedekind::category::IsMonicArrow<EmbD>);
  STATIC_REQUIRE(dedekind::algebra::EmbedsAsSubalgebra<EmbD>);
}

TEST_CASE("HSP-S: the embeddings preserve the ring operations (+, ×, 0, 1)",
          "[analysis][numbers][hsp][subalgebra]") {
  for (const R2& x : kReals) {
    for (const R2& y : kReals) {
      CHECK(embed_ℝ_ℂ(x + y) == embed_ℝ_ℂ(x) + embed_ℝ_ℂ(y));
      CHECK(embed_ℝ_ℂ(x * y) == embed_ℝ_ℂ(x) * embed_ℝ_ℂ(y));
      CHECK(embed_ℝ_𝔻(x + y) == embed_ℝ_𝔻(x) + embed_ℝ_𝔻(y));
      CHECK(embed_ℝ_𝔻(x * y) == embed_ℝ_𝔻(x) * embed_ℝ_𝔻(y));
    }
  }
  CHECK(embed_ℝ_ℂ(R2{}) == Cx{});
  CHECK(embed_ℝ_ℂ(R2{1}) == Cx{R2{1}, R2{}});
  CHECK(embed_ℝ_𝔻(R2{}) == Du{});
  CHECK(embed_ℝ_𝔻(R2{1}) == Du{R2{1}, R2{}});
}

TEST_CASE("HSP-S: the embeddings are injective, into the constant subalgebra",
          "[analysis][numbers][hsp][subalgebra]") {
  for (std::size_t i = 0; i < kReals.size(); ++i) {
    // Image carries no i / ε component: a genuine copy of ℝ inside ℂ / 𝔻.
    CHECK(embed_ℝ_ℂ(kReals[i]).imag() == R2{});
    CHECK(embed_ℝ_𝔻(kReals[i]).derivative() == R2{});
    for (std::size_t j = 0; j < kReals.size(); ++j) {
      const bool same = kReals[i] == kReals[j];
      CHECK(same == (embed_ℝ_ℂ(kReals[i]) == embed_ℝ_ℂ(kReals[j])));
      CHECK(same == (embed_ℝ_𝔻(kReals[i]) == embed_ℝ_𝔻(kReals[j])));
    }
  }
}

// ── P-leg ────────────────────────────────────────────────────────────────────
TEST_CASE("HSP-P: ℂ and 𝔻 are both IsProduct ≅ ℝ×ℝ via the canonical π_1/π_2",
          "[analysis][numbers][hsp][product]") {
  // UNIFORM now: both satisfy IsProduct through the canonical projections ---
  // ℂ via the pair-like default (re/im), 𝔻 via its val/der overload.
  STATIC_REQUIRE(dedekind::category::IsProduct<Cx, R2, R2>);
  STATIC_REQUIRE(dedekind::category::IsProduct<Du, R2, R2>);
  for (const R2& a : kReals) {
    for (const R2& b : kReals) {
      const Cx z{a, b};
      CHECK(π_1(z) == a);
      CHECK(π_2(z) == b);
      CHECK(π_1(z) == z.real());       // π_1 == the re alias
      CHECK(π_2(z) == z.imag());       // π_2 == the im alias
      CHECK(Cx{π_1(z), π_2(z)} == z);  // ⟨π₁, π₂⟩ = id

      const Du d{a, b};
      CHECK(π_1(d) == a);
      CHECK(π_2(d) == b);
      CHECK(π_1(d) == d.value());       // π_1 == the val alias
      CHECK(π_2(d) == d.derivative());  // π_2 == the der alias
      CHECK(Du{π_1(d), π_2(d)} == d);   // ⟨π₁, π₂⟩ = id
    }
  }
}

// ── H-leg ────────────────────────────────────────────────────────────────────
TEST_CASE("HSP-H: ℂ = ℝ[i]/(i²+1) and 𝔻 = ℝ[ε]/(ε²) as quotient algebras",
          "[analysis][numbers][hsp][quotient]") {
  STATIC_REQUIRE(dedekind::category::IsQuotientAlgebra<Cx>);
  STATIC_REQUIRE(dedekind::category::IsQuotientAlgebra<Du>);

  // The ALGEBRAIC classification of the two H-leg quotients --- the
  // discriminant trichotomy.  Both are commutative rings (rig traits lifted
  // from ℝ by propagation); field-ness is discriminant-dependent:
  //   ℂ = ℝ[i]/(x²+1): x²+1 IRREDUCIBLE over the formally-real ℝ ⇒ a FIELD;
  //   𝔻 = ℝ[ε]/(ε²):  ε is NILPOTENT (ε²=0, a zero divisor) ⇒ a ring, NOT a
  //                    field (the parabolic/local-ring sibling).
  STATIC_REQUIRE(
      dedekind::category::IsRing<Cx, std::plus<Cx>, std::multiplies<Cx>>);
  STATIC_REQUIRE(
      dedekind::category::IsRing<Du, std::plus<Du>, std::multiplies<Du>>);
  // Assert the public IsField concept (composes commutative-ring +
  // multiplicative-group), so the witness fails if any part of the field chain
  // is missing --- not just the is_invertible_v flag.
  STATIC_REQUIRE(
      dedekind::category::IsField<Cx, std::plus<Cx>, std::multiplies<Cx>>);
  // The field certificate is NOT ℚ(√2)-specific: it is gated on R being an
  // ORDERED FIELD (formally real ⇒ x²+1 irreducible), so ℂ over the base
  // rational field ℚ is a field through the same single registration.
  STATIC_REQUIRE(dedekind::category::IsField<Complex<Q>, std::plus<Complex<Q>>,
                                             std::multiplies<Complex<Q>>>);
  STATIC_REQUIRE(  // 𝔻 is a ring, NOT a field (ε nilpotent)
      !dedekind::category::IsField<Du, std::plus<Du>, std::multiplies<Du>>);

  // The defining quotient relations, run: i² = −1 and ε² = 0.
  const Cx i{R2{}, R2{1}};
  CHECK(i * i == Cx{R2{-1}, R2{}});
  const Du eps{R2{}, R2{1}};
  CHECK(eps * eps == Du{});
}

// ── Composability: the two 2nd-order functors nest, in either order
// ───────────
TEST_CASE("HSP: the quotient functors compose (D<C<·>>, C<D<·>>)",
          "[analysis][numbers][hsp][quotient][compose]") {
  using DCQ = Dual<Complex<Q>>;  // D<C<Q>>
  using DCR = Dual<Cx>;          // D<C<R>>
  using CDR = Complex<Du>;       // C<D<R>>
  STATIC_REQUIRE(dedekind::category::IsQuotientAlgebra<DCQ>);
  STATIC_REQUIRE(dedekind::category::IsQuotientAlgebra<DCR>);
  STATIC_REQUIRE(dedekind::category::IsQuotientAlgebra<CDR>);

  // Each nested carrier is a bona-fide ring: exercise +, × at the outer level.
  const DCQ p{Complex<Q>{Q{2}, Q{1}}, Complex<Q>{Q{1}, Q{}}};
  CHECK((p + p).value() == Complex<Q>{Q{4}, Q{2}});

  // C<D<R>>: w = (2+ε) + (1)i; Re(w²) = Re² − Im² = (2+ε)² − 1 = 3 + 4ε.
  const CDR w{Du{R2{2}, R2{1}}, Du{R2{1}, R2{}}};
  CHECK((w * w).real() == Du{R2{3}, R2{4}});
}

// ── The legs as graphs, threaded through the functional relative product
// ──────
TEST_CASE("HSP: the legs reify as functional graphs Γ ⊆ A×B and compose",
          "[analysis][numbers][hsp][relational][compose]") {
  using dedekind::sets::graph;
  using namespace dedekind::category;  // the functional relative product >>

  const Q q{2, 3};
  const R2 r = embed_ℚ_ℝ(q);
  const Cx c = embed_ℝ_ℂ(r);
  const Du d = embed_ℝ_𝔻(r);

  // Each leg reifies as a functional graph: (a, f(a)) lies on it, a wrong image
  // does not.  ℚ↪ℝ↪ℂ / ↪𝔻 are the S-legs; ℂ↔𝔻 are the P-leg module isos.
  CHECK(graph(embed_ℚ_ℝ)(std::pair{q, r}));
  CHECK(graph(embed_ℝ_ℂ)(std::pair{r, c}));
  CHECK(graph(embed_ℝ_𝔻)(std::pair{r, d}));
  CHECK(graph(cx_to_du)(std::pair{c, cx_to_du(c)}));
  CHECK(graph(du_to_cx)(std::pair{d, du_to_cx(d)}));
  CHECK_FALSE(graph(embed_ℝ_ℂ)(std::pair{r, Cx{}}));

  // Composition = the functional relative product (#807): graph(f);graph(g) =
  // graph(f;g), the ∃-middle discharged by functionality.  ℚ ↬ ℝ ↬ ℂ:
  const auto ΓQC = graph(embed_ℚ_ℝ) >> graph(embed_ℝ_ℂ);
  CHECK(ΓQC(std::pair{q, c}));  // (q, embed_ℝ_ℂ(embed_ℚ_ℝ(q))) is on it
  CHECK_FALSE(ΓQC(std::pair{q, Cx{}}));  // a wrong image is rejected

  // ℝ ↬ ℂ ↬ 𝔻: an S-leg composed with the P-leg module iso.
  const auto ΓRD_viaC = graph(embed_ℝ_ℂ) >> graph(cx_to_du);
  CHECK(ΓRD_viaC(std::pair{r, cx_to_du(c)}));
}
