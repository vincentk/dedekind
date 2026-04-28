/**
 * @file initial_ring_test.cpp
 * @brief Test suite for the @c algebra:initial_ring partition.
 *
 * @section Scope
 * Exercises the dual universal-property reification of @c ℤ
 * (closes part of #446):
 *
 *   1. @c IsInitialRing<SignedCardinality> — for every ring @c R
 *      there exists a unique ring homomorphism
 *      @c SignedCardinality → @c R.  The test exercises the universal
 *      property at @c R = @c Modular<n>, where the unique
 *      homomorphism is the mod-@c n reduction.
 *
 *   2. @c IsGrothendieckGroup<SignedCardinality, Cardinality> —
 *      @c SignedCardinality is the free abelian group on the
 *      commutative monoid @c (Cardinality, +, 0).  The test
 *      exercises the closure-forcing operator
 *      @c Cardinality - Cardinality → SignedCardinality as the
 *      operator-level realisation of the Grothendieck construction.
 *
 * Both Forms anchor the same carrier; this test suite is the
 * @b formal, @b type-checked @b documentation that the dual
 * inhabitance holds operationally.
 *
 *   3. Carrier-lattice arrows (Figure 1 in @c paper.tex /
 *      @c report.tex) — the labelled arrows are mechanically
 *      witnessed as @c IsMonicArrow / @c IsArrow concepts so the
 *      figure's claims are not schematic prose but type-checked
 *      facts.
 */
#include <catch2/catch_test_macros.hpp>
#include <type_traits>
#include <variant>  // load-bearing for variant operator== via ADL

import dedekind.algebra;
import dedekind.category;
import dedekind.morphologies;
import dedekind.numbers;
import dedekind.sets;

using namespace dedekind::category;
using namespace dedekind::numbers;
using namespace dedekind::sets;
using dedekind::algebra::is_grothendieck_group_v;
using dedekind::algebra::is_initial_ring_v;
using dedekind::algebra::IsGrothendieckGroup;
using dedekind::algebra::IsInitialRing;
using dedekind::morphologies::Modular;

// ===========================================================================
// (1) Concept fires on the canonical witnesses
// ===========================================================================

TEST_CASE("initial_ring: IsInitialRing<SignedCardinality> fires",
          "[algebra][initial_ring][witness]") {
  STATIC_CHECK(IsInitialRing<SignedCardinality>);
  STATIC_CHECK(is_initial_ring_v<SignedCardinality>);
}

TEST_CASE(
    "initial_ring: IsGrothendieckGroup<SignedCardinality, Cardinality> fires",
    "[algebra][grothendieck][witness]") {
  STATIC_CHECK(IsGrothendieckGroup<SignedCardinality, Cardinality>);
  STATIC_CHECK((is_grothendieck_group_v<SignedCardinality, Cardinality>));
}

TEST_CASE(
    "initial_ring: opt-in trait defaults to false (no initial-ring witness "
    "without explicit registration)",
    "[algebra][initial_ring][negative]") {
  // Default: not initial.  Carriers must opt in explicitly.
  STATIC_CHECK(!is_initial_ring_v<int>);
  STATIC_CHECK(!is_initial_ring_v<unsigned int>);
  STATIC_CHECK(!is_initial_ring_v<Cardinality>);
  STATIC_CHECK((!is_grothendieck_group_v<unsigned int, Cardinality>));
  STATIC_CHECK((!is_grothendieck_group_v<Cardinality, Cardinality>));
}

// ===========================================================================
// (2) Universal property exercised: χ_{Modular<n>} as mod-n reduction
// ===========================================================================
//
// The unique ring homomorphism @c χ_R : @c SignedCardinality → @c R
// for @c R = @c Modular<n> is the mod-@c n reduction.  We exercise
// this operationally — the test suite pins what the universal-
// property witness's @b operational behaviour must be on a concrete
// target ring.

namespace {
// Helper: mod-n reduction of a finite SignedCardinality value.
// Implements χ_{Modular<n>} for the finite fragment.
//
// Precision: take the modulus in the limb type (the wider unsigned
// type the @c ExtensionalCardinal stores @c magnitude.limbs[0] in)
// before narrowing to @c Modular<N>::machine_type.  Casting first
// would truncate the magnitude when @c machine_type is narrower
// than the limb type.
template <auto N>
constexpr Modular<N> χ_to_modular(SignedCardinality const& z) noexcept {
  if (auto const* sec = std::get_if<SignedExtensionalCardinal<>>(&z)) {
    using ML = typename Modular<N>::machine_type;
    auto const limb = sec->magnitude.limbs[0];
    using LimbType = std::remove_cv_t<std::remove_reference_t<decltype(limb)>>;
    auto const reduced_in_limb = limb % static_cast<LimbType>(N);
    auto const reduced_mag = static_cast<ML>(reduced_in_limb);
    if (sec->negative && reduced_mag != 0) {
      return Modular<N>{static_cast<ML>(N) - reduced_mag};
    }
    return Modular<N>{reduced_mag};
  }
  // Sentinels (±ℵ_0, NaZ): not part of the textbook ℤ; the
  // universal property does not extend.  Return zero as a
  // placeholder; this case is out of scope for the test.
  return Modular<N>{0};
}
}  // namespace

TEST_CASE(
    "initial_ring: χ_{Modular<5>} is the unique ring homomorphism — sends "
    "1 to 1, preserves +, *, zero",
    "[algebra][initial_ring][modular][universal-property]") {
  // Sends 1_ℤ to 1_M (the load-bearing universal-property clause).
  CHECK(χ_to_modular<5>(finite_signed_cardinality(1)).value == 1);
  CHECK(χ_to_modular<5>(finite_signed_cardinality(0)).value == 0);
  // Mod-5 reduction on positive finite values.
  CHECK(χ_to_modular<5>(finite_signed_cardinality(7)).value == 2);
  CHECK(χ_to_modular<5>(finite_signed_cardinality(13)).value == 3);
  CHECK(χ_to_modular<5>(finite_signed_cardinality(25)).value == 0);
  // Mod-5 reduction on negative values (sign-aware: -3 ≡ 2 (mod 5)).
  CHECK(χ_to_modular<5>(finite_signed_cardinality(-1)).value == 4);
  CHECK(χ_to_modular<5>(finite_signed_cardinality(-3)).value == 2);
  CHECK(χ_to_modular<5>(finite_signed_cardinality(-5)).value == 0);
  CHECK(χ_to_modular<5>(finite_signed_cardinality(-7)).value == 3);
}

TEST_CASE(
    "initial_ring: χ_{Modular<n>} is a ring homomorphism — preserves + and "
    "* on representative values",
    "[algebra][initial_ring][modular][ring-homomorphism]") {
  auto const z2 = finite_signed_cardinality(2);
  auto const z3 = finite_signed_cardinality(3);
  auto const z5 = finite_signed_cardinality(5);
  // χ(a + b) = χ(a) + χ(b)
  auto const sum_via_z = χ_to_modular<7>(z2 + z3);
  auto const sum_via_m = χ_to_modular<7>(z2) + χ_to_modular<7>(z3);
  CHECK(sum_via_z.value == sum_via_m.value);
  // χ(a * b) = χ(a) * χ(b)
  auto const prod_via_z = χ_to_modular<7>(z2 * z5);
  auto const prod_via_m = χ_to_modular<7>(z2) * χ_to_modular<7>(z5);
  CHECK(prod_via_z.value == prod_via_m.value);
  // Wrap example: χ(8) = χ(1) since 8 ≡ 1 (mod 7).
  CHECK(χ_to_modular<7>(finite_signed_cardinality(8)).value == 1);
}

// ===========================================================================
// (3) Grothendieck construction exercised: closure-forcing operator
// ===========================================================================

TEST_CASE(
    "grothendieck: Cardinality - Cardinality → SignedCardinality realises "
    "the Grothendieck construction at the operator level",
    "[algebra][grothendieck][universal-property][closure-forcing]") {
  // Exercise the textbook construction: [(a, b)] ∈ ℤ represents
  // a - b.  The closure-forcing operator's existence with the wider
  // codomain is the Grothendieck-construction's @b multiplication
  // (the construction map at the operator level); the @b unit is
  // the embedding @c lift_ℕ_ℤ_ that lifts a single Cardinality into
  // SignedCardinality (covered in test (5) below).
  auto const a3 = finite_cardinality(3);
  auto const a5 = finite_cardinality(5);
  auto const a7 = finite_cardinality(7);
  // 5 - 3 = 2.
  CHECK(a5 - a3 == finite_signed_cardinality(2));
  // 3 - 5 = -2 (the construction's defining clause: the operator
  // widens to the larger carrier where the result lives).
  CHECK(a3 - a5 == finite_signed_cardinality(-2));
  // 7 - 7 = 0 (the equivalence-class neutral element).
  CHECK(a7 - a7 == finite_signed_cardinality(0));
  // 0 - 5 = -5.
  CHECK(finite_cardinality(0) - a5 == finite_signed_cardinality(-5));
}

TEST_CASE(
    "grothendieck: equivalence classes [(a, b)] ~ [(c, d)] iff a + d = b + c",
    "[algebra][grothendieck][universal-property][equivalence]") {
  // Concrete witnesses of the Grothendieck quotient relation:
  // (a, b) ~ (c, d) iff a + d = b + c, both representing a - b.
  // The closure-forcing operator should yield equal results for
  // equivalent input pairs.
  // [(5, 3)] = [(7, 5)] = [(2, 0)] all represent 2.
  auto const v1 = finite_cardinality(5) - finite_cardinality(3);
  auto const v2 = finite_cardinality(7) - finite_cardinality(5);
  auto const v3 = finite_cardinality(2) - finite_cardinality(0);
  CHECK(v1 == v2);
  CHECK(v1 == v3);
  CHECK(v1 == finite_signed_cardinality(2));
  // Negative side: [(0, 4)] = [(2, 6)] = [(7, 11)] all represent -4.
  auto const n1 = finite_cardinality(0) - finite_cardinality(4);
  auto const n2 = finite_cardinality(2) - finite_cardinality(6);
  auto const n3 = finite_cardinality(7) - finite_cardinality(11);
  CHECK(n1 == n2);
  CHECK(n1 == n3);
  CHECK(n1 == finite_signed_cardinality(-4));
}

TEST_CASE(
    "grothendieck: closure-forcing operator produces a SignedCardinality "
    "that is structurally an abelian group (witnesses the codomain widening)",
    "[algebra][grothendieck][carrier-lattice]") {
  // The construction's load-bearing claim: the codomain admits
  // additive inverses, which the source @c Cardinality (a
  // saturating commutative monoid) does not.  Verify the inverse
  // structure on the lifted side.
  auto const z = finite_cardinality(5) - finite_cardinality(3);  // = 2
  auto const neg_z = -z;                                         // = -2
  CHECK((z + neg_z) == finite_signed_cardinality(0));
  CHECK(neg_z == finite_signed_cardinality(-2));
}

// ===========================================================================
// (4) The dual reification holds: same carrier inhabits both Forms
// ===========================================================================

TEST_CASE(
    "initial_ring: SignedCardinality inhabits BOTH IsInitialRing AND "
    "IsGrothendieckGroup — formal type-checked documentation of the dual "
    "universal-property reading",
    "[algebra][initial_ring][grothendieck][duality]") {
  // The load-bearing structural claim of #446: the same carrier
  // simultaneously witnesses two distinct universal properties.
  STATIC_CHECK(IsInitialRing<SignedCardinality>);
  STATIC_CHECK(IsGrothendieckGroup<SignedCardinality, Cardinality>);

  // The two readings agree up to canonical isomorphism on the
  // operational level: a value constructed via the closure-forcing
  // operator (Grothendieck side) equals the same value constructed
  // directly (initial-ring side).
  auto const via_grothendieck = finite_cardinality(7) - finite_cardinality(0);
  auto const via_initial_ring = finite_signed_cardinality(7);
  CHECK(via_grothendieck == via_initial_ring);
  // χ_{Modular<5>} of either yields the same residue.
  CHECK(χ_to_modular<5>(via_grothendieck).value ==
        χ_to_modular<5>(via_initial_ring).value);
}

// ===========================================================================
// (5) Carrier-lattice arrows are mechanically witnessed as monic
// ===========================================================================
//
// Figure 1 in @c paper.tex / @c report.tex labels seven arrows.
// The static_asserts below pin each one as @c IsMonicArrow at the
// concept level — the figure's claim that they are canonical
// embeddings registered as monic morphisms is not schematic prose
// but a type-checked fact.  The dashed retraction @c abs (also
// shown in the figure) is intentionally @b not asserted monic
// here; it is a split-mono partner of @c lift_ℕ_ℤ_ (i.e.,
// @c abs @c ∘ @c embed @c = @c id on the non-negative fragment),
// which is a different categorical concept.

TEST_CASE(
    "carrier-lattice: every figure-1 arrow is a structural arrow and a "
    "type-checked monic morphism",
    "[carrier-lattice][figure][monic][witness]") {
  // Top-row variant-layer arrow: ℕ ↪ ℤ canonical embedding.
  STATIC_CHECK(IsArrow<std::decay_t<decltype(lift_ℕ_ℤ_)>>);
  STATIC_CHECK(IsMonicArrow<std::decay_t<decltype(lift_ℕ_ℤ_)>>);
  // Operational witness: lift_ℕ_ℤ_ realises the canonical embedding
  // — applied to a Cardinality, returns the corresponding non-
  // negative SignedCardinality.  Covers the arrow's lambda body.
  CHECK(lift_ℕ_ℤ_(finite_cardinality(7)) == finite_signed_cardinality(7));
  CHECK(lift_ℕ_ℤ_(finite_cardinality(0)) == finite_signed_cardinality(0));
  // Middle-row vertical arrows (machine → variant lifts).
  STATIC_CHECK(IsArrow<std::decay_t<decltype(embed_unsigned_Cardinality_)>>);
  STATIC_CHECK(
      IsMonicArrow<std::decay_t<decltype(embed_unsigned_Cardinality_)>>);
  STATIC_CHECK(IsArrow<std::decay_t<decltype(embed_int_SignedCardinality_)>>);
  STATIC_CHECK(
      IsMonicArrow<std::decay_t<decltype(embed_int_SignedCardinality_)>>);
  // Middle-row horizontal arrow: machine-layer ℕ → ℤ sign reinterpretation.
  STATIC_CHECK(IsArrow<std::decay_t<decltype(embed_ℕ_ℤ)>>);
  STATIC_CHECK(IsMonicArrow<std::decay_t<decltype(embed_ℕ_ℤ)>>);
  // Bottom-row vertical arrow: 𝔹 ↪ ℕ.
  STATIC_CHECK(IsArrow<std::decay_t<decltype(embed_𝔹_ℕ)>>);
  STATIC_CHECK(IsMonicArrow<std::decay_t<decltype(embed_𝔹_ℕ)>>);
  // Bottom-row horizontal arrow: 𝔹 ↪ 𝕂3.
  STATIC_CHECK(IsArrow<std::decay_t<decltype(embed_𝔹_𝕂3)>>);
  STATIC_CHECK(IsMonicArrow<std::decay_t<decltype(embed_𝔹_𝕂3)>>);
  // Bottom-to-top diagonal arrow: 𝕂3 ↪ ℤ (skips the machine row).
  STATIC_CHECK(IsArrow<std::decay_t<decltype(embed_K3_ℤ)>>);
  STATIC_CHECK(IsMonicArrow<std::decay_t<decltype(embed_K3_ℤ)>>);
}

// ===========================================================================
// (6) embed_𝔹_𝕂3 operational behaviour (covers the lambda body for
//     codecov; this PR's only new arrow at the carrier-lattice layer)
// ===========================================================================

TEST_CASE(
    "carrier-lattice: embed_𝔹_𝕂3 maps true to Ternary::True and false to "
    "Ternary::False (canonical 2-valued ↪ 3-valued Kleene inclusion)",
    "[carrier-lattice][boolean][kleene][embed]") {
  CHECK(embed_𝔹_𝕂3(true) == Ternary::True);
  CHECK(embed_𝔹_𝕂3(false) == Ternary::False);
  // Ternary::Unknown is by construction NOT in the image of the
  // canonical embedding — it represents the third truth-value that
  // 𝔹 lacks.  This negative fact is the structural reason the
  // arrow is monic but not surjective.
  CHECK(embed_𝔹_𝕂3(true) != Ternary::Unknown);
  CHECK(embed_𝔹_𝕂3(false) != Ternary::Unknown);
}

// ===========================================================================
// (7) Carrier-lattice lift unification (#455): existential-proof
//     dispatch on the central (Cardinality, SignedCardinality) pair.
// ===========================================================================

TEST_CASE(
    "carrier-lattice lift dispatch: lift<Cardinality, SignedCardinality>(c) "
    "agrees with the canonical bespoke lift_ℕ_ℤ_(c) — discoverability alias "
    "without categorical overclaim",
    "[carrier-lattice][lift-unification][dispatch]") {
  // The unified lift<From, To> trait dispatches to the canonical
  // bespoke arrow registered for the (From, To) pair.  For the
  // central variant-layer ℕ → ℤ pair, the dispatch resolves to
  // lift_ℕ_ℤ_; the result must agree on every input.
  for (const std::size_t v : {std::size_t{0}, std::size_t{1}, std::size_t{42},
                              std::size_t{1000}, std::size_t{2147483648u}}) {
    const auto via_alias =
        lift<Cardinality, SignedCardinality>(finite_cardinality(v));
    const auto via_bespoke = lift_ℕ_ℤ_(finite_cardinality(v));
    CHECK(via_alias == via_bespoke);
  }
  // The transfinite case: lift_ℕ_ℤ_ promotes ℵ_0 to PositiveInfinity;
  // the alias dispatch agrees.
  const auto inf = Cardinality{ℵ_0{}};
  CHECK(lift<Cardinality, SignedCardinality>(inf) == lift_ℕ_ℤ_(inf));
}
