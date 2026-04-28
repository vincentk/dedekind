/**
 * @file uint_test.cpp
 * @brief Dedicated test suite for the @c numbers:uint partition.
 *
 * @section Scope
 * Exercises the @c std::unsigned_integral family classification shipped
 * by @c numbers:uint (closes part of #417): the textbook @c ℤ/2^wℤ
 * stance, the universal machine→variant lift @c
 * embed_unsigned_to_Cardinality, the @c Modular<N> / @c IsCyclic
 * correspondence, and the width-ladder ring-hom witnesses.  The
 * compile-time witnesses live inside the partition itself; this file
 * exercises the @b runtime / @b composition surface.
 */
#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <limits>
#include <type_traits>

import dedekind.category;
import dedekind.morphologies;
import dedekind.numbers;
import dedekind.sets;

using namespace dedekind::category;
using namespace dedekind::numbers;
using namespace dedekind::sets;
using dedekind::morphologies::IsCyclic;
using dedekind::morphologies::Modular;

// ===========================================================================
// (1) The universal lift arrow: std::unsigned_integral → Cardinality
// ===========================================================================

TEST_CASE("uint: embed_unsigned_to_Cardinality across canonical widths",
          "[numbers][uint][lift][carrier-lattice]") {
  // Function-template form covers any std::unsigned_integral width.
  CHECK(embed_unsigned_to_Cardinality(0u) == finite_cardinality(0));
  CHECK(embed_unsigned_to_Cardinality(1u) == finite_cardinality(1));
  CHECK(embed_unsigned_to_Cardinality(42u) == finite_cardinality(42));
  CHECK(embed_unsigned_to_Cardinality(static_cast<unsigned long>(1000)) ==
        finite_cardinality(1000));
  CHECK(embed_unsigned_to_Cardinality(static_cast<std::size_t>(7)) ==
        finite_cardinality(7));
  CHECK(embed_unsigned_to_Cardinality(static_cast<unsigned char>(255)) ==
        finite_cardinality(255));
  CHECK(embed_unsigned_to_Cardinality(static_cast<unsigned short>(0xFFFF)) ==
        finite_cardinality(0xFFFF));
}

TEST_CASE("uint: embed_unsigned_Cardinality_ named-arrow form",
          "[numbers][uint][lift][monic-arrow]") {
  STATIC_CHECK(IsArrow<std::decay_t<decltype(embed_unsigned_Cardinality_)>>);
  STATIC_CHECK(
      IsMonicArrow<std::decay_t<decltype(embed_unsigned_Cardinality_)>>);
  STATIC_CHECK(
      std::same_as<Dom<std::decay_t<decltype(embed_unsigned_Cardinality_)>>,
                   unsigned>);
  STATIC_CHECK(
      std::same_as<Cod<std::decay_t<decltype(embed_unsigned_Cardinality_)>>,
                   Cardinality>);

  CHECK(embed_unsigned_Cardinality_(0u) == finite_cardinality(0));
  CHECK(embed_unsigned_Cardinality_(42u) == finite_cardinality(42));
  CHECK(embed_unsigned_Cardinality_(1000u) == finite_cardinality(1000));
}

TEST_CASE("uint: lift is injective on the finite fragment",
          "[numbers][uint][lift][injective]") {
  // Distinct unsigned values yield distinct Cardinality values
  // (witnesses the monic claim).
  CHECK(embed_unsigned_to_Cardinality(0u) != embed_unsigned_to_Cardinality(1u));
  CHECK(embed_unsigned_to_Cardinality(41u) !=
        embed_unsigned_to_Cardinality(42u));
  CHECK(embed_unsigned_to_Cardinality(0xFFFFu) !=
        embed_unsigned_to_Cardinality(0u));
}

TEST_CASE("uint: lift forgets the modular ring structure",
          "[numbers][uint][lift][forgetful]") {
  // The source has additive inverses via mod wrap; the codomain does
  // not.  The arrow is structure-forgetting (not a ring homomorphism).
  // Exercised here by verifying the source's mod-wrap behaviour
  // (UINT_MAX + 1 == 0) does NOT carry over to the lifted values.
  const auto u_max = std::numeric_limits<unsigned int>::max();
  const auto lifted_u_max = embed_unsigned_to_Cardinality(u_max);
  const auto lifted_zero = embed_unsigned_to_Cardinality(0u);
  // Source-side: u_max + 1 wraps to 0.  Lifted side: u_max ≠ 0.
  CHECK(static_cast<unsigned int>(u_max + 1u) == 0u);  // mod wrap on source
  CHECK(lifted_u_max != lifted_zero);                  // no wrap on target
}

// ===========================================================================
// (2) Modular<N> / IsCyclic correspondence
// ===========================================================================

TEST_CASE(
    "uint: std::unsigned_integral is axiomatically cyclic but lacks "
    "structural IsCyclic API",
    "[numbers][uint][cyclic][modular]") {
  // unsigned int IS axiomatically cyclic under +.
  STATIC_CHECK(IsCyclicGroup<unsigned int, std::plus<unsigned int>>);

  // unsigned int does NOT satisfy structural IsCyclic (no
  // Domain/generator/successor member API).
  STATIC_CHECK(!IsCyclic<unsigned int>);
  STATIC_CHECK(!IsCyclic<unsigned long>);
  STATIC_CHECK(!IsCyclic<std::size_t>);
}

TEST_CASE("uint: Modular<N> as the structural cyclic-ring sibling",
          "[numbers][uint][modular][cyclic]") {
  // Modular<N> ships the Domain/generator/successor surface that
  // primitives lack — it satisfies BOTH the structural IsCyclic and
  // the axiomatic IsCyclicGroup.
  STATIC_CHECK(IsCyclic<Modular<256>>);
  STATIC_CHECK(IsCyclicGroup<Modular<256>, std::plus<Modular<256>>>);

  // Spot-check the runtime semantics: Modular<N>'s + reduces mod N,
  // matching std::unsigned_integral's mod-wrap behaviour (the
  // semantic isomorphism the partition documents).
  Modular<256> a{255};
  Modular<256> b{1};
  const auto sum = a + b;
  CHECK(sum.value == 0);  // 255 + 1 ≡ 0 (mod 256)
}

// ===========================================================================
// (3) Width-ladder ring-homomorphism (composition with the lift)
// ===========================================================================

TEST_CASE(
    "uint: width-ladder + lift compose into a Cardinality value (no overflow "
    "below the ladder)",
    "[numbers][uint][width-ladder][carrier-lattice]") {
  // unsigned char ↪ unsigned short ↪ unsigned int ↪ Cardinality.
  // On the non-overflow fragment, all four values represent the same
  // textbook integer; the lift lands them as identical Cardinality
  // values.
  const unsigned char uc = 42;
  const unsigned short us = 42;
  const unsigned int ui = 42;
  const std::size_t uz = 42;

  CHECK(embed_unsigned_to_Cardinality(uc) == embed_unsigned_to_Cardinality(ui));
  CHECK(embed_unsigned_to_Cardinality(us) == embed_unsigned_to_Cardinality(ui));
  CHECK(embed_unsigned_to_Cardinality(uz) == embed_unsigned_to_Cardinality(ui));
  CHECK(embed_unsigned_to_Cardinality(ui) == finite_cardinality(42));
}

TEST_CASE("uint: lift composes with the bool→unsigned arrow (𝔹 → uint → ℕ)",
          "[numbers][uint][lift][embedding-chain]") {
  // The composition embed_𝔹_ℕ >> embed_unsigned_Cardinality_ realises
  // the embedding chain 𝔹 → std::unsigned_integral → ℕ documented in
  // the carrier-lattice section of paper.tex / report.tex.
  CHECK(embed_unsigned_Cardinality_(embed_𝔹_ℕ(false)) == finite_cardinality(0));
  CHECK(embed_unsigned_Cardinality_(embed_𝔹_ℕ(true)) == finite_cardinality(1));
}
