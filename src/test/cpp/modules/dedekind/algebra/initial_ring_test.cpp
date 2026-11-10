/**
 * @file initial_ring_test.cpp
 * @brief Test suite for the @c algebra:initial_ring partition.
 *
 * @section Scope
 * Exercises that no machine word and no saturating proxy claims ℤ's
 * universal properties (a word wraps, ±ℵ_0 are no integers); ℤ's own
 * witnesses are pinned at the definition site, @c algebra/initial_ring.cppm.
 *
 *   3. Carrier-lattice arrows (Figure 1 in @c paper.tex /
 *      @c report.tex) — the labelled arrows are mechanically
 *      witnessed as @c IsMonicArrow / @c IsArrow concepts so the
 *      figure's claims are not schematic prose but type-checked
 *      facts.
 */
#include <catch2/catch_test_macros.hpp>
#include <type_traits>

import dedekind.algebra;
import dedekind.category;
import dedekind.sets;

using namespace dedekind::algebra;
using namespace dedekind::category;
using namespace dedekind::sets;
using dedekind::algebra::is_grothendieck_group_v;
using dedekind::algebra::is_initial_ring_v;

// ===========================================================================
// (1) Concept fires on the canonical witnesses
// ===========================================================================

TEST_CASE("initial_ring: the saturating proxy is no initial ring",
          "[algebra][initial_ring][witness]") {
  // ℤ's witnesses are pinned at the definition site, algebra/initial_ring.cppm.
  // The saturating proxy is neither: ±ℵ_0 are no integers.
  STATIC_CHECK(!is_initial_ring_v<SignedCardinality>);
  STATIC_CHECK(!(is_grothendieck_group_v<SignedCardinality, Numeral>));
}

TEST_CASE(
    "initial_ring: opt-in trait defaults to false (no initial-ring witness "
    "without explicit registration)",
    "[algebra][initial_ring][negative]") {
  // Default: not initial.  Carriers must opt in explicitly.
  STATIC_CHECK(!is_initial_ring_v<int>);
  STATIC_CHECK(!is_initial_ring_v<unsigned int>);
  STATIC_CHECK(!is_initial_ring_v<Numeral>);
  STATIC_CHECK((!is_grothendieck_group_v<unsigned int, Numeral>));
  STATIC_CHECK((!is_grothendieck_group_v<Numeral, Numeral>));
}

TEST_CASE(
    "carrier-lattice: embed_𝔹_𝕂3_ maps true to Ternary::True and false to "
    "Ternary::False (canonical 2-valued ↪ 3-valued Kleene inclusion)",
    "[carrier-lattice][boolean][kleene][embed]") {
  // Runtime invocation of the arrow's lambda body — codecov-load-bearing.
  // The compile-time witnesses live in @c :algebra:boolean; this exercises
  // the same equalities at runtime so the lambda body shows up as covered.
  CHECK(embed_𝔹_𝕂3_(true) == Ternary::True);
  CHECK(embed_𝔹_𝕂3_(false) == Ternary::False);
  // Ternary::Unknown is by construction NOT in the image of the
  // canonical embedding — it represents the third truth-value that
  // 𝔹 lacks.  This negative fact is the structural reason the arrow
  // is monic but not surjective.
  CHECK(embed_𝔹_𝕂3_(true) != Ternary::Unknown);
  CHECK(embed_𝔹_𝕂3_(false) != Ternary::Unknown);
}

TEST_CASE(
    "carrier-lattice: embed_𝔹_𝕂3 (set-level lift) on Singleton<true> lands at "
    "Singleton<Ternary::True> in 𝕂3",
    "[carrier-lattice][boolean][kleene][embed][image]") {
  // The per-shape dispatch (which sets::image overload to use) is
  // resolved at compile time via template overload resolution; this
  // test invokes the resulting code at runtime so the lambda /
  // forwarding-reference body show up as covered for codecov.
  // Sister of PR #624's embed_𝔹_ℕ test; same pattern, different
  // codomain.
  constexpr Singleton<bool, Boole> s_true{true};
  const auto image_set = embed_𝔹_𝕂3(s_true);
  CHECK(origin(image_set) == Ternary::True);

  constexpr Singleton<bool, Boole> s_false{false};
  const auto image_set_false = embed_𝔹_𝕂3(s_false);
  CHECK(origin(image_set_false) == Ternary::False);
}
