/**
 * @file z_scout_algebra_test.cpp
 * @brief End-to-end witnesses tying the canonical @c ℤ alias to the
 *        in-line scout-algebra comprehension surface (#664 + #670).
 *
 * @section z_scout_algebra_test__Scope
 *
 * Validates that the project's canonical @c ℤ alias --- now retargeted
 * to the saturating @c sets::SignedCardinality carrier under #670 ---
 * ties cleanly into the in-line scout-algebra surface (#664) that
 * requires @c IsOrderedAdditiveGroup<Z> for halfspace-pivot transport.
 *
 * This is the test the user asked for in the #668 review: "tie straight
 * to ℤ in numbers."  Post-#670, the binding is direct --- no companion
 * alias needed.
 */
#include <catch2/catch_test_macros.hpp>
#include <compare>
#include <concepts>
#include <type_traits>

import dedekind.algebra;
import dedekind.category;
import dedekind.numbers;
import dedekind.order;
import dedekind.sets;

using namespace dedekind::numbers;

// ---------------------------------------------------------------------------
// ℤ is well-formed and ties cleanly to SignedCardinality (#670).
// ---------------------------------------------------------------------------

static_assert(std::same_as<typename std::remove_cvref_t<decltype(ℤ)>::Domain,
                           dedekind::sets::SignedCardinality>,
              "ℤ's underlying carrier IS SignedCardinality, mirroring ℕ's "
              "carrier being Cardinality (post-#670).");

// ---------------------------------------------------------------------------
// ℤ's carrier inhabits the standard concepts (closes #669 + #670).
// ---------------------------------------------------------------------------

static_assert(std::regular<dedekind::sets::SignedCardinality>,
              "SignedCardinality must be std::regular for ℤ to flow "
              "cleanly through downstream concept-binding (closes #669).");
static_assert(std::three_way_comparable<dedekind::sets::SignedCardinality,
                                        std::partial_ordering>,
              "SignedCardinality is partially ordered (NaZ is unordered with "
              "non-NaZ); the standard concept fires post-#669.");

// ---------------------------------------------------------------------------
// ℤ ties to the in-line scout-algebra surface (#664).
//
// The structural binding: SignedCardinality satisfies
// IsOrderedAdditiveGroup (specialised in :algebra:scout_algebra), so a
// comprehension like Set{in<ℤ> + bound<3> | (in<ℤ> > bound<5>)}
// participates in the halfspace-pivot transport pipe.
// ---------------------------------------------------------------------------

TEST_CASE(
    "ℤ: in-line scout-algebra transport fires on the saturating ℤ "
    "(#670 + #664)",
    "[numbers][integer][scout_algebra][slice2]") {
  constexpr auto x = dedekind::sets::element<ℤ>;
  constexpr auto S = dedekind::sets::Set{x + dedekind::order::bound<3> |
                                         (x > dedekind::order::bound<5>)};

  // The result IS a Set whose typed predicate is the SHIFTED halfspace:
  // pivot 5 + Element 3 = 8, direction (Upward) and strictness (Strict)
  // preserved.  This is type-directed collapse at compile time on the
  // project's saturating ℤ proxy --- the discipline-consistent canonical
  // ℤ post-#670.
  using SetL = typename dedekind::sets::NaturalLogic<
      std::remove_cvref_t<decltype(ℤ)>>::type;
  using ExpectedPredicate =
      dedekind::order::Halfspace<dedekind::sets::SignedCardinality, 8,
                                 dedekind::order::Direction::Upward,
                                 dedekind::order::Strictness::Strict>;
  using ExpectedSet = dedekind::sets::Set<dedekind::sets::SignedCardinality,
                                          SetL, ExpectedPredicate>;
  using ResultType = std::remove_cvref_t<decltype(S)>;
  STATIC_CHECK(std::same_as<ResultType, ExpectedSet>);
}

// ---------------------------------------------------------------------------
// Ring-retract multiplicative scaling on ℤ (#664 Slice 5).
//
// ℤ is the initial ring, NOT a field --- non-units lack multiplicative
// inverses.  The map x ↦ M*x is therefore a retract, not an iso: only
// multiples of M land in the image.  The scout-algebra ring-retract
// pipe produces an AffineImageOfHalfspace predicate that checks both
// divisibility AND the source halfspace at the preimage y/M.
//
// The result is type-directed: writing `Set{x * bound<2> | x > bound<5>}`
// on ℤ produces a Set whose predicate is
// `AffineImageOfHalfspace<ℤ, 2, source_halfspace>`, NOT a plain Halfspace.
// ---------------------------------------------------------------------------

TEST_CASE(
    "ℤ: ring-retract scaling produces AffineImageOfHalfspace (#664 Slice 5)",
    "[numbers][integer][scout_algebra][slice5]") {
  constexpr auto x = dedekind::sets::element<ℤ>;
  // {n ∈ ℤ | n > 5} pushed through λn. 2*n → {2n | n > 5} = {12, 14, ...}.
  constexpr auto S = dedekind::sets::Set{x * dedekind::order::bound<2> |
                                         (x > dedekind::order::bound<5>)};

  using SetL = typename dedekind::sets::NaturalLogic<
      std::remove_cvref_t<decltype(ℤ)>>::type;
  using SourceHalfspace =
      dedekind::order::Halfspace<dedekind::sets::SignedCardinality, 5,
                                 dedekind::order::Direction::Upward,
                                 dedekind::order::Strictness::Strict>;
  using ExpectedPredicate = dedekind::algebra::AffineImageOfHalfspace<
      dedekind::sets::SignedCardinality, 2, SourceHalfspace>;
  using ExpectedSet = dedekind::sets::Set<dedekind::sets::SignedCardinality,
                                          SetL, ExpectedPredicate>;
  STATIC_CHECK(std::same_as<std::remove_cvref_t<decltype(S)>, ExpectedSet>);

  // Membership behaviour: S = {12, 14, 16, ...}.
  using L = SetL;
  CHECK(S(dedekind::sets::finite_signed_cardinality(12)) == L::True);
  CHECK(S(dedekind::sets::finite_signed_cardinality(14)) == L::True);
  CHECK(S(dedekind::sets::finite_signed_cardinality(13)) == L::False);
  CHECK(S(dedekind::sets::finite_signed_cardinality(10)) == L::False);
  CHECK(S(dedekind::sets::finite_signed_cardinality(11)) == L::False);

  // Sentinel behaviour: ℤ = Ω<SignedCardinality> includes ±ℵ_0 and
  // NaZ.  The scaling map x ↦ M*x (with finite non-zero M) maps
  //   +ℵ_0 ↦ +ℵ_0,  -ℵ_0 ↦ -ℵ_0,  NaZ ↦ NaZ
  // per @c SignedCardinality::operator*.  The predicate uses the
  // identity-based divisibility test @c M * (y/M) == y rather than
  // the modular @c y % M == 0 form precisely so the sentinels
  // propagate correctly --- the identity holds at sentinels.
  // Source halfspace `{n | n > 5}` lifts the membership question
  // to the source on each sentinel: +ℵ_0 > 5 is True (in image);
  // -ℵ_0 > 5 is False (not in image); NaZ > 5 is unordered, so
  // Halfspace's < check fails into False.
  constexpr auto pos_inf =
      dedekind::sets::SignedCardinality{dedekind::sets::PositiveInfinity{}};
  constexpr auto neg_inf =
      dedekind::sets::SignedCardinality{dedekind::sets::NegativeInfinity{}};
  constexpr auto naz = dedekind::sets::SignedCardinality{dedekind::sets::NaZ{}};
  CHECK(S(pos_inf) == L::True);
  CHECK(S(neg_inf) == L::False);
  CHECK(S(naz) == L::False);
}
