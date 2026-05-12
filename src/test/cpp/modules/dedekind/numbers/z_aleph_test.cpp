/**
 * @file z_aleph_test.cpp
 * @brief End-to-end witnesses for @c ℤ_aleph (#670) --- the saturating
 *        ℤ companion alias whose carrier is @c sets::SignedCardinality
 *        (mirroring @c ℕ's @c Cardinality discipline).
 *
 * @section z_aleph_test__Scope
 *
 * Validates that the new saturating-discipline ℤ proxy
 * (@c numbers::ℤ_aleph @c = @c Ω<SignedCardinality>) ties cleanly into
 * downstream consumers --- specifically, the in-line scout-algebra
 * comprehension surface (#664) that requires
 * @c IsOrderedAdditiveGroup<Z> for halfspace-pivot transport.  This is
 * the test the user asked for: "tie straight to ℤ in numbers."
 *
 * The literal @c ℤ alias (cyclic @c SignedExtensionalCardinal<>) is
 * separately tested in @c integer_test.cpp and is intentionally left
 * unchanged by #670 to avoid the ~400-reference cascade; the new
 * @c ℤ_aleph alias is the discipline-consistent companion.
 */
#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <type_traits>

import dedekind.algebra;
import dedekind.category;
import dedekind.numbers;
import dedekind.order;
import dedekind.sets;

using namespace dedekind::numbers;

// ---------------------------------------------------------------------------
// ℤ_aleph is well-formed and ties cleanly to SignedCardinality.
// ---------------------------------------------------------------------------

static_assert(
    std::same_as<typename std::remove_cvref_t<decltype(ℤ_aleph)>::Domain,
                 dedekind::sets::SignedCardinality>,
    "ℤ_aleph's underlying carrier IS SignedCardinality, mirroring ℕ's "
    "carrier being Cardinality.");

// ---------------------------------------------------------------------------
// ℤ_aleph's carrier inhabits the standard concepts (closes #669 + #670).
// ---------------------------------------------------------------------------

static_assert(std::regular<dedekind::sets::SignedCardinality>,
              "SignedCardinality must be std::regular for ℤ_aleph to flow "
              "cleanly through downstream concept-binding (closes #669).");
static_assert(std::three_way_comparable<dedekind::sets::SignedCardinality,
                                        std::partial_ordering>,
              "SignedCardinality is partially ordered (NaZ is unordered with "
              "non-NaZ); the standard concept fires post-#669.");

// ---------------------------------------------------------------------------
// ℤ_aleph ties to the in-line scout-algebra surface (#664).
//
// The structural binding: SignedCardinality satisfies
// IsOrderedAdditiveGroup (specialised in :algebra:scout_algebra), so a
// comprehension like Set{in<ℤ_aleph> + bound<3> | (in<ℤ_aleph> > bound<5>)}
// participates in the halfspace-pivot transport pipe.  This is the test
// the user asked for: "tie straight to ℤ in numbers."
// ---------------------------------------------------------------------------

TEST_CASE(
    "ℤ_aleph: in-line scout-algebra transport fires on the saturating ℤ "
    "(#670 + #664)",
    "[numbers][integer][scout_algebra][slice2]") {
  constexpr auto x = dedekind::sets::element<ℤ_aleph>;
  constexpr auto S = dedekind::sets::Set{x + dedekind::order::bound<3> |
                                         (x > dedekind::order::bound<5>)};

  // The result IS a Set whose typed predicate is the SHIFTED halfspace:
  // pivot 5 + Element 3 = 8, direction (Upward) and strictness (Strict)
  // preserved.  This is type-directed collapse at compile time on the
  // project's saturating ℤ proxy --- the discipline-consistent companion
  // to ℕ.
  using SetL = typename dedekind::sets::NaturalLogic<
      std::remove_cvref_t<decltype(ℤ_aleph)>>::type;
  using ExpectedPredicate =
      dedekind::order::Halfspace<dedekind::sets::SignedCardinality, 8,
                                 dedekind::order::Direction::Upward,
                                 dedekind::order::Strictness::Strict>;
  using ExpectedSet = dedekind::sets::Set<dedekind::sets::SignedCardinality,
                                          SetL, ExpectedPredicate>;
  using ResultType = std::remove_cvref_t<decltype(S)>;
  STATIC_CHECK(std::same_as<ResultType, ExpectedSet>);
}
