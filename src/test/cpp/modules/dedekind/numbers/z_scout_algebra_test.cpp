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
