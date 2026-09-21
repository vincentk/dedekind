/** @file dedekind/sets/undecidable_reduction_test.cpp
 *
 * The decidability-reduction line (#894): can the lattice reducer collapse an
 * UNDECIDABLE operand to a DECIDABLE result, reusing the meccano verbatim?
 *
 * This first slice pins the two atoms it is built from:
 *   - @c UnknownPredicate, the archetypal undecidable predicate (answers
 *     @c Unknown everywhere), the Kleene-interior companion of the always-true
 *     @c UniversalPredicate and always-false @c EmptyPredicate; and
 *   - @c is_decided, the value-level Rosolini decided-core test Σ ⊔ ¬Σ
 *     (@c :logic), which recognises exactly the two-valued endpoints.
 *
 * The lattice-collapse witnesses (a meet with the bottom @c Ø, a join with the
 * top @c 𝔸, annihilating the undecidable operand) build on these and land in a
 * follow-up once the reducer path is confirmed.
 */

#include <catch2/catch_test_macros.hpp>

import dedekind.category;
import dedekind.sets;

using namespace dedekind::category;
using namespace dedekind::sets;

TEST_CASE("is_decided detects the two-valued core Σ ⊔ ¬Σ of Ω",
          "[category][logic][decidable][rosolini]") {
  // Classical: Σ = Ω, so every answer is decided.
  STATIC_CHECK(is_decided<ClassicalLogic>(true));
  STATIC_CHECK(is_decided<ClassicalLogic>(false));

  // Kleene K₃: the two endpoints are decided; the interior Unknown is not.
  STATIC_CHECK(is_decided<TernaryLogic>(Ternary::True));
  STATIC_CHECK(is_decided<TernaryLogic>(Ternary::False));
  STATIC_CHECK_FALSE(is_decided<TernaryLogic>(Ternary::Unknown));
}

TEST_CASE("UnknownPredicate is the archetypal undecidable predicate",
          "[sets][decidable][rosolini][undecidable]") {
  constexpr UnknownPredicate<int> U{};

  // A bona fide characteristic map χ: T → Ω, not an ad-hoc callable.
  STATIC_CHECK(IsCharacteristic<UnknownPredicate<int>>);

  // Answers Unknown everywhere: the Kleene interior, never a decided bound.
  STATIC_CHECK(U(0) == Ternary::Unknown);
  STATIC_CHECK(U(42) == Ternary::Unknown);
  STATIC_CHECK(U(-7) == Ternary::Unknown);

  // Hence it sits strictly outside the decided core the reducer needs.
  STATIC_CHECK_FALSE(is_decided<TernaryLogic>(U(0)));
  STATIC_CHECK_FALSE(is_decided<TernaryLogic>(U(42)));
}
