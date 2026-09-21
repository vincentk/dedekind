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
#include <concepts>
#include <type_traits>

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

TEST_CASE("Reduction restores decidability: U ∧ Ø → Ø, decided (#894)",
          "[sets][decidable][rosolini][reduction]") {
  // A Kleene ambient. U is the maximally-undecidable set (χ ≡ Unknown); the
  // empty boundary and the universe are declared conservatively in that
  // ambient.
  constexpr Set<int, TernaryLogic, UnknownPredicate<int>> U{
      UnknownPredicate<int>{}};
  constexpr Ø<int, TernaryLogic> E{};
  constexpr UniversalSet<int, TernaryLogic> A{};

  // As DECLARED, none of the three is decidable (Kleene codomain tag).
  STATIC_CHECK_FALSE(HasDecidableMembership<decltype(U)>);
  STATIC_CHECK_FALSE(HasDecidableMembership<decltype(E)>);
  STATIC_CHECK_FALSE(HasDecidableMembership<decltype(A)>);

  // Domain leg: the ⊥ boundary annihilates U (χ_U is never evaluated).
  // Codomain leg (#894): the surviving boundary factors through Σ, so it is
  // re-tagged to the Boolean codomain. The undecidable operand is gone AND the
  // result reads decidable.
  constexpr auto meet = U & E;
  STATIC_CHECK(
      std::same_as<std::decay_t<decltype(meet)>, Ø<int, ClassicalLogic>>);
  STATIC_CHECK(HasDecidableMembership<decltype(meet)>);

  // Dual: the ⊤ boundary annihilates U in the join.
  constexpr auto join = U | A;
  STATIC_CHECK(HasDecidableMembership<decltype(join)>);
}

TEST_CASE("Cross-species combine: Boole ∩ Kleene lifts into the reducer (#894)",
          "[sets][decidable][rosolini][mixed]") {
  // Same carrier, different codomains: A over Boole, B over Kleene.
  constexpr Set<int, ClassicalLogic, UniversalPredicate<int>> A{
      UniversalPredicate<int>{}};
  constexpr Set<int, TernaryLogic, UnknownPredicate<int>> B{
      UnknownPredicate<int>{}};
  constexpr Ø<int, TernaryLogic> E{};

  // The mixed-species meet now type-checks; the same-species gate rejected it
  // before.  Operands lift to the joined codomain K₃, then fold (membership is
  // pointwise-correct; the structural collapse is the follow-up).
  constexpr auto mixed = A & B;
  STATIC_CHECK_FALSE(HasDecidableMembership<decltype(mixed)>);
  STATIC_CHECK(mixed(7) == Ternary::Unknown);  // lift(⊤) ∧ Unknown = Unknown

  // A boundary annihilates across the species join, and the survivor carries
  // the decided Boolean codomain.
  constexpr auto collapsed = A & E;
  STATIC_CHECK(HasDecidableMembership<decltype(collapsed)>);

  // Runtime exercise so the lift / cross-species overload / membership are
  // covered (the STATIC_CHECKs above run at compile time, invisible to
  // Codecov).
  const auto mixed_rt = A & B;
  CHECK(mixed_rt(7) == Ternary::Unknown);  // lift(⊤) ∧ Unknown = Unknown
  const auto collapsed_rt = A & E;
  CHECK_FALSE(collapsed_rt(3));  // Ø membership is false

  // The cross-species JOIN overload too: ⊤ ∨ Unknown = True.
  const auto joined_rt = A | B;
  CHECK(joined_rt(7) == Ternary::True);
}

TEST_CASE("Codomain leg on complement / product / symmetric difference (#894)",
          "[sets][decidable][rosolini][reduction]") {
  constexpr Set<int, ClassicalLogic, UniversalPredicate<int>> A{
      UniversalPredicate<int>{}};

  // Runtime calls (not static_assert) so the boundary-operator bodies are
  // exercised for coverage; each lands on a decided boundary.  (Complement of a
  // boundary, !Ø / !𝔸, is NOT covered here: `!Ø` resolves to the greedy free
  // operator! -- a Morphism -- not the boundary member, so the codomain leg
  // does not reach it; tracked in FIXME(#894).)
  const auto empty_prod = Ø<int, TernaryLogic>{} * A;  // Ø × S = Ø
  CHECK(HasDecidableMembership<std::decay_t<decltype(empty_prod)>>);
  const auto excluded = A ^ !A;  // A △ ¬A = 𝔸
  CHECK(HasDecidableMembership<std::decay_t<decltype(excluded)>>);
}
