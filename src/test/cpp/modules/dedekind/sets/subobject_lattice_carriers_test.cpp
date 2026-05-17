/** @file dedekind/sets/subobject_lattice_carriers_test.cpp
 *
 * #698 Slice 9 — `:etcs` harmonisation on the @c :sets side.
 *
 * Witnesses that the @c :sets carriers (@c Ø, @c UniversalSet,
 * @c SingletonSet) participate in @c :lattice::IsSubobjectLattice
 * structurally:
 *
 *   - All three already exposed @c Ambient + @c logic_species
 *     pre-Slice-9.
 *   - All three already had @c operator<= returning @c L::Ω
 *     pre-Slice-9.
 *   - The free @c meet / @c join / @c complement (Slice 9 added the
 *     @c complement alias in @c :etcs::concrete) produce
 *     @c IsSubobjectFamilyMember-shaped results.
 *
 * The post-#712-review refactor uses @b structural recognition via
 * @c { a @c <= @c b } @c -> @c L::Ω instead of carrier-side
 * @c SubsetEqRel nested-struct typedefs — no new function-object
 * structs anywhere ("Structs will lock us in").
 *
 * The full @c IsSet @c ⟹ @c IsSubobjectLattice static_assert is
 * deferred to a follow-up pending a @c Set::χ static-init refactor
 * (capturing-lambda Predicates fail default-construction at the
 * out-of-class definition); see the Sollbruchstelle text in
 * @c :etcs::etcs at the Axiom 10 section.
 */

#include <catch2/catch_test_macros.hpp>
#include <concepts>

import dedekind.category;
import dedekind.sets;

using namespace dedekind::category;
using namespace dedekind::sets;

TEST_CASE("sets:subobject-lattice — Ø participates in IsSubobjectLattice",
          "[sets][lattice][subobject][etcs][empty]") {
  STATIC_CHECK(IsSubobjectLattice<Ø<bool>>);
}

TEST_CASE(
    "sets:subobject-lattice — UniversalSet participates in IsSubobjectLattice",
    "[sets][lattice][subobject][etcs][universal]") {
  STATIC_CHECK(IsSubobjectLattice<UniversalSet<bool>>);
}

TEST_CASE(
    "sets:subobject-lattice — SingletonSet participates in IsSubobjectLattice",
    "[sets][lattice][subobject][etcs][singleton]") {
  STATIC_CHECK(IsSubobjectLattice<SingletonSet<bool>>);
}

TEST_CASE(
    "sets:subobject-lattice — ClassicalLogic carriers fire IsBooleanSubobjectLattice",
    "[sets][lattice][subobject][boolean][classical]") {
  /** @brief Mechanises the user's downstream intuition: parametrising a
   *         carrier with @c ClassicalLogic automatically participates in
   *         the Boolean refinement.  Type-checked, not documented. */
  STATIC_CHECK(IsBooleanSubobjectLattice<Ø<bool>>);
  STATIC_CHECK(IsBooleanSubobjectLattice<UniversalSet<bool>>);
  STATIC_CHECK(IsBooleanSubobjectLattice<SingletonSet<bool>>);
  STATIC_CHECK(IsBooleanSubobjectLattice<Ø<bool, ClassicalLogic>>);
}

TEST_CASE(
    "sets:subobject-lattice — TernaryLogic carriers do NOT fire IsBooleanSubobjectLattice",
    "[sets][lattice][subobject][boolean][kleene][negative]") {
  /** @brief Honest Rejection: parametrising a carrier with
   *         @c TernaryLogic falls out of the Boolean refinement.
   *         Heyting structure still holds via @c IsSubobjectLattice. */
  STATIC_CHECK_FALSE(IsBooleanSubobjectLattice<Ø<bool, TernaryLogic>>);
  STATIC_CHECK_FALSE(IsBooleanSubobjectLattice<UniversalSet<bool, TernaryLogic>>);
  STATIC_CHECK_FALSE(IsBooleanSubobjectLattice<SingletonSet<bool, TernaryLogic>>);
}

TEST_CASE("sets:subobject-lattice — IsSet still fires post-Axiom-10 update",
          "[sets][lattice][subobject][etcs][axiom10][regression]") {
  /** @brief Regression: the Axiom 10 generalisation added a
   *         @c logic_species typedef requirement.  The canonical
   *         carriers all expose it pre-Slice-9, so @c IsSet still
   *         fires.  Defensive witness from the @c :sets side. */
  STATIC_CHECK(IsSet<Ø<bool>>);
  STATIC_CHECK(IsSet<UniversalSet<bool>>);
  STATIC_CHECK(IsSet<SingletonSet<bool>>);
}
