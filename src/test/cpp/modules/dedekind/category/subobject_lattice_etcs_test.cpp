/** @file dedekind/category/subobject_lattice_etcs_test.cpp
 *
 * #698 Slice 9 — `:etcs` harmonisation on the @c :topoi side.
 *
 * Witnesses that the architectural commit of Slice 8 (the
 * @c IsSubobjectLattice<S> concept) fires structurally on
 * @c Subobject<A, Chi> after the Slice 9 additions:
 *
 *   - @c Subobject<A, Chi> now exposes @c logic_species via
 *     @c GetLogic<Cod<Chi>> (the single new typedef in Slice 9).
 *
 * The @c :sets-side carriers (@c Ø, @c UniversalSet, @c SingletonSet)
 * are witnessed in
 * @c test/cpp/modules/dedekind/sets/subobject_lattice_carriers_test.cpp
 * (which can import @c dedekind.sets).
 *
 * The concept body uses @b structural recognition on
 * @c meet / @c join / @c complement shapes; the Form-chain row-1
 * @c ≤ relation is @b derivable from @c meet (Birkhoff §1.4) and is
 * not a separate clause.  No carrier-side @c SubsetEqRel typedef and
 * no new function-object structs introduced ("Structs will lock us
 * in", #712 review).
 */

#include <catch2/catch_test_macros.hpp>
#include <concepts>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE("category:subobject-lattice-etcs — Subobject<bool, Chi> participates",
          "[category][lattice][subobject][etcs][topoi]") {
  /** @brief A @c Subobject<bool, Chi> produced by @c classify
   *         satisfies @c IsSubobjectLattice — Slice 9 added the
   *         @c logic_species typedef (via @c GetLogic<Cod<Chi>>); the
   *         free @c meet / @c join / @c complement are provided in
   *         @c :etcs::concrete; the @c ≤ relation is derivable from
   *         @c meet (Birkhoff §1.4) and therefore not required by the
   *         concept body. */
  auto pred = [](const bool&) { return true; };
  using SubBool = decltype(classify<bool>(pred));
  STATIC_CHECK(IsSubobjectLattice<SubBool>);
}

TEST_CASE("category:subobject-lattice-etcs — Subobject exposes logic_species",
          "[category][lattice][subobject][etcs][topoi][metadata]") {
  /** @brief Slice 9 addition: @c Subobject<A, Chi>::logic_species is
   *         derived from @c Chi's codomain via @c GetLogic.  Predicates
   *         returning @c bool resolve to @c ClassicalLogic. */
  auto pred = [](const bool&) { return true; };
  using SubBool = decltype(classify<bool>(pred));
  STATIC_CHECK(std::same_as<SubBool::Ambient, bool>);
  STATIC_CHECK(std::same_as<SubBool::logic_species, ClassicalLogic>);
}
