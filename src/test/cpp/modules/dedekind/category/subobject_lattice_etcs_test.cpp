/** @file dedekind/category/subobject_lattice_etcs_test.cpp
 *
 * #698 Slice 9 — `:etcs` harmonisation on the @c :topoi side.
 *
 * Witnesses that the architectural commit of Slice 8 (the
 * @c IsSubobjectLattice<S> concept) fires structurally on
 * @c Subobject<A, Chi> after the Slice 9 additions:
 *
 *   - @c Subobject<A, Chi> now exposes @c logic_species via
 *     @c GetLogic<Cod<Chi>>;
 *   - member @c operator<= returns @c L::Ω (identity-case
 *     @c L::True; heterogeneous-Chi @c L::Unknown when @c L
 *     admits it).
 *
 * The @c :sets-side carriers (@c Ø, @c UniversalSet, @c SingletonSet,
 * @c Set<T, L, P>) are witnessed in
 * @c test/cpp/modules/dedekind/sets/subobject_lattice_carriers_test.cpp
 * (which can import @c dedekind.sets).
 *
 * The post-#712-review refactor uses @b structural recognition via
 * @c { a @c <= @c b } @c -> @c L::Ω instead of carrier-side
 * @c SubsetEqRel nested-struct typedefs — no new function-object
 * structs anywhere ("Structs will lock us in").
 */

#include <catch2/catch_test_macros.hpp>
#include <concepts>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE(
    "category:subobject-lattice-etcs — Subobject<bool, Chi> participates",
    "[category][lattice][subobject][etcs][topoi]") {
  /** @brief A @c Subobject<bool, Chi> produced by @c classify
   *         satisfies @c IsSubobjectLattice — Slice 9 added the
   *         @c logic_species typedef (via @c GetLogic<Cod<Chi>>) and
   *         member @c operator<= returning @c L::Ω.  The free
   *         meet / join / complement are provided in
   *         @c :etcs::concrete. */
  auto pred = [](const bool&) { return true; };
  using SubBool = decltype(classify<bool>(pred));
  STATIC_CHECK(IsSubobjectLattice<SubBool>);
}

TEST_CASE(
    "category:subobject-lattice-etcs — Subobject exposes logic_species",
    "[category][lattice][subobject][etcs][topoi][metadata]") {
  /** @brief Slice 9 additions: @c Subobject<A, Chi>::logic_species is
   *         derived from @c Chi's codomain via @c GetLogic.  Predicates
   *         returning @c bool resolve to @c ClassicalLogic. */
  auto pred = [](const bool&) { return true; };
  using SubBool = decltype(classify<bool>(pred));
  STATIC_CHECK(std::same_as<SubBool::Ambient, bool>);
  STATIC_CHECK(std::same_as<SubBool::logic_species, ClassicalLogic>);
}
