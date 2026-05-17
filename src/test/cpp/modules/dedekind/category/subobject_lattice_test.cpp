/** @file dedekind/category/subobject_lattice_test.cpp
 *
 * Unit coverage for #698 Slice 8:
 *
 *   - @c IsSubobjectLattice<S> — the CT-vocabulary concept binding
 *     subobject carriers to the Form-chain via the classifier
 *     @c S::logic_species::Ω.  Refines @c IsThinCategory<S,
 *     S::SubsetEqRel, S::logic_species>; requires the carrier to
 *     expose its own @c SubsetEqRel callable type (an
 *     @c :morphism::IsArrow-shaped binary relation).
 *   - @c IsSubobjectFamilyMember<R, A, L> — anchored on the ambient
 *     and the classifier, the family concept paralleling
 *     @c :sets::mereology::IsSystem<S, Species, L>.
 *   - @c operator<=> on @c Ternary in @c :logic — enables stdlib
 *     niebloids (@c std::ranges::min / @c max) to compute Kleene
 *     meet / join on Ternary directly, so @c :lattice carries no
 *     Ternary-specific function-object struct types.
 *
 * Slice 8 is the @b architectural commit.  Concrete carrier witnesses
 * (@c SubsetEqRel typedefs on @c Set / @c Subobject, free-function
 * @c meet / @c join / @c complement, the @c HasAxiom10PowerObjectLattice
 * generalisation in @c :etcs, Ternary's Form-chain participation via a
 * concrete @c L = @c TernaryLogic carrier) land in Slice 9 with the
 * @c :etcs harmonisation.
 */

#include <catch2/catch_test_macros.hpp>
#include <algorithm>  // std::ranges::min / max — Kleene meet / join on Ternary
#include <compare>
#include <concepts>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE("category:subobject-lattice — Ternary <=> enables stdlib niebloids",
          "[category][logic][ternary][spaceship]") {
  /** @brief @c Ternary has @c operator<=> returning @c std::strong_ordering
   *         on the truth-order chain @c False @c (-1) @c < @c Unknown
   *         @c (0) @c < @c True @c (1).
   *
   *  @note Comparison between two @c Ternary values is itself classically
   *  decided — the truth ordering is total.  @c Unknown values arise from
   *  undecidable predicates over an intensional ambient, not from
   *  comparing two @c Ternary values directly. */
  STATIC_CHECK((Ternary::False <=> Ternary::Unknown) == std::strong_ordering::less);
  STATIC_CHECK((Ternary::Unknown <=> Ternary::True) == std::strong_ordering::less);
  STATIC_CHECK((Ternary::True <=> Ternary::True) == std::strong_ordering::equal);

  // Auto-derived <, <=, >, >=, ==, != all work.
  STATIC_CHECK(Ternary::False < Ternary::True);
  STATIC_CHECK(Ternary::Unknown <= Ternary::Unknown);
  STATIC_CHECK(Ternary::True > Ternary::False);
  STATIC_CHECK(Ternary::False == Ternary::False);
  STATIC_CHECK(Ternary::Unknown != Ternary::True);
}

TEST_CASE(
    "category:subobject-lattice — Kleene meet / join via std::ranges niebloids",
    "[category][logic][ternary][niebloid][lattice]") {
  /** @brief With @c operator<=> on Ternary, @c std::ranges::min / @c max
   *         compute the Kleene strong AND / OR directly.  No
   *         Ternary-specific function-object struct types live in
   *         @c :lattice (#712 review — "Structs will lock us in"). */
  STATIC_CHECK(std::ranges::min(Ternary::True, Ternary::Unknown) ==
               Ternary::Unknown);  // Kleene AND
  STATIC_CHECK(std::ranges::min(Ternary::Unknown, Ternary::False) ==
               Ternary::False);  // Kleene AND, False annihilator
  STATIC_CHECK(std::ranges::max(Ternary::False, Ternary::Unknown) ==
               Ternary::Unknown);  // Kleene OR
  STATIC_CHECK(std::ranges::max(Ternary::Unknown, Ternary::True) ==
               Ternary::True);  // Kleene OR, True annihilator

  // Truth-functional consistency with TernaryLogic::AND / OR.
  STATIC_CHECK(std::ranges::min(Ternary::True, Ternary::Unknown) ==
               TernaryLogic::AND(Ternary::True, Ternary::Unknown));
  STATIC_CHECK(std::ranges::max(Ternary::False, Ternary::Unknown) ==
               TernaryLogic::OR(Ternary::False, Ternary::Unknown));
}

namespace {

// Architectural fixture: a minimal carrier exposing the CT-vocabulary
// metadata that IsSubobjectLattice<S>'s body requires.  No operational
// content yet — the SubsetEqRel typedef, meet / join / complement free
// functions land in Slice 9 with the :etcs harmonisation; this fixture
// exists to prove the concept's metadata clause fires structurally.
struct FauxSubobject {
  using Ambient = bool;
  using logic_species = dedekind::category::ClassicalLogic;
};

}  // namespace

TEST_CASE(
    "category:subobject-lattice — IsSubobjectLattice metadata clause fires",
    "[category][lattice][subobject][concept][metadata]") {
  /** @brief Sanity that the concept's CT-vocabulary metadata
   *         (@c Ambient + @c logic_species typedefs, with
   *         @c IsLogicalSpecies on the species) is the shape carriers
   *         must expose.
   *
   *  @note The full concept additionally requires a @c SubsetEqRel
   *  typedef and free-function @c meet, @c join, @c complement —
   *  those land in Slice 9 with the @c :etcs harmonisation, at which
   *  point @c Subobject<A, Chi> and @c Set<T, L, P> will fire the
   *  concept end-to-end.  This test pins the Slice 8 architectural
   *  commit: the metadata shape is what we want, and the carriers
   *  already expose @c Ambient + @c logic_species. */
  STATIC_CHECK(IsLogicalSpecies<FauxSubobject::logic_species>);
  STATIC_CHECK(std::same_as<FauxSubobject::Ambient, bool>);

  /** @brief Negative witness: without @c SubsetEqRel + the free
   *         functions, the full concept does @b not fire.  Slice 9
   *         lifts this by adding the missing pieces on @c Set /
   *         @c Subobject. */
  STATIC_CHECK_FALSE(IsSubobjectLattice<FauxSubobject>);
}
