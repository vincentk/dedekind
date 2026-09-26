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
 *   - @c operator<=> on @c Ternary in @c :logic: supplies the @c < the
 *     value-returning @c :species ops @c Inf / @c Sup use to compute Kleene
 *     meet / join on Ternary directly, so @c :lattice carries no
 *     Ternary-specific function-object struct types.
 *
 * Slice 8 is the @b architectural commit.  Concrete carrier witnesses
 * (@c SubsetEqRel typedefs on @c Set / @c Subobject, free-function
 * @c meet / @c join / @c complement, the @c HasAxiom10PowerObjectLattice
 * generalisation in @c :etcs, Ternary's Form-chain participation via a
 * concrete @c L = @c Kleene carrier) land in Slice 9 with the
 * @c :etcs harmonisation.
 */

#include <catch2/catch_test_macros.hpp>
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
  STATIC_CHECK((Ternary::False <=> Ternary::Unknown) ==
               std::strong_ordering::less);
  STATIC_CHECK((Ternary::Unknown <=> Ternary::True) ==
               std::strong_ordering::less);
  STATIC_CHECK((Ternary::True <=> Ternary::True) ==
               std::strong_ordering::equal);

  // Auto-derived <, <=, >, >=, ==, != all work.
  STATIC_CHECK(Ternary::False < Ternary::True);
  STATIC_CHECK(Ternary::Unknown <= Ternary::Unknown);
  STATIC_CHECK(Ternary::True > Ternary::False);
  STATIC_CHECK(Ternary::False == Ternary::False);
  STATIC_CHECK(Ternary::Unknown != Ternary::True);
}

TEST_CASE("category:subobject-lattice: Kleene meet / join via value Sup / Inf",
          "[category][logic][ternary][supinf][lattice]") {
  /** @brief With @c operator<=> on Ternary, @c Inf / @c Sup
   *         compute the Kleene strong AND / OR directly.  No
   *         Ternary-specific function-object struct types live in
   *         @c :lattice (#712 review — "Structs will lock us in"). */
  STATIC_CHECK(Inf{}(Ternary::True, Ternary::Unknown) ==
               Ternary::Unknown);  // Kleene AND
  STATIC_CHECK(Inf{}(Ternary::Unknown, Ternary::False) ==
               Ternary::False);  // Kleene AND, False annihilator
  STATIC_CHECK(Sup{}(Ternary::False, Ternary::Unknown) ==
               Ternary::Unknown);  // Kleene OR
  STATIC_CHECK(Sup{}(Ternary::Unknown, Ternary::True) ==
               Ternary::True);  // Kleene OR, True annihilator

  // Truth-functional consistency with Kleene::AND / OR.
  STATIC_CHECK(Inf{}(Ternary::True, Ternary::Unknown) ==
               Kleene::AND(Ternary::True, Ternary::Unknown));
  STATIC_CHECK(Sup{}(Ternary::False, Ternary::Unknown) ==
               Kleene::OR(Ternary::False, Ternary::Unknown));
}

namespace {

// A minimal carrier exposing the metadata IsSubobjectLattice<S> requires:
// Domain + logic_species with IsOckhamAlgebra on the species.  The subobject
// lattice is INDUCED pointwise from the classifier L, so that metadata IS the
// whole concept (no meet / join / complement free functions by name).
struct FauxSubobject {
  using Domain = bool;
  using logic_species = dedekind::category::Boole;
};

// Negative fixture: same metadata SHAPE, but the species is not an Ockham
// algebra, so it induces no complemented subobject lattice.
struct NonOckhamCarrier {
  using Domain = bool;
  using logic_species = int;  // not IsOckhamAlgebra
};

}  // namespace

TEST_CASE(
    "category:subobject-lattice — IsSubobjectLattice metadata clause fires",
    "[category][lattice][subobject][concept][metadata]") {
  /** @brief The concept's CT-vocabulary metadata (@c Domain +
   *         @c logic_species typedefs, with @c IsOckhamAlgebra on the
   *         species) IS the whole concept: the subobject lattice is
   *         induced pointwise from the classifier @c L, so a carrier
   *         exposing the metadata over an Ockham species already fires
   *         @c IsSubobjectLattice. */
  STATIC_CHECK(IsOckhamAlgebra<FauxSubobject::logic_species>);
  STATIC_CHECK(std::same_as<FauxSubobject::Domain, bool>);
  STATIC_CHECK(IsSubobjectLattice<FauxSubobject>);

  /** @brief Negative witness: the same metadata shape over a species that
   *         is @b not an Ockham algebra induces no complemented subobject
   *         lattice, so the concept does not fire. */
  STATIC_CHECK_FALSE(IsOckhamAlgebra<NonOckhamCarrier::logic_species>);
  STATIC_CHECK_FALSE(IsSubobjectLattice<NonOckhamCarrier>);
}
