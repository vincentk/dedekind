/** @file dedekind/category/subobject_lattice_test.cpp
 *
 * Unit coverage for #698 Slice 8:
 *
 *   - @c IsSubobjectLattice<S> — the CT-vocabulary concept binding
 *     subobject carriers to the Form-chain via the
 *     classifier @c S::logic_species::Ω.
 *   - The Ternary Form-chain witness — Kleene K3 participates in rows
 *     1 (@c IsThinCategory) and 2 (@c IsPosetal) under the @c TernaryLogic
 *     species; row 7 (@c IsBooleanLatticeCategory) fails honestly
 *     because K3 is the smallest non-Boolean Heyting algebra (complement
 *     laws break at @c Unknown).
 *
 * Architectural commits (no concrete carrier witnesses yet — those land
 * in Slice 9 with the @c :etcs harmonisation):
 *
 *   - The Form-chain is L-parametric from day 0: @c IsThinCategory<T,
 *     Rel, L> binds @c rel(a, b) @c -> @c L::Ω, so a Ternary-valued
 *     @c Rel is admissible without any concept-shape changes.
 *   - The collapse rule: @c L = @c ClassicalLogic → rows 1–7 (standard
 *     set theory falls out as the decidable collapse); @c L = @c
 *     TernaryLogic → rows 1–6 only (Heyting; the "tricky to decide"
 *     escape door).
 */

#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <cstdint>
#include <functional>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE("category:subobject-lattice — Ternary callables compute correctly",
          "[category][lattice][subobject][ternary][callable]") {
  /** @brief Sanity: the Form-chain callables on Ternary forward to
   *         @c TernaryLogic's strong Kleene operators. */
  constexpr TernaryLessEqual le{};
  constexpr TernaryMeet meet{};
  constexpr TernaryJoin join{};
  constexpr TernaryNot not_op{};

  // Chain order: False < Unknown < True.
  STATIC_CHECK(le(Ternary::False, Ternary::Unknown) == Ternary::True);
  STATIC_CHECK(le(Ternary::Unknown, Ternary::True) == Ternary::True);
  STATIC_CHECK(le(Ternary::True, Ternary::False) == Ternary::False);

  // Meet = Kleene AND = min.
  STATIC_CHECK(meet(Ternary::True, Ternary::Unknown) == Ternary::Unknown);
  STATIC_CHECK(meet(Ternary::Unknown, Ternary::False) == Ternary::False);
  STATIC_CHECK(meet(Ternary::True, Ternary::True) == Ternary::True);

  // Join = Kleene OR = max.
  STATIC_CHECK(join(Ternary::False, Ternary::Unknown) == Ternary::Unknown);
  STATIC_CHECK(join(Ternary::Unknown, Ternary::True) == Ternary::True);
  STATIC_CHECK(join(Ternary::False, Ternary::False) == Ternary::False);

  // Kleene rotation: NOT(False) = True, NOT(Unknown) = Unknown,
  // NOT(True) = False.  This is the load-bearing feature for "complement
  // laws fail at Unknown": Unknown ∧ ¬Unknown = min(Unknown, Unknown)
  // = Unknown, not False.
  STATIC_CHECK(not_op(Ternary::False) == Ternary::True);
  STATIC_CHECK(not_op(Ternary::Unknown) == Ternary::Unknown);
  STATIC_CHECK(not_op(Ternary::True) == Ternary::False);

  // Involutiveness: NOT(NOT(x)) = x for every x — required by
  // IsInvolutiveEndofunctor and the load-bearing reason TernaryNot
  // sits at the Form-chain Not slot.
  STATIC_CHECK(not_op(not_op(Ternary::False)) == Ternary::False);
  STATIC_CHECK(not_op(not_op(Ternary::Unknown)) == Ternary::Unknown);
  STATIC_CHECK(not_op(not_op(Ternary::True)) == Ternary::True);
}

TEST_CASE("category:subobject-lattice — Ternary participates in rows 1–2",
          "[category][lattice][subobject][ternary][form-chain]") {
  /** @brief Ternary at @c IsThinCategory (row 1): preorder with @c L::Ω-
   *         valued @c rel. */
  STATIC_CHECK(IsThinCategory<Ternary, TernaryLessEqual, TernaryLogic>);

  /** @brief Ternary at @c IsPosetal (row 2): thin + antisymmetric. */
  STATIC_CHECK(IsPosetal<Ternary, TernaryLessEqual, TernaryLogic>);

  /** @brief @c TernaryNot is an involutive endofunctor (Slice 5
   *         machinery), independent of the Form-chain rows.  Used at
   *         the @c Not slot when a Boolean-style complement is
   *         intended; in K3 the complement laws fail, so this
   *         involution does @b not promote to a Boolean lattice
   *         complement. */
  STATIC_CHECK(IsInvolutiveEndofunctor<TernaryNot, Ternary>);
}

TEST_CASE(
    "category:subobject-lattice — Ternary is NOT Boolean (honest rejection)",
    "[category][lattice][subobject][ternary][negative][honest-rejection]") {
  /** @brief @c K3 is the smallest non-Boolean Heyting algebra.
   *         @c is_complement_v is @b deliberately not registered for
   *         @c (TernaryNot, Ternary, TernaryLessEqual, TernaryJoin,
   *         TernaryMeet), so @c IsBooleanLatticeCategory<Ternary, …>
   *         fails closed via Slice 7's opt-in trait pattern. */
  STATIC_CHECK_FALSE(
      IsBooleanLatticeCategory<Ternary, TernaryLessEqual, TernaryJoin,
                               TernaryMeet, TernaryNot, TernaryLogic>);

  /** @brief Direct semantic witness of why: complement laws fail at
   *         @c Unknown.  This is the value-level evidence the opt-in
   *         trait avoided registering. */
  constexpr TernaryMeet meet{};
  constexpr TernaryJoin join{};
  constexpr TernaryNot not_op{};

  // x ∧ ¬x = ⊥ fails at Unknown: Unknown ∧ ¬Unknown = Unknown, not False.
  STATIC_CHECK(meet(Ternary::Unknown, not_op(Ternary::Unknown)) ==
               Ternary::Unknown);
  // Holds at True / False (the classically-decidable subset of K3):
  STATIC_CHECK(meet(Ternary::True, not_op(Ternary::True)) == Ternary::False);
  STATIC_CHECK(meet(Ternary::False, not_op(Ternary::False)) == Ternary::False);

  // x ∨ ¬x = ⊤ fails at Unknown likewise.
  STATIC_CHECK(join(Ternary::Unknown, not_op(Ternary::Unknown)) ==
               Ternary::Unknown);
}

namespace {

// Architectural fixture: a minimal carrier exposing the CT-vocabulary
// metadata that IsSubobjectLattice<S>'s body requires.  No operational
// content yet — the meet / join / subset_eq free-function witnesses
// land in Slice 9 with the :etcs harmonisation; this fixture exists to
// prove the concept's metadata clause fires structurally.
struct FauxSubobject {
  using Ambient = bool;
  using logic_species = dedekind::category::ClassicalLogic;
};

// Confirm fixture exposes the CT-vocabulary metadata IsSubobjectLattice
// needs.  Slice 9 adds the free-function witnesses on Subobject<A, Chi>
// and Set<T, L, P> at which point the concept fires end-to-end.

}  // namespace

TEST_CASE(
    "category:subobject-lattice — IsSubobjectLattice metadata clause fires",
    "[category][lattice][subobject][concept][metadata]") {
  /** @brief Sanity that the concept's CT-vocabulary metadata clause
   *         (@c Ambient + @c logic_species typedefs, with
   *         @c IsLogicalSpecies on the species) fires structurally on
   *         a carrier exposing the required typedefs.
   *
   *  @note The full concept additionally requires free-function @c meet,
   *  @c join, @c subset_eq (and classical-gated @c complement) — those
   *  witnesses land in Slice 9 with the @c :etcs harmonisation, at which
   *  point @c Subobject<A, Chi> and @c Set<T, L, P> will fire the
   *  concept end-to-end.  This test pins the Slice 8 architectural
   *  commit: the metadata shape is what we want, and the carriers
   *  already expose it (@c Set has @c Ambient + @c logic_species
   *  typedefs per @c expressions.cppm; @c Subobject has @c Ambient and
   *  derives @c logic_species via the carrier's @c χ). */
  STATIC_CHECK(IsLogicalSpecies<FauxSubobject::logic_species>);
  STATIC_CHECK(std::same_as<FauxSubobject::Ambient, bool>);
}
