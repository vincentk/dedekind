#include <catch2/catch_test_macros.hpp>
import dedekind.category;
import dedekind.sets;

using namespace dedekind::category;
using namespace dedekind::sets;

/** @section Morphic_Mimes: Pure Structuralist Mocking */
struct MockSpecies {
  using Domain = int;
};

struct MockPart {
  using Domain = int;
  using ambient_species = MockSpecies;
  // The Characteristic Morphism: y(x)
  constexpr bool operator()(int) const { return true; }
};

struct MockSystem {
  using Domain = MockPart;

  /** @section Bounded_Lattice_Interface */
  // Using the exact names required by IsBoundedLattice concept
  constexpr MockPart lower_bound() const { return {}; }
  constexpr MockPart upper_bound() const { return {}; }

  // Meet and Join operators to satisfy IsLattice
  constexpr MockSystem operator&(const MockSystem&) const { return *this; }
  constexpr MockSystem operator|(const MockSystem&) const { return *this; }
};

/** @section Mock_Certification_Registry */
// We manually verify the algebraic properties of MockSystem
// to satisfy the IsSemigroupoid and IsLattice concepts.

namespace dedekind::category {

template <>
struct is_associative<MockSystem, std::bit_and<MockSystem>> : std::true_type {};

template <>
struct is_associative<MockSystem, std::bit_or<MockSystem>> : std::true_type {};

template <>
struct is_idempotent<MockSystem, std::bit_and<MockSystem>> : std::true_type {};

template <>
struct is_idempotent<MockSystem, std::bit_or<MockSystem>> : std::true_type {};

}  // namespace dedekind::category

TEST_CASE("Mereology: Ontological Concept Verification",
          "[ontology][mereology]") {
  // 1. Verify the Presence Morphism (The Functional Essence)
  // Does the concept correctly identify a callable 'Whole' for a 'Part'?
  // Post-consolidation: route through @c :category:mereology's
  // canonical @c IsPartOfRelation (the 2026-05-09 alignment dropped
  // the @c :sets:mereology aliases @c IsProperPart / @c IsPartOf as
  // redundant with the upstream concept).
  STATIC_REQUIRE(IsPartOfRelation<int, MockPart, bool>);

  // 2. Verify the Systemic Lattice (The Space of Parts)
  // Does the concept recognize a Bounded Lattice inhabited by valid Parts?
  STATIC_REQUIRE(IsSystem<MockSystem, MockSpecies>);

  // 3. Verify the order-style encoding of parthood is recognised.
  // Note: the @c operator>= auto-synthesis (dual of <=) was retired
  // in the 2026-05-09 mereology consolidation; only the forward <=
  // direction is checked here.  If the converse spelling is needed
  // back, file as a follow-up against the consolidated
  // @c :category:mereology surface.
  struct WeakPart {
    constexpr bool operator<=(const WeakPart&) const { return true; }
  };
  STATIC_REQUIRE(IsPartOfRelation<WeakPart, WeakPart, bool>);
}

TEST_CASE("Mereology: IsMereologicalCutCandidate structural contract",
          "[ontology][mereology][cut]") {
  // Minimal positive witness: a shell that filters an int manifold to a
  // mereological part (order-style encoding).
  struct WitnessFilteredSpace {
    constexpr bool operator<=(const int&) const { return true; }
  };
  struct CutShell {
    using FilteredSpace = WitnessFilteredSpace;
    constexpr WitnessFilteredSpace filter(const int&) const { return {}; }
  };
  STATIC_REQUIRE(IsMereologicalCutCandidate<CutShell, int>);

  // Negative witness: missing FilteredSpace alias — must NOT satisfy concept.
  struct NoAlias {
    constexpr int filter(const int& m) const { return m; }
  };
  STATIC_REQUIRE_FALSE(IsMereologicalCutCandidate<NoAlias, int>);

  // Negative witness: FilteredSpace not a mereological part — must NOT satisfy.
  struct BadFilteredSpace {};  // no operator<= or call/index for int
  struct BadShell {
    using FilteredSpace = BadFilteredSpace;
    constexpr BadFilteredSpace filter(const int&) const { return {}; }
  };
  STATIC_REQUIRE_FALSE(IsMereologicalCutCandidate<BadShell, int>);
}
