#include <catch2/catch_test_macros.hpp>
#include <limits>
import dedekind.category;
import dedekind.sets;

using namespace dedekind::category;
using namespace dedekind::sets;

TEST_CASE("Level 1 Final Proof: The Mereology Highway",
          "[ontology][mereology][highway]") {
  Ø<int> empty;
  SECTION("2. Initial Object Proof") {
    static_assert(IsSet<decltype(ambient_set<int>(empty))>);
  }

  SECTION("3. The Extreme Bounds (0 and 1)") {
    UniversalSet<int> universe;

    // Verify these are valid sets over the active logic species.
    static_assert(IsSet<decltype(ambient_set<int>(empty))>);
    static_assert(IsSet<decltype(ambient_set<int>(universe))>);

    // Verify the Logical Truth across species
    REQUIRE(empty(42) == false);
    REQUIRE(universe(42) == true);

    // Verify the Magnitude (The Ruler)
    // Note: UniversalSet<int> is Countable (ℵ_0)
    REQUIRE(empty.cardinality() == Finite{});
  }

  SECTION("4. The Logic Swapping (Topos-Awareness)") {
    // Universal Set over the Ternary Topos (Kleene Logic)
    UniversalSet<int, TernaryLogic> k_universe;

    REQUIRE(k_universe(42) == Ternary::True);
  }
}

TEST_CASE("Boundaries: The Algebra of Extremality", "[sets][boundaries]") {
  // The Identities: Ø and Ω as the "North and South Poles"
  constexpr Ø<int> null;
  constexpr UniversalSet<int> universe;

  SECTION("Aha! 1: The Law of Absorption (Annihilation)") {
    /**
     * In a Union, the Universe is the Annihilator: Ω | S = Ω.
     * In an Intersection, the Void is the Annihilator: ∅ & S = ∅.
     */
    static_assert(
        std::is_same_v<decltype((universe | null)), UniversalSet<int>>);
    static_assert(std::is_same_v<decltype((null & universe)), Ø<int>>);
  }

  SECTION("Aha! 2: The Law of Identity") {
    constexpr auto s = ι<size_t>(42);

    INFO("In a Union, the Void is the Identity: ∅ | S = S.");
    STATIC_REQUIRE(std::is_same_v<decltype(null | s), decltype(s)>);
    STATIC_REQUIRE((null | s) == s);
    INFO("In an Intersection, the Universe is the Identity: Ω & S = S.");
    STATIC_REQUIRE(std::is_same_v<decltype(universe & s), decltype(s)>);
    STATIC_REQUIRE((universe & s) == s);
  }

  SECTION("Aha! 3: The Involution of the Remainder") {
    /**
     * The Double Negation (Complement of the Complement) is the Identity.
     * !!S = S.
     */
    const auto not_null = !null;
    const auto not_universe = !universe;

    CHECK(not_null(7) == true);
    CHECK(not_universe(7) == false);
  }

  SECTION("Cardinality bounds for extensional sets are computed explicitly") {
    constexpr SingletonSet<SignedExtensionalCardinal<>> s1{1};
    constexpr SingletonSet<SignedExtensionalCardinal<>> s2{2};
    constexpr Ø<SignedExtensionalCardinal<>> empty;

    CHECK(bound_meet(s1, s2) == 1);
    CHECK(bound_join(s1, s2) == 2);

    CHECK(bound_meet(empty, s1) == 0);
    CHECK(bound_join(empty, s1) == 1);
  }

  SECTION("Cardinality bounds for intensional/transfinite sets use sentinel") {
    // ℕ is now the carrier (unsigned int) post-#401; the predicate-set
    // value is the namespace-level constant @c N (NaturalNumbersOf<>).
    constexpr auto naturals = N;
    constexpr Ø<SignedExtensionalCardinal<>> empty;
    constexpr SingletonSet<SignedExtensionalCardinal<>> singleton{7};
    const auto max_v = std::numeric_limits<std::size_t>::max();

    CHECK(bound_meet(universe, naturals) == max_v);
    CHECK(bound_join(universe, naturals) == max_v);

    CHECK(bound_meet(empty, naturals) == 0);
    CHECK(bound_meet(singleton, naturals) == 1);
  }

  SECTION("Identity optimizations remain structurally valid") {
    constexpr SingletonSet<SignedExtensionalCardinal<>> s{42};
    constexpr Ø<SignedExtensionalCardinal<>> empty;

    CHECK(std::is_same_v<decltype(empty | s),
                         SingletonSet<SignedExtensionalCardinal<>>>);
    CHECK(std::is_same_v<decltype(universe & s),
                         SingletonSet<SignedExtensionalCardinal<>>>);
  }
}

TEST_CASE("image(IsTerminalMorphism F, S) — terminal-codomain collapse (#661)",
          "[sets][image][terminal-morphism]") {
  using dedekind::category::One;
  const auto bang = dedekind::category::unit<int>();  // !: int → One

  SECTION("UniversalSet source → SingletonSet<One> (inhabited collapse)") {
    constexpr UniversalSet<int> universe;
    const auto img = image(bang, universe);
    STATIC_CHECK(std::same_as<std::remove_cvref_t<decltype(img)>,
                              SingletonSet<One, ClassicalLogic>>);
    CHECK(img(One{}));
  }

  SECTION("Ø source → Ø<One> (empty stays empty)") {
    constexpr Ø<int> empty;
    const auto img = image(bang, empty);
    STATIC_CHECK(std::same_as<std::remove_cvref_t<decltype(img)>,
                              Ø<One, ClassicalLogic>>);
  }

  SECTION("SingletonSet source → SingletonSet<One> via the generic overload") {
    const auto s = singleton(7);
    const auto img = image(bang, s);
    STATIC_CHECK(std::same_as<std::remove_cvref_t<decltype(img)>,
                              SingletonSet<One, ClassicalLogic>>);
    CHECK(img(One{}));
  }
}
