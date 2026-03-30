#include <catch2/catch_test_macros.hpp>
#include <type_traits>

import dedekind.sets;

TEST_CASE("Boolean Universal Set (Theorems for Free)", "[algebra][boolean]") {
  using namespace dedekind::sets;

  // 1. Ground the Universe
  // For bool, the ground is Extensional(2)
  using U_Bool = UniversalSet<bool, Extensional>;
  auto universe = U_Bool(Extensional(2));

  SECTION("Axioms of Identity") {
    // Membership is always true for the identity
    REQUIRE(universe[true]);
    REQUIRE(universe[false]);

    // Cardinality is preserved
    STATIC_REQUIRE(
        std::is_same_v<typename U_Bool::cardinality_type, Extensional>);
    REQUIRE(universe.cardinality().bound == 2);
  }

  SECTION("Symbolic Negation (The Law of !U)") {
    // !U should return the Empty Set (ø) type, not a ComplementNode
    auto empty = !universe;

    // Proof: The type is exactly ø
    using EmptyType = decltype(empty);
    STATIC_REQUIRE(std::is_same_v<EmptyType, ø<bool, U_Bool>>);

    // Axiom of Emptiness: Nothing is in ø
    REQUIRE_FALSE(empty[true]);
    REQUIRE_FALSE(empty[false]);
    REQUIRE(empty.cardinality().bound == 0);
  }

  SECTION("Double Negation (!!U == U)") {
    auto double_neg = !!universe;

    // Proof: Double negation collapsed at compile-time back to the original
    // type
    using ResultType = decltype(double_neg);
    STATIC_REQUIRE(std::is_same_v<ResultType, U_Bool>);

    REQUIRE(double_neg[true]);
  }

  SECTION("Relative Context") {
    auto empty = !universe;
    // Every empty set must know its base set (the context)
    auto base = empty.base_set();

    STATIC_REQUIRE(std::is_same_v<decltype(base), U_Bool>);
    REQUIRE(base[true]);
  }
}

TEST_CASE("Symbolic Set Algebra: Intersection & Booleans",
          "[algebra][symbolic]") {
  using namespace dedekind::sets;

  // Prototypical Rich Set: The Boolean Universe
  // Cardinality: ℕ1 (Size 2)
  auto U = bool_universe();
  auto E = !U;  // The Empty Set (ø) derived from U

  SECTION("Lattice Axioms: Intersection (Meet)") {
    // Theorem 1: Idempotency (A ∩ A = A)
    // The operator should return the exact same object/type
    auto intersect_self = U & U;
    STATIC_REQUIRE(std::is_same_v<decltype(intersect_self), decltype(U)>);

    // Theorem 2: Short-circuit Optimization (A ∩ ø = ø)
    auto intersect_empty = U & E;
    STATIC_REQUIRE(std::is_same_v<decltype(intersect_empty), decltype(E)>);

    // Theorem 3: Short-circuit Optimization (ø ∩ A = ø)
    auto empty_intersect = E & U;
    STATIC_REQUIRE(std::is_same_v<decltype(empty_intersect), decltype(E)>);
  }

  SECTION("Boolean Membership (Axioms)") {
    // U contains everything in its domain
    REQUIRE(U.contains(true) == true);
    REQUIRE(U.contains(false) == true);

    // E contains nothing
    REQUIRE(E.contains(true) == false);
    REQUIRE(E.contains(false) == false);

    // Using the operator[] alias from SetExpression
    REQUIRE(U[true] == true);
    REQUIRE(E[false] == false);
  }

  SECTION("Rich Set Proofs (Category Theory)") {
    // Verify that the Boolean Universe satisfies our Concepts
    STATIC_REQUIRE(IsSet<decltype(U), bool>);
    STATIC_REQUIRE(IsLatticeSet<decltype(U), bool>);
    STATIC_REQUIRE(IsComplementedSet<decltype(U), bool>);
    // STATIC_REQUIRE(IsHigherOrderSet<decltype(U), bool>);

    // The Grand Theorem: Boolean Universe is a Rich Set
    STATIC_REQUIRE(IsRichSet<decltype(U), bool>);
  }

  SECTION("Cardinality Propagation") {
    // Cardinality of U is ℕ1
    STATIC_REQUIRE(std::is_same_v<typename decltype(U)::cardinality_type, ℕ1>);

    // Cardinality of E is Zero (Empty)
    STATIC_REQUIRE(
        std::is_same_v<typename decltype(E)::cardinality_type, Zero>);

    // Power set of U has cardinality Extensional(4)
    // auto P = U.power();
    // STATIC_REQUIRE(P.cardinality().bound == 4);
  }

  SECTION("The Complete Symbolic System") {
    // 1. Lattice Identity
    STATIC_REQUIRE(IsLatticeSet<decltype(U), bool>);

    // 2. Complemented Identity
    STATIC_REQUIRE(IsComplementedSet<decltype(U), bool>);

    // 3. Morphic Identity (Pairs)
    auto Cross = U * U;
    STATIC_REQUIRE(IsMorphicSet<decltype(U), bool>);
    STATIC_REQUIRE(std::is_same_v<typename decltype(Cross)::element_type,
                                  std::pair<bool, bool>>);

    // 4. The Grand Slam
    STATIC_REQUIRE(IsRichSet<decltype(U), bool>);
  }
}

TEST_CASE("Symbolic Predicates (The Specification Axiom)",
          "[algebra][predicate]") {
  using namespace dedekind::sets;

  // We start with our Rich Boolean Universe
  auto U = bool_universe();

  // 1. Define a "Naked" Predicate (Lambda)
  auto is_true = [](bool x) { return x == true; };

  // 2. Create the PredicateNode using the Caret (^) operator
  // This represents {x ∈ bool | x == true}
  auto OnlyTrue = U ^ is_true;

  SECTION("Membership Axioms") {
    // Truth: true is in U AND is_true(true) is true
    REQUIRE(OnlyTrue.contains(true) == true);

    // Falsity: false is in U BUT is_true(false) is false
    REQUIRE(OnlyTrue.contains(false) == false);

    // Using the CRTP operator[] alias
    REQUIRE(OnlyTrue[true] == true);
  }

  SECTION("Symbolic Cardinality (The Upper Bound)") {
    // Theorem: Filtering a set S results in a cardinality <= |S|
    // In our symbolic engine, we use the parent's cardinality as the upper
    // bound.
    STATIC_REQUIRE(
        std::is_same_v<typename decltype(OnlyTrue)::cardinality_type, ℕ1>);

    // Even if we filter a 64-bit space, it stays symbolically ℕ64
    auto LargeFiltered = long_universe() ^ [](uint64_t x) { return x > 0; };
    STATIC_REQUIRE(
        std::is_same_v<typename decltype(LargeFiltered)::cardinality_type,
                       ℕ64>);
  }

  SECTION("Lattice Composition") {
    // We can intersect a PredicateNode with another set
    // {x | x == true} ∩ {x | x == false} should be ø symbolically
    auto is_false = [](bool x) { return x == false; };
    auto OnlyFalse = U ^ is_false;

    auto Intersect = OnlyTrue & OnlyFalse;

    // Since we don't have a "Theorem of Contradictory Predicates" yet,
    // this will result in an IntersectionNode.
    REQUIRE(Intersect.contains(true) == false);
    REQUIRE(Intersect.contains(false) == false);
  }

  SECTION("Predicate Merging") {
    auto complex = (U ^ [](bool x) { return x; }) ^ [](bool x) { return !x; };

    // The type should NOT be a nested PredicateNode<PredicateNode<...>>
    // It should be a flattened PredicateNode pointing directly to UniversalSet
    using ResultType = decltype(complex);
    STATIC_REQUIRE(
        std::is_same_v<typename ResultType::base_set_type, decltype(U)>);

    // Logic: {x | x && !x} is empty
    REQUIRE(complex[true] == false);
    REQUIRE(complex[false] == false);
  }
}

TEST_CASE("Symbolic Algebra: Pure Type Proofs", "[algebra][static]") {
  using namespace dedekind::sets;

  // We only need the types for these proofs
  using U = UniversalSet<int, ℵ_0>;
  using Empty = ø<int, U>;

  SECTION("Identity Annihilation") {
    // Theorem: !!A == A
    // We prove the 'ComplementNode' layers are physically gone
    using A = PredicateSet<int, U>;
    using DoubleNeg = decltype(!!std::declval<A>());

    STATIC_REQUIRE(std::is_same_v<DoubleNeg, A>);
  }

  SECTION("Universal Identities") {
    // Theorem: !U == ø
    STATIC_REQUIRE(std::is_same_v<decltype(!std::declval<U>()), Empty>);

    // Theorem: !ø == U
    STATIC_REQUIRE(std::is_same_v<decltype(!std::declval<Empty>()), U>);
  }
  SECTION("De Morgan Normalization (Distinct Types)") {
    // We create two distinct types by using different lambda types
    auto p1 = [](int x) { return x > 0; };
    auto p2 = [](int x) { return x < 10; };

    using A = PredicateNode<int, U, decltype(p1), ℵ_0>;
    using B = PredicateNode<int, U, decltype(p2), ℵ_0>;

    // Now A and B are DIFFERENT types. Idempotency will be skipped.
    using Raw = decltype(!(std::declval<A>() & std::declval<B>()));
    using Normalized = decltype(normalize(std::declval<Raw>()));
    using Expected = decltype(!std::declval<A>() | !std::declval<B>());

    // This should now be a UnionNode!
    STATIC_REQUIRE(std::is_same_v<Normalized, Expected>);
  }
}
