/** @file test/cpp/modules/dedekind/category/total_test.cpp */
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <functional>
#include <type_traits>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE("Total: The Path to Symmetry (Table 2)",
          "[category][total][algebra]") {
  SECTION("The Ideal: Unsigned Integers (Z/2^nZ)") {
    // Table 2: uint addition is marked 'M' (Modular success)
    // Verify maturation from Magma up to Abelian Group
    STATIC_CHECK(IsMagma<unsigned int, std::plus<unsigned int>>);
    STATIC_CHECK(IsSemigroup<unsigned int, std::plus<unsigned int>>);
    STATIC_CHECK(IsMonoid<unsigned int, std::plus<unsigned int>>);
    STATIC_CHECK(IsGroup<unsigned int, std::plus<unsigned int>>);
    STATIC_CHECK(IsAbelianGroup<unsigned int, std::plus<unsigned int>>);

    // Multiplicative Monoid (No integer inverse for 2)
    STATIC_CHECK(
        IsCommutativeMonoid<unsigned int, std::multiplies<unsigned int>>);
    STATIC_CHECK_FALSE(IsGroup<unsigned int, std::multiplies<unsigned int>>);
  }

  SECTION("The Honest Rejection: Signed Integers") {
    // Table 2: int + is marked 'HzU' (Undefined Behavior / Overflow)
    // Rejection Check: A species with UB is NOT a Total Magma.
    STATIC_CHECK_FALSE(IsMagma<int, std::plus<int>>);

    // Identity (0) is safe and discoverable, but Associativity is rejected
    STATIC_CHECK(IsPointed<int, std::plus<int>>);
    STATIC_CHECK_FALSE(IsAssociative<int, std::plus<int>>);
  }

  SECTION("Multi-operation Species: Rigs & Rings") {
    // bool (OR/AND) -> Table 2 'Logic' row
    // Valid Rig (Semiring), but OR has no inverse (Not a Ring)
    STATIC_CHECK(IsRig<bool, std::logical_or<bool>, std::logical_and<bool>>);
    STATIC_CHECK_FALSE(
        IsRing<bool, std::logical_or<bool>, std::logical_and<bool>>);

    // uint (Modular +, *) -> Valid Ring
    STATIC_CHECK(IsRing<unsigned int, std::plus<unsigned int>,
                        std::multiplies<unsigned int>>);
  }

  SECTION("Pointed factories: zero<A,B,Op>() and unit<A,B,Op>() at runtime") {
    // Runtime witnesses for the @c IsPointed-gated constant-morphism factories
    // re-homed from @c :discrete to @c :total under #637.  Compile-time
    // witnesses are pinned alongside the definitions in @c :total; this section
    // exercises the function bodies at runtime so coverage instrumentation
    // sees the move.

    // Additive zero: maps everything to identity_v<int, std::plus<int>> = 0.
    auto z_int = zero<int, int, std::plus<int>>();
    CHECK(z_int(0) == 0);
    CHECK(z_int(7) == 0);
    CHECK(z_int(-42) == 0);

    // Multiplicative unit: maps everything to identity_v<int,
    // std::multiplies<int>> = 1.
    auto u_int = unit<int, int, std::multiplies<int>>();
    CHECK(u_int(0) == 1);
    CHECK(u_int(99) == 1);
    CHECK(u_int(-7) == 1);

    // Cross-codomain: A=double, B=int, Op=std::plus<int>.
    auto z_dbl_to_int = zero<double, int, std::plus<int>>();
    CHECK(z_dbl_to_int(3.14) == 0);
    CHECK(z_dbl_to_int(-1.5) == 0);

    // Boolean multiplicative unit: identity_v<bool, std::logical_and<bool>> =
    // true.
    auto u_int_to_bool = unit<int, bool, std::logical_and<bool>>();
    CHECK(u_int_to_bool(0) == true);
    CHECK(u_int_to_bool(123) == true);
  }
}

TEST_CASE("Total: Lattice Structures (Relational Presence)",
          "[category][total][lattice]") {
  SECTION("Boolean Distributive Lattice") {
    STATIC_CHECK(IsJoinSemilattice<bool, std::logical_or<bool>>);
    STATIC_CHECK(IsMeetSemilattice<bool, std::logical_and<bool>>);
    STATIC_CHECK(IsDistributiveLattice<bool, std::logical_or<bool>,
                                       std::logical_and<bool>>);
  }

  SECTION("Boolean Algebra: top of the expanded signature Alg(∨,∧,¬) (#809)") {
    // 𝔹 = bool is the initial Boolean algebra.  Note the signature: bounded +
    // distributive is the top WITHIN Alg(∧,∨), but the complement ¬ (and bounds
    // ⊥/⊤) EXPAND the signature to Alg(∨,∧,¬,⊥,⊤) --- IsBooleanAlgebra tops
    // that expanded signature, not pure Alg(∧,∨).  Mirrors 𝔽₂ = bool topping
    // the (signature-expanded) ring line at IsField.
    STATIC_CHECK(
        IsBoundedLattice<bool, std::logical_or<bool>, std::logical_and<bool>>);
    STATIC_CHECK(
        IsBooleanAlgebra<bool, std::logical_or<bool>, std::logical_and<bool>,
                         std::logical_not<bool>>);

    // Runtime companion for the complement law (Codecov-visible; the
    // static witness is invisible to coverage): a ∨ ¬a = ⊤, a ∧ ¬a = ⊥
    // over the whole carrier {false, true}.
    for (bool a : {false, true}) {
      CHECK(std::logical_or<bool>{}(a, std::logical_not<bool>{}(a)) == true);
      CHECK(std::logical_and<bool>{}(a, std::logical_not<bool>{}(a)) == false);
    }
  }

  SECTION("Order Lattices (Total Order Species)") {
    // Value-returning :species lattice ops (join = Sup, meet = Inf).
    using Max = Sup;
    using Min = Inf;

    // Verify structural properties of the operations
    STATIC_CHECK(IsSemilattice<int, Max>);
    STATIC_CHECK(IsSemilattice<int, Min>);
  }
}

TEST_CASE(
    "Total: composition is a monoid --- (IsArrow, >>, id()) (#961 review)",
    "[category][total][monoid][composition][961]") {
  // The endomorphisms End(A) of one object A, under composition @c >> with
  // unit @c id<A>(), form a MONOID --- the one-object-category reading of
  // @c IsMonoid.  The two arrows below are members of End(int).
  const auto inc = endo<int>([](int x) { return x + 1; });
  const auto dbl = endo<int>([](int x) { return x * 2; });
  const auto neg = endo<int>([](int x) { return -x; });
  const auto e = id<int>();  // the monoid unit

  // The carrier and the unit are arrows; @c >> is closed on End(int).
  STATIC_CHECK(IsArrow<decltype(e)>);
  STATIC_CHECK(IsArrow<decltype(inc)>);
  STATIC_CHECK(IsArrow<decltype(inc >> dbl)>);
  STATIC_CHECK(std::same_as<Dom<decltype(inc >> dbl)>, int>);
  STATIC_CHECK(std::same_as<Cod<decltype(inc >> dbl)>, int>);

  // NOTE on placement: @c IsMonoid<T, Op> in @c :total is the @b
  // set-indexed monoid --- a single value carrier @c T with a binary @c Op.
  // The composition monoid is @b type-indexed (one-object category): its
  // hom-set End(int) is a SET of arrows of heterogeneous C++ type (each
  // lambda has its own type), not one carrier type, so @c IsMonoid<T, Op>
  // does not fire on it directly.  The monoid laws are therefore witnessed
  // OPERATIONALLY here --- the type-vs-set-indexed boundary made visible.
  for (int x : {-3, 0, 1, 7}) {
    // Associativity: (inc >> dbl) >> neg = inc >> (dbl >> neg).
    CHECK(((inc >> dbl) >> neg)(x) == (inc >> (dbl >> neg))(x));
    // Left / right unit: id >> f = f = f >> id.
    CHECK((e >> inc)(x) == inc(x));
    CHECK((inc >> e)(x) == inc(x));
  }

  // The unit law is exactly what @c cata (@c :f_algebra) normalises away:
  // @c cata drops the unit leg, so the reducer's β IS this monoid law.
  const auto reduced_left = cata(e >> inc);
  const auto reduced_right = cata(inc >> e);
  STATIC_CHECK(std::same_as<std::remove_cvref_t<decltype(reduced_left)>,
                            std::remove_cvref_t<decltype(inc)>>);
  STATIC_CHECK(std::same_as<std::remove_cvref_t<decltype(reduced_right)>,
                            std::remove_cvref_t<decltype(inc)>>);
  CHECK(reduced_left(5) == inc(5));
  CHECK(reduced_right(5) == inc(5));
}