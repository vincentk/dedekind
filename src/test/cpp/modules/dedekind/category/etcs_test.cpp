/** @file test/cpp/modules/dedekind/category/etcs_test.cpp */
#include <catch2/catch_test_macros.hpp>
#include <ranges>
#include <set>
#include <unordered_set>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE("ETCS: std::ranges views lift to sets via ambient_set",
          "[category][etcs][sets][ranges]") {
  // A lazy view IS a set — membership is std::ranges::find (Jlt: the set is
  // its membership test).  Exercised at runtime so the lift is covered.
  const auto six = ambient_set(std::views::single(6));
  const auto small = ambient_set(std::views::iota(0, 5));

  STATIC_CHECK(IsSet<decltype(six)>);
  STATIC_CHECK(IsSet<decltype(small)>);

  CHECK(six.χ(6));
  CHECK_FALSE(six.χ(7));
  CHECK(small.χ(3));
  CHECK_FALSE(small.χ(9));
}

TEST_CASE("ETCS: primitive ambient species can be materialized as sets",
          "[category][etcs][sets][primitives]") {
  const auto int_non_negative =
      ambient_set<int>([](const int& x) { return x >= 0; });
  const auto uint_even =
      ambient_set<unsigned>([](const unsigned& x) { return (x % 2u) == 0u; });
  const auto bool_true = ambient_set<bool>([](const bool& x) { return x; });
  const auto unit_interval =
      ambient_set<double>([](const double& x) { return x >= 0.0 && x <= 1.0; });

  STATIC_CHECK(IsSet<decltype(int_non_negative)>);
  STATIC_CHECK(IsSet<decltype(uint_even)>);
  STATIC_CHECK(IsSet<decltype(bool_true)>);
  STATIC_CHECK(IsSet<decltype(unit_interval)>);

  CHECK(int_non_negative.χ(0));
  CHECK_FALSE(int_non_negative.χ(-1));
  CHECK(uint_even.χ(2u));
  CHECK_FALSE(uint_even.χ(3u));
  CHECK(bool_true.χ(true));
  CHECK_FALSE(bool_true.χ(false));
  CHECK(unit_interval.χ(0.5));
  CHECK_FALSE(unit_interval.χ(-0.1));
}

TEST_CASE("ETCS: canonical int ambient satisfies ETCS witnesses",
          "[category][etcs][axioms]") {
  using CanonicalIntSetObject =
      decltype(ambient_set<int>([](int) { return true; }));

  STATIC_CHECK(IsSet<CanonicalIntSetObject>);
  STATIC_CHECK(
      HasAxiom5CartesianProduct<typename CanonicalIntSetObject::Domain>);
  STATIC_CHECK(HasAxiom6Exponentiation<typename CanonicalIntSetObject::Domain>);

  CHECK(true);
}

// NOTE (#834): the set-LATTICE test cases (intersection/union/complement,
// meet/join aliases, ternary support, Boolean-algebra laws) moved to the
// :sets test layer (sets/etcs_lattice_test.cpp) alongside the ops themselves.
// Membership (in / in_via) is χ-evaluation and stays a :category concern, so
// the embedding-membership case below remains here.

TEST_CASE("ETCS: embedding-mediated membership avoids subset claims",
          "[category][etcs][embedding][membership]") {
  const auto integers = ambient_set<int>([](const int&) { return true; });
  const auto naturals = ambient_set<int>([](const int& x) { return x >= 0; });

  // Identity embedding int -> int as the carrier inclusion into Z's ambient.
  const auto embed_int_in_Z = arrow<int, int>([](const int& x) { return x; });

  CHECK(in_via(7, embed_int_in_Z, integers) == true);
  CHECK(in_via(-7, embed_int_in_Z, integers) == true);

  // Canonical widening embedding unsigned -> int for N-membership checks.
  const auto embed_unsigned_in_N = arrow<unsigned, int>(
      [](const unsigned& x) { return static_cast<int>(x); });

  CHECK(in(-1, naturals) == false);
  CHECK(in(7, naturals) == true);
  CHECK(in_via(7u, embed_unsigned_in_N, naturals) == true);
}

TEST_CASE("ETCS: std containers lift to IsSet directly via ambient_set (#607)",
          "[category][etcs][juliet][stdcontainer]") {
  // Slice 1 of #607's wrapper-dissolution: std::set / std::unordered_set
  // values lift to IsSet without going through a project-shipped wrapper.
  // Each overload wraps `.contains(x)` as the membership predicate.

  SECTION("std::unordered_set lvalue lift — borrows lifetime, zero copy") {
    const std::unordered_set<int> primes{2, 3, 5, 7, 11};
    const auto S = ambient_set(primes);
    STATIC_CHECK(IsSet<decltype(S)>);
    CHECK(S.χ(2));
    CHECK(S.χ(3));
    CHECK(S.χ(5));
    CHECK(S.χ(7));
    CHECK(S.χ(11));
    CHECK_FALSE(S.χ(4));
    CHECK_FALSE(S.χ(0));
    CHECK_FALSE(S.χ(-1));
  }

  SECTION("std::unordered_set rvalue lift — moves into the predicate") {
    auto S = ambient_set(std::unordered_set<int>{1, 2, 4, 8, 16});
    STATIC_CHECK(IsSet<decltype(S)>);
    CHECK(S.χ(1));
    CHECK(S.χ(2));
    CHECK(S.χ(4));
    CHECK(S.χ(8));
    CHECK(S.χ(16));
    CHECK_FALSE(S.χ(3));
    CHECK_FALSE(S.χ(7));
  }

  SECTION("std::set lvalue lift — borrows lifetime, zero copy") {
    const std::set<int> evens{0, 2, 4, 6, 8};
    const auto S = ambient_set(evens);
    STATIC_CHECK(IsSet<decltype(S)>);
    CHECK(S.χ(0));
    CHECK(S.χ(2));
    CHECK(S.χ(8));
    CHECK_FALSE(S.χ(1));
    CHECK_FALSE(S.χ(7));
  }

  SECTION("std::set rvalue lift — moves into the predicate") {
    auto S = ambient_set(std::set<int>{10, 20, 30});
    STATIC_CHECK(IsSet<decltype(S)>);
    CHECK(S.χ(10));
    CHECK(S.χ(20));
    CHECK(S.χ(30));
    CHECK_FALSE(S.χ(15));
    CHECK_FALSE(S.χ(0));
  }

  SECTION("std::unordered_set<bool>: 𝔹 as a listed extensional set") {
    const std::unordered_set<bool> B{false, true};
    const auto S = ambient_set(B);
    STATIC_CHECK(IsSet<decltype(S)>);
    CHECK(S.χ(false));
    CHECK(S.χ(true));
  }

  SECTION("Lifetime: lvalue overload tracks the underlying container") {
    // The lvalue overload captures by reference; mutating the container
    // after the lift is reflected in subsequent χ queries.  This is the
    // documented contract: caller keeps the container alive, and the
    // lifted Subobject sees its current state.
    std::unordered_set<int> evolving{1, 2, 3};
    const auto S = ambient_set(evolving);
    CHECK(S.χ(2));
    CHECK_FALSE(S.χ(4));
    evolving.insert(4);
    CHECK(S.χ(4));  // ← the lift saw the mutation
    evolving.erase(2);
    CHECK_FALSE(S.χ(2));  // ← and the deletion
  }
}
