#include <catch2/catch_test_macros.hpp>

import dedekind.category;
import dedekind.sets;

using namespace dedekind::category;
using namespace dedekind::sets;

TEST_CASE("Sets+Category: boundaries lift into ETCS subobjects",
          "[sets][category][etcs][integration]") {
  const Ω<int> universe{};
  const Ø<int> empty{};

  const auto u_set = ambient_set<int>(universe);
  const auto e_set = ambient_set<int>(empty);

  STATIC_CHECK(dedekind::category::IsSet<decltype(u_set)>);
  STATIC_CHECK(dedekind::category::IsSet<decltype(e_set)>);
  STATIC_CHECK(std::same_as<Cod<decltype(u_set.χ)>, bool>);
  STATIC_CHECK(std::same_as<Cod<decltype(e_set.χ)>, bool>);

  CHECK(u_set.χ(7) == true);
  CHECK(u_set.χ(-7) == true);
  CHECK(e_set.χ(7) == false);
  CHECK(e_set.χ(-7) == false);
}

TEST_CASE("Sets+Category: singleton and comprehension predicates satisfy ETCS",
          "[sets][category][etcs][integration]") {
  const auto atom = singleton(42);
  const auto atom_set = ambient_set<int>(atom);

  STATIC_CHECK(dedekind::category::IsSet<decltype(atom_set)>);
  CHECK(atom_set.χ(42) == true);
  CHECK(atom_set.χ(7) == false);

  auto x = var<ℕ>;
  const auto positive = Set{x % N | (x > 0u)};
  const auto bounded = Set{x % N | (x <= 10u)};

  // ambient_set<ℕ> lifts the predicate-set into an unsigned-int
  // ambient (matching the post-#401 ℕ = unsigned int carrier).  The
  // characteristic-function χ then reads on unsigned values.
  const auto positive_set = ambient_set<ℕ>(positive);
  const auto bounded_set = ambient_set<ℕ>(bounded);
  const auto support = set_intersection(positive_set, bounded_set);

  STATIC_CHECK(dedekind::category::IsSet<decltype(positive_set)>);
  STATIC_CHECK(dedekind::category::HasTernarySupport<decltype(positive_set)>);
  STATIC_CHECK(dedekind::category::HasTernarySupport<decltype(support)>);

  CHECK(positive_set.χ(5u) == Ternary::True);
  CHECK(positive_set.χ(0u) == Ternary::False);
  CHECK(support.χ(5u) == Ternary::True);
  CHECK(support.χ(50u) == Ternary::False);
}

TEST_CASE("Sets+Category: Set naming boundary is explicit",
          "[sets][category][etcs][alignment]") {
  auto x = var<ℕ>;
  const auto positive = Set{x % N | (x > 0u)};

  // `sets::Set` (DSL species) and `category::Set` (CCC witness) are distinct.
  STATIC_CHECK(!std::same_as<decltype(positive),
                             dedekind::category::CanonicalSetCCC<ℕ>>);
  STATIC_CHECK(dedekind::category::HasCanonicalSetCCC<ℕ>);

  // Bridge through ETCS object construction over the post-#401 ℕ carrier.
  const auto positive_set = ambient_set<ℕ>(positive);
  STATIC_CHECK(dedekind::category::IsSetInCanonicalCCC<decltype(positive_set)>);

  CHECK(positive_set.χ(3u) == Ternary::True);
  CHECK(positive_set.χ(0u) == Ternary::False);
}
