#include <catch2/catch_test_macros.hpp>

import dedekind.category;
import dedekind.sets;

using namespace dedekind::category;
using namespace dedekind::sets;

TEST_CASE("Sets+Category: boundaries lift into ETCS subobjects",
          "[sets][category][etcs][integration]") {
  const UniversalSet<int> universe{};
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

  auto x = element<Ω<Cardinality>>;
  const auto positive = Set{x | (x > 0u)};
  const auto bounded = Set{x | (x <= 10u)};

  // ambient_set<ℕ> lifts the predicate-set into the post-#402 variant
  // ℕ-proxy ambient (ℕ = Cardinality).  The characteristic-function χ
  // then reads on Cardinality values; unsigned literals like @c 5u /
  // @c 0u lift implicitly into the variant's finite alternative on the
  // call site.
  const auto positive_set = ambient_set<Cardinality>(positive);
  const auto bounded_set = ambient_set<Cardinality>(bounded);
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
  auto x = element<Ω<Cardinality>>;
  const auto positive = Set{x | (x > 0u)};

  // `sets::Set` (DSL species) and `category::Set` (CCC witness) are distinct.
  STATIC_CHECK(!std::same_as<decltype(positive),
                             dedekind::category::CanonicalSetCCC<Cardinality>>);
  STATIC_CHECK(dedekind::category::HasCanonicalSetCCC<Cardinality>);

  // Bridge through ETCS object construction over the post-#402 ℕ carrier
  // (ℕ = Cardinality, the variant ℕ-proxy).
  const auto positive_set = ambient_set<Cardinality>(positive);
  STATIC_CHECK(dedekind::category::IsSetInCanonicalCCC<decltype(positive_set)>);

  CHECK(positive_set.χ(3u) == Ternary::True);
  CHECK(positive_set.χ(0u) == Ternary::False);
}
