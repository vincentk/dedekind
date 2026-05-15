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

  auto x = element<ℕ>;
  const auto positive = Set{x | (x > 0u)};
  const auto bounded = Set{x | (x <= 10u)};

  // ambient_set<Cardinality> lifts the predicate-set into the variant
  // ℕ-proxy ambient (the carrier of the ℕ universe; post-#402 / #559
  // ℕ = Ω<Cardinality>).  The characteristic-function χ then reads on
  // Cardinality values; unsigned literals like @c 5u / @c 0u lift
  // implicitly into the variant's finite alternative on the call site.
  const auto positive_set = ambient_set<Cardinality>(positive);
  const auto bounded_set = ambient_set<Cardinality>(bounded);
  const auto support = set_intersection(positive_set, bounded_set);

  STATIC_CHECK(dedekind::category::IsSet<decltype(positive_set)>);
  // Post-#622 (cardinality cut): ℕ → ClassicalLogic on the carrier axis,
  // so the sets-DSL Sets @c positive / @c bounded are decidable on the
  // carrier-axis fast path.  (Pre-#622 these were @c HasTernarySupport
  // assertions on the @c ambient_set-lifted wrappers.)
  STATIC_CHECK(dedekind::sets::HasDecidableMembership<decltype(positive)>);
  STATIC_CHECK(dedekind::sets::HasDecidableMembership<decltype(bounded)>);

  CHECK(positive_set.χ(5u));
  CHECK_FALSE(positive_set.χ(0u));
  CHECK(support.χ(5u));
  CHECK_FALSE(support.χ(50u));
}

TEST_CASE("Sets+Category: Set naming boundary is explicit",
          "[sets][category][etcs][alignment]") {
  auto x = element<ℕ>;
  const auto positive = Set{x | (x > 0u)};

  // `sets::Set` (DSL species) and `category::Set` (CCC witness) are distinct.
  STATIC_CHECK(!std::same_as<decltype(positive),
                             dedekind::category::CanonicalSetCCC<Cardinality>>);

  // Bridge through ETCS object construction over the post-#402 ℕ carrier
  // (ℕ = Cardinality, the variant ℕ-proxy).
  const auto positive_set = ambient_set<Cardinality>(positive);
  STATIC_CHECK(IsSet<decltype(positive_set)>);

  CHECK(positive_set.χ(3u));
  CHECK_FALSE(positive_set.χ(0u));
}
