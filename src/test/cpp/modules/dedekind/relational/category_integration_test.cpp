#include <catch2/catch_test_macros.hpp>

import dedekind.category;
import dedekind.sets;
import dedekind.relational;

using namespace dedekind::category;
using namespace dedekind::sets;

TEST_CASE("Sets+Category: boundaries lift into ETCS subobjects",
          "[sets][category][etcs][integration]") {
  const 𝔸<int> universe{};
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

  // A :relational test may not import :order, so π/fix is unavailable; use
  // named predicates (comprehension form) rather than inline lambdas.
  constexpr auto gt_zero = [](const auto& v) { return v > Numeral{0u}; };
  constexpr auto le_ten = [](const auto& v) { return v <= Numeral{10u}; };
  const auto positive = Comprehension{ℕ, gt_zero};
  const auto bounded = Comprehension{ℕ, le_ten};

  // ambient_set<Numeral> lifts the predicate-set into ℕ's ambient
  // (ℕ = 𝔸<Numeral>).  The characteristic-function χ then reads on
  // Numeral values.
  const auto positive_set = ambient_set<Numeral>(positive);
  const auto bounded_set = ambient_set<Numeral>(bounded);
  const auto support = positive_set & bounded_set;

  STATIC_CHECK(dedekind::category::IsSet<decltype(positive_set)>);
  // Post-#622 (cardinality cut): ℕ → Boole on the carrier axis,
  // so the sets-DSL Sets @c positive / @c bounded are decidable on the
  // carrier-axis fast path.  (Pre-#622 these were @c HasTernarySupport
  // assertions on the @c ambient_set-lifted wrappers.)
  STATIC_CHECK(dedekind::sets::HasDecidableMembership<decltype(positive)>);
  STATIC_CHECK(dedekind::sets::HasDecidableMembership<decltype(bounded)>);

  CHECK(positive_set.χ(Numeral{5u}));
  CHECK_FALSE(positive_set.χ(Numeral{0u}));
  CHECK(support(Numeral{5u}));
  CHECK_FALSE(support(Numeral{50u}));
}

TEST_CASE("Sets+Category: Set naming boundary is explicit",
          "[sets][category][etcs][alignment]") {
  constexpr auto gt_zero = [](const auto& v) { return v > Numeral{0u}; };
  const auto positive = Comprehension{ℕ, gt_zero};

  // `sets::Set` (DSL species) and `category::Set` (CCC witness) are distinct.
  STATIC_CHECK(!std::same_as<decltype(positive),
                             dedekind::category::CanonicalSetCCC<Numeral>>);

  // Bridge through ETCS object construction over ℕ's carrier, Numeral.
  const auto positive_set = ambient_set<Numeral>(positive);
  STATIC_CHECK(IsSet<decltype(positive_set)>);

  CHECK(positive_set.χ(Numeral{3u}));
  CHECK_FALSE(positive_set.χ(Numeral{0u}));
}
