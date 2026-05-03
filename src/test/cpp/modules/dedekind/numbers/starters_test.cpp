#include <catch2/catch_test_macros.hpp>

import dedekind.category;
import dedekind.numbers;
import dedekind.sets;

using namespace dedekind::category;
using namespace dedekind::numbers;
using namespace dedekind::sets;

TEST_CASE("Numbers: canonical starter symbols", "[numbers][starter]") {
  // Per #551 (Ω<carrier> ambient redesign), the canonical species symbols
  // are pure carrier types; the value-level *ambient* at each carrier is
  // the universal-predicate value @c Ω<carrier>, of type
  // @c UniversalSet<carrier>.  The schism between carrier-type and
  // ambient-value is now uniform and minimal: one carrier, one
  // @c Ω<carrier> value, no per-symbol predicate-set type to remember.

  STATIC_CHECK(std::same_as<Cardinality, Cardinality>);
  STATIC_CHECK(
      std::same_as<std::remove_cvref_t<decltype(Ω<Cardinality>)>, UniversalSet<Cardinality>>);

  STATIC_CHECK(std::same_as<ℤ, SignedExtensionalCardinal<>>);
  STATIC_CHECK(
      std::same_as<std::remove_cvref_t<decltype(Ω<ℤ>)>, UniversalSet<ℤ>>);

  STATIC_CHECK(std::same_as<ℚ, Rational<default_integer>>);
  STATIC_CHECK(
      std::same_as<std::remove_cvref_t<decltype(Ω<ℚ>)>, UniversalSet<ℚ>>);

  // 𝔻 / D / DualSet starter aliases moved to dedekind.analysis:dual at
  // PR ; analogous STATIC_CHECKs live in
  // src/test/cpp/modules/dedekind/analysis/dual_test.cpp.
}

TEST_CASE("Numbers: starter universes construct from ambient values",
          "[numbers][starter][sets]") {
  // Per #551, the new spelling is:
  //   constexpr auto n = element<Ω<ℕ>>;     // typed scout
  //   constexpr auto naturals = Set{n};      // universal Set on ℕ
  //   static_assert(naturals.contains(7u));  // value-level membership query
  //
  // The `% N` binding step is gone: the scout already knows its ambient.

  constexpr auto n = element<Ω<Cardinality>>;
  constexpr auto naturals = Set{n};
  static_assert(naturals(7u) == Ternary::True);
  static_assert(naturals(0u) == Ternary::True);
  // Direct ambient-call route: Ω<ℕ>.contains(value) returns L::True for
  // every Cardinality value (the universal-set semantics).
  static_assert(Ω<Cardinality>.contains(7u));
  static_assert(Ω<Cardinality>.contains(0u));

  constexpr auto z = element<Ω<ℤ>>;
  constexpr auto integers = Set{z};
  static_assert(integers(-7) == Ternary::True);

  // 𝔻 starter-universe construction moved to
  // src/test/cpp/modules/dedekind/analysis/dual_test.cpp at PR #513
  // (Dual<F> relocated from :numbers to :analysis).
}

TEST_CASE("Numbers: starter universes satisfy lattice identities",
          "[numbers][starter][algebra]") {
  {
    constexpr auto n = element<Ω<Cardinality>>;
    const auto U = Set{n};
    const auto O = !U;
    CHECK((U | O)(7u) == Ternary::True);
    CHECK((U & O)(7u) == Ternary::False);
    CHECK((U | O)(0u) == Ternary::True);
    CHECK((U & O)(0u) == Ternary::False);
  }

  {
    constexpr auto z = element<Ω<ℤ>>;
    const auto U = Set{z};
    const auto O = !U;
    CHECK((U | O)(4) == Ternary::True);
    CHECK((U & O)(4) == Ternary::False);
  }

  // 𝔻 / D / Dual lattice-identity check moved to
  // src/test/cpp/modules/dedekind/analysis/dual_test.cpp at PR #513
  // (Dual<F> relocated from :numbers to :analysis).
}
