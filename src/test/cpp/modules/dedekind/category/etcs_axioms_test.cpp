/** @file test/cpp/modules/dedekind/category/etcs_axioms_test.cpp */
#include <catch2/catch_test_macros.hpp>
#include <functional>
#include <type_traits>
#include <utility>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE("ETCS axioms: 1 and 2 (composition and identity)",
          "[category][etcs][axioms]") {
  auto f = arrow<int>([](int x) { return x + 1; });
  auto g = arrow<int>([](int x) { return x * 2; });

  auto composite = f >> g;
  STATIC_CHECK(IsArrow<decltype(composite)>);
  CHECK(composite(3) == 8);

  auto i = id<int>();
  CHECK((i >> f)(7) == f(7));
  CHECK((f >> i)(7) == f(7));
}

TEST_CASE("ETCS axioms: 3 and 8 (terminal and initial objects)",
          "[category][etcs][axioms]") {
  STATIC_CHECK(IsTerminalObject<One>);
  STATIC_CHECK(std::same_as<One, std::monostate>);
  STATIC_CHECK(IsInitialObject<Zero>);
  STATIC_CHECK(std::same_as<Zero, std::nullptr_t>);

  auto u = unit<int>();
  CHECK(u(42) == One{});

  STATIC_CHECK(HasUniqueMorphismTo<int, One>);
  STATIC_CHECK(HasUniqueMorphismFrom<Zero, int>);
}

TEST_CASE("ETCS axioms: 4, 7, 10 (well-pointed eval, classifier, lattice)",
          "[category][etcs][axioms]") {
  const auto s_even = ambient_set<int>([](const int& x) { return x % 2 == 0; });
  const auto s_positive = ambient_set<int>([](const int& x) { return x > 0; });

  STATIC_CHECK(IsSubobject<decltype(s_even), int>);
  STATIC_CHECK(IsSet<decltype(s_even)>);
  STATIC_CHECK(std::same_as<Cod<decltype(s_even.χ)>, bool>);

  CHECK(s_even.χ(2));
  CHECK_FALSE(s_even.χ(3));

  const auto m = meet(s_even, s_positive);
  const auto j = join(s_even, s_positive);
  CHECK(m.χ(4));
  CHECK_FALSE(m.χ(-4));
  CHECK(j.χ(-4));
  CHECK_FALSE(j.χ(-3));

  // Axiom 7 includes classifier constants and pullback-backed stability.
  STATIC_CHECK(HasAxiom7SubobjectClassifier<decltype(s_even)>);

  // Axiom 10 (choice) currently has an explicit split-epi witness surface.
  STATIC_CHECK(HasAxiom10ChoiceSplitEpicWitness<decltype(s_even), Identity<int>,
                                                Identity<int>>);
}

TEST_CASE("ETCS axioms: 5 and 6 (product and exponentials)",
          "[category][etcs][axioms]") {
  STATIC_CHECK(IsProduct<std::pair<int, bool>, int, bool>);
  STATIC_CHECK(IsExponential<std::function<int(int)>, int, int>);

  const auto add_one = exponential<int, int>([](int x) { return x + 1; });
  STATIC_CHECK(IsArrow<decltype(add_one)>);
  CHECK(add_one(10) == 11);
}

TEST_CASE("ETCS axiom: 9 (NNO witness in species atlas)",
          "[category][etcs][axioms]") {
  STATIC_CHECK(IsSpecies<unsigned>);
}

TEST_CASE("ETCS: IsSet exposes the grouped axiom witnesses",
          "[category][etcs][axioms][visibility]") {
  const auto s = ambient_set<int>([](const int& x) { return x >= 0; });

  // Lower-level promises reused by ETCS (keeps the proof spine visible).
  STATIC_CHECK(IsArrow<Identity<int>>);
  STATIC_CHECK(IsTerminalObject<One>);
  STATIC_CHECK(IsInitialObject<Zero>);
  STATIC_CHECK(IsProduct<std::pair<int, int>, int, int>);
  STATIC_CHECK(IsExponential<Exponential<int, int>, int, int>);
  STATIC_CHECK(IsSubobject<decltype(s), int>);

  STATIC_CHECK(HasAxiom1Composition<int>);
  STATIC_CHECK(HasAxiom2Identity<int>);
  STATIC_CHECK(HasAxiom3TerminalObject<int>);
  STATIC_CHECK(HasAxiom4WellPointedness<decltype(s)>);
  STATIC_CHECK(HasAxiom5CartesianProduct<int>);
  STATIC_CHECK(HasAxiom6Exponentiation<int>);
  STATIC_CHECK(HasAxiom7SubobjectClassifier<decltype(s)>);
  STATIC_CHECK(HasAxiom8EmptySet<int>);
  STATIC_CHECK(HasAxiom9NNO<int>);
  STATIC_CHECK(HasAxiom10PowerObjectLattice<decltype(s)>);

  STATIC_CHECK(IsSet<decltype(s)>);
}

TEST_CASE("ETCS axiom 10: split-epi witness surface is explicit",
          "[category][etcs][axioms][choice]") {
  const auto s = ambient_set<int>([](const int& x) { return x >= 0; });

  // Positive witness: identity is epic and provides its own section.
  STATIC_CHECK(IsSplitEpicPair<Identity<int>, Identity<int>>);
  STATIC_CHECK(HasAxiom10ChoiceSplitEpicWitness<decltype(s), Identity<int>,
                                                Identity<int>>);

  // Negative witness: plain arrows are not epic unless explicitly declared.
  auto plus_one = arrow<int, int>([](const int& x) { return x + 1; });
  STATIC_CHECK_FALSE(IsSplitEpicPair<decltype(plus_one), Identity<int>>);
  STATIC_CHECK_FALSE(
      HasAxiom10ChoiceSplitEpicWitness<decltype(s), decltype(plus_one),
                                       Identity<int>>);
}

TEST_CASE("ETCS axiom 7: naturality witness can be attached explicitly",
          "[category][etcs][axioms][naturality]") {
  using IntCat = DiscreteCategory<int>;
  using IdF = identity_functor<IntCat>;

  struct IdentityComponent {
    auto operator()(int) const { return id<int>(); }
  };

  const auto s = ambient_set<int>([](const int& x) { return x >= 0; });

  STATIC_CHECK(
      HasAxiom7ClassifierNaturalityWitness<decltype(s), IdentityComponent, IdF,
                                           IdF>);
}
