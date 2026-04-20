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

TEST_CASE("ETCS axiom 10: split-epi semantic law is checked explicitly",
          "[category][etcs][axioms][choice][law]") {
  const auto s = ambient_set<int>([](const int& x) { return x >= 0; });

  STATIC_CHECK(HasAxiom10ChoiceSplitEpicLawSurface<decltype(s), Identity<int>,
                                                   Identity<int>>);
  CHECK(split_epi_section_law_at(Identity<int>{}, Identity<int>{}, 0));
  CHECK(split_epi_section_law_at(Identity<int>{}, Identity<int>{}, 7));

  auto bad_section = arrow<int, int>([](const int& x) { return x + 1; });
  STATIC_CHECK(IsSplitEpicPair<Identity<int>, decltype(bad_section)>);
  CHECK_FALSE(split_epi_section_law_at(Identity<int>{}, bad_section, 0));
}

TEST_CASE("ETCS axiom 10: law surface with custom epi and section",
          "[category][etcs][axioms][choice][law][custom]") {
  // Double identity should preserve the law
  CHECK(split_epi_section_law_at(Identity<double>{}, Identity<double>{}, 1.5));
  CHECK(split_epi_section_law_at(Identity<double>{}, Identity<double>{}, -3.2));

  // Bool identity
  CHECK(split_epi_section_law_at(Identity<bool>{}, Identity<bool>{}, true));
  CHECK(split_epi_section_law_at(Identity<bool>{}, Identity<bool>{}, false));
}

TEST_CASE("ETCS axiom 10: law surface concept strictness",
          "[category][etcs][axioms][choice][law][strictness]") {
  const auto s = ambient_set<int>([](const int& x) { return x % 2 == 0; });

  // HasAxiom10ChoiceSplitEpicLawSurface requires both the witness shape
  // AND std::equality_comparable codomain
  STATIC_CHECK(HasAxiom10ChoiceSplitEpicLawSurface<decltype(s), Identity<int>,
                                                   Identity<int>>);

  // This should still be true even though the section would violate the law
  // (the concept only checks shape + equality, not law truth itself)
  auto bad_section = arrow<int, int>([](const int x) { return x + 1; });
  STATIC_CHECK(IsSplitEpicPair<Identity<int>, decltype(bad_section)>);
  STATIC_CHECK(HasAxiom10ChoiceSplitEpicLawSurface<decltype(s), Identity<int>,
                                                   decltype(bad_section)>);

  // But the law itself fails:
  CHECK_FALSE(split_epi_section_law_at(Identity<int>{}, bad_section, 0));
  CHECK_FALSE(split_epi_section_law_at(Identity<int>{}, bad_section, 5));
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

TEST_CASE("ETCS axiom 7: pullback reindexing definitional witness surface",
                  "[category][etcs][axioms][reindexing][definitional]") {
  const auto s = ambient_set<int>([](const int& x) { return x % 2 == 0; });
  auto embed = arrow<bool, int>([](const bool b) { return b ? 2 : 3; });
  const auto pulled_back = ambient_set<bool>([&](const bool& b) {
        return s.χ(embed(b));
  });

  STATIC_CHECK(
          HasAxiom7PullbackReindexingDefinitionalSurface<decltype(s),
                                                                                                         decltype(embed)>);

  CHECK(pulled_back.χ(true));
  CHECK_FALSE(pulled_back.χ(false));
  CHECK(in_via(true, embed, s) == pulled_back.χ(true));
  CHECK(in_via(false, embed, s) == pulled_back.χ(false));

  CHECK(classifier_reindexing_definitional_witness_at(s, embed, true));
  CHECK(classifier_reindexing_definitional_witness_at(s, embed, false));

  auto bad_embed =
      arrow<int, double>([](const int x) { return static_cast<double>(x); });
  STATIC_CHECK_FALSE(
          HasAxiom7PullbackReindexingDefinitionalSurface<decltype(s),
                                                                                                         decltype(bad_embed)>);
}

TEST_CASE("ETCS axiom 7: definitional surface with composite predicates",
                  "[category][etcs][axioms][reindexing][definitional][composite]") {
  // Composite predicate: x > 0 AND x % 3 == 0
  const auto s_composite =
      ambient_set<int>([](const int& x) { return x > 0 && x % 3 == 0; });
  auto scale = arrow<int, int>([](const int x) { return x * 2; });

        STATIC_CHECK(HasAxiom7PullbackReindexingDefinitionalSurface<
                                                         decltype(s_composite), decltype(scale)>);

        CHECK(in_via(2, scale, s_composite) == false);  // 4 is not div 3
        CHECK(in_via(3, scale, s_composite));           // 6 is div 3
        CHECK_FALSE(in_via(-1, scale, s_composite));    // negative

        CHECK(classifier_reindexing_definitional_witness_at(s_composite, scale, 2));
        CHECK(classifier_reindexing_definitional_witness_at(s_composite, scale, 3));
        CHECK(classifier_reindexing_definitional_witness_at(s_composite, scale, -1));
}

TEST_CASE("ETCS axiom 7: definitional surface across different source types",
                                        "[category][etcs][axioms][reindexing][definitional][type-variation]") {
  const auto s_bool = ambient_set<int>([](const int& x) { return x > 0; });

  // Embed from double domain
  auto from_double =
      arrow<double, int>([](const double d) { return static_cast<int>(d); });
        STATIC_CHECK(HasAxiom7PullbackReindexingDefinitionalSurface<
                                                         decltype(s_bool), decltype(from_double)>);
        CHECK(in_via(1.5, from_double, s_bool));
        CHECK_FALSE(in_via(-2.7, from_double, s_bool));
        CHECK(classifier_reindexing_definitional_witness_at(s_bool, from_double,
                                                                                                                                                                                                                        1.5));
        CHECK(classifier_reindexing_definitional_witness_at(s_bool, from_double,
                                                                                                                                                                                                                        -2.7));

  // Embed from unsigned domain
  auto from_unsigned = arrow<unsigned, int>(
      [](const unsigned u) { return static_cast<int>(u); });
        STATIC_CHECK(HasAxiom7PullbackReindexingDefinitionalSurface<
                                                         decltype(s_bool), decltype(from_unsigned)>);
        CHECK(in_via(5U, from_unsigned, s_bool));
        CHECK(classifier_reindexing_definitional_witness_at(s_bool, from_unsigned,
                                                                                                                                                                                                                        5U));
}
