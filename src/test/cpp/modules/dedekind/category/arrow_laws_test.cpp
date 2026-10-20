/**
 * @file arrow_laws_test.cpp
 * @brief The laws of an arrow, one engine, two readers (#1049): by type at
 *        compile time (the traits on @c Compose, a @c cata with
 *        @c LawAlgebra), by value at runtime (a fold over a pipeline built
 *        from input: @c std::accumulate, the free monoid's fold).
 */
#include <catch2/catch_test_macros.hpp>
#include <functional>
#include <numeric>
#include <vector>

import dedekind.category;
using namespace dedekind::category;

namespace {
using Negation = NegationArrow<Boole>;
using Same = Identity<bool>;

constexpr ArrowLaws profile_of_not = arrow_laws_v<Negation>;
constexpr ArrowLaws profile_of_id = arrow_laws_v<Same>;

/** @brief A step of a pipeline built at runtime. */
enum class Step { Id, Not };
ArrowLaws laws_of_step(Step s) {
  return s == Step::Not ? profile_of_not : profile_of_id;
}
ArrowLaws compose_laws(const ArrowLaws& so_far, Step next) {
  return arrow_node(ArrowNode::Compose, so_far, laws_of_step(next));
}
ArrowLaws laws_of_pipeline(const std::vector<Step>& steps) {
  return std::accumulate(steps.begin(), steps.end(), profile_of_id,
                         compose_laws);
}
}  // namespace

TEST_CASE("arrow laws: the leaves, declared", "[category][laws]") {
  STATIC_CHECK(profile_of_not[ArrowLaw::Antitone] == Ternary::True);
  STATIC_CHECK(profile_of_id[ArrowLaw::Monotone] == Ternary::True);
  STATIC_CHECK(profile_of_id[ArrowLaw::Injective] == Ternary::True);
}

TEST_CASE("arrow laws: reader 1, by type at compile time", "[category][laws]") {
  // The traits on Compose read the engine: the sign table.
  STATIC_CHECK(IsMonotone<Compose<Negation, Negation>>);  // anti ∘ anti
  STATIC_CHECK(IsAntiMonotone<Compose<Same, Negation>>);  // mono ∘ anti
  STATIC_CHECK(!IsMonotone<Compose<Same, Negation>>);
  // And a cata with the law algebra, the same engine as a fold.
  constexpr ArrowLaws twice =
      cata(LawAlgebra<>{}, Compose<Negation, Negation>{Negation{}, Negation{}});
  STATIC_CHECK(twice[ArrowLaw::Monotone] == Ternary::True);
  STATIC_CHECK(twice[ArrowLaw::Antitone] == Ternary::Unknown);
}

TEST_CASE("arrow laws: reader 2, by value at runtime", "[category][laws]") {
  // The same arrows composed from data: the free monoid on the steps, folded.
  CHECK(laws_of_pipeline({Step::Not, Step::Not})[ArrowLaw::Monotone] ==
        Ternary::True);
  CHECK(laws_of_pipeline({Step::Not})[ArrowLaw::Antitone] == Ternary::True);
  CHECK(laws_of_pipeline({Step::Not, Step::Not,
                          Step::Not})[ArrowLaw::Antitone] == Ternary::True);
  CHECK(laws_of_pipeline({})[ArrowLaw::Monotone] == Ternary::True);  // id
  // The two readers agree.
  CHECK(laws_of_pipeline({Step::Not, Step::Not}) ==
        cata(LawAlgebra<>{},
             Compose<Negation, Negation>{Negation{}, Negation{}}));
}

TEST_CASE("arrow laws: the formers, × + ⟨,⟩ [,]", "[category][laws]") {
  using Pair = ProductArrow<Negation, Negation>;
  // By type: the readers on the formers.
  STATIC_CHECK(IsMonicArrow<ProductArrow<Same, Same>>);
  STATIC_CHECK(IsEpicArrow<Copairing<Same, Same>>);
  STATIC_CHECK(
      IsAntiMonotone<Pair, ProductLeq<std::less_equal<>, std::less_equal<>>>);
  // By fold: cata reaches through a former and a composition.
  constexpr ArrowLaws through =
      cata(LawAlgebra<>{}, Compose<Pair, Pair>{Pair{}, Pair{}});
  STATIC_CHECK(through[ArrowLaw::Antitone] == Ternary::Unknown);
  constexpr ArrowLaws pair = cata(LawAlgebra<>{}, Pair{});
  STATIC_CHECK(pair[ArrowLaw::Antitone] == Ternary::True);  // anti × anti
  CHECK(Pair{}({true, false}) == std::pair{false, true});
}
