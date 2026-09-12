/** @file test/cpp/modules/dedekind/relational/relation_core_test.cpp
 *
 * Runtime exercises for the relation CORE --- the @c Relation / @c SetFunction
 * aliases, the @c IsRelation concept, and the @c relates / @c
 * is_single_valued_at query surface.  These moved OUT of @c :sets into
 * @c dedekind.relational:dyadic (#792 follow-up, breaking the
 * sets→relational-concept layering inversion), so their tests moved here with
 * them (the test DAG imports upstream only).
 */
#include <catch2/catch_test_macros.hpp>
#include <utility>

import dedekind.sets;
import dedekind.relational;
import dedekind.category;

using namespace dedekind::sets;
using namespace dedekind::relational;
using namespace dedekind::category;  // ClassicalLogic / TernaryLogic / Ternary

TEST_CASE(
    "relation core: Relation / IsRelation / relates / is_single_valued_at",
    "[relational][relations]") {
  const auto graph_pred = [](const std::pair<int, int>& p) {
    return p.second == 2 * p.first;
  };
  const Relation<int, int, ClassicalLogic, decltype(graph_pred)> R{graph_pred};

  STATIC_CHECK(IsRelation<decltype(R), int, int>);
  CHECK(relates(R, 3, 6) == true);
  CHECK(relates(R, 3, 7) == false);

  const SetFunction<int, int, ClassicalLogic, decltype(graph_pred)> F{
      graph_pred};
  CHECK(is_single_valued_at(F, 3, 6, 6) == true);
  CHECK(is_single_valued_at(F, 3, 6, 7) == true);
}

TEST_CASE("relation core: witnesses preserve ternary logic",
          "[relational][relations][logic]") {
  const auto tri_rel_pred = [](const std::pair<int, int>& p) {
    if (p.first == 3 && p.second == 6) return Ternary::Unknown;
    if (p.first == 3 && p.second == 7) return Ternary::True;
    return Ternary::False;
  };

  const Relation<int, int, TernaryLogic, decltype(tri_rel_pred)> R{
      tri_rel_pred};

  // R is explicitly TernaryLogic-parameterised, so @c relates returns
  // @c Ternary directly --- these comparisons stay Ternary-valued regardless
  // of the carrier-axis cut (#622).
  CHECK(relates(R, 3, 6) == Ternary::Unknown);
  CHECK(relates(R, 3, 7) == Ternary::True);

  const SetFunction<int, int, TernaryLogic, decltype(tri_rel_pred)> F{
      tri_rel_pred};
  CHECK(is_single_valued_at(F, 3, 6, 7) == Ternary::Unknown);
}
