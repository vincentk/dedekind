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
#include <concepts>  // std::same_as (dom / cod agree in type)
#include <utility>

import dedekind.sets;
import dedekind.relational;
import dedekind.category;

using namespace dedekind::sets;
using namespace dedekind::relational;
using namespace dedekind::category;  // Boole / Kleene / Ternary

TEST_CASE(
    "relation core: Relation / IsRelation / relates / is_single_valued_at",
    "[relational][relations]") {
  const auto graph_pred = [](const std::pair<int, int>& p) {
    return p.second == 2 * p.first;
  };
  const Relation<int, int, Boole, decltype(graph_pred)> R{graph_pred};

  STATIC_CHECK(IsRelation<decltype(R), int, int>);
  CHECK(relates(R, 3, 6) == true);
  CHECK(relates(R, 3, 7) == false);

  // Definition Trsk at runtime: the relation's universe is 𝔸<int × int>, and
  // dom / cod are its two projections --- the factor universes, which accept
  // every carrier value.
  CHECK(dedekind::sets::universe(R)(std::pair<int, int>{3, 7}));
  CHECK(dom(R)(3));
  CHECK(cod(R)(-7));
  STATIC_CHECK(std::same_as<decltype(dom(R)), decltype(cod(R))>);

  const SetFunction<int, int, Boole, decltype(graph_pred)> F{graph_pred};
  CHECK(is_single_valued_at(F, 3, 6, 6) == true);
  CHECK(is_single_valued_at(F, 3, 6, 7) == true);

  // The query surface relocated alongside the type (relates above; dom / cod /
  // apply here) — runtime coverage, since the partition's own witnesses are
  // static_asserts (invisible to coverage).
  // apply(R, x) = the fibre {b | (x,b) ∈ R}; here R doubles, so apply(R,3)={6}.
  // Qualified: R is a Set<std::pair<...>>, so std::pair pulls `std` into the
  // ADL set and the unconstrained libc++ `std::apply(fn, tuple)` becomes a
  // rival candidate that hard-errors on the int second argument (a
  // libc++/libstdc++ divergence; CI's libstdc++ constrains std::apply out).
  // `apply` is legacy anyway (not part of the §4 grammar); qualifying pins the
  // relational one.
  CHECK(dedekind::relational::apply(R, 3)(6));  // (3,6) ∈ R  ⇒  6 ∈ apply(R,3)
  CHECK_FALSE(dedekind::relational::apply(R, 3)(7));  // (3,7) ∉ R
  // dom / cod are the DECLARED factor universals 𝔸<A> / 𝔸<B> (total, no ∃).
  CHECK(dom(R)(42));  // declared domain is all of int
  CHECK(cod(R)(42));  // declared codomain is all of int
}

TEST_CASE("relation core: witnesses preserve ternary logic",
          "[relational][relations][logic]") {
  const auto tri_rel_pred = [](const std::pair<int, int>& p) {
    if (p.first == 3 && p.second == 6) return Ternary::Unknown;
    if (p.first == 3 && p.second == 7) return Ternary::True;
    return Ternary::False;
  };

  const Relation<int, int, Kleene, decltype(tri_rel_pred)> R{tri_rel_pred};

  // R is explicitly Kleene-parameterised, so @c relates returns
  // @c Ternary directly --- these comparisons stay Ternary-valued regardless
  // of the carrier-axis cut (#622).
  CHECK(relates(R, 3, 6) == Ternary::Unknown);
  CHECK(relates(R, 3, 7) == Ternary::True);

  const SetFunction<int, int, Kleene, decltype(tri_rel_pred)> F{tri_rel_pred};
  CHECK(is_single_valued_at(F, 3, 6, 7) == Ternary::Unknown);
}

namespace {
/** @brief A Kleene-valued relation predicate, used over a Boole-tagged base. */
struct TriRel {
  constexpr Ternary operator()(const std::pair<int, int>& p) const {
    if (p.first == 3 && p.second == 6) return Ternary::Unknown;
    if (p.first == 3 && p.second == 7) return Ternary::True;
    return Ternary::False;
  }
};
}  // namespace

TEST_CASE(
    "relation core: a Kleene answer over a Boole-tagged base is a "
    "Kleene relation (3a)",
    "[relational][relations][logic][species]") {
  // The base says Boole; the answer says Kleene; the relation is Kleene, and
  // every query answers in the relation's own species.
  const Relation<int, int, Boole, TriRel> R{TriRel{}};
  STATIC_CHECK(std::same_as<typename decltype(R)::logic_species, Kleene>);
  STATIC_CHECK(IsRelation<decltype(R), int, int>);
  CHECK(relates(R, 3, 6) == Ternary::Unknown);
  CHECK(relates(R, 3, 7) == Ternary::True);
  CHECK(relates(R, 1, 1) == Ternary::False);

  const SetFunction<int, int, Boole, TriRel> F{TriRel{}};
  CHECK(is_single_valued_at(F, 3, 6, 7) == Ternary::Unknown);
}
