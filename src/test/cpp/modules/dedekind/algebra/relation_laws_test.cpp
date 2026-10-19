/**
 * @file dedekind/algebra/relation_laws_test.cpp
 * @brief The laws of the run-time relations (relational:rows), decided by
 *        the normal form: Rel = Kl(𝒫) (η the unit, >> associative), the
 *        dagger, the Frobenius spider R ∧ S = copy >> (R ⊗ S) >> merge, and
 *        the compact-closed iso behind table / arrow.
 *
 * relational:rows witnesses them at points (verdict); canonical is algebra's,
 * downstream of relational, so the decided form of each law lives here.  A
 * law holds when both sides' classifiers have the same normal form.
 */
#include <catch2/catch_test_macros.hpp>
#include <cstddef>

import dedekind.algebra;
import dedekind.category;
import dedekind.relational;
import dedekind.sets;

using dedekind::algebra::canonical;
using dedekind::relational::Object;
using dedekind::relational::predicate_of;
using dedekind::sets::Arith;
using dedekind::sets::Rel;
using dedekind::sets::same_shape;

namespace {

using Word = unsigned;
using Re = dedekind::relational::RelExpr<Word, Word, bool>;
using Point = dedekind::sets::Row<Word, bool>;
using Q = dedekind::sets::Pred<Word>;
using T = dedekind::sets::Term<Word>;

constexpr std::size_t word = 0;  // the sort of the word among the carriers
const Object W{word};

/** @brief A carrier's value as a constant of the grammar. */
struct Lift {
  Word operator()(Word c) const { return c; }
  Word operator()(bool b) const { return b ? 1u : 0u; }
};

/** @brief The law @c a = @c b, decided: the two classifiers' normal forms are
 *  the same tree. */
bool same(const Re& a, const Re& b) {
  return same_shape(canonical<Point>(predicate_of(a), Lift{}, a.rep()),
                    canonical<Point>(predicate_of(b), Lift{}, a.rep()));
}

const Re words = Re::universe(W, W);
const Re succ =
    (words & Q::cmp(Rel::Eq, T::proj(2),
                    T::bin(Arith::Add, T::proj(1), T::constant(1u))));
const Re square =
    (words &
     Q::cmp(Rel::Eq, T::bin(Arith::Mul, T::proj(2), T::proj(2)), T::proj(1)));
const Re three =
    (words & Q::cmp(Rel::Eq, T::proj(2),
                    T::bin(Arith::Add, T::proj(1), T::constant(3u))));

}  // namespace

TEST_CASE("relational laws: the Kleisli structure of 𝒫, decided",
          "[algebra][relational][laws]") {
  const Re eta = dedekind::relational::diag<Word, Word, bool>(W);
  CHECK(same(eta >> succ, succ));  // η the left unit
  CHECK(same(succ >> eta, succ));  // η the right unit
  // >> associative; the middles eliminated by substitution: x + 3.
  CHECK(same((succ >> succ) >> succ, succ >> (succ >> succ)));
  CHECK(same((succ >> succ) >> succ, three));
  CHECK_FALSE(same(succ, square));  // different laws stay apart
}

TEST_CASE("relational laws: the dagger, decided",
          "[algebra][relational][laws]") {
  using dedekind::relational::converse;
  CHECK(same(converse(converse(succ)), succ));  // R°° = R
}

TEST_CASE("relational laws: the Frobenius spider, decided",
          "[algebra][relational][laws]") {
  // R ∧ S = merge ∘ (R ⊗ S) ∘ copy: the meet of the cartesian bicategory.
  const Re spider = dedekind::relational::copy<Word, Word, bool>(W) >>
                    (succ * square) >>
                    dedekind::relational::merge<Word, Word, bool>(W);
  CHECK(same(spider, succ & square));
}

TEST_CASE("relational laws: table and arrow, Rel's compact-closed iso",
          "[algebra][relational][laws]") {
  // A × B ⇸ C ≅ A ⇸ B × C: the rows are the same, wherever the factors sit.
  const Re t = dedekind::relational::table(succ);
  CHECK(t.dom == Object{});
  CHECK(same(dedekind::relational::arrow(t), succ));
}

TEST_CASE("relational laws: the inferred laws agree with the normal form",
          "[algebra][relational][laws]") {
  using dedekind::relational::converse;
  using dedekind::relational::Law;
  using dedekind::relational::laws;
  const Re eta = dedekind::relational::diag<Word, Word, bool>(W);
  // Total, Δ ≤ R ; R°, as Δ ∧ (R ; R°) = Δ: inferred, and decided.
  REQUIRE(laws(succ)[Law::Total] == dedekind::category::Ternary::True);
  CHECK(same(eta & (succ >> converse(succ)), eta));
  REQUIRE(laws(eta)[Law::Total] == dedekind::category::Ternary::True);
  CHECK(same(eta & (eta >> converse(eta)), eta));
  // Univalent, R° ; R ≤ Δ, as (R° ; R) ∧ Δ = R° ; R.
  REQUIRE(laws(eta)[Law::Univalent] == dedekind::category::Ternary::True);
  CHECK(same((converse(eta) >> eta) & eta, converse(eta) >> eta));
}
