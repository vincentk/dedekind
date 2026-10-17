/** @file dedekind/relational/rows_test.cpp
 *
 * Relations of any arity as terms (#1037): the set grammar's nodes and the
 * relational calculus's on relations whose schema is data; the predicate a
 * reading of the term, a composite's the ∃ over the middle (the powerset
 * monad's μ), η the identity relation.  The verdict reads a quantifier over
 * a finite chain by enumeration and over a word by a bounded search, in Σ
 * (∃) or Π (∀).  Elimination (canonical) is algebra's, downstream, and is
 * exercised from Python (test_pst_spans.py), the monad laws with it.
 * Comparisons here are == / != only: an order comparison needs order's rule,
 * downstream of relational as well.
 */

#include <catch2/catch_test_macros.hpp>
#include <vector>

import dedekind.category; // Ternary
import dedekind.relational;
import dedekind.sets;

using namespace dedekind::sets;
using dedekind::category::Ternary;
using dedekind::relational::diag;
using dedekind::relational::function_term;
using dedekind::relational::predicate;
using dedekind::relational::project;
using dedekind::relational::RelExpr;
using dedekind::relational::verdict;
using dedekind::relational::where;

namespace {
using Re = RelExpr<int, unsigned, bool>;
using R = Row<unsigned, bool>;
using T = Term<int>;
using Q = Pred<int>;
constexpr std::size_t word = 0, flag = 1;  // the sorts among the carriers

const Re words = Re::universe({word, word});
const Re succ =
    where(words, Q::cmp(Rel::Eq, T::proj(2),
                        T::bin(Arith::Add, T::proj(1), T::constant(1))));
const Re square = where(
    words,
    Q::cmp(Rel::Eq, T::bin(Arith::Mul, T::proj(2), T::proj(2)), T::proj(1)));
const Re cube = where(
    words, Q::cmp(Rel::Eq,
                  T::bin(Arith::Mul, T::bin(Arith::Mul, T::proj(1), T::proj(1)),
                         T::proj(1)),
                  T::proj(2)));
}  // namespace

TEST_CASE("relational:rows: a relation is a term; its predicate a reading",
          "[relational][rows]") {
  const Re two = succ >> succ;
  CHECK(two.kind == Re::Kind::Compose);  // not flattened
  CHECK(two.arity() == 2);
  const Q p = predicate(two);  // the reading: ∃ over the middle, μ
  REQUIRE(p.kind == Q::Kind::Exists);
  CHECK(p.slot == 3);  // the next slot past the two columns
  CHECK(p.sort == word);
  // The bounded search: a witness m = 4 for (3, 5); none for (3, 6) within
  // the budget, so U, never ⊥.
  CHECK(verdict(two, R{{3u, 5u}}) == Ternary::True);
  CHECK(verdict(two, R{{3u, 6u}}) == Ternary::Unknown);
  CHECK(function_term(two, p) == nullptr);  // not quantifier-free as read
  // The lattice keeps its nodes: R & S is a Meet, ~R a Not.
  CHECK((succ & square).kind == Re::Kind::Meet);
  CHECK((~succ).kind == Re::Kind::Not);
  CHECK(predicate(~succ).rel == Rel::Ne);  // read: negation normal form
}

TEST_CASE("relational:rows: ∀ is ∃'s dual; a projection quantifies away",
          "[relational][rows]") {
  const Re dom = project(succ, {1});  // ∃π_2. π_2 == π_1 + 1
  CHECK(dom.kind == Re::Kind::Exists);
  CHECK(dom.arity() == 1);
  const Q p = predicate(dom);
  CHECK(p.kind == Q::Kind::Exists);
  CHECK(p.slot == 2);
  CHECK(verdict(dom, R{{5u}}) == Ternary::True);  // 6 is found
  const Re co = ~dom;                             // ∀π_2. π_2 != π_1 + 1
  CHECK(predicate(co).kind == Q::Kind::Forall);
  CHECK(verdict(co, R{{5u}}) == Ternary::False);  // 6 refutes it
  // The converse: the columns in the other order.
  const Re flipped = project(succ, {2, 1});
  CHECK(verdict(flipped, R{{4u, 3u}}) == Ternary::True);
  CHECK(verdict(flipped, R{{3u, 4u}}) == Ternary::False);
  CHECK_THROWS_AS(project(succ, {1, 1}), not_admitted);
}

TEST_CASE("relational:rows: Σ and Π, and one level of search",
          "[relational][rows]") {
  const Re sc = square >> cube;                          // ∃b. b² = a ∧ b³ = c
  CHECK(verdict(sc, R{{4u, 8u}}) == Ternary::True);      // b = 2
  CHECK(verdict(sc, R{{4u, 9u}}) == Ternary::Unknown);   // never ⊥
  const Re nsc = ~sc;                                    // ∀b. b² ≠ a ∨ b³ ≠ c
  CHECK(verdict(nsc, R{{4u, 8u}}) == Ternary::False);    // b = 2 refutes
  CHECK(verdict(nsc, R{{4u, 9u}}) == Ternary::Unknown);  // never ⊤
  Re narrow = sc;
  narrow.budget = 1;  // b = 0 only
  CHECK(verdict(narrow, R{{4u, 8u}}) == Ternary::Unknown);
  // ∃c ∀b: a search under a search is not made; U.
  CHECK(verdict(project(nsc, {1}), R{{4u}}) == Ternary::Unknown);
}

TEST_CASE("relational:rows: η, a finite middle, the tensor's shift",
          "[relational][rows]") {
  // η: the identity relation, the unit of >>, read at points.
  const Re id = diag<int, unsigned, bool>(word);
  CHECK(verdict(id, R{{3u, 3u}}) == Ternary::True);
  CHECK(verdict(id, R{{3u, 4u}}) == Ternary::False);
  CHECK(verdict(id >> succ, R{{3u, 4u}}) == Ternary::True);  // Δ;R = R
  CHECK(verdict(succ >> id, R{{3u, 4u}}) == Ternary::True);  // R;Δ = R
  // R: a ≠ 5 on word × 𝔹; S: 𝔹 × word, c = 1 where the flag is set.
  const Re above = where(Re::universe({word, flag}),
                         Q::cmp(Rel::Ne, T::proj(1), T::constant(5)));
  const Re pick =
      where(Re::universe({flag, word}),
            Q::disj(Q::conj(Q::cmp(Rel::Eq, T::proj(1), T::constant(1)),
                            Q::cmp(Rel::Eq, T::proj(2), T::constant(1))),
                    Q::conj(Q::cmp(Rel::Eq, T::proj(1), T::constant(0)),
                            Q::cmp(Rel::Eq, T::proj(2), T::constant(0)))));
  const Re both = above >> pick;  // ∃f ∈ 𝔹. a ≠ 5 ∧ pick(f, c)
  const Q p = predicate(both);
  CHECK(p.sort == flag);
  CHECK(p(R{{7u, 1u}}));  // the bool reading enumerates 𝔹
  CHECK(p(R{{7u, 0u}}));
  CHECK_FALSE(p(R{{7u, 2u}}));
  CHECK_FALSE(p(R{{5u, 1u}}));
  CHECK_THROWS_AS(above >> succ, not_admitted);  // 𝔹 is not the word
  // A tensor shifts the right factor's slots, bound ones included; two
  // universes make the product universe.
  const Re dom = project(succ, {1});
  const Re pair = dom * dom;
  CHECK(pair.kind == Re::Kind::Tensor);
  CHECK(pair.arity() == 2);
  CHECK(verdict(pair, R{{1u, 2u}}) == Ternary::True);
  CHECK((words * words).kind == Re::Kind::Universe);
  CHECK_THROWS_AS(dom & succ, not_admitted);  // two products
  CHECK_THROWS_AS(where(succ, Q::cmp(Rel::Eq, T::proj(3), T::constant(1))),
                  not_admitted);  // a slot past the columns
}
