/** @file dedekind/relational/rows_test.cpp
 *
 * Relations of any arity as tables (#1037): the predicate of a composite is
 * built symbolically, its middle a quantified column; a projection quantifies
 * its columns away; ~ swaps ∃ and ∀.  The verdict reads a quantifier over a
 * finite chain by enumeration and over a word by a bounded search, valued in
 * Σ (∃) or Π (∀).  Elimination (canonical) is algebra's and is exercised from
 * Python (test_pst_spans.py).
 */

#include <catch2/catch_test_macros.hpp>
#include <vector>

import dedekind.category; // Ternary
import dedekind.relational;
import dedekind.sets;

using namespace dedekind::sets;
using dedekind::category::Ternary;
using dedekind::relational::function_term;
using dedekind::relational::project;
using dedekind::relational::Table;
using dedekind::relational::verdict;
using dedekind::relational::where;

namespace {
using Tb = Table<int, unsigned, bool>;
using R = Row<unsigned, bool>;
using T = Term<int>;
using Q = Pred<int>;
constexpr std::size_t word = 0, flag = 1;  // the sorts among the carriers

const Tb succ{{word, word},
              Q::cmp(Rel::Eq, T::proj(2),
                     T::bin(Arith::Add, T::proj(1), T::constant(1)))};
const Tb square{
    {word, word},
    Q::cmp(Rel::Eq, T::bin(Arith::Mul, T::proj(2), T::proj(2)), T::proj(1))};
const Tb cube{
    {word, word},
    Q::cmp(Rel::Eq,
           T::bin(Arith::Mul, T::bin(Arith::Mul, T::proj(1), T::proj(1)),
                  T::proj(1)),
           T::proj(2))};
}  // namespace

TEST_CASE("relational:rows: the composite's predicate quantifies the middle",
          "[relational][rows]") {
  const Tb two = succ >> succ;
  CHECK(two.arity() == 2);
  REQUIRE(two.predicate.kind == Q::Kind::Exists);
  CHECK(two.predicate.slot == 3);  // the next slot past the two columns
  CHECK(two.predicate.sort == word);
  // Read by the bounded search: a witness m = 4 for (3, 5); none for (3, 6)
  // within the budget, so U, never ⊥.
  CHECK(verdict(two, R{{3u, 5u}}) == Ternary::True);
  CHECK(verdict(two, R{{3u, 6u}}) == Ternary::Unknown);
  CHECK(function_term(two, two.predicate) == nullptr);  // not quantifier-free
}

TEST_CASE("relational:rows: ∀ is ∃'s dual; a projection quantifies away",
          "[relational][rows]") {
  const Tb dom = project(succ, {1});  // ∃π_2. π_2 == π_1 + 1
  CHECK(dom.arity() == 1);
  CHECK(dom.predicate.kind == Q::Kind::Exists);
  CHECK(dom.predicate.slot == 2);
  CHECK(verdict(dom, R{{5u}}) == Ternary::True);  // 6 is found
  const Tb co = ~dom;                             // ∀π_2. π_2 != π_1 + 1
  CHECK(co.predicate.kind == Q::Kind::Forall);
  CHECK(verdict(co, R{{5u}}) == Ternary::False);  // 6 refutes it
  // The converse: the columns in the other order.
  const Tb flipped = project(succ, {2, 1});
  CHECK(verdict(flipped, R{{4u, 3u}}) == Ternary::True);
  CHECK(verdict(flipped, R{{3u, 4u}}) == Ternary::False);
  CHECK_THROWS_AS(project(succ, {1, 1}), not_admitted);
}

TEST_CASE("relational:rows: Σ and Π, and one level of search",
          "[relational][rows]") {
  const Tb sc = square >> cube;                          // ∃b. b² = a ∧ b³ = c
  CHECK(verdict(sc, R{{4u, 8u}}) == Ternary::True);      // b = 2
  CHECK(verdict(sc, R{{4u, 9u}}) == Ternary::Unknown);   // never ⊥
  const Tb nsc = ~sc;                                    // ∀b. b² ≠ a ∨ b³ ≠ c
  CHECK(verdict(nsc, R{{4u, 8u}}) == Ternary::False);    // b = 2 refutes
  CHECK(verdict(nsc, R{{4u, 9u}}) == Ternary::Unknown);  // never ⊤
  Tb narrow = sc;
  narrow.budget = 1;  // b = 0 only
  CHECK(verdict(narrow, R{{4u, 8u}}) == Ternary::Unknown);
  // ∃c ∀b: a search under a search is not made; U.
  CHECK(verdict(project(nsc, {1}), R{{4u}}) == Ternary::Unknown);
}

TEST_CASE("relational:rows: a finite middle is decided, the product shifts",
          "[relational][rows]") {
  // R: a > 5 on word × 𝔹; S: 𝔹 × word, c = 1 where the flag is set.
  // (== / != only: an order comparison needs order's rule, downstream of
  // relational.)
  const Tb above{{word, flag}, Q::cmp(Rel::Ne, T::proj(1), T::constant(5))};
  const Tb pick{{flag, word},
                Q::disj(Q::conj(Q::cmp(Rel::Eq, T::proj(1), T::constant(1)),
                                Q::cmp(Rel::Eq, T::proj(2), T::constant(1))),
                        Q::conj(Q::cmp(Rel::Eq, T::proj(1), T::constant(0)),
                                Q::cmp(Rel::Eq, T::proj(2), T::constant(0))))};
  const Tb both = above >> pick;  // ∃f ∈ 𝔹. a > 5 ∧ pick(f, c)
  CHECK(both.predicate.sort == flag);
  CHECK(both.predicate(R{{7u, 1u}}));  // the bool reading enumerates 𝔹
  CHECK(both.predicate(R{{7u, 0u}}));
  CHECK_FALSE(both.predicate(R{{7u, 2u}}));
  CHECK_FALSE(both.predicate(R{{5u, 1u}}));
  CHECK_THROWS_AS(above >> succ, not_admitted);  // 𝔹 is not the word
  // A product shifts the right factor's slots, bound ones included.
  const Tb dom = project(succ, {1});
  const Tb pair = dom * dom;
  CHECK(pair.arity() == 2);
  CHECK(verdict(pair, R{{1u, 2u}}) == Ternary::True);
  CHECK_THROWS_AS(dom & succ, not_admitted);  // two products
  CHECK_THROWS_AS(where(succ, Q::cmp(Rel::Eq, T::proj(3), T::constant(1))),
                  not_admitted);  // a slot past the columns
}
