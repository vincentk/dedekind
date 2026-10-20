/** @file dedekind/relational/rows_test.cpp
 *
 * Relations built at run time as terms (#1037): arrows between products of
 * carriers, their objects data; the set grammar's nodes and the relational
 * calculus's; the predicate a reading, a composite's the ∃ over the whole
 * middle object (the powerset monad's μ), η the identity relation, Copy and
 * Merge the Frobenius spiders, the converse the dagger.  The verdict reads a
 * quantifier over a finite chain by enumeration and over a word by a bounded
 * search, in Σ (∃) or Π (∀).  Elimination (canonical) is algebra's,
 * downstream, so the laws are witnessed at points here and DECIDED by the
 * normal form in algebra/relation_laws_test.cpp: the monad's unit and
 * associativity, the dagger, the Frobenius spider, table / arrow.
 * Comparisons are == / != only: an order comparison needs order's rule,
 * downstream of relational as well.
 */

#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <utility>
#include <vector>

import dedekind.category; // Ternary
import dedekind.relational;
import dedekind.sets;

using namespace dedekind::sets;
using dedekind::category::Ternary;
using dedekind::relational::converse;
using dedekind::relational::copy;
using dedekind::relational::diag;
using dedekind::relational::function_term;
using dedekind::relational::merge;
using dedekind::relational::Object;
using dedekind::relational::predicate_of;
using dedekind::relational::proj;
using dedekind::relational::RelExpr;
using dedekind::relational::table;
using dedekind::relational::verdict;

namespace {
using Re = RelExpr<int, unsigned, bool>;
using R = Row<unsigned, bool>;
using T = Term<int>;
using Q = Pred<int>;
constexpr std::size_t word = 0, flag = 1;  // the sorts among the carriers
const Object W{word};

const Re words = Re::universe(W, W);  // the full relation on the word
const Re succ =
    (words & Q::cmp(Rel::Eq, T::proj(2),
                    T::bin(Arith::Add, T::proj(1), T::constant(1))));
const Re square =
    (words &
     Q::cmp(Rel::Eq, T::bin(Arith::Mul, T::proj(2), T::proj(2)), T::proj(1)));
const Re cube =
    (words &
     Q::cmp(Rel::Eq,
            T::bin(Arith::Mul, T::bin(Arith::Mul, T::proj(1), T::proj(1)),
                   T::proj(1)),
            T::proj(2)));
bool holds(const Re& r, const R& row) {
  return verdict(r, row) == Ternary::True;
}
}  // namespace

TEST_CASE("relational:rows: a relation is a term; its predicate a reading",
          "[relational][rows]") {
  const Re two = succ >> succ;
  CHECK(two.kind == Re::Kind::Compose);  // not flattened
  CHECK(two.dom == W);
  CHECK(two.cod == W);
  const Q p = predicate_of(two);  // the reading: ∃ over the middle, μ
  REQUIRE(p.kind == Q::Kind::Exists);
  CHECK(p.slot == 3);  // the next slot past the two columns
  CHECK(p.sort == word);
  // ∃m. m = a + 1 ∧ c = m + 1: the atom defines m, so the verdict eliminates
  // the ∃ (c = a + 1 + 1) and refutes (3, 6) outright: ⊥, not U.  The
  // reading keeps the ∃ as written.
  CHECK(verdict(two, R{{3u, 5u}}) == Ternary::True);
  CHECK(verdict(two, R{{3u, 6u}}) == Ternary::False);
  CHECK(function_term(two, p) == nullptr);  // not quantifier-free as read
  // A quantifier is found in a conditional's guard, too.
  CHECK(has_quantifier(
      Q::cmp(Rel::Eq, T::cond(p, T::constant(1), T::proj(1)), T::proj(2))));
  CHECK_FALSE(has_quantifier(predicate_of(succ)));
  // The lattice keeps its nodes: R & S is a Meet, ~R a Not.
  CHECK((succ & square).kind == Re::Kind::Meet);
  CHECK((~succ).kind == Re::Kind::Not);
  CHECK(predicate_of(~succ).rel == Rel::Ne);  // read: negation normal form
  CHECK_THROWS_AS((succ & diag<int, unsigned, bool>({flag})), not_admitted);
}

TEST_CASE("relational:rows: η, the dagger, and the structural projection",
          "[relational][rows]") {
  const Re id = diag<int, unsigned, bool>(W);
  CHECK(holds(id, R{{3u, 3u}}));
  CHECK_FALSE(holds(id, R{{3u, 4u}}));
  CHECK(holds(id >> succ, R{{3u, 4u}}));  // Δ;R = R
  CHECK(holds(succ >> id, R{{3u, 4u}}));  // R;Δ = R
  // The converse exchanges the objects; the dagger laws at points.
  const Re pred = converse(succ);
  CHECK(pred.kind == Re::Kind::Converse);
  CHECK(holds(pred, R{{4u, 3u}}));
  CHECK_FALSE(holds(pred, R{{3u, 4u}}));
  CHECK(holds(converse(converse(succ)), R{{3u, 4u}}));  // R°° = R
  const Re sc = square >> cube;
  for (const R& row :
       {R{{8u, 4u}}, R{{9u, 4u}}, R{{27u, 9u}}})  // (R;S)° = S°;R°
    CHECK(verdict(converse(sc), row) ==
          verdict(converse(cube) >> converse(square), row));
  // A projection is composition with π: the domain of succ.
  const Re rows = table(succ);  // () ⇸ word × word
  const Re dom = rows >> proj<int, unsigned, bool>({word, word}, {1});
  CHECK(dom.dom.factors.empty());
  CHECK(dom.cod == W);
  const Q p = predicate_of(dom);
  CHECK(p.kind == Q::Kind::Exists);
  CHECK(verdict(dom, R{{5u}}) == Ternary::True);    // 6 is found
  CHECK(verdict(~dom, R{{5u}}) == Ternary::False);  // ∀: 6 refutes it
  CHECK_THROWS_AS((proj<int, unsigned, bool>(W, {1, 1})), not_admitted);
  CHECK_THROWS_AS(succ >> dom, not_admitted);  // cod word ≠ dom ()
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
  const Re alt = table(nsc) >> proj<int, unsigned, bool>({word, word}, {1});
  CHECK(verdict(alt, R{{4u}}) == Ternary::Unknown);
}

TEST_CASE("relational:rows: the Frobenius spiders, a finite middle object",
          "[relational][rows]") {
  // R ∧ S = merge ∘ (R ⊗ S) ∘ copy, at points: the spider form of the meet.
  // The spider's middles are four ∃ over the word, each defined by an atom
  // (copy's and merge's diagonals, succ's graph), so the verdict eliminates
  // them all and decides the meet at a point as it decides R & S: ⊥ where it
  // fails, not U.  The law itself is decided by the normal form in
  // algebra/relation_laws_test (canonical is algebra's, downstream).
  const Re spider = copy<int, unsigned, bool>(W) >> (succ * words) >>
                    merge<int, unsigned, bool>(W);
  CHECK(spider.dom == W);
  CHECK(spider.cod == W);
  CHECK(verdict(succ & words, R{{3u, 4u}}) == Ternary::True);
  CHECK(verdict(spider, R{{3u, 4u}}) == Ternary::True);  // the witness
  CHECK(verdict(succ & words, R{{3u, 5u}}) == Ternary::False);
  CHECK(verdict(spider, R{{3u, 5u}}) == Ternary::False);  // decided
  // The middle of copy >> (R ⊗ S) is two columns: ∃∃, outermost first.
  const Q p = predicate_of(copy<int, unsigned, bool>(W) >> (succ * square));
  REQUIRE(p.kind == Q::Kind::Exists);
  CHECK(p.slot == 4);
  REQUIRE(p.p->kind == Q::Kind::Exists);
  CHECK(p.p->slot == 5);
  // A finite middle: ∃f ∈ 𝔹 enumerated by the bool reading.
  const Re above =
      (Re::universe(W, {flag}) & Q::cmp(Rel::Ne, T::proj(1), T::constant(5)));
  const Re pick =
      (Re::universe({flag}, W) &
       Q::disj(Q::conj(Q::cmp(Rel::Eq, T::proj(1), T::constant(1)),
                       Q::cmp(Rel::Eq, T::proj(2), T::constant(1))),
               Q::conj(Q::cmp(Rel::Eq, T::proj(1), T::constant(0)),
                       Q::cmp(Rel::Eq, T::proj(2), T::constant(0)))));
  const Q both = predicate_of(above >> pick);
  CHECK(both.sort == flag);
  CHECK(both(R{{7u, 1u}}));
  CHECK(both(R{{7u, 0u}}));
  CHECK_FALSE(both(R{{7u, 2u}}));
  CHECK_FALSE(both(R{{5u, 1u}}));
  CHECK_THROWS_AS(above >> succ, not_admitted);  // 𝔹 is not the word
  // Two universes make the product universe.
  CHECK((words * words).kind == Re::Kind::Universe);
  CHECK((succ * square).kind == Re::Kind::Tensor);
  CHECK_THROWS_AS((succ & Q::cmp(Rel::Eq, T::proj(3), T::constant(1))),
                  not_admitted);  // a slot past the columns
}

TEST_CASE("relational:rows: an object is a product in normal form (#1039)",
          "[relational][rows][objects]") {
  const Object A{word}, B{flag}, C{word, flag};
  CHECK((A * B) * A == A * (B * A));  // associative by construction
  CHECK(A * Object{} == A);           // 𝟙 the unit
  CHECK(A * B == Object{word, flag});
  CHECK(columns(C) == dedekind::relational::Columns{word, flag});
  CHECK((succ * square).dom == A * A);  // the tensor's objects are products
}

TEST_CASE("relational:rows: the sum of two tables; table and arrow (#1039)",
          "[relational][rows][sums]") {
  namespace rel = dedekind::relational;
  const Object S = rel::sum(W, Object{flag}, flag);
  // A sum lays out its tag, then the left summand's columns, then the right's.
  CHECK(rel::columns(S) == rel::Columns{flag, word, flag});
  CHECK(S == rel::sum(W, Object{flag}, flag));        // deep equality
  CHECK_FALSE(S == rel::sum(Object{flag}, W, flag));  // the order of summands
  const Re s = rel::sum(Re::universe({}, W), Re::universe({}, {flag}));
  CHECK(s.kind == Re::Kind::Sum);
  CHECK(s.cod == S);
  CHECK(predicate_of(s).kind == Q::Kind::Or);  // the copairing on the tag
  // A row of the sum is a point: the inactive side at its defaults.
  CHECK(holds(s, R{{false, 3u, false}}));       // inl(3)
  CHECK_FALSE(holds(s, R{{false, 3u, true}}));  // no point: the 𝔹 not default
  CHECK(holds(s, R{{true, 0u, true}}));         // inr(⊤)
  CHECK_FALSE(holds(s, R{{true, 3u, true}}));  // no point: the word not default
  CHECK_THROWS_AS(rel::sum(succ, succ), not_admitted);  // tables only, for now
  // table and arrow move whole factors: Rel's compact-closed iso.
  CHECK(rel::table(succ).dom == Object{});
  CHECK(rel::table(succ).cod == W * W);
  CHECK(rel::arrow(rel::table(succ)).dom == W);
  CHECK(rel::arrow(rel::table(succ)).cod == W);
}

TEST_CASE("relational:rows: a RelExpr is a relation (IsRelation)",
          "[relational][rows][relation]") {
  // Its points are a row of the domain and a row of the codomain, read through
  // the product's API; membership is the verdict there, in K₃.
  STATIC_CHECK(dedekind::relational::IsRelation<Re, R, R>);
  CHECK(succ(std::pair{R{{3u}}, R{{4u}}}) == Ternary::True);
  CHECK(succ(std::pair{R{{3u}}, R{{5u}}}) == Ternary::False);
  // A point not of the shape dom × cod is none of the relation's.
  CHECK(succ(std::pair{R{}, R{{3u, 4u}}}) == Ternary::False);
}

TEST_CASE("relational:rows: the laws of an arrow, inferred over the term",
          "[relational][rows][laws]") {
  using dedekind::relational::Law;
  using dedekind::relational::Laws;
  using dedekind::relational::laws;
  using dedekind::sets::𝔸;
  const auto all = Laws::from(true, true, true, true);
  // Leaves, by their shape: Δ is an isomorphism; a graph a map; the universe
  // total and surjective; copy a map and injective, merge its converse.
  CHECK(laws(diag<int, unsigned, bool>(W)) == all);
  CHECK(laws(succ) == Laws::from(true, true, false, false));
  CHECK(laws(words) == Laws::from(true, false, false, true));
  CHECK(laws(dedekind::relational::copy<int, unsigned, bool>(W)) ==
        Laws::from(true, true, true, false));
  CHECK(laws(merge<int, unsigned, bool>(W)) ==
        Laws::from(false, true, true, true));
  // Nodes, by the engine on the declared facts: closure under ;, the dagger's
  // action under °, the down-sets under &, nothing under ~.
  CHECK(laws(succ >> succ) == Laws::from(true, true, false, false));
  CHECK(laws(converse(succ)) == Laws::from(false, false, true, true));
  CHECK(laws(converse(converse(succ))) == laws(succ));
  // square is {(a, c) | c² = a}: a is a function of c, so it is injective
  // and surjective; the meet keeps univalence from succ, injectivity from it.
  CHECK(laws(square) == Laws::from(false, false, true, true));
  CHECK(laws(square & succ) == Laws::from(false, true, true, false));
  CHECK(laws(~succ) == Laws{});
  CHECK(laws(diag<int, unsigned, bool>(W) >>
             converse(diag<int, unsigned, bool>(W))) == all);
  // The facts hold at an operation's own boundary: re-split as a table, Δ ; Δ
  // is 1 → W × W with every diagonal row above the one input, and is read by
  // its shape (a ∃ it cannot see through): nothing certified.
  CHECK(laws(table(diag<int, unsigned, bool>(W) >>
                   diag<int, unsigned, bool>(W))) == Laws{});
  CHECK(laws(converse(table(succ))) == Laws{});
  // The sets: 𝔸<Re> & Total<Re>{} is the set of total relations, a map the
  // meet of two such sets, χ in K₃.
  using dedekind::relational::Total;
  using dedekind::relational::Univalent;
  const auto total = 𝔸<Re>{} & Total<Re>{};
  const auto map = total & (𝔸<Re>{} & Univalent<Re>{});
  STATIC_CHECK(dedekind::sets::IsSetObject<decltype(total)>);
  CHECK(total(succ) == Ternary::True);
  CHECK(total(converse(succ)) == Ternary::Unknown);
  CHECK(map(succ >> succ) == Ternary::True);
  CHECK(map(words) == Ternary::Unknown);
  // The complement certifies nothing: refuted where inferred, else Unknown.
  CHECK((~total)(succ) == Ternary::False);
  CHECK((~total)(~succ) == Ternary::Unknown);
}

TEST_CASE("relational:rows: the laws of χ: closure, variance, the dagger",
          "[relational][rows][laws]") {
  // The facts a law-set is declared, as laws of its χ over the fixtures: for
  // a set closed under >>, S(f) ∧ S(g) ≤ S(f >> g); a down-set holds f & g
  // as soon as it holds a leg, an up-set f | g; S(f°) = S°(f).
  using dedekind::category::Kleene;
  using dedekind::relational::dagger;
  using dedekind::relational::every_law;
  using dedekind::relational::is_up_set;
  using dedekind::relational::Law;
  using dedekind::relational::laws;
  const std::vector<Re> fixtures{
      succ, square, words, diag<int, unsigned, bool>(W), ~succ, converse(succ),
      succ >> succ, square & succ,
      // the spider form of Δ: W → W, through the middle W × W
      dedekind::relational::copy<int, unsigned, bool>(W) >>
          merge<int, unsigned, bool>(W)};
  const auto chi = [](Law w, const Re& r) { return laws(r)[w]; };
  for (const Law w : every_law) {
    for (const Re& f : fixtures) {
      CHECK(chi(w, converse(f)) == chi(dagger(w), f));
      for (const Re& g : fixtures) {
        CHECK(Kleene::AND(chi(w, f), chi(w, g)) <= chi(w, f >> g));
        if (is_up_set(w))
          CHECK(Kleene::OR(chi(w, f), chi(w, g)) <= chi(w, f | g));
        else
          CHECK(Kleene::OR(chi(w, f), chi(w, g)) <= chi(w, f & g));
      }
    }
  }
}

TEST_CASE("relational:rows: a bare numeral stays under its quantifier",
          "[relational][rows]") {
  // ∃m. m == 1 ∧ m == 1: the defining term 1 has no sort of its own, so the
  // verdict keeps the quantifier (substituted, 1 == 1 would stand on a mixed
  // row with no column to read it in) and the search finds m = 1.
  const Re one = words & Q::cmp(Rel::Eq, T::proj(2), T::constant(1));
  const Re pick =
      Re::universe(W, {flag}) & Q::cmp(Rel::Eq, T::proj(1), T::constant(1));
  CHECK(verdict(one >> pick, R{{3u, false}}) == Ternary::True);
  CHECK(verdict(one >> pick, R{{3u, true}}) == Ternary::True);
}
