/** @file dedekind/sets/spans_test.cpp
 *
 * Relations of any arity held as spans (#1037): a composite or a projection
 * keeps its middle as a hidden column; the image eliminates it by
 * substitution where an atom defines it; membership searches where nothing
 * does, valued in Σ (∃) or Π (∀); the complement pushes down by duality.
 * Shannon's expansion needs constants that name their sort, so it is
 * exercised from Python (test_pst_spans.py).
 */

#include <catch2/catch_test_macros.hpp>
#include <vector>

import dedekind.category; // Ternary
import dedekind.sets;

using namespace dedekind::sets;
using dedekind::category::Ternary;

namespace {
using S = Span<int, unsigned, bool>;
using R = Row<unsigned, bool>;
using T = Term<int>;
using Q = Pred<int>;
/** @brief A value as a constant: the numerals of this test. */
struct Lift {
  int operator()(unsigned v) const { return static_cast<int>(v); }
  int operator()(bool b) const { return b ? 1 : 0; }
};
constexpr std::size_t word = 0;  // the sort of unsigned among the carriers

const S succ{{word, word},
             Q::cmp(Rel::Eq, T::proj(2),
                    T::bin(Arith::Add, T::proj(1), T::constant(1)))};
const S square{
    {word, word},
    Q::cmp(Rel::Eq, T::bin(Arith::Mul, T::proj(2), T::proj(2)), T::proj(1))};
const S cube{
    {word, word},
    Q::cmp(Rel::Eq,
           T::bin(Arith::Mul, T::bin(Arith::Mul, T::proj(1), T::proj(1)),
                  T::proj(1)),
           T::proj(2))};
}  // namespace

TEST_CASE("sets:spans: a composite keeps its middle until its image is taken",
          "[sets][spans]") {
  const S two = succ.then(succ);
  CHECK(two.arity() == 2);
  REQUIRE(two.hidden.size() == 1);
  CHECK_FALSE(two.hidden.front().universal);
  // The middle is defined (π_3 == π_1 + 1): substituted, the fusion law.
  const S image = two.fused(Lift{});
  CHECK(image.hidden.empty());
  CHECK(image.verdict(R{{3u, 5u}}, Lift{}) == Ternary::True);
  CHECK(image.verdict(R{{3u, 6u}}, Lift{}) == Ternary::False);
  CHECK(image.function_term(image.predicate) != nullptr);
}

TEST_CASE("sets:spans: a projection hides its columns by ∃; ~ by duality",
          "[sets][spans]") {
  const S dom = succ.project({1});  // ∃π_2. π_2 == π_1 + 1
  CHECK(dom.arity() == 1);
  CHECK(dom.fused(Lift{}).predicate.kind == Q::Kind::Top);  // entire
  const S co = dom.complement();  // ∀π_2. π_2 != π_1 + 1
  REQUIRE(co.hidden.size() == 1);
  CHECK(co.hidden.front().universal);
  CHECK(co.fused(Lift{}).predicate.kind == Q::Kind::Bottom);
  // The converse: the columns in another order.
  const S flipped = succ.project({2, 1});
  CHECK(flipped.verdict(R{{4u, 3u}}, Lift{}) == Ternary::True);
  CHECK(flipped.verdict(R{{3u, 4u}}, Lift{}) == Ternary::False);
  CHECK_THROWS_AS(succ.project({1, 1}), not_admitted);
}

TEST_CASE("sets:spans: an undefined middle is searched, in Σ or in Π",
          "[sets][spans]") {
  const S sc = square.then(cube);  // ∃b. b² = a ∧ b³ = c
  CHECK_THROWS_AS(sc.fused(Lift{}), not_admitted);
  CHECK(sc.verdict(R{{4u, 8u}}, Lift{}) == Ternary::True);     // b = 2
  CHECK(sc.verdict(R{{4u, 9u}}, Lift{}) == Ternary::Unknown);  // never ⊥
  const S nsc = sc.complement();  // ∀b. b² ≠ a ∨ b³ ≠ c
  CHECK(nsc.verdict(R{{4u, 8u}}, Lift{}) == Ternary::False);    // b = 2
  CHECK(nsc.verdict(R{{4u, 9u}}, Lift{}) == Ternary::Unknown);  // never ⊤
  S narrow = sc;
  narrow.budget = 1;  // b = 0 only
  CHECK(narrow.verdict(R{{4u, 8u}}, Lift{}) == Ternary::Unknown);
  // ∃c ∀b: alternating quantifiers answer U.
  CHECK(nsc.project({1}).verdict(R{{4u}}, Lift{}) == Ternary::Unknown);
}

TEST_CASE("sets:spans: products, the lattice and the composite's carriers",
          "[sets][spans]") {
  const S dom = succ.project({1});
  const S both = dom.times(dom);  // two columns, two hidden ones
  CHECK(both.arity() == 2);
  CHECK(both.hidden.size() == 2);
  CHECK(both.fused(Lift{}).predicate.kind == Q::Kind::Top);
  CHECK_THROWS_AS(dom.meet(succ), not_admitted);    // two products
  const S flags{{1, 1}, Q::top()};                  // 𝔹 × 𝔹
  CHECK_THROWS_AS(succ.then(flags), not_admitted);  // the word is no 𝔹
}
