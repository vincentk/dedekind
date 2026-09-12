/** @file test/cpp/modules/dedekind/relational/graph_test.cpp
 *
 * Runtime exercises for :graph — graph(f), the relation/function lattice,
 * and is_graph_of.  The partition's witnesses are static_asserts (invisible
 * to coverage), so the constructor and the finite witness are driven at run
 * time here too.
 */
#include <catch2/catch_test_macros.hpp>
#include <ranges>
#include <utility>

import dedekind.sets;
import dedekind.relational;
import dedekind.category;

using namespace dedekind::sets;
using namespace dedekind::relational;
using namespace dedekind::category;  // the arrow `>>` (Succ ∘ Succ) below

namespace {
// A distinct arrow to separate from the identity: succ(x) = x + 1.
struct Succ {
  using Domain = int;
  using Codomain = int;
  constexpr int operator()(int x) const { return x + 1; }
};

// A TERNARY-logic set over int (Kleene): even → True, odd → False, 0 → Unknown.
// Used to prove preimage PRESERVES Ternary::Unknown (no bool collapse).
struct TriEven {
  using logic_species = TernaryLogic;
  using Domain = int;
  constexpr Ternary operator()(int x) const {
    if (x == 0) return Ternary::Unknown;  // the indeterminate case
    return (x % 2 == 0) ? Ternary::True : Ternary::False;
  }
};
}  // namespace

TEST_CASE("graph: functional relative product Γ_f ; Γ_g = Γ_{f;g} at runtime",
          "[sets][graph][compose]") {
  const Succ succ{};
  // Γ_succ ; Γ_succ = Γ_{x+2}: (a, a+2) lies on it, off-diagonal does not.  The
  // ∃b of the relative product is discharged by functionality (b = succ(a)),
  // so this composes over the int intermediate (not just a Boolean middle).
  const auto twice = graph(succ) >> graph(succ);
  CHECK(twice(std::pair{5, 7}));
  CHECK_FALSE(twice(std::pair{5, 8}));
  // Γ_f ; Γ_g agrees with the graph of the categorical composite.
  CHECK(twice(std::pair{3, 5}) == graph(succ >> succ)(std::pair{3, 5}));
}

TEST_CASE("graph: graph(f) is the diagonal; membership is b == f(a)",
          "[sets][graph]") {
  const auto g = graph(dedekind::category::Identity<int>{});
  CHECK(g(std::pair{5, 5}));
  CHECK_FALSE(g(std::pair{5, 6}));
}

TEST_CASE("graph: is_graph_of decides equality on a finite domain",
          "[sets][graph]") {
  const auto id = dedekind::category::Identity<int>{};
  const auto dom = std::views::iota(0, 4);

  // graph(id) IS the graph of id... (int -> int, so codomain range == dom)
  CHECK(is_graph_of(graph(id), id, dom, dom));
  // ...and graph(succ) is NOT: the witness distinguishes the two functions.
  CHECK_FALSE(is_graph_of(graph(Succ{}), id, dom, dom));
}

// NOTE: `preimage(f, S)` is defined here (:graph) but exercised where the
// point-free Trsk halfspace surface (`Ω | π ⋈ fix`, in :order) and faithful
// tower inclusions are in scope --- downstream, in algebra's
// halfspace_transport_test and numbers' strength_reduction_test.

// preimage preserves the TERNARY classifier value: a pulled-back
// Ternary::Unknown must survive, NOT collapse to false.  Regression guard for
// the classifier-value fix (PreimagePredicate returns L::Ω, not bool == True).
TEST_CASE("graph: preimage preserves Ternary::Unknown (no bool collapse)",
          "[sets][graph][preimage][ternary]") {
  const auto pre = preimage(Succ{}, TriEven{});  // {a | succ(a) ∈ TriEven}
  CHECK(pre(-1) == Ternary::Unknown);  // succ(-1)=0 → Unknown, PRESERVED
  CHECK(pre(1) == Ternary::True);      // succ(1)=2 → even
  CHECK(pre(2) == Ternary::False);     // succ(2)=3 → odd
}
