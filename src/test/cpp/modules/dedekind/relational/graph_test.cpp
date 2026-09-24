/** @file test/cpp/modules/dedekind/relational/graph_test.cpp
 *
 * Runtime exercises for :graph — graph(f), the relation/function lattice,
 * and is_graph_of.  The partition's witnesses are static_asserts (invisible
 * to coverage), so the constructor and the finite witness are driven at run
 * time here too.
 */
#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <functional>
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
  using logic_species = Kleene;
  using Domain = int;
  constexpr Ternary operator()(int x) const {
    if (x == 0) return Ternary::Unknown;  // the indeterminate case
    return (x % 2 == 0) ? Ternary::True : Ternary::False;
  }
};

// The parallel pair whose equalizer is the graph Γ_succ ⊆ int×int:
// (succ∘π₁, π₂) : int×int → int, coinciding exactly where b = succ(a).
struct SuccPi1Arrow {
  using Domain = std::pair<int, int>;
  using Codomain = int;
  constexpr int operator()(const std::pair<int, int>& p) const {
    return Succ{}(p.first);
  }
};
struct Pi2Arrow {
  using Domain = std::pair<int, int>;
  using Codomain = int;
  constexpr int operator()(const std::pair<int, int>& p) const {
    return p.second;
  }
};
// The graph of a function IS a categorical equalizer, reified through the graph
// adapter (dyadic.cppm's conceptual chain): Γ_succ is the equalizer subobject
// of int×int of (succ∘π₁, π₂).  The :relational sibling of the affine
// translation-graph equalizer witnessed in algebra:halfspace_transport (#876).
static_assert(dedekind::category::IsEqualizer<decltype(graph(Succ{})),
                                              SuccPi1Arrow, Pi2Arrow>,
              "a functional graph IS an equalizer subobject of A×B: "
              "Γ_succ = equalizer(succ∘π₁, π₂).");
}  // namespace

// ═══ #950 load-bearing proof: the function/relation duality is served by the
//     EXISTING graph adapter --- NO arrow_traits (#953), NO :morphism change.
//
// "IsFunction is two arrows, functorially related via graph."  A function has
// two readings that differ ONLY by codomain, bridged by applying graph(-):
//   MAP reading      : f : A → B          (Cod = B)   the analytic arrow;
//   RELATION reading : graph(f) : A×B → Ω (Cod = Ω)   a CHARACTERISTIC
//                                                      predicate on PAIRS.
// The relation reading IS a characteristic predicate on pairs; being an IsArrow
// is SUBSUMED (IsCharacteristic ⊆ IsArrow).  It is reached by APPLYING the
// existing :relational adapter graph(-) --- no tag, no arrow_traits, no touch
// to :morphism.  The spider (:category) is upstream, graph (:relational)
// downstream, so this witness lives in a TU that imports BOTH.
namespace graph_duality_witness {

// MAP reading: Succ : int → int, Cod = B = int (the analytic arrow, :morphism).
static_assert(IsArrow<Succ>, "map reading: the function IS an arrow.");
static_assert(std::same_as<Dom<Succ>, int> && std::same_as<Cod<Succ>, int>,
              "map reading: Succ : int → int (Cod = B = int).");

// RELATION reading, reached by APPLYING graph: graph(f) : A×B → Ω.
using GammaSucc = Graph<Succ>;
// (a) it is an arrow whose Domain is the PAIR type A×B and Codomain is Ω...
static_assert(IsArrow<GammaSucc>, "relation reading: graph(f) IS an arrow.");
static_assert(std::same_as<Dom<GammaSucc>, std::pair<int, int>>,
              "relation reading: graph(f)'s Domain is the PAIR type A×B.");
static_assert(
    std::same_as<Cod<GammaSucc>, bool>,
    "relation reading: graph(f)'s Codomain is Ω (= Boole::Ω = bool).");
// (b) ...and, precisely, a CHARACTERISTIC predicate on pairs (χ:A×B→Ω); the
//     IsArrow reading is SUBSUMED (IsCharacteristic ⊆ IsArrow), not a rival.
static_assert(IsCharacteristic<GammaSucc>,
              "relation reading: graph(f) is a characteristic predicate on "
              "pairs (χ:A×B→Ω); being an IsArrow is subsumed.");
// (c) ...and it IS a binary relation on A×B (dyadic.cppm's IsRelation).
static_assert(IsRelation<GammaSucc, int, int>,
              "relation reading: graph(f) IS a binary relation on int × int.");

// PUNCHLINE (load-bearing): ONE datum, TWO codomains.  The map reading
// (Cod = B) and the relation reading (Cod = Ω, a characteristic predicate on
// pairs that subsumes IsArrow) are related functorially by graph(-) --- the
// existing :relational adapter carries the whole duality with no arrow_traits
// and no :morphism edit.  #950's spider-out use case needs NOTHING from #953.
static_assert(
    !std::same_as<Cod<Succ>, Cod<GammaSucc>>,
    "the two readings differ ONLY by codomain (B = int vs Ω = bool).");

}  // namespace graph_duality_witness

// ═══ #950 spider-out: how far does graph(f) compose with the :category spider
//     (Copy/Merge/Tensor/Intersect)?  Reported HONESTLY (clean vs the gap).
namespace graph_spider_witness {

using X = std::pair<int, int>;                       // the relation carrier A×B
using R = Graph<dedekind::category::Identity<int>>;  // R : X → Ω  (b == a)
using S = Graph<Succ>;                               // S : X → Ω  (b == a+1)

// graph(f) itself satisfies NONE of the spider shape concepts: its Cod is Ω
// (bool), not a product, and its Dom is A×B, not the square of Ω.  Honest
// negatives --- graph(f) is a relation, not a comonoid leg.
static_assert(!IsTensor<R>,
              "graph(f) is NOT a Tensor: its Cod Ω is not a product object.");
static_assert(!IsCopy<R>,
              "graph(f) is NOT a Copy Δ: Cod Ω ≠ (A×B)² (the diagonal shape).");
static_assert(!IsMerge<R>,
              "graph(f) is NOT a Merge Δ†: Dom A×B ≠ Ω² (the fold shape).");

// BUT the GENERIC comonoid pieces DO compose for relations.  The relational
// intersection (R∩S)(a,b) = R(a,b) ∧ S(a,b) IS Merge ∘ (R⊗S) ∘ Copy with
// carrier X = A×B, legs R,S : X → Ω, and Merge = ∧ on Ω:
//   Copy<X>    : X → X×X    (diagonal on the PAIR carrier)
//   R ⊗ S      : X×X → Ω×Ω  (run both relations in parallel)
//   Merge<Ω,∧> : Ω×Ω → Ω    (glb = Boolean AND on the classifier)
static_assert(IsCopy<Copy<X>>, "Copy on the pair carrier X is a diagonal.");
static_assert(IsTensor<Tensor<R, S>>,
              "R ⊗ S (X×X → Ω×Ω) is the arrow-half of the product bifunctor.");
static_assert(IsMerge<Merge<bool, std::logical_and<bool>>>,
              "Merge = ∧ on Ω is a fold Ω×Ω → Ω.");
static_assert(IsArrow<Copy<X>> && IsArrow<Tensor<R, S>> &&
                  IsArrow<Merge<bool, std::logical_and<bool>>>,
              "the generic spider legs are all arrows, so Merge∘(R⊗S)∘Copy "
              "types as X → Ω (the relational meet; value witness below).");

// THE GAP (a SMALL follow-up, NOT a #950 blocker).  The PACKAGED Intersect is
// ENDO-ONLY: its requires clause demands Cod<R> == Dom<R> (an endomap A→A), but
// a graph R : X → Ω has Cod = Ω ≠ Dom = X, so Intersect<R,S,∧> does NOT type.
// The generic Copy/Tensor/Merge above ALREADY realise R∩S; only the convenience
// wrapper needs loosening from endo A→A legs to X→Ω legs.  Witnessed here as
// the EXACT endo predicate Intersect gates on (Cod == Dom), which is
// unambiguously safe --- no dependence on naming a constraint-failing template.
static_assert(std::same_as<Cod<dedekind::category::Identity<bool>>,
                           Dom<dedekind::category::Identity<bool>>>,
              "endo legs A→A meet Intersect's Cod==Dom clause: it applies (the "
              "arrow-level idempotent meet a∧a).");
static_assert(
    !std::same_as<Cod<R>, Dom<R>>,
    "graph legs X→Ω do NOT: Cod = Ω (bool) ≠ Dom = X (A×B), so the packaged "
    "endo-only Intersect<R,S,∧> does not type.  FIXME: loosen Intersect from "
    "A→A to X→Ω legs (a small #950 follow-up, NOT a blocker --- the generic "
    "Copy/Tensor/Merge above already realise R∩S).");

}  // namespace graph_spider_witness

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
// point-free Trsk halfspace surface (`𝔸 | π ⋈ fix`, in :order) and faithful
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

// #950 spider-out, at run time: the relational meet of two graphs built from
// the GENERIC :category spider pieces --- Merge ∘ (R⊗S) ∘ Copy over the pair
// carrier X = A×B, with NO endo assumption (the legs are X→Ω, not A→A).
// Exercises the Copy / Tensor / Merge operator() bodies (invisible to Codecov
// as compile-time witnesses) and confirms the composite genuinely computes R∩S.
TEST_CASE("graph: relational meet via the generic spider Merge∘(R⊗S)∘Copy",
          "[sets][graph][spider][meet]") {
  using X = std::pair<int, int>;
  using R = Graph<dedekind::category::Identity<int>>;  // (a,b) ↦ b == a
  using S = Graph<Succ>;                               // (a,b) ↦ b == a+1
  const R r = graph(dedekind::category::Identity<int>{});
  const S s = graph(Succ{});
  const Copy<X> copy{};
  const Tensor<R, S> both{r, s};
  const Merge<bool, std::logical_and<bool>> meet{};

  // (R∩S)(a,b) = R(a,b) ∧ S(a,b): "b==a" AND "b==a+1" is never both true.
  const X on_diag{5, 5};     // R true, S false → ∧ false
  const X off_by_one{5, 6};  // R false, S true → ∧ false
  CHECK_FALSE(meet(both(copy(on_diag))));
  CHECK_FALSE(meet(both(copy(off_by_one))));
  // The generic-spider composite agrees with the pointwise Boolean meet of the
  // two graph memberships --- it really is R∩S, no packaged Intersect needed.
  CHECK(meet(both(copy(on_diag))) == (r(on_diag) && s(on_diag)));
  CHECK(meet(both(copy(off_by_one))) == (r(off_by_one) && s(off_by_one)));
}
