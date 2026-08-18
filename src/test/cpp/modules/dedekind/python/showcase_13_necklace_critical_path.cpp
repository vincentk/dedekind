/**
 * @file
 * src/test/cpp/modules/dedekind/python/showcase_13_necklace_critical_path.cpp
 * @brief Showcase 13 — the diamond necklace as a relation, closed over a
 *        semiring by a fold over its edges.  One wheel, three problems a
 *        semiring apart: reachability, critical path, and its sensitivity.
 *
 *        (1,1)     (3,1)     (5,1)     (7,1)
 *         / \       / \       / \       / \
 *   (0,0)     (2,0)     (4,0)     (6,0)     (8,0)
 *         \ /       \ /       \ /       \ /
 *        (1,-1)    (3,-1)    (5,-1)    (7,-1)
 *
 * The edge relation is one analytic rule, E((x,y),(x',y')) <=> x'=x+1 &
 * |y'-y|=1; it is materialised extensionally (once, in topological order) and
 * the closure is a fold over that sequence, not nested loops.  The cost of
 * arriving at (x',y') is y'*(K-x'), so the longest path is the chevron
 * up,up,down,down = 8.
 *
 * Expected LLVM IR (at -O2), a literal load each, no solver:
 *   witness_necklace_reachable              -> ret i64 1
 *   witness_necklace_critical               -> ret i64 8
 *   witness_necklace_sensitivity_critical   -> ret i64 1   (on-path branch)
 *   witness_necklace_sensitivity_floated    -> ret i64 0   (floated branch)
 *   witness_necklace_critical_between       -> a real loop  (partial eval)
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */

#include <array>
#include <cstddef>
#include <cstdint>

import dedekind.algebra;      // MaxPlus, Tropical
import dedekind.analysis;     // Dual<F>
import dedekind.optimization; // semiring_closure, annotate, critical_path, Edge
import dedekind.sequences;    // fold

namespace {

// The necklace: W=8 (four diamonds), midfield K=4; node (x, y in {-1,0,1})
// flattens to nidx = 3x + (y+1).
constexpr int NW = 8;
constexpr int NK = 4;
constexpr std::size_t NCAP = 3 * (NW + 1);
constexpr std::size_t nidx(int x, int y) {
  return static_cast<std::size_t>(x * 3 + (y + 1));
}
constexpr std::size_t NSRC = nidx(0, 0);
constexpr std::size_t NSINK = nidx(NW, 0);

// The analytic edge relation: one step right, one step in |y|.
constexpr bool nedge(std::size_t u, std::size_t v) {
  const int x = static_cast<int>(u) / 3, y = static_cast<int>(u) % 3 - 1;
  const int x2 = static_cast<int>(v) / 3, y2 = static_cast<int>(v) % 3 - 1;
  return (x2 == x + 1) && (y2 - y == 1 || y2 - y == -1);
}

// The intensional rule, materialised extensionally (once) as the edge
// sequence the fold threads — point-free, no loop at this site.
constexpr auto necklace_edges =
    dedekind::optimization::materialise<NCAP, 32>(nedge);

}  // namespace

using dedekind::algebra::MaxPlus;
using dedekind::optimization::semiring_closure;

// --- Reachability: the Boolean semiring, defaulted to (OR, AND). ---
constexpr bool necklace_reaches = semiring_closure<bool, NCAP>(
    NSRC, NSINK, necklace_edges, [](std::size_t, std::size_t) { return true; });
static_assert(necklace_reaches);

// --- Critical path: MaxPlus, defaulted to (max, +). ---
using MPll = MaxPlus<long long>;
constexpr auto cpm_cost = [](std::size_t, std::size_t v) {
  const int x2 = static_cast<int>(v) / 3, y2 = static_cast<int>(v) % 3 - 1;
  return MPll::of(static_cast<long long>(y2) * (NK - x2));
};
constexpr MPll necklace_cpm =
    semiring_closure<MPll, NCAP>(NSRC, NSINK, necklace_edges, cpm_cost);
static_assert(necklace_cpm.finite && necklace_cpm.val == 8);

// --- Sensitivity: MaxPlus over dual numbers; the tangent is the envelope
// theorem (1 on the chosen branch, 0 otherwise). ---
using DLL = dedekind::analysis::Dual<long long>;
using MPd = MaxPlus<DLL>;
constexpr auto sens_cost = [](int peak) {
  return [peak](std::size_t, std::size_t v) {
    const int x2 = static_cast<int>(v) / 3, y2 = static_cast<int>(v) % 3 - 1;
    const long long value = static_cast<long long>(y2) * (NK - x2);
    return MPd::of(DLL{value, (v == nidx(peak, 1)) ? 1 : 0});
  };
};
constexpr MPd necklace_sens_crit =
    semiring_closure<MPd, NCAP>(NSRC, NSINK, necklace_edges, sens_cost(1));
static_assert(necklace_sens_crit.val.val == 8 &&
              necklace_sens_crit.val.der == 1);
constexpr MPd necklace_sens_float =
    semiring_closure<MPd, NCAP>(NSRC, NSINK, necklace_edges, sens_cost(7));
static_assert(necklace_sens_float.val.val == 8 &&
              necklace_sens_float.val.der == 0);

/** @brief Sink reachability.  IR: ret i64 1. */
extern "C" __attribute__((noinline)) int64_t witness_necklace_reachable() {
  return necklace_reaches ? 1 : 0;
}
/** @brief Critical-path value.  IR: ret i64 8. */
extern "C" __attribute__((noinline)) int64_t witness_necklace_critical() {
  return static_cast<int64_t>(necklace_cpm.val);
}
/** @brief Criticality of the up-branch at x=1 (on the path).  IR: ret i64 1. */
extern "C" __attribute__((noinline)) int64_t
witness_necklace_sensitivity_critical() {
  return static_cast<int64_t>(necklace_sens_crit.val.der);
}
/** @brief Criticality of the up-branch at x=7 (floated).  IR: ret i64 0. */
extern "C" __attribute__((noinline)) int64_t
witness_necklace_sensitivity_floated() {
  return static_cast<int64_t>(necklace_sens_float.val.der);
}

/** @brief Partial evaluation: endpoints at run time.  The topology, semiring,
 *  and costs are specialised into the residual; only the query is parametric.
 *  IR: a real fold loop, not a constant. */
extern "C" __attribute__((noinline)) int64_t
witness_necklace_critical_between(std::size_t source, std::size_t sink) {
  return static_cast<int64_t>(
      semiring_closure<MPll, NCAP>(source, sink, necklace_edges, cpm_cost).val);
}

// --- The same result the ddk way: collapse the relation to the critical-path
// FUNCTION (annotate → pred net), iterate it to the path, fold the path. ---
constexpr auto necklace_pred = dedekind::optimization::annotate<MPll, NCAP>(
    NSRC, necklace_edges, cpm_cost);
constexpr auto necklace_path =
    dedekind::optimization::critical_path<NCAP>(necklace_pred, NSRC, NSINK);

// Value: fold the ⊗-costs along the path sequence to a scalar (= d(sink)).
constexpr long long necklace_path_value = dedekind::sequences::fold(
    necklace_path, 0LL,
    [](long long& acc, const dedekind::optimization::Edge& e) {
      acc += cpm_cost(e.tail, e.head).val;
    });
static_assert(necklace_path_value == 8);

// Sensitivity is now membership: an edge is critical iff it lies on the path.
constexpr auto on_path = [](const auto& path, std::size_t u, std::size_t v) {
  return dedekind::sequences::fold(
      path, false, [u, v](bool& hit, const dedekind::optimization::Edge& e) {
        hit = hit || (e.tail == u && e.head == v);
      });
};
static_assert(on_path(necklace_path, nidx(0, 0),
                      nidx(1, 1)));  // up @ x=1: critical
static_assert(!on_path(necklace_path, nidx(6, 0),
                       nidx(7, 1)));  // up @ x=7: floated

/** @brief Critical-path value via annotate → path → fold.  IR: ret i64 8. */
extern "C" __attribute__((noinline)) int64_t witness_necklace_path_value() {
  return static_cast<int64_t>(necklace_path_value);
}
