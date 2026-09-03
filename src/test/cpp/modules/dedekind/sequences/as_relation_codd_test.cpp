// ---------------------------------------------------------------------------
// as_relation + Codd-Date σ-with-derived-pivot (#753).
//
// Per the §3 review (2026-06-03), a sequence is — in Bourbaki's reading —
// the set of pairs that is its graph.  `as_relation(Path<T>)` lifts a Path
// into that set-of-pairs form, after which Codd's relational machinery
// (σ via `select`, × via `cartesian_product`, ⋈ via `select ∘ ×`) acts on
// it directly.
//
// In particular, Codd's `LIMIT N` (and its dual, the relational `DROP N`)
// is σ with a derived index-cutoff predicate — `select(R, λ(i,t). i < n)`
// for the prefix and `i >= n` for the tail.  We do NOT ship `take` /
// `drop` / `limit` as separate named primitives on the relation form, in
// the same posture by which we do not ship `zip` (it is
// `select(cartesian_product(R, S), key_match)`): naming such
// σ-specialisations obscures the algebraic content the codebase wants the
// reader to read off directly.
//
// These tests exhibit the lift and the σ-specialisation reading; the
// Bird-Meertens iterable `prefix(Path, n) → FinitePath<T>` (the form that
// participates in `for`-loops, `partial_sum`, etc.) is exercised in
// `path_test.cpp`.
//
// @copyright 2026 The Dedekind Authors
// Licensed under the Apache License, Version 2.0.
// ---------------------------------------------------------------------------

#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <utility>

import dedekind.sequences;
import dedekind.sets;
import dedekind.relational;

using namespace dedekind::sequences;
using namespace dedekind::sets;

namespace {

// A small Path<unsigned, Finite> for testing: i ↦ 10 * (i + 1).
constexpr auto make_test_path() {
  return FinitePath<unsigned>{[](std::size_t i) -> unsigned {
                                return static_cast<unsigned>(10 * (i + 1));
                              },
                              /*extent=*/5};
}

}  // namespace

TEST_CASE("as_relation lifts a Path to its graph predicate",
          "[sequences][as_relation][graph]") {
  const auto path = make_test_path();
  const auto rel = as_relation(path);

  SECTION("Pairs on the graph satisfy the relation") {
    // path.at(i) = 10 * (i + 1), so the graph contains:
    //   (0, 10), (1, 20), (2, 30), (3, 40), (4, 50)
    REQUIRE(rel(std::pair<std::size_t, unsigned>{0u, 10u}));
    REQUIRE(rel(std::pair<std::size_t, unsigned>{1u, 20u}));
    REQUIRE(rel(std::pair<std::size_t, unsigned>{2u, 30u}));
    REQUIRE(rel(std::pair<std::size_t, unsigned>{4u, 50u}));
  }

  SECTION("Pairs not on the graph fail the relation") {
    // Wrong value at index 0
    REQUIRE_FALSE(rel(std::pair<std::size_t, unsigned>{0u, 99u}));
    // Index 5 is past the path's extent
    REQUIRE_FALSE(rel(std::pair<std::size_t, unsigned>{5u, 60u}));
    // Mismatched (correct index, wrong value)
    REQUIRE_FALSE(rel(std::pair<std::size_t, unsigned>{2u, 31u}));
  }
}

TEST_CASE("Codd LIMIT N as σ-with-derived-pivot: select(R, λ(i,t). i < n)",
          "[sequences][as_relation][codd][limit]") {
  // The §3-page-2 exhibit: Codd's LIMIT N (Date & Darwen, Third Manifesto,
  // Prescription 7) is σ with a derived upper-cutoff pivot on the index
  // column.  We spell it inline as `select(R, λ(i,t). i < n)` — no named
  // primitive — making the algebraic content visible at the call site.
  const auto path = make_test_path();
  const auto rel = as_relation(path);
  const auto first_three =
      select(rel, [](const std::pair<std::size_t, unsigned>& p) -> bool {
        return p.first < std::size_t{3};
      });

  SECTION("Pairs with index < n that are on the graph belong") {
    REQUIRE(first_three(std::pair<std::size_t, unsigned>{0u, 10u}));
    REQUIRE(first_three(std::pair<std::size_t, unsigned>{1u, 20u}));
    REQUIRE(first_three(std::pair<std::size_t, unsigned>{2u, 30u}));
  }

  SECTION("Pairs with index >= n are excluded even if on the graph") {
    REQUIRE_FALSE(first_three(std::pair<std::size_t, unsigned>{3u, 40u}));
    REQUIRE_FALSE(first_three(std::pair<std::size_t, unsigned>{4u, 50u}));
  }

  SECTION("Pairs not on the graph remain excluded (source predicate veto)") {
    REQUIRE_FALSE(first_three(std::pair<std::size_t, unsigned>{0u, 99u}));
    REQUIRE_FALSE(first_three(std::pair<std::size_t, unsigned>{1u, 21u}));
  }
}

TEST_CASE("Dual of LIMIT (relational tail) as select(R, λ(i,t). i >= n)",
          "[sequences][as_relation][codd][tail]") {
  // The dual cutoff: the relational tail from index n onward.  Same
  // σ-specialisation shape, opposite predicate on the index column.
  const auto path = make_test_path();
  const auto rel = as_relation(path);
  const auto skipped_two =
      select(rel, [](const std::pair<std::size_t, unsigned>& p) -> bool {
        return p.first >= std::size_t{2};
      });

  SECTION("Pairs with index >= n that are on the graph belong") {
    REQUIRE(skipped_two(std::pair<std::size_t, unsigned>{2u, 30u}));
    REQUIRE(skipped_two(std::pair<std::size_t, unsigned>{3u, 40u}));
    REQUIRE(skipped_two(std::pair<std::size_t, unsigned>{4u, 50u}));
  }

  SECTION("Pairs with index < n are excluded even if on the graph") {
    REQUIRE_FALSE(skipped_two(std::pair<std::size_t, unsigned>{0u, 10u}));
    REQUIRE_FALSE(skipped_two(std::pair<std::size_t, unsigned>{1u, 20u}));
  }
}

TEST_CASE("Conjoining cutoffs as σ ∘ σ gives an index-column window",
          "[sequences][as_relation][window]") {
  // Codd-via-Date again: the sub-relation at index positions [m, n) is
  // the σ-conjunction of the two cutoff predicates.  Same algebra as
  // SQL `WHERE index >= m AND index < n` — exhibited here as the
  // composition of two `select` calls.
  const auto path = make_test_path();
  const auto rel = as_relation(path);

  // The window at indices [1, 4) — pairs (1,20), (2,30), (3,40).
  const auto window =
      select(select(rel,
                    [](const std::pair<std::size_t, unsigned>& p) -> bool {
                      return p.first < std::size_t{4};
                    }),
             [](const std::pair<std::size_t, unsigned>& p) -> bool {
               return p.first >= std::size_t{1};
             });

  SECTION("Pairs in the window belong") {
    REQUIRE(window(std::pair<std::size_t, unsigned>{1u, 20u}));
    REQUIRE(window(std::pair<std::size_t, unsigned>{2u, 30u}));
    REQUIRE(window(std::pair<std::size_t, unsigned>{3u, 40u}));
  }

  SECTION("Pairs outside the window are excluded") {
    REQUIRE_FALSE(window(std::pair<std::size_t, unsigned>{0u, 10u}));
    REQUIRE_FALSE(window(std::pair<std::size_t, unsigned>{4u, 50u}));
  }

  SECTION("Pairs not on the graph remain excluded") {
    REQUIRE_FALSE(window(std::pair<std::size_t, unsigned>{2u, 31u}));
  }
}
