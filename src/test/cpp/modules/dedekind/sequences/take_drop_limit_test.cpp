// ---------------------------------------------------------------------------
// take / limit / drop (#753) — predicate-defined subobjects on countable
// sets of pairs (the relational form of a Path's graph).
//
// Per the §3 review (2026-06-03), take/drop/limit are σ-with-derived-pivot
// operators in Codd-Date *Third Manifesto* vocabulary: predicate-defined
// subobjects of a sequence's graph (relation form), with the cutoff named
// as a predicate on the index column.  This is the "drop / take are
// re-written as predicates on countable set" design tracked in #753.
//
// The Path-level iterable forms (prefix / drop returning FinitePath) remain
// at :sequences:path under their Bird-Meertens names and are tested
// elsewhere (path_test.cpp).
//
// @copyright 2026 The Dedekind Authors
// Licensed under the Apache License, Version 2.0.
// ---------------------------------------------------------------------------

#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <utility>

import dedekind.sequences;
import dedekind.sets;

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

TEST_CASE("take(relation, n) restricts the index column to [0, n)",
          "[sequences][take][relation]") {
  const auto path = make_test_path();
  const auto rel = as_relation(path);
  const auto first_three = take(rel, std::size_t{3});

  SECTION("Pairs with index < n that are on the graph belong to take") {
    REQUIRE(first_three(std::pair<std::size_t, unsigned>{0u, 10u}));
    REQUIRE(first_three(std::pair<std::size_t, unsigned>{1u, 20u}));
    REQUIRE(first_three(std::pair<std::size_t, unsigned>{2u, 30u}));
  }

  SECTION("Pairs with index >= n are excluded even if on the graph") {
    REQUIRE_FALSE(first_three(std::pair<std::size_t, unsigned>{3u, 40u}));
    REQUIRE_FALSE(first_three(std::pair<std::size_t, unsigned>{4u, 50u}));
  }

  SECTION("Pairs not on the graph remain excluded") {
    REQUIRE_FALSE(first_three(std::pair<std::size_t, unsigned>{0u, 99u}));
    REQUIRE_FALSE(first_three(std::pair<std::size_t, unsigned>{1u, 21u}));
  }

  SECTION("limit is an alias for take") {
    const auto via_limit = limit(rel, std::size_t{3});
    REQUIRE(via_limit(std::pair<std::size_t, unsigned>{0u, 10u}));
    REQUIRE_FALSE(via_limit(std::pair<std::size_t, unsigned>{3u, 40u}));
  }
}

TEST_CASE("drop(relation, n) restricts the index column to [n, +inf)",
          "[sequences][drop][relation]") {
  const auto path = make_test_path();
  const auto rel = as_relation(path);
  const auto skipped_two = drop(rel, std::size_t{2});

  SECTION("Pairs with index >= n that are on the graph belong to drop") {
    REQUIRE(skipped_two(std::pair<std::size_t, unsigned>{2u, 30u}));
    REQUIRE(skipped_two(std::pair<std::size_t, unsigned>{3u, 40u}));
    REQUIRE(skipped_two(std::pair<std::size_t, unsigned>{4u, 50u}));
  }

  SECTION("Pairs with index < n are excluded even if on the graph") {
    REQUIRE_FALSE(skipped_two(std::pair<std::size_t, unsigned>{0u, 10u}));
    REQUIRE_FALSE(skipped_two(std::pair<std::size_t, unsigned>{1u, 20u}));
  }
}

TEST_CASE("drop ∘ take gives a window on the index column",
          "[sequences][take][drop][window]") {
  // The §3-page-2 canonical exhibit: drop(take(rel, m), n) is the
  // sub-relation restricted to index positions [n, m).  Take=cutoff above,
  // drop=cutoff below; their composition is the natural interval-on-index
  // operation, mirroring the OrderInterval pattern §5.4 names.
  const auto path = make_test_path();
  const auto rel = as_relation(path);

  // SQL: SELECT * FROM rel WHERE index >= 1 AND index < 4
  //    = the sub-relation at indices [1, 4) = pairs (1,20), (2,30), (3,40)
  const auto window = drop(take(rel, std::size_t{4}), std::size_t{1});

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

TEST_CASE("Codd-Date σ-with-derived-pivot: take ≡ select with index cutoff",
          "[sequences][take][codd]") {
  // The §3-page-2 thesis: take is σ with a derived upper-cutoff pivot
  // (Date & Darwen, Third Manifesto, Prescription 7).  Demonstrate the
  // equivalence mechanically: take(rel, n) and select(rel, λp. p.first<n)
  // produce subobjects with the same membership predicate.
  const auto path = make_test_path();
  const auto rel = as_relation(path);

  const auto via_take = take(rel, std::size_t{3});
  const auto via_select =
      select(rel, [](const std::pair<std::size_t, unsigned>& p) -> bool {
        return p.first < std::size_t{3};
      });

  SECTION("Both produce the same membership decisions") {
    // On-graph pairs in [0, 3): both accept
    REQUIRE(via_take(std::pair<std::size_t, unsigned>{0u, 10u}));
    REQUIRE(via_select(std::pair<std::size_t, unsigned>{0u, 10u}));
    REQUIRE(via_take(std::pair<std::size_t, unsigned>{2u, 30u}));
    REQUIRE(via_select(std::pair<std::size_t, unsigned>{2u, 30u}));
    // On-graph pairs at index >= 3: both reject
    REQUIRE_FALSE(via_take(std::pair<std::size_t, unsigned>{3u, 40u}));
    REQUIRE_FALSE(via_select(std::pair<std::size_t, unsigned>{3u, 40u}));
    // Off-graph pairs: both reject (source predicate veto)
    REQUIRE_FALSE(via_take(std::pair<std::size_t, unsigned>{0u, 99u}));
    REQUIRE_FALSE(via_select(std::pair<std::size_t, unsigned>{0u, 99u}));
  }
}
