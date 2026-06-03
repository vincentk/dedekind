// ---------------------------------------------------------------------------
// take / limit / drop (#753) — structure-preserving subobject operators.
//
// Per the design pivot tracked in #753 (2026-06-03), take / limit / drop are
// subobject operations that PRESERVE the source's enumeration structure
// rather than materialising it.  The three traditions converge on this
// reading: Codd-via-Date treats LIMIT as σ-with-derived-pivot; Lawvere via
// Mac Lane treats take as precomposition with the initial-ordinal inclusion
// (an image factorisation); Pierce-via-Wadler treats it as a parametric
// early-stopped hylomorphism.  The output is a FinitePath<T> — the
// IsFiniteSequence inhabitant whose generator reads through to the source
// lazily.
//
// take / limit / drop live at :sequences:path as free functions exported
// alongside prefix, as_sequence, from_range.  This test exercises them
// against the stdlib adapters (std::vector, std::set).
//
// @copyright 2026 The Dedekind Authors
// Licensed under the Apache License, Version 2.0.
// ---------------------------------------------------------------------------

#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <set>
#include <type_traits>
#include <vector>

import dedekind.sequences;

using namespace dedekind::sequences;

TEST_CASE("take / limit on std::vector — lazy prefix view",
          "[sequences][take][limit][vector]") {
  const std::vector<unsigned> source{10u, 20u, 30u, 40u, 50u};

  SECTION("take(v, n) returns first n elements in vector order") {
    const auto picked = take(source, 3u);
    REQUIRE(picked.size() == 3u);
    REQUIRE(picked.at(0) == 10u);
    REQUIRE(picked.at(1) == 20u);
    REQUIRE(picked.at(2) == 30u);
  }

  SECTION("take(v, n) for n > |v| returns the full source") {
    const auto picked = take(source, 100u);
    REQUIRE(picked.size() == source.size());
    for (std::size_t i = 0; i < picked.size(); ++i) {
      REQUIRE(picked.at(i) == source[i]);
    }
  }

  SECTION("take(v, 0) returns an empty FinitePath") {
    const auto picked = take(source, 0u);
    REQUIRE(picked.size() == 0u);
  }

  SECTION("limit is an alias for take") {
    const auto via_take = take(source, 3u);
    const auto via_limit = limit(source, 3u);
    REQUIRE(via_take.size() == via_limit.size());
    for (std::size_t i = 0; i < via_take.size(); ++i) {
      REQUIRE(via_take.at(i) == via_limit.at(i));
    }
  }
}

TEST_CASE("take on std::set — first n in sorted order",
          "[sequences][take][set]") {
  // std::set is sorted by Compare; as_sequence(s) returns elements in
  // ascending order via in-order traversal.
  const std::set<unsigned> source{50u, 10u, 30u, 20u, 40u};  // insertion order
                                                             // doesn't matter

  SECTION("take(s, n) returns the n smallest elements (sorted)") {
    const auto picked = take(source, 3u);
    REQUIRE(picked.size() == 3u);
    REQUIRE(picked.at(0) == 10u);
    REQUIRE(picked.at(1) == 20u);
    REQUIRE(picked.at(2) == 30u);
  }
}

TEST_CASE("drop on std::vector — finite tail of size max(0, |v|-n)",
          "[sequences][drop][vector]") {
  const std::vector<unsigned> source{10u, 20u, 30u, 40u, 50u};

  SECTION("drop(v, n) skips the first n elements") {
    const auto remaining = drop(source, 2u);
    REQUIRE(remaining.size() == 3u);
    REQUIRE(remaining.at(0) == 30u);
    REQUIRE(remaining.at(1) == 40u);
    REQUIRE(remaining.at(2) == 50u);
  }

  SECTION("drop(v, n) for n >= |v| returns an empty path") {
    const auto remaining = drop(source, 10u);
    REQUIRE(remaining.size() == 0u);
  }
}

TEST_CASE("drop on std::set — skip the n smallest elements",
          "[sequences][drop][set]") {
  const std::set<unsigned> source{10u, 20u, 30u, 40u, 50u};

  SECTION("drop(s, n) yields the |s|-n largest elements in sorted order") {
    const auto remaining = drop(source, 2u);
    REQUIRE(remaining.size() == 3u);
    REQUIRE(remaining.at(0) == 30u);
    REQUIRE(remaining.at(1) == 40u);
    REQUIRE(remaining.at(2) == 50u);
  }
}

TEST_CASE("drop ∘ take = interval — sequences-by-default composition",
          "[sequences][take][drop][interval]") {
  // The §3 canonical exhibit: drop(take(s, m), n) is the sub-sequence
  // in positions [n, m).  For std::set this is the value-based interval
  // between the n-th and (m-1)-th smallest elements (inclusive of the
  // n-th, exclusive of the m-th).
  //
  // This composition is what the paper §3 reshape (#744 B3) cites as
  // the "sequences-by-default" exhibit: take and drop preserve the
  // source's enumeration structure, returning FinitePath<T> views that
  // compose mechanically to produce intervals.
  const std::set<unsigned> source{10u, 20u, 30u, 40u, 50u};

  // SQL: SELECT * FROM source ORDER BY value LIMIT 4 OFFSET 1
  // = sub-sequence in positions [1, 4) of the sorted order
  // = {20, 30, 40}
  const auto interval = drop(take(source, 4u), 1u);

  REQUIRE(interval.size() == 3u);
  REQUIRE(interval.at(0) == 20u);
  REQUIRE(interval.at(1) == 30u);
  REQUIRE(interval.at(2) == 40u);
}

TEST_CASE("take output is an IsFiniteSequence (categorical commitment)",
          "[sequences][take][is-finite-sequence]") {
  // Per the design pivot, take's output is a structure-preserving
  // subobject of the source: a FinitePath<T>, which is the canonical
  // IsFiniteSequence inhabitant.  Static-assert the shape at the type
  // level so the categorical commitment is mechanical, not prose.
  const std::vector<unsigned> source{1u, 2u, 3u, 4u, 5u};
  const auto picked = take(source, 3u);

  static_assert(IsFiniteSequence<std::remove_cvref_t<decltype(picked)>>,
                "take's output must be an IsFiniteSequence — structure "
                "preservation is the design commitment per #753.");
}
