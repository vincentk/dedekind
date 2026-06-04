// ---------------------------------------------------------------------------
// :sequences:samples — `is_even` and `fibonacci` (#753 §3 page-2).
//
// Tests the named §3 page-2 sample pair: their first few values pin the
// canonical sequences mechanically, their `as_relation` lifts respect
// the graph predicate (Bourbaki function-as-graph, from #755), and the
// θ-join via σ∘× exhibits a heterogeneous row (ℕ, 𝔹, ℕ) — the page-2
// "tuples not lists" beat that the §3 reshape PR's listing will cite.
//
// @copyright 2026 The Dedekind Authors
// Licensed under the Apache License, Version 2.0.
// ---------------------------------------------------------------------------

#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <utility>

import dedekind.category;
import dedekind.sequences;
import dedekind.sets;

using namespace dedekind::category;
using namespace dedekind::sequences;
using namespace dedekind::sets;

TEST_CASE("is_even pins the parity sequence on its first six indices",
          "[sequences][samples][is_even]") {
  REQUIRE(is_even.at(0) == Boolean{true});
  REQUIRE(is_even.at(1) == Boolean{false});
  REQUIRE(is_even.at(2) == Boolean{true});
  REQUIRE(is_even.at(3) == Boolean{false});
  REQUIRE(is_even.at(4) == Boolean{true});
  REQUIRE(is_even.at(5) == Boolean{false});
}

TEST_CASE("fibonacci pins the canonical sequence 0, 1, 1, 2, 3, 5, 8, 13",
          "[sequences][samples][fibonacci]") {
  // Canonical Fibonacci F_0 = 0, F_1 = 1, F_{n+2} = F_n + F_{n+1}.
  REQUIRE(fibonacci.at(0) == 0u);
  REQUIRE(fibonacci.at(1) == 1u);
  REQUIRE(fibonacci.at(2) == 1u);
  REQUIRE(fibonacci.at(3) == 2u);
  REQUIRE(fibonacci.at(4) == 3u);
  REQUIRE(fibonacci.at(5) == 5u);
  REQUIRE(fibonacci.at(6) == 8u);
  REQUIRE(fibonacci.at(7) == 13u);
}

TEST_CASE("fibonacci_of<T> is Form-generic across IsRingIntegral carriers",
          "[sequences][samples][fibonacci][parametric]") {
  // The §3-page-2 algebraic-genericity beat: the recurrence is parameterised
  // on the same IsRingIntegral gate that IsSequence imposes on its Domain
  // (per :order:halfspace and the #755 substrate PR).  Instantiating
  // fibonacci_of<T> at a non-default T (here unsigned int) gives the same
  // canonical sequence — mechanical witness that the named `fibonacci`
  // is just the T = std::size_t instantiation of a Form-generic recurrence,
  // not a carrier-specific exhibit.
  const auto& fib_unsigned = fibonacci_of<unsigned int>;
  REQUIRE(fib_unsigned.at(0) == 0u);
  REQUIRE(fib_unsigned.at(1) == 1u);
  REQUIRE(fib_unsigned.at(5) == 5u);
  REQUIRE(fib_unsigned.at(7) == 13u);
}

TEST_CASE("as_relation(is_even) graph membership",
          "[sequences][samples][as_relation]") {
  const auto rel = as_relation(is_even);
  // On-graph pairs: (n, is_even(n)).
  REQUIRE(rel(std::pair<std::size_t, Boolean>{0u, Boolean{true}}));
  REQUIRE(rel(std::pair<std::size_t, Boolean>{3u, Boolean{false}}));
  REQUIRE(rel(std::pair<std::size_t, Boolean>{42u, Boolean{true}}));
  // Off-graph (wrong value at the index): rejected.
  REQUIRE_FALSE(rel(std::pair<std::size_t, Boolean>{0u, Boolean{false}}));
  REQUIRE_FALSE(rel(std::pair<std::size_t, Boolean>{3u, Boolean{true}}));
}

TEST_CASE("as_relation(fibonacci) graph membership",
          "[sequences][samples][as_relation]") {
  const auto rel = as_relation(fibonacci);
  // On-graph: (n, F_n).
  REQUIRE(rel(std::pair<std::size_t, std::size_t>{0u, 0u}));
  REQUIRE(rel(std::pair<std::size_t, std::size_t>{7u, 13u}));
  REQUIRE(rel(std::pair<std::size_t, std::size_t>{10u, 55u}));
  // Off-graph (wrong value): rejected.
  REQUIRE_FALSE(rel(std::pair<std::size_t, std::size_t>{7u, 14u}));
  REQUIRE_FALSE(rel(std::pair<std::size_t, std::size_t>{10u, 21u}));
}

TEST_CASE("θ-join via σ∘× on (is_even, fibonacci) exhibits a heterogeneous row",
          "[sequences][samples][theta_join][page2]") {
  // The §3 page-2 page-anchor: lift both sequences to their graph
  // relations, take their Cartesian product (×), and select (σ) the
  // pairs whose index columns agree.  This is Codd's general θ-join
  // shape, the same posture by which natural_join / zip / LIMIT are
  // not separately named (per #753).  The output row's type is
  //   pair<pair<ℕ, 𝔹>, pair<ℕ, ℕ>>
  // — heterogeneous in the Boolean column, a tuple-of-tuples, NOT a
  // list of one type.  That heterogeneity is the page-2 beat the
  // paper §3 listing exhibits.
  using EvenPair = std::pair<std::size_t, Boolean>;
  using FibPair = std::pair<std::size_t, std::size_t>;
  using JoinedRow = std::pair<EvenPair, FibPair>;

  const auto rel_even = as_relation(is_even);
  const auto rel_fib = as_relation(fibonacci);

  const auto joined = select(cartesian_product(rel_even, rel_fib),
                             [](const JoinedRow& row) -> bool {
                               return row.first.first == row.second.first;
                             });

  SECTION("Diagonal row at n=7: (7, false, 13) is in the join") {
    // is_even(7) = false, fibonacci(7) = 13 — the canonical paper-citable
    // row that exhibits the heterogeneous (ℕ, 𝔹, ℕ) shape.
    REQUIRE(joined(JoinedRow{EvenPair{7u, Boolean{false}}, FibPair{7u, 13u}}));
  }

  SECTION("Diagonal row at n=8: (8, true, 21) is in the join") {
    // is_even(8) = true, fibonacci(8) = 21 — a second on-diagonal row,
    // pinning that the join is not a coincidence at n=7 only.
    REQUIRE(joined(JoinedRow{EvenPair{8u, Boolean{true}}, FibPair{8u, 21u}}));
  }

  SECTION("Off-diagonal row (mismatched indices) is rejected") {
    // is_even(7) = false, fibonacci(8) = 21 — but the indices disagree
    // (7 ≠ 8), so the θ-predicate vetoes the row.
    REQUIRE_FALSE(
        joined(JoinedRow{EvenPair{7u, Boolean{false}}, FibPair{8u, 21u}}));
  }

  SECTION("Off-graph row (wrong fibonacci value) is rejected") {
    // The source-predicate veto from as_relation(fibonacci) fires:
    // fibonacci(7) ≠ 99, so even with matched indices, (7, false, 99)
    // is off the fibonacci graph.
    REQUIRE_FALSE(
        joined(JoinedRow{EvenPair{7u, Boolean{false}}, FibPair{7u, 99u}}));
  }
}
