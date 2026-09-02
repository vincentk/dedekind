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
import dedekind.relational;

using namespace dedekind::category;
using namespace dedekind::sequences;
using namespace dedekind::sets;

// Use the same Form-shaped ℕ alias the samples partition exports —
// @c ExtensionalCardinal<> is the algebraically-certified finite ℕ
// carrier, and Path's @c Index template parameter carries it at both
// the Index and the carrier roles on @c is_even and @c fibonacci .
// Test literals on the value side use this alias to avoid the
// @c (unsigned @c int) @c vs @c (ExtensionalCardinal<>) implicit-
// conversion ambiguity that bare @c 0u literals trigger.
using ℕ_Form = ExtensionalCardinal<>;

TEST_CASE("is_even pins the parity sequence on its first six indices",
          "[sequences][samples][is_even]") {
  REQUIRE(is_even.at(ℕ_Form{0u}) == Boolean{true});
  REQUIRE(is_even.at(ℕ_Form{1u}) == Boolean{false});
  REQUIRE(is_even.at(ℕ_Form{2u}) == Boolean{true});
  REQUIRE(is_even.at(ℕ_Form{3u}) == Boolean{false});
  REQUIRE(is_even.at(ℕ_Form{4u}) == Boolean{true});
  REQUIRE(is_even.at(ℕ_Form{5u}) == Boolean{false});
}

TEST_CASE("fibonacci pins the canonical sequence 0, 1, 1, 2, 3, 5, 8, 13",
          "[sequences][samples][fibonacci]") {
  // Canonical Fibonacci F_0 = 0, F_1 = 1, F_{n+2} = F_n + F_{n+1}.
  REQUIRE(fibonacci.at(0) == ℕ_Form{0u});
  REQUIRE(fibonacci.at(1) == ℕ_Form{1u});
  REQUIRE(fibonacci.at(2) == ℕ_Form{1u});
  REQUIRE(fibonacci.at(3) == ℕ_Form{2u});
  REQUIRE(fibonacci.at(4) == ℕ_Form{3u});
  REQUIRE(fibonacci.at(5) == ℕ_Form{5u});
  REQUIRE(fibonacci.at(6) == ℕ_Form{8u});
  REQUIRE(fibonacci.at(7) == ℕ_Form{13u});
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
  // On-graph pairs: (n, is_even(n)).  Pair index column is ℕ (Form-
  // shaped) because @c is_even now carries @c ExtensionalCardinal<>
  // at its Path's @c Index role.
  REQUIRE(rel(std::pair<ℕ_Form, Boolean>{ℕ_Form{0u}, Boolean{true}}));
  REQUIRE(rel(std::pair<ℕ_Form, Boolean>{ℕ_Form{3u}, Boolean{false}}));
  REQUIRE(rel(std::pair<ℕ_Form, Boolean>{ℕ_Form{42u}, Boolean{true}}));
  // Off-graph (wrong value at the index): rejected.
  REQUIRE_FALSE(rel(std::pair<ℕ_Form, Boolean>{ℕ_Form{0u}, Boolean{false}}));
  REQUIRE_FALSE(rel(std::pair<ℕ_Form, Boolean>{ℕ_Form{3u}, Boolean{true}}));
}

TEST_CASE("as_relation(fibonacci) graph membership",
          "[sequences][samples][as_relation]") {
  const auto rel = as_relation(fibonacci);
  // On-graph: (n, F_n).  Both pair components carry ℕ — index because
  // @c fibonacci 's Path Index is @c std::size_t (n-ary @c iterate
  // FIXME), value because @c fibonacci 's carrier @c T is
  // @c ExtensionalCardinal<> .
  REQUIRE(rel(std::pair<std::size_t, ℕ_Form>{0u, ℕ_Form{0u}}));
  REQUIRE(rel(std::pair<std::size_t, ℕ_Form>{7u, ℕ_Form{13u}}));
  REQUIRE(rel(std::pair<std::size_t, ℕ_Form>{10u, ℕ_Form{55u}}));
  // Off-graph (wrong value): rejected.
  REQUIRE_FALSE(rel(std::pair<std::size_t, ℕ_Form>{7u, ℕ_Form{14u}}));
  REQUIRE_FALSE(rel(std::pair<std::size_t, ℕ_Form>{10u, ℕ_Form{21u}}));
}

TEST_CASE("θ-join via σ∘× on (is_even, fibonacci) exhibits a heterogeneous row",
          "[sequences][samples][theta_join][page2]") {
  // The §3 page-2 page-anchor: lift both sequences to their graph
  // relations, take their Cartesian product (×), and select (σ) the
  // pairs whose index columns agree.  This is Codd's general θ-join
  // shape, the same posture by which natural_join / zip / LIMIT are
  // not separately named (per #753).  The output row's type is
  //   pair<pair<ℕ_Form, 𝔹>, pair<ℕ_Form, ℕ>>
  // — heterogeneous in the Boolean column, a tuple-of-tuples, NOT a
  // list of one type.  That heterogeneity is the page-2 beat the
  // paper §3 listing exhibits.
  // Note on the @em heterogeneity of the row's index columns:
  // @c is_even 's index column is @c ℕ_Form (@c ExtensionalCardinal<> )
  // while @c fibonacci 's index column is @c std::size_t (the n-ary
  // @c iterate FIXME documented in @c :sequences:path ).  The θ
  // predicate compares them via @c ExtensionalCardinal<> 's implicit
  // ctor from @c std::unsigned_integral .
  using EvenPair = std::pair<ℕ_Form, Boolean>;
  using FibPair = std::pair<std::size_t, ℕ_Form>;
  using JoinedRow = std::pair<EvenPair, FibPair>;

  const auto rel_even = as_relation(is_even);
  const auto rel_fib = as_relation(fibonacci);

  const auto joined = select(
      cartesian_product(rel_even, rel_fib), [](const JoinedRow& row) -> bool {
        // Compare via the ℕ-Form: lift size_t
        // through ExtensionalCardinal's implicit
        // ctor and use the defaulted ==.
        return row.first.first == ℕ_Form{row.second.first};
      });

  SECTION("Diagonal row at n=7: (7, false, 13) is in the join") {
    // is_even(7) = false, fibonacci(7) = 13 — the canonical paper-citable
    // row that exhibits the heterogeneous (ℕ, 𝔹, ℕ) shape.
    REQUIRE(joined(JoinedRow{EvenPair{ℕ_Form{7u}, Boolean{false}},
                             FibPair{7u, ℕ_Form{13u}}}));
  }

  SECTION("Diagonal row at n=8: (8, true, 21) is in the join") {
    // is_even(8) = true, fibonacci(8) = 21 — a second on-diagonal row,
    // pinning that the join is not a coincidence at n=7 only.
    REQUIRE(joined(JoinedRow{EvenPair{ℕ_Form{8u}, Boolean{true}},
                             FibPair{8u, ℕ_Form{21u}}}));
  }

  SECTION("Off-diagonal row (mismatched indices) is rejected") {
    // is_even(7) = false, fibonacci(8) = 21 — but the indices disagree
    // (7 ≠ 8), so the θ-predicate vetoes the row.
    REQUIRE_FALSE(joined(JoinedRow{EvenPair{ℕ_Form{7u}, Boolean{false}},
                                   FibPair{8u, ℕ_Form{21u}}}));
  }

  SECTION("Off-graph row (wrong fibonacci value) is rejected") {
    // The source-predicate veto from as_relation(fibonacci) fires:
    // fibonacci(7) ≠ 99, so even with matched indices, (7, false, 99)
    // is off the fibonacci graph.
    REQUIRE_FALSE(joined(JoinedRow{EvenPair{ℕ_Form{7u}, Boolean{false}},
                                   FibPair{7u, ℕ_Form{99u}}}));
  }
}
