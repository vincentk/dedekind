#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <utility>

// ─────────────────────────────────────────────────────────────────────────────
// KATA — a SYMBOLIC TRIDIAGONAL BAND over the infinite line ℕ, in idiomatic
// Trsk + linear_algebra.  The shape (user's flavour):
//
//     ℕ × ℕ  |  |y − x| ≤ 1   ⟨masked⟩   u(x) ⊗ u(y)
//
// read as: over the product ℕ×ℕ (an INFINITE, intensional basis), RESTRICT to
// the tridiagonal band |y−x| ≤ 1, and give each surviving pair the rank-1
// (outer-product) entry u(x) ⊗ u(y).  Nothing is materialised — band and value
// are both intensional; you query an entry on demand.
//
// Carrier = the MAX-PLUS dioid (the CPM / critical-path semiring): there the
// log-domain "exp(x)·exp(y)" IS the tropical dyad u(x) ⊗ u(y) = x + y, exact.
// The COMPLEX-exponential sibling (ζ^x·ζ^y over the torus ℤ/8, the DFT flavour)
// is braket_symmetrize_test / plane_wave_spike_test — the SAME construction one
// SEMIRING SWAP away (bool ⟹ reachability, MaxPlus ⟹ CPM, ℂ ⟹ Fourier).
//
// Three designed SEAMS this kata makes visible (Sollbruchstellen, not bugs):
//   (S1) |y−x| ≤ 1 is spelled as the DISJUNCTION of three adjacency graphs
//        (y=x ∨ y=x+1 ∨ x=y+1) via RelOr (+).  A single difference-cut
//        (π2 − π1) ⋈ fix is the 2-D linear-form predicate tracked in #816.
//   (S2) value attachment is an OuterProduct, NOT the relative product >>
//        (which composes relations over a Boolean middle only — FIXME #795).
//   (S3) there is no masked-matrix / Hadamard-of-(relation, matrix) combinator
//        yet; the support↔value fusion is done compositionally (the `entry`
//        lambda gates the dyad by the band, ⊕-zero off-band).
// ─────────────────────────────────────────────────────────────────────────────

import dedekind.category; // IsArrow, identity_v
import dedekind.algebra;  // MaxPlus, semiring_ops
import dedekind.sets;     // ℕ, finite_cardinality, the product (*) / filter (|)
import dedekind.order;    // π1, π2, fix, _c — the relational-predicate DSL
import dedekind.relational;     // RelOr — relation union (+)
import dedekind.linear_algebra; // OuterProduct — the rank-1 dyad carrier

using namespace dedekind::category;
using namespace dedekind::sets;
using namespace dedekind::order;
using dedekind::algebra::MaxPlus;
using dedekind::algebra::semiring_ops;
using dedekind::linear_algebra::OuterProduct;

namespace {
using MP = MaxPlus<unsigned long long>;   // the max-plus dioid (CPM carrier)
using TropMult = semiring_ops<MP>::mult;  // ⊗ = saturating +
using TropPlus = semiring_ops<MP>::add;   // ⊕ = max
constexpr MP kTropZero = identity_v<MP, TropPlus>;  // ⊕-identity (−∞): off-band

// The rank-1 weight rule u : ℕ → max-plus, u(x) = x (the log-domain "exp(x)").
// An IsArrow over the INFINITE basis — intensional, never tabulated.
struct LogWeight {
  using Domain = std::size_t;
  using Codomain = MP;
  constexpr MP operator()(std::size_t x) const {
    return MP::of(static_cast<unsigned long long>(x));
  }
};
}  // namespace

TEST_CASE("kata: a symbolic tridiagonal band over the infinite line ℕ",
          "[linear_algebra][funcspace][transfer][band][kata]") {
  // (S1) The band support |y−x| ≤ 1 as the DISJUNCTION of three adjacency
  //   graphs, point-free over ℕ×ℕ — intensional, infinite basis:
  const auto band = (ℕ * ℕ | π1 == π2)  // y = x       (main diagonal)
                    + (ℕ * ℕ | π1 + fix(1_c) == π2)   // y = x + 1 (super-diag)
                    + (ℕ * ℕ | π2 + fix(1_c) == π1);  // x = y + 1 (sub-diag)

  const auto at = [](std::size_t x, std::size_t y) {
    return std::pair{finite_cardinality(x), finite_cardinality(y)};
  };
  CHECK(band(at(3, 3)));        // on band: |0| ≤ 1
  CHECK(band(at(3, 4)));        // on band: |1| ≤ 1  (super-diagonal)
  CHECK(band(at(4, 3)));        // on band: |1| ≤ 1  (sub-diagonal)
  CHECK_FALSE(band(at(3, 5)));  // OFF band: |2| > 1

  // (S2) The rank-1 VALUE u(x) ⊗ u(y) as an OuterProduct (not >>), intensional
  //   over the infinite basis.  Tropical ⊗ = + : value(x,y) = x + y.
  constexpr OuterProduct<LogWeight, LogWeight, TropMult> value{LogWeight{},
                                                               LogWeight{}};
  CHECK(value(std::size_t{3}, std::size_t{4}) == MP::of(7));  // 3 ⊗ 4 = 3+4

  // (S3) The banded operator = value MASKED by the band (no masked-matrix
  //   combinator yet): on-band → the dyad entry, off-band → the ⊕-zero (−∞).
  const auto entry = [&](std::size_t x, std::size_t y) -> MP {
    return band(at(x, y)) ? value(x, y) : kTropZero;
  };
  CHECK(entry(3, 3) == MP::of(6));  // diagonal:       3 + 3 = 6
  CHECK(entry(3, 4) == MP::of(7));  // super-diagonal: 3 + 4 = 7
  CHECK(entry(4, 3) == MP::of(7));  // sub-diagonal:   4 + 3 = 7
  CHECK(entry(3, 5) == kTropZero);  // OFF band:       ⊕-zero (−∞)

  // The value is genuinely INFINITE-basis: query a far-off-band pair — the band
  // excludes it structurally, no tabulation of ℕ ever happens.
  CHECK(entry(1000, 1000) == MP::of(2000));  // diagonal, deep in ℕ
  CHECK(entry(1000, 1002) == kTropZero);     // off band, deep in ℕ
}
