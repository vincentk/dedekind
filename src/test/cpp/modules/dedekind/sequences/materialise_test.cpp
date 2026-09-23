/** @file dedekind/sequences/materialise_test.cpp
 *
 * The last plank of the intensional → intensional-finite → ext →
 * extensional bridge: `ext` realises a finite (IsExtensional) interval
 * domain into its `ExtensionalSet`, via the halfspace→iota_view bridge and the
 * existing `sets::ext`.  An unbounded domain has no `to_iota_view`, so
 * it cannot reach `ext` at all. The Rice wall is structural, not checked.
 */

#include <array>
#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <cstddef>
#include <functional>
#include <type_traits>

import dedekind.sequences; // ext, to_iota_view
import dedekind.order;     // OrderInterval, Strictness
import dedekind.category;  // Boole

using namespace dedekind::sequences;
using dedekind::category::Boole;
using dedekind::order::OrderInterval;
using dedekind::order::Strictness;

namespace {
// [0, 4] — a closed integer interval, the finite prefix {0,1,2,3,4} of ℕ.
using Prefix5 = OrderInterval<int, 0, 4, Strictness::NonStrict,
                              Strictness::NonStrict, Boole>;

// A sequence (index → value) — the bra/ket / Path shape.
struct squares {
  using Domain = std::size_t;
  using Codomain = int;
  constexpr int operator()(std::size_t i) const {
    return static_cast<int>(i * i);
  }
};
}  // namespace

TEST_CASE("ext: a closed interval becomes its ExtensionalSet",
          "[sequences][ranges][ext]") {
  constexpr Prefix5 oi{};
  const auto xs = ext(oi);

  CHECK(xs.size() == 5);
  for (int x = 0; x <= 4; ++x) CHECK(xs.contains(x));
  CHECK(!xs.contains(5));
  CHECK(!xs.contains(-1));
}

TEST_CASE("ext: the filtered form realises argmax over a finite domain",
          "[sequences][ranges][ext]") {
  constexpr Prefix5 oi{};

  // The two-argument form keeps only the members satisfying the predicate —
  // exactly how an argmax/tie-set over a finite domain is realised.
  const auto evens = ext(oi, [](int x) { return x % 2 == 0; });
  CHECK(evens.size() == 3);  // {0, 2, 4}
  CHECK(evens.contains(0));
  CHECK(evens.contains(2));
  CHECK(evens.contains(4));
  CHECK(!evens.contains(1));
  CHECK(!evens.contains(3));

  // A unique optimum materialises to a singleton; an empty predicate to ∅.
  const auto sole = ext(oi, [](int x) { return x == 3; });
  CHECK(sole.size() == 1);
  CHECK(sole.contains(3));

  const auto empty = ext(oi, [](int) { return false; });
  CHECK(empty.size() == 0);
}

TEST_CASE("ext(argmax(interval, cost)): the endorsed one-liner",
          "[sequences][ranges][ext][argmax]") {
  // A unique optimum: the concave cap x·(6−x) over [0,6] peaks at x=3.
  constexpr OrderInterval<int, 0, 6, Strictness::NonStrict,
                          Strictness::NonStrict, Boole>
      dom6{};

  // Type-level regression (#915): argmax's refinement is the NAMED
  // DominanceRefinement, not an opaque lambda.  A regression back to a
  // capturing lambda would leave the behavioural CHECKs below green, so pin the
  // type here.
  struct Cap6 {
    constexpr int operator()(int x) const { return x * (6 - x); }
  };
  static_assert(
      std::same_as<decltype(argmax(dom6, Cap6{})),
                   BoundedSet<std::remove_cvref_t<decltype(dom6)>,
                              DominanceRefinement<
                                  int, std::remove_cvref_t<decltype(dom6)>,
                                  Cap6, std::less_equal<>>>>,
      "argmax returns a BoundedSet carrying the named DominanceRefinement");

  const auto peak = ext(argmax(dom6, [](int x) { return x * (6 - x); }));
  CHECK(peak.size() == 1);  // {3} — argmax is a function
  CHECK(peak.contains(3));

  // A tie: parity x mod 2 over [0,5] is maximal (=1) at every odd argument.
  constexpr OrderInterval<int, 0, 5, Strictness::NonStrict,
                          Strictness::NonStrict, Boole>
      dom5{};
  const auto odds = ext(argmax(dom5, [](int x) { return x % 2; }));
  CHECK(odds.size() == 3);  // {1,3,5} — argmax is a proper relation
  CHECK(odds.contains(1));
  CHECK(odds.contains(3));
  CHECK(odds.contains(5));
  CHECK(!odds.contains(0));
}

TEST_CASE("ext<N>: a sequence realises to a positional std::array",
          "[sequences][ranges][ext]") {
  // The sequence flavour (dual to the set flavour): a bra/ket / Path bounded to
  // its first N terms becomes a concrete finite-dimensional vector.
  constexpr auto vec = ext<4>(squares{});
  static_assert(
      std::same_as<std::remove_cvref_t<decltype(vec)>, std::array<int, 4>>,
      "sequence ext yields a std::array, not a std::set.");
  CHECK(vec[0] == 0);
  CHECK(vec[1] == 1);
  CHECK(vec[2] == 4);
  CHECK(vec[3] == 9);
}
