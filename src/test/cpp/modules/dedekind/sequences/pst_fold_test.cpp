/**
 * @file dedekind/sequences/pst_fold_test.cpp
 * @brief The Pst fragment decided by one fold along the chain (#975, slice 1):
 *        ∃ / ∀ in L, == and ⊆ decided, the runs read off; homogeneous Boole
 *        (1a) and homogeneous K₃ (1b).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <ranges>
#include <vector>

import dedekind.category;
import dedekind.order;
import dedekind.sequences;
import dedekind.sets;

using namespace dedekind::category;
using namespace dedekind::order;
using namespace dedekind::sequences;
using namespace dedekind::sets;

namespace {
/** @brief The identity classifier on K₃, valued in K₃: χ(x) = x.  The simplest
 *  predicate with a genuine @c Unknown level; the comprehension over
 *  @c 𝔸<Ternary, Kleene> makes it the set. */
struct Hedge {
  constexpr Ternary operator()(const Ternary& x) const { return x; }
};

template <typename View>
std::vector<std::ranges::range_value_t<View>> collect(View v) {
  std::vector<std::ranges::range_value_t<View>> out;
  for (const auto x : v) out.push_back(x);
  return out;
}
}  // namespace

TEST_CASE("sequences:pst — chain_view agrees with iota and with the orbit",
          "[sequences][pst][chain]") {
  CHECK(std::ranges::equal(chain_view{3, 7}, std::views::iota(3, 8)));
  const auto prefix_37 = prefix(SuccessorOrbit<int>{3}, 5);
  CHECK(std::ranges::equal(chain_view{3, 7}, as_range(prefix_37)));
  CHECK(collect(chain_view{Ternary::False, Ternary::True}) ==
        std::vector{Ternary::False, Ternary::Unknown, Ternary::True});
}

TEST_CASE("sequences:pst — 1a: Boole-valued sets over 𝔹 and K₃ are decided",
          "[sequences][pst][boole]") {
  constexpr auto top = 𝔸<bool>{} | (π == true);
  STATIC_CHECK(IsPstSet<decltype(top)>);
  CHECK(exists(top));
  CHECK_FALSE(forall(top));
  CHECK(equal(top, top));
  CHECK_FALSE(equal(top, 𝔸<bool>{}));
  CHECK(subset(top, 𝔸<bool>{}));
  CHECK_FALSE(subset(𝔸<bool>{}, top));
  CHECK(collect(runs(top)) == std::vector{Run<bool, bool>{true, true, true}});

  constexpr auto above_bottom = 𝔸<Ternary>{} | (π > Ternary::False);
  CHECK(exists(above_bottom));
  CHECK_FALSE(forall(above_bottom));
  CHECK(forall(above_bottom, Ternary::Unknown, Ternary::True));
  CHECK(collect(runs(above_bottom)) ==
        std::vector{Run<Ternary, bool>{true, Ternary::Unknown, Ternary::True}});
}

TEST_CASE("sequences:pst — a window on the integral chain, ends as values",
          "[sequences][pst][window]") {
  constexpr auto above5 = 𝔸<int>{} | (π > 5);
  constexpr auto at_least6 = 𝔸<int>{} | (π >= 6);
  constexpr auto below3 = 𝔸<int>{} | (π < 3);
  CHECK(equal(above5, at_least6, 0, 20));
  CHECK(exists(above5, 0, 20));
  CHECK_FALSE(exists(above5, 0, 5));
  CHECK(forall(above5, 6, 20));
  // The #365 showcase, by evaluation: no distributivity rule is involved.
  const auto meet = above5 & below3;
  STATIC_CHECK(IsPstSet<decltype(meet)>);
  CHECK_FALSE(exists(meet, 0, 20));
  CHECK(equal(meet, Ø<int>{}, 0, 20));
  CHECK(collect(runs(above5, 0, 9)) == std::vector{Run<int, bool>{true, 6, 9}});
  CHECK(collect(runs(above5 | below3, 0, 9)) ==
        std::vector{Run<int, bool>{true, 0, 2}, Run<int, bool>{true, 6, 9}});
}

TEST_CASE("sequences:pst — 1b: a K₃-valued set has an Unknown level",
          "[sequences][pst][kleene]") {
  constexpr auto hedge = Comprehension{𝔸<Ternary, Kleene>{}, Hedge{}};
  STATIC_CHECK(IsPstSet<decltype(hedge)>);
  CHECK(exists(hedge) == Ternary::True);
  CHECK(forall(hedge) == Ternary::False);
  // On the window [U, ⊤] the meet is U: the honest Kleene verdict.
  CHECK(forall(hedge, Ternary::Unknown, Ternary::True) == Ternary::Unknown);
  CHECK(exists(hedge, Ternary::False, Ternary::Unknown) == Ternary::Unknown);
  // Two runs, two levels: the α-cuts {χ ≥ U} = [U, ⊤] and {χ ≥ ⊤} = [⊤, ⊤].
  CHECK(collect(runs(hedge)) ==
        std::vector{Run<Ternary, Ternary>{Ternary::Unknown, Ternary::Unknown,
                                          Ternary::Unknown},
                    Run<Ternary, Ternary>{Ternary::True, Ternary::True,
                                          Ternary::True}});
  CHECK(equal(hedge, hedge));
  CHECK(subset(hedge, hedge));
}
