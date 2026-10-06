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
/** @brief The identity classifier on K₃, valued in K₃: χ(x) = x, the simplest
 *  datum with a genuine @c Unknown level. */
struct Hedge {
  using Domain = Ternary;
  using Codomain = Ternary;
  constexpr Ternary operator()(const Ternary& x) const { return x; }
};
/** @brief @c U at ⊥, @c ⊤ above: a set that is everything "at least maybe". */
struct AtLeastMaybe {
  using Domain = Ternary;
  using Codomain = Ternary;
  constexpr Ternary operator()(const Ternary& x) const {
    return x == Ternary::False ? Ternary::Unknown : Ternary::True;
  }
};

template <typename View>
std::vector<std::ranges::range_value_t<View>> collect(View v) {
  std::vector<std::ranges::range_value_t<View>> out;
  for (const auto x : v) out.push_back(x);
  return out;
}
}  // namespace

TEST_CASE("sequences:pst: chain_view agrees with iota and with the orbit",
          "[sequences][pst][chain]") {
  CHECK(std::ranges::equal(chain_view{3, 7}, std::views::iota(3, 8)));
  const auto prefix_37 = prefix(SuccessorOrbit<int>{3}, 5);
  CHECK(std::ranges::equal(chain_view{3, 7}, as_range(prefix_37)));
  CHECK(collect(chain_view{Ternary::False, Ternary::True}) ==
        std::vector{Ternary::False, Ternary::Unknown, Ternary::True});
}

TEST_CASE("sequences:pst: 1a: Boole-valued sets over 𝔹 and K₃ are decided",
          "[sequences][pst][boole]") {
  // The quantifiers are sets' own: ∃ = not empty, ∀ = equal to the domain,
  // both decided by exhausting the truth chain (sets:boundaries).
  CHECK(exists(𝔸<bool>{}, π == true));
  CHECK_FALSE(forall(𝔸<bool>{}, π == true));
  CHECK(exists(𝔸<Ternary>{}, π > Ternary::False));
  CHECK_FALSE(forall(𝔸<Ternary>{}, π > Ternary::False));
  CHECK(forall(𝔸<Ternary>{}, π >= Ternary::False));
  // Set equality on a truth chain is table equality.
  constexpr auto above_bottom = 𝔸<Ternary>{} | (π > Ternary::False);
  constexpr auto at_least_u = 𝔸<Ternary>{} | (π >= Ternary::Unknown);
  STATIC_CHECK(IsPstSet<decltype(above_bottom)>);
  CHECK(above_bottom == at_least_u);
  CHECK_FALSE(above_bottom == 𝔸<Ternary>{});
  CHECK(collect(runs(above_bottom)) ==
        std::vector{Run<Ternary, bool>{true, Ternary::Unknown, Ternary::True}});
  CHECK(collect(runs(𝔸<bool>{} | (π == true))) ==
        std::vector{Run<bool, bool>{true, true, true}});
}

TEST_CASE("sequences:pst: a window on the integral chain, ends as values",
          "[sequences][pst][window]") {
  constexpr auto above5 = 𝔸<int>{} | (π > 5);
  constexpr auto at_least6 = 𝔸<int>{} | (π >= 6);
  constexpr auto below3 = 𝔸<int>{} | (π < 3);
  CHECK(std::ranges::equal(runs(above5, 0, 20), runs(at_least6, 0, 20)));
  // The #365 showcase, by evaluation: no distributivity rule is involved.
  const auto meet = above5 & below3;
  STATIC_CHECK(IsPstSet<decltype(meet)>);
  CHECK(collect(runs(meet, 0, 20)).empty());
  CHECK(collect(runs(above5, 0, 9)) == std::vector{Run<int, bool>{true, 6, 9}});
  CHECK(collect(runs(above5 | below3, 0, 9)) ==
        std::vector{Run<int, bool>{true, 0, 2}, Run<int, bool>{true, 6, 9}});
}

TEST_CASE("sequences:pst: 1b: a K₃-valued set answers in K₃",
          "[sequences][pst][kleene]") {
  constexpr auto hedge = 𝔸<Ternary, Kleene>{} | Hedge{};
  STATIC_CHECK(IsPstSet<decltype(hedge)>);
  // ∃ = ⋁χ and ∀ = ⋀χ in K₃, through == alone: Unknown is a verdict.
  CHECK(exists(𝔸<Ternary, Kleene>{}, Hedge{}) == Ternary::True);
  CHECK(forall(𝔸<Ternary, Kleene>{}, Hedge{}) == Ternary::False);
  CHECK(forall(𝔸<Ternary, Kleene>{}, AtLeastMaybe{}) == Ternary::Unknown);
  // Equality is the internal biconditional: reflexive only up to the excluded
  // middle, so a set agrees with itself to degree U where it is U.
  CHECK((hedge == hedge) == Ternary::Unknown);
  CHECK((𝔸<Ternary, Kleene>{} == (𝔸<Ternary, Kleene>{} | AtLeastMaybe{})) ==
        Ternary::Unknown);  // 𝔸 first: the Ω-valued member, not a rewrite
  // The fibres of χ: two runs of constant level.
  CHECK(collect(runs(hedge)) ==
        std::vector{Run<Ternary, Ternary>{Ternary::Unknown, Ternary::Unknown,
                                          Ternary::Unknown},
                    Run<Ternary, Ternary>{Ternary::True, Ternary::True,
                                          Ternary::True}});
  // The α-cuts, nested: {χ ≥ U} = [U, ⊤] and {χ ≥ ⊤} = [⊤, ⊤].
  CHECK(collect(alpha_cut(hedge, Ternary::Unknown)) ==
        std::vector{Run<Ternary, bool>{true, Ternary::Unknown, Ternary::True}});
  CHECK(collect(alpha_cut(hedge, Ternary::True)) ==
        std::vector{Run<Ternary, bool>{true, Ternary::True, Ternary::True}});
}
