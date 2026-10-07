/**
 * @file dedekind/sequences/pst_fold_test.cpp
 * @brief The Pst fragment (#975, slice 1): chain_view, the runs of a decidable
 *        set over 𝔹, K₃ and an int window, and an L-valued set read through its
 *        α-cuts and fibres as preimages; the quantifiers and equality are sets'
 *        own, decided by exhaustion.
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
/** @brief @c U at ⊥, @c ⊤ above: a K₃-valued set that is everything "at least
 *  maybe". */
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

TEST_CASE(
    "sequences:pst: decidable sets over 𝔹 and K₃ are decided by exhaustion",
    "[sequences][pst][boole]") {
  // The quantifiers are sets' own: ∃ = not empty, ∀ = equal to the domain,
  // both decided by exhausting the truth chain (sets:boundaries).
  CHECK(exists(𝔸<bool>{}, π == true));
  CHECK_FALSE(forall(𝔸<bool>{}, π == true));
  CHECK(exists(𝔸<Ternary>{}, π > Ternary::False));
  CHECK_FALSE(forall(𝔸<Ternary>{}, π > Ternary::False));
  CHECK(forall(𝔸<Ternary>{}, π >= Ternary::False));
  // Set equality on a truth chain is table equality, extensional.
  constexpr auto above_bottom = 𝔸<Ternary>{} | (π > Ternary::False);
  constexpr auto at_least_u = 𝔸<Ternary>{} | (π >= Ternary::Unknown);
  STATIC_CHECK(IsPstSet<decltype(above_bottom)>);
  CHECK(above_bottom == at_least_u);
  CHECK_FALSE(above_bottom == 𝔸<Ternary>{});
  CHECK(collect(runs(above_bottom)) ==
        std::vector{Run<Ternary>{Ternary::Unknown, Ternary::True}});
  CHECK(collect(runs(𝔸<bool>{} | (π == true))) ==
        std::vector{Run<bool>{true, true}});
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
  CHECK(collect(runs(above5, 0, 9)) == std::vector{Run<int>{6, 9}});
  CHECK(collect(runs(above5 | below3, 0, 9)) ==
        std::vector{Run<int>{0, 2}, Run<int>{6, 9}});
}

TEST_CASE(
    "sequences:pst: a K₃-valued set answers in K₃ and is read through "
    "its α-cuts",
    "[sequences][pst][kleene]") {
  // χ = id on K₃, valued in K₃: the simplest set with a genuine Unknown level.
  constexpr auto hedge = 𝔸<Ternary, Kleene>{} | Identity<Ternary>{};
  // ∃ = ⋁χ and ∀ = ⋀χ in K₃, through == alone: Unknown is a verdict.
  CHECK(exists(𝔸<Ternary, Kleene>{}, Identity<Ternary>{}) == Ternary::True);
  CHECK(forall(𝔸<Ternary, Kleene>{}, Identity<Ternary>{}) == Ternary::False);
  CHECK(forall(𝔸<Ternary, Kleene>{}, AtLeastMaybe{}) == Ternary::Unknown);
  // Equality is the internal biconditional: reflexive only up to the excluded
  // middle, so a set agrees with itself to degree U where it is U.
  CHECK((hedge == hedge) == Ternary::Unknown);
  // Across species, compared in their join: {x > ⊥} (Boolean) against χ = id
  // (K₃-valued) agrees at ⊥ and ⊤, and at U to degree U.
  constexpr auto above_bottom = 𝔸<Ternary>{} | (π > Ternary::False);
  STATIC_CHECK((above_bottom == hedge) == Ternary::Unknown);
  STATIC_CHECK((hedge == above_bottom) == Ternary::Unknown);
  CHECK((above_bottom == hedge) == Ternary::Unknown);  // the runtime companions
  CHECK((hedge == above_bottom) == Ternary::Unknown);
  // ⊥ against U is itself only U; {x < U} (Boolean) against χ = id differs
  // outright at ⊥, ⊤ against ⊥, and the biconditional's meet is ⊥.
  CHECK(((𝔸<Ternary>{} | (π > Ternary::Unknown)) == hedge) == Ternary::Unknown);
  CHECK(((𝔸<Ternary>{} | (π < Ternary::Unknown)) == hedge) == Ternary::False);
  CHECK((𝔸<Ternary, Kleene>{} == (𝔸<Ternary, Kleene>{} | AtLeastMaybe{})) ==
        Ternary::Unknown);  // 𝔸 first: the Ω-valued member, not a rewrite
  // The α-cuts, nested, as preimages of the upper rays on Ω: {χ ≥ U} = [U, ⊤]
  // and {χ ≥ ⊤} = [⊤, ⊤]; the fibre at U, preimage(χ, η(U)), is [U, U].
  constexpr auto cut_u =
      𝔸<Ternary>{} | preimage(hedge, 𝔸<Ternary>{} | (π >= Ternary::Unknown));
  constexpr auto cut_top =
      𝔸<Ternary>{} | preimage(hedge, 𝔸<Ternary>{} | (π >= Ternary::True));
  constexpr auto fibre_u = 𝔸<Ternary>{} | preimage(hedge, η(Ternary::Unknown));
  STATIC_CHECK(IsPstSet<decltype(cut_u)>);
  CHECK(collect(runs(cut_u)) ==
        std::vector{Run<Ternary>{Ternary::Unknown, Ternary::True}});
  CHECK(collect(runs(cut_top)) ==
        std::vector{Run<Ternary>{Ternary::True, Ternary::True}});
  CHECK(collect(runs(fibre_u)) ==
        std::vector{Run<Ternary>{Ternary::Unknown, Ternary::Unknown}});
}
