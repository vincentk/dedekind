/** @file dedekind/numbers/collatz_test.cpp
 *
 * The Collatz case study (docs/python/notebooks/03_collatz_by_budget.ipynb)
 * mirrored in C++: the library supplies the combinators, the Collatz-specific
 * terms are spelled here, as in the notebook, and nowhere in a library module.
 * The step as one relation (a function by structure) and its arrow, the orbit
 * as the iterate, the bounded search read in K₃ by Σ, the set C by recursion
 * on the budget through χ, and the window ∀.  Compile-time witnesses where the
 * constant-evaluation budget allows, runtime companions beyond it.
 */

#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <optional>
#include <utility>

import dedekind.category;
import dedekind.numbers;
import dedekind.order;
import dedekind.relational;
import dedekind.sequences;
import dedekind.sets;

namespace {
using namespace dedekind::category;
using namespace dedekind::order;  // the product binders
using namespace dedekind::relational;
using namespace dedekind::sequences;
using namespace dedekind::sets;
using N = std::size_t;
constexpr Cardinality n_(N n) { return finite_cardinality(n); }

// ── The step, as one relation on ℕ × ℕ: n/2 where even, 3n+1 where odd ──────
inline constexpr auto collatz =
    ℕ * ℕ | (π2 == cond(π1 % 2 == 0, π1 / 2, π1 * 3 + 1));
static_assert(IsFunctional<std::remove_cvref_t<decltype(collatz)>> &&
                  IsEntire<std::remove_cvref_t<decltype(collatz)>>,
              "T is a function by structure: π2 equals a term in π1 alone, "
              "every operator of it total on ℕ");
// ── and as the arrow that relation names ────────────────────────────────────
inline constexpr auto step = arrow_of(collatz);
static_assert(step(n_(6)) == n_(3) && step(n_(7)) == n_(22), "6 ↦ 3, 7 ↦ 22");

// ── The orbit, the search, and Σ ────────────────────────────────────────────
/** @brief Σ ∘ first_where(π == 1, B) ∘ iterate(step): the verdict in K₃.
 *  Runtime only: a Path holds a std::function, which constant evaluation does
 *  not admit. */
Ternary reaches_within(N n, N budget) {
  return Semidecided{}(
      first_where(iterate(n_(n), step), η(n_(1)), budget).has_value());
}

// ── The set C by recursion on the budget: the notebook's two rules ───────────
//   C(N, K) :- N = 1.
//   C(N, K) :- N > 1, K > 1, f(N, M), C(M, K − 1).
inline constexpr auto collatz_within =
    ((ℕ | (π > 0)) * (𝔸<N>{} | (π > 0))) |
    ((π1 == 1) || ((π1 > 1) && (π2 > 1) && χ(ap(step, π1), π2 - 1)));
constexpr bool within(N n, N k) { return collatz_within(std::pair{n_(n), k}); }
static_assert(IsSetObject<decltype(collatz_within)>,
              "C is a set object on ℕ⁺ × ℕ⁺");
static_assert(
    within(1, 1) && !within(2, 1) && within(2, 2) && within(6, 9) &&
        !within(6, 8),
    "C(n, k) is 'within k − 1 steps': 1 at any budget, 2 at one step, "
    "6 at eight");
}  // namespace

TEST_CASE("Collatz — the step is a relation and the arrow it names",
          "[numbers][collatz][exhibit]") {
  CHECK(step(n_(27)) == n_(82));
  for (N n = 1; n < 100; ++n) CHECK(collatz(std::pair{n_(n), step(n_(n))}));
  CHECK_FALSE(collatz(std::pair{n_(6), n_(4)}));
  // The converse fibre at 1 and the bounded relative product (a finite middle).
  const auto converges_in_1 = fibre(converse(collatz), n_(1));
  CHECK(converges_in_1(n_(2)));
  CHECK_FALSE(converges_in_1(n_(1)));
  const auto collatzM = collatz | (π1 < 64);
  const auto collatz2 = collatzM >> collatzM;
  CHECK(collatz2(std::pair{n_(4), n_(1)}));
  CHECK(collatz2(std::pair{n_(6), n_(10)}));
  CHECK_FALSE(collatz2(std::pair{n_(6), n_(5)}));
}

TEST_CASE("Collatz — the orbit, the search, Σ", "[numbers][collatz][exhibit]") {
  const auto orbit = iterate(n_(27), step);
  CHECK(orbit.at(1) == n_(82));
  CHECK(first_where(orbit, η(n_(1)), 120) == std::optional<std::size_t>{111});
  CHECK_FALSE(first_where(orbit, η(n_(1)), 50).has_value());
  CHECK(reaches_within(6, 8) ==
        Ternary::True);  // 6 → 3 → 10 → 5 → 16 → 8 → 4 → 2 → 1
  CHECK(reaches_within(6, 7) == Ternary::Unknown);
  CHECK(reaches_within(27, 120) == Ternary::True);
  CHECK(reaches_within(27, 50) == Ternary::Unknown);
}

TEST_CASE("Collatz — C by recursion agrees with the iterated verdict",
          "[numbers][collatz][exhibit][recursion]") {
  CHECK(within(27, 112));
  CHECK_FALSE(within(27, 111));
  for (N n = 1; n < 60; ++n)
    for (N k = 1; k < 130; ++k)
      CHECK(within(n, k) == (reaches_within(n, k - 1) == Ternary::True));
}

TEST_CASE("Collatz — the window decides with a sufficient budget",
          "[numbers][collatz][exhibit]") {
  // ∀ n ∈ [1, 1000): within 178 steps (871 is the slowest seed), not within
  // 177.
  bool all_178 = true, all_177 = true;
  for (N n = 1; n < 1000; ++n) {
    all_178 = all_178 && reaches_within(n, 178) == Ternary::True;
    all_177 = all_177 && reaches_within(n, 177) == Ternary::True;
  }
  CHECK(all_178);
  CHECK_FALSE(all_177);
}
