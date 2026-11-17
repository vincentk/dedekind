/** @file test/cpp/modules/dedekind/category/pierce_constructions_test.cpp
 *
 * A probe, not a listing: the basic constructions of Pierce, "Basic Category
 * Theory for Computer Scientists" (arrows and composition; monic, epic and
 * iso arrows; terminal object and points; products; sums; the natural numbers
 * as an F-algebra), each written in the shortest form the library offers,
 * over the carriers bool and size_t and their pairings, and the equations
 * between them asked of the library's == on arrow terms.
 *
 * Each equation has one of three outcomes: PROVED (the laws decide it),
 * UNKNOWN (an == exists and the laws do not reach it) or NO-EQ (the two terms
 * have no == at all).  The TEST_CASE prints one line per equation, prefixed
 * "PROBE |", and checks the equation pointwise, so that a line the laws do
 * not decide is still known to be true of the functions.  The printed table
 * is the inventory the Jlt paper's listings are to be built from: the
 * expectations asserted below are only those already known to hold.
 */
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <cstdio>
#include <functional>
#include <initializer_list>
#include <optional>
#include <ranges>
#include <utility>
#include <variant>

import dedekind.category;

namespace pierce_probe {
using std::size_t;
using namespace dedekind::category;

enum class Verdict { NoEq, Unknown, Proved };

constexpr const char* name(Verdict v) {
  return v == Verdict::Proved    ? "PROVED "
         : v == Verdict::Unknown ? "UNKNOWN"
                                 : "NO-EQ  ";
}

/** @brief What the library's == says of two arrow terms. */
template <typename F, typename G>
constexpr Verdict verdict(const F& f, const G& g) {
  if constexpr (requires { static_cast<bool>(f == g); })
    return static_cast<bool>(f == g) ? Verdict::Proved : Verdict::Unknown;
  else
    return Verdict::NoEq;
}

/** @brief Whether two arrows agree at a point: the fact, by the functions. */
template <typename F, typename G, typename X>
constexpr bool agree(const F& f, const G& g, const X& x) {
  return f(x) == g(x);
}

// ---------------------------------------------------------------------------
// The carriers and their pairings.
// ---------------------------------------------------------------------------
using B = bool;
using N = size_t;
using BN = std::pair<B, N>;
using NN = std::pair<N, N>;
using BB = std::pair<B, B>;
using BorN = std::variant<B, N>;

// ---------------------------------------------------------------------------
// 1. Arrows and composition.
// ---------------------------------------------------------------------------
constexpr auto id_b = id<B>();
constexpr auto id_n = id<N>();
constexpr auto neg = -id_n;                      // x ↦ -x, an involution
constexpr auto cpl = ~id_n;                      // x ↦ ~x, an involution
constexpr auto zero_b = constant<B>(size_t{0});  // B → N, the constant 0
constexpr auto ans = constant<B>(size_t{42});    // B → N, the constant 42
constexpr auto no_n = constant<N>(false);        // N → B, the constant false
constexpr Successor<N> succ{};
constexpr Predecessor<N> pred{};

constexpr Verdict unit_left_b = verdict(id_b >> id_b, id_b);
constexpr Verdict unit_left_n = verdict(id_n >> cpl, cpl);
constexpr Verdict unit_right_n = verdict(cpl >> id_n, cpl);
constexpr Verdict assoc = verdict((neg >> cpl) >> neg, neg >> (cpl >> neg));
constexpr Verdict absorb = verdict(zero_b >> no_n >> ans, ans);
constexpr Verdict cpl_involution = verdict(cpl >> cpl, id_n);
constexpr Verdict neg_involution = verdict(neg >> neg, id_n);
constexpr Verdict succ_pred = verdict(succ >> pred, id_n);
constexpr Verdict pred_succ = verdict(pred >> succ, id_n);
// The two's-complement steps: ~(-x) = x - 1 and -(~x) = x + 1.
constexpr Verdict dec_is_pred = verdict(neg >> cpl, pred);
constexpr Verdict inc_is_succ = verdict(cpl >> neg, succ);

// + curried and given 1 is the successor.
constexpr auto plus_curried = curry<N>(std::plus<N>{});
constexpr auto add_one = plus_curried(size_t{1});
constexpr Verdict curried_plus_is_succ = verdict(add_one, succ);

// ---------------------------------------------------------------------------
// 2. Terminal object and points.
// ---------------------------------------------------------------------------
constexpr Delete<N> bang_n{};                          // N → 1
constexpr auto zero_point = constant<One>(size_t{0});  // 1 → N, the point 0
constexpr Verdict bang_absorbs = verdict(cpl >> bang_n, bang_n);
constexpr Verdict point_then_succ =
    verdict(zero_point >> succ, constant<One>(size_t{1}));

// Pierce's FPL (his Example 1.1.15), at the carriers bool and
// size_t: the points true, false and zero, the arrows not and iszero, and his
// two equations, false = not . true and iszero . zero = true, read left to
// right.
constexpr auto true_pt = constant<One>(true);
constexpr auto false_pt = constant<One>(false);
constexpr auto not_b = !id_b;
constexpr auto iszero = equals(size_t{0});
constexpr Verdict fpl_not_true = verdict(true_pt >> not_b, false_pt);
constexpr Verdict fpl_iszero_zero = verdict(zero_point >> iszero, true_pt);
constexpr Verdict fpl_iszero_one =
    verdict(zero_point >> succ >> iszero, false_pt);
constexpr Verdict not_involution = verdict(not_b >> not_b, id_b);

// ---------------------------------------------------------------------------
// 3. Products: projections, pairing, the product of arrows, the symmetry.
// ---------------------------------------------------------------------------
constexpr auto fst = Π<1, BN>{};
constexpr auto snd = Π<2, BN>{};
constexpr auto fan = Pairing{id_b, zero_b};  // B → B × N, x ↦ (x, 0)
constexpr auto fst_nn = Π<1, NN>{};
constexpr auto snd_nn = Π<2, NN>{};

constexpr Verdict beta_1 = verdict(fan >> fst, id_b);
constexpr Verdict beta_2 = verdict(fan >> snd, zero_b);
constexpr Verdict eta_product = verdict(Pairing{fst, snd}, id<BN>());
// f × g, written f * g: its first leg is f after the first projection.
constexpr auto cpl_times_neg = cpl * neg;  // N × N → N × N
constexpr Verdict times_1 = verdict(cpl_times_neg >> fst_nn, fst_nn >> cpl);
constexpr Verdict times_2 = verdict(cpl_times_neg >> snd_nn, snd_nn >> neg);
// Pierce's Definition 1.5.4: f × g is the pairing ⟨f ∘ π₁, g ∘ π₂⟩.  In the
// library the two are one type, so the equation is asked and the type too.
constexpr auto times_spelled = Pairing{fst_nn >> cpl, snd_nn >> neg};
constexpr Verdict times_def = verdict(cpl_times_neg, times_spelled);
constexpr bool times_same_type =
    std::same_as<decltype(cpl_times_neg), decltype(times_spelled)>;
constexpr auto flip_nn = flip<N, N>();
constexpr auto flip_bn = flip<B, N>();
constexpr auto flip_nb = flip<N, B>();
constexpr Verdict flip_involution = verdict(flip_nn >> flip_nn, id<NN>());
constexpr Verdict flip_round_trip = verdict(flip_bn >> flip_nb, id<BN>());

// ---------------------------------------------------------------------------
// 4. Sums: injections, copairing, the sum of arrows.
// ---------------------------------------------------------------------------
constexpr auto inl = ι<1, B, N>{};
constexpr auto inr = ι<2, B, N>{};
constexpr auto either = Copairing{zero_b, id_n};  // B + N → N, [0, id]

constexpr Verdict beta_sum_1 = verdict(inl >> either, zero_b);
constexpr Verdict beta_sum_2 = verdict(inr >> either, id_n);
constexpr Verdict eta_sum = verdict(Copairing{inl, inr}, id<BorN>());
// f + g: after the first injection it is f, then the first injection.
constexpr auto id_plus_cpl = id_b + cpl;  // B + N → B + N
constexpr Verdict plus_1 = verdict(inl >> id_plus_cpl, id_b >> inl);
constexpr Verdict plus_2 = verdict(inr >> id_plus_cpl, cpl >> inr);

// ---------------------------------------------------------------------------
// 5. The natural numbers as an F-algebra: in and its inverse.
// ---------------------------------------------------------------------------
constexpr In<N> in_n{};        // 1 + N → N, [zero, succ]
constexpr Prior<N> prior_n{};  // N → 1 + N
constexpr Verdict prior_in = verdict(prior_n >> in_n, id_n);
constexpr Verdict in_prior = verdict(in_n >> prior_n, id<std::optional<N>>());

// ---------------------------------------------------------------------------
// What is already known to hold, asserted: the rest is printed.
// ---------------------------------------------------------------------------
// An arrow after a point is the point of its value (Pierce, Example 1.4.5):
// his two FPL equations, and the rule applied twice.
static_assert(fpl_not_true == Verdict::Proved &&
              fpl_iszero_zero == Verdict::Proved &&
              fpl_iszero_one == Verdict::Proved &&
              point_then_succ == Verdict::Proved);
static_assert(unit_left_b == Verdict::Proved && absorb == Verdict::Proved &&
              cpl_involution == Verdict::Proved && beta_1 == Verdict::Proved &&
              beta_2 == Verdict::Proved);

// Laws asked on the value: those the listings already assert.
static_assert(is_involution(cpl) && is_involution(neg) && is_iso(neg >> cpl) &&
              !is_involution(neg >> cpl) && is_epic(fst) && is_monic(fan));
// And those this probe asks for the first time, printed below.
constexpr bool iso_id = is_iso(id_n);
constexpr bool iso_succ = is_iso(succ);
constexpr bool monic_succ = is_monic(succ);
constexpr bool monic_inl = is_monic(inl);
constexpr bool monic_inr = is_monic(inr);
constexpr bool epic_snd = is_epic(snd);
constexpr bool epic_either = is_epic(either);
constexpr bool iso_flip = is_iso(flip_nn);
constexpr bool involution_flip = is_involution(flip_nn);
constexpr bool iso_times = is_iso(cpl_times_neg);
constexpr bool monic_times = is_monic(cpl_times_neg);
constexpr bool epic_times = is_epic(cpl_times_neg);
constexpr bool iso_plus = is_iso(id_plus_cpl);
constexpr bool iso_in = is_iso(in_n);

// The fold over an interval of the naturals, by the standard's fold.
static_assert(std::ranges::fold_left(std::views::iota(size_t{3}, size_t{7}),
                                     size_t{0}, std::plus<N>{}) == 18);
}  // namespace pierce_probe

TEST_CASE("Pierce's basic constructions, composed: what the laws decide",
          "[category][pierce][probe]") {
  using namespace pierce_probe;
  const auto row = [](const char* equation, Verdict v) {
    std::printf("PROBE | %s | %s\n", name(v), equation);
  };
  std::printf("PROBE | verdict | equation\n");
  row("id_b >> id_b == id_b", unit_left_b);
  row("id_n >> cpl == cpl", unit_left_n);
  row("cpl >> id_n == cpl", unit_right_n);
  row("(neg >> cpl) >> neg == neg >> (cpl >> neg)", assoc);
  row("zero_b >> no_n >> ans == ans", absorb);
  row("cpl >> cpl == id_n", cpl_involution);
  row("neg >> neg == id_n", neg_involution);
  row("succ >> pred == id_n", succ_pred);
  row("pred >> succ == id_n", pred_succ);
  row("neg >> cpl == pred", dec_is_pred);
  row("cpl >> neg == succ", inc_is_succ);
  row("curry(+)(1) == succ", curried_plus_is_succ);
  row("cpl >> bang_n == bang_n", bang_absorbs);
  row("zero_point >> succ == the point 1", point_then_succ);
  row("true >> not == false", fpl_not_true);
  row("zero >> iszero == true", fpl_iszero_zero);
  row("zero >> succ >> iszero == false", fpl_iszero_one);
  row("not >> not == id_b", not_involution);
  row("fan >> fst == id_b", beta_1);
  row("fan >> snd == zero_b", beta_2);
  row("Pairing{fst, snd} == id<B x N>", eta_product);
  row("(cpl * neg) >> fst == fst >> cpl", times_1);
  row("(cpl * neg) >> snd == snd >> neg", times_2);
  row("cpl * neg == Pairing{fst >> cpl, snd >> neg}", times_def);
  row("flip >> flip == id<N x N>", flip_involution);
  row("flip<B,N> >> flip<N,B> == id<B x N>", flip_round_trip);
  row("inl >> [zero_b, id_n] == zero_b", beta_sum_1);
  row("inr >> [zero_b, id_n] == id_n", beta_sum_2);
  row("[inl, inr] == id<B + N>", eta_sum);
  row("inl >> (id_b + cpl) == id_b >> inl", plus_1);
  row("inr >> (id_b + cpl) == cpl >> inr", plus_2);
  row("prior >> in == id_n", prior_in);
  row("in >> prior == id<1 + N>", in_prior);

  const auto fact = [](const char* law, bool holds) {
    std::printf("PROBE | %s | %s\n", holds ? "YES    " : "NO     ", law);
  };
  fact("is_iso(id_n)", iso_id);
  fact("is_iso(succ)", iso_succ);
  fact("is_monic(succ)", monic_succ);
  fact("is_monic(inl)", monic_inl);
  fact("is_monic(inr)", monic_inr);
  fact("is_epic(snd)", epic_snd);
  fact("is_epic([zero_b, id_n])", epic_either);
  fact("is_iso(flip<N,N>)", iso_flip);
  fact("is_involution(flip<N,N>)", involution_flip);
  fact("is_iso(cpl * neg)", iso_times);
  fact("is_monic(cpl * neg)", monic_times);
  fact("is_epic(cpl * neg)", epic_times);
  fact("cpl * neg and the spelled pairing are one type", times_same_type);
  fact("is_iso(id_b + cpl)", iso_plus);
  fact("is_iso(in)", iso_in);

  // The same equations as facts about the functions, at sample points.
  for (const size_t x : {size_t{0}, size_t{1}, size_t{5}, size_t{41}}) {
    CHECK(agree(succ >> pred, id_n, x));
    CHECK(agree(neg >> cpl, pred, size_t{x + 1}));
    CHECK(agree(cpl >> neg, succ, x));
    CHECK(agree(add_one, succ, x));
    CHECK(agree(prior_n >> in_n, id_n, x));
    CHECK(agree(inr >> either, id_n, x));
    CHECK(agree(inr >> id_plus_cpl, cpl >> inr, x));
    const NN p{x, size_t{x + 2}};
    CHECK(agree(cpl_times_neg >> fst_nn, fst_nn >> cpl, p));
    CHECK(agree(cpl_times_neg >> snd_nn, snd_nn >> neg, p));
    CHECK(agree(flip_nn >> flip_nn, id<NN>(), p));
    CHECK(agree(in_n >> prior_n, id<std::optional<N>>(), std::optional<N>{x}));
  }
  for (const bool b : {false, true}) {
    CHECK(agree(fan >> fst, id_b, b));
    CHECK(agree(fan >> snd, zero_b, b));
    CHECK(agree(inl >> either, zero_b, b));
    CHECK(agree(inl >> id_plus_cpl, id_b >> inl, b));
    const BN p{b, size_t{7}};
    CHECK(agree(Pairing{fst, snd}, id<BN>(), p));
    CHECK(agree(flip_bn >> flip_nb, id<BN>(), p));
  }
  CHECK(agree(in_n >> prior_n, id<std::optional<N>>(), std::optional<N>{}));
  CHECK(succ(zero_point(One{})) == 1);
  CHECK(agree(true_pt >> not_b, false_pt, One{}));
  CHECK(agree(zero_point >> iszero, true_pt, One{}));
  CHECK(agree(zero_point >> succ >> iszero, false_pt, One{}));
}
