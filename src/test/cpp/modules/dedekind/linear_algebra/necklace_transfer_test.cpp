#include <array>
#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <functional>

import dedekind.linear_algebra; // :transfer — inner_product, transfer_chain
import dedekind.algebra;        // MaxPlus, MinPlus
import dedekind.analysis;       // Dual — forward-mode AD carrier
import dedekind.sequences;      // argmax over the branch interval (§3 filter)
import dedekind.order;          // OrderInterval, Strictness
import dedekind.category;       // ClassicalLogic

// The diamond necklace of showcase_13 as rank-1 transfers: the value is the
// ⊗-fold of the per-diamond eigenvalues (transfer_chain), and the critical path
// is the per-diamond argmax over the raw arrival cost — reusing §3's
// `dedekind::sequences::argmax`, not a transfer-local reimplementation.
//
//        (1,1)     (3,1)     (5,1)     (7,1)
//         / \       / \       / \       / \
//   (0,0)     (2,0)     (4,0)     (6,0)     (8,0)
//         \ /       \ /       \ /       \ /
//        (1,-1)    (3,-1)    (5,-1)    (7,-1)

using namespace dedekind::linear_algebra;
using dedekind::algebra::MaxPlus;
using dedekind::algebra::MinPlus;

namespace {

// showcase_13's cost, arriving at (x, y): c = max(y·(K−x), 0), K = 4.
constexpr unsigned long long cost(int x, int y) {
  const long long c = static_cast<long long>(y) * (4 - x);
  return c > 0 ? static_cast<unsigned long long>(c) : 0ULL;
}

// The mid-layer ket of diamond m: 0 ↦ peak (y=+1) arrival, 1 ↦ trough (y=−1) —
// a plain Ket<MP,2> literal (IsArrow), not a bespoke index→scalar struct.
template <typename MP>
constexpr Ket<MP, 2> ket_of(int m) {
  return {{MP::of(cost(2 * m + 1, +1)), MP::of(cost(2 * m + 1, -1))}};
}

// diamond m's rank-1 eigenvalue λ_m = ⟨w|v⟩.  The exit-edge bra is the constant
// cost-0 Bra (both exit edges land on the rail node (x+1, 0)).
template <typename MP>
constexpr MP bead_eigenvalue(int m) {
  return inner_product<2>(Bra<MP, 2>{{MP::of(0), MP::of(0)}}, ket_of<MP>(m));
}

// The closure: ⊗-fold the four eigenvalues via the structured transfer_chain.
template <typename MP>
constexpr MP necklace_value() {
  return transfer_chain<4>(
      [](std::size_t m) { return bead_eigenvalue<MP>(static_cast<int>(m)); });
}

// The critical (max-cost) branch of diamond m, as the §3 forall-filter over the
// two-branch interval [0,1]: `dedekind::sequences::argmax`, ordered by the raw
// arrival cost so it selects the branch on the longest (critical) path.
inline constexpr auto branch_interval =
    dedekind::order::OrderInterval<int, 0, 1,
                                   dedekind::order::Strictness::NonStrict,
                                   dedekind::order::Strictness::NonStrict,
                                   dedekind::category::ClassicalLogic>{};

constexpr auto diamond_argmax(int m) {
  return dedekind::sequences::argmax(branch_interval, [m](int k) {
    return cost(2 * m + 1, k == 0 ? +1 : -1);
  });
}

// A path is critical iff every diamond's branch is in that diamond's argmax;
// the optimal-path set stays intensional, membership tested on a concrete path.
constexpr bool is_optimal_path(const std::array<std::size_t, 4>& path) {
  bool ok = true;
  for (int m = 0; m < 4; ++m)
    ok = ok &&
         diamond_argmax(m)(static_cast<int>(path[static_cast<std::size_t>(m)]));
  return ok;
}

}  // namespace

// Critical path value = 3 ⊗ 1 ⊗ 1 ⊗ 3 = 8 — showcase_13's value, no FiniteNet.
static_assert(necklace_value<MaxPlus<unsigned long long>>().val == 8);

// The optimal-path set contains the up,up,down,down chevron, and excludes a
// path that deviates at diamond 0 (forcing the dearer trough off the critical
// path).
static_assert(is_optimal_path({0, 0, 1, 1}),
              "the up,up,down,down chevron is the critical path.");
static_assert(!is_optimal_path({1, 0, 1, 1}),
              "deviating at diamond 0 leaves the critical path.");

TEST_CASE(
    "necklace: transfer_chain value + argmax path reproduce the CPM optimum",
    "[linear_algebra][transfer][necklace]") {
  using MPu = MaxPlus<unsigned long long>;
  CHECK(necklace_value<MPu>().val == 8);

  CHECK(is_optimal_path({0, 0, 1, 1}));
  CHECK(!is_optimal_path({1, 0, 1, 1}));

  // Diamond 0 is a strict optimum (unique critical branch): {0} only.
  const auto d0 = diamond_argmax(0);
  CHECK(d0(0));
  CHECK(!d0(1));
}

TEST_CASE("necklace: the semiring is the choice of problem (transfer form)",
          "[linear_algebra][transfer][necklace][semiring-swap]") {
  // MinPlus ⟹ shortest necklace: each diamond takes its cheaper branch
  // (min(peak, trough) = 0), so the shortest-path value is 0.
  using MPl = MinPlus<unsigned long long>;
  CHECK(bead_eigenvalue<MPl>(0).val == 0);  // min(3, 0)
  CHECK(necklace_value<MPl>().val == 0);    // 0 ⊗ 0 ⊗ 0 ⊗ 0

  // Its optimal branch is the dual ARGMIN — the SAME argmax with ≥ pulled back
  // (the order as a template parameter), no second function needed.
  const auto d0_min = dedekind::sequences::argmax(
      branch_interval, [](int k) { return cost(1, k == 0 ? +1 : -1); },
      std::greater_equal<>{});
  CHECK(d0_min(1));  // diamond 0's cheap branch is the trough (branch 1)
  CHECK(!d0_min(0));
}

TEST_CASE(
    "necklace generalizes to dual numbers: sensitivity = envelope theorem",
    "[linear_algebra][transfer][necklace][dual]") {
  using DUL = dedekind::analysis::Dual<unsigned long long>;
  using MPd = MaxPlus<DUL>;

  // A dual-valued diamond: probe the PEAK branch with a unit tangent (∂=1). The
  // transfer machinery is UNCHANGED — it asked only for IsSemiring, which
  // MaxPlus<Dual> satisfies — so inner_product / transfer_chain carry the
  // derivative (forward-mode AD, the envelope theorem).
  struct probe_peak_ket {
    using Domain = std::size_t;
    using Codomain = MPd;
    constexpr MPd operator()(std::size_t k) const {
      return k == 0 ? MPd::of(DUL{3, 1}) : MPd::of(DUL{0, 0});
    }
  };
  struct zero_bra {
    using Domain = std::size_t;
    using Codomain = MPd;
    constexpr MPd operator()(std::size_t) const { return MPd::of(DUL{0, 0}); }
  };

  const MPd lam = inner_product<2>(zero_bra{}, probe_peak_ket{});
  CHECK(lam.val.val == 3);  // the critical value
  CHECK(lam.val.der == 1);  // the peak branch lies on the critical path

  // Along a chain the derivative accumulates: two critical beads → value 6,
  // ∂ 2.
  const MPd two = transfer_chain<2>([&](std::size_t) { return lam; });
  CHECK(two.val.val == 6);
  CHECK(two.val.der == 2);
}
