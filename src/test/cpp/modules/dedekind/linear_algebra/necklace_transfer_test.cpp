#include <array>
#include <catch2/catch_test_macros.hpp>
#include <cstddef>

import dedekind.linear_algebra; // :transfer — inner_product, transfer_chain,
                                // argmax_index
import dedekind.algebra;        // MaxPlus, MinPlus, semiring_ops

// The diamond necklace of showcase_13, re-expressed as `materialize(argmax(
// cost_function))` over rank-1 transfers — no edge materialisation, no
// FiniteNet potential, and the optimum is the ONLY realize point.
//
//        (1,1)     (3,1)     (5,1)     (7,1)
//         / \       / \       / \       / \
//   (0,0)     (2,0)     (4,0)     (6,0)     (8,0)
//         \ /       \ /       \ /       \ /
//        (1,-1)    (3,-1)    (5,-1)    (7,-1)
//
// Each diamond m (x: 2m → 2m+2) is ONE rank-1 transfer: its two parallel
// branches are a 2-component ket (the mid-node arrival costs) contracted
// against a bra (the exit-edge costs, all 0), so its eigenvalue is the tropical
// bra·ket λ_m = ⟨w|v⟩ = max(peak, trough).  The closure `transfer_chain` is the
// ⊗-fold of the four eigenvalues; because every transfer is rank-1 the streamed
// memo is a SINGLE scalar (rank = 1 ⟹ dim(memo) = 1, Fliess).  `argmax_index`
// recovers the optimal branch per diamond — the filtration where we "mean it".

using namespace dedekind::linear_algebra;
using dedekind::algebra::MaxPlus;
using dedekind::algebra::MinPlus;

namespace {

// showcase_13's cost, arriving at (x, y): c = max(y·(K−x), 0), K = 4.
constexpr unsigned long long cost(int x, int y) {
  const long long c = static_cast<long long>(y) * (4 - x);
  return c > 0 ? static_cast<unsigned long long>(c) : 0ULL;
}

// The mid-layer ket of diamond m: 0 ↦ peak (y=+1) arrival, 1 ↦ trough (y=−1).
template <typename MP>
struct branch_ket {
  MP peak, trough;
  using Domain = std::size_t;
  using Codomain = MP;
  constexpr MP operator()(std::size_t k) const {
    return k == 0 ? peak : trough;
  }
};
// The exit-edge bra: both exit edges land on the rail node (x+1, 0), cost 0.
template <typename MP>
struct exit_bra {
  using Domain = std::size_t;
  using Codomain = MP;
  constexpr MP operator()(std::size_t) const { return MP::of(0); }
};

template <typename MP>
constexpr branch_ket<MP> ket_of(int m) {
  return {MP::of(cost(2 * m + 1, +1)), MP::of(cost(2 * m + 1, -1))};
}

// cost_function: diamond m's rank-1 eigenvalue λ_m = ⟨w|v⟩ (intensional).
template <typename MP>
constexpr MP bead_eigenvalue(int m) {
  return inner_product<2>(exit_bra<MP>{}, ket_of<MP>(m));
}

// The closure: ⊗-fold the four eigenvalues via the structured `transfer_chain`.
template <typename MP>
constexpr MP necklace_value() {
  return transfer_chain<4>(
      [](std::size_t m) { return bead_eigenvalue<MP>(static_cast<int>(m)); });
}

// argmax(cost_function): the optimal branch per diamond — realize (materialize)
// the path as the concrete array of choices.  0 = up (peak), 1 = down (trough).
template <typename MP>
constexpr std::array<std::size_t, 4> necklace_argmax() {
  std::array<std::size_t, 4> path{};
  for (int m = 0; m < 4; ++m)
    path[static_cast<std::size_t>(m)] =
        argmax_index<2>(exit_bra<MP>{}, ket_of<MP>(m));
  return path;
}

}  // namespace

// Per-diamond eigenvalues 3, 1, 1, 3 (the tropical bra·ket of each diamond).
static_assert(bead_eigenvalue<MaxPlus<unsigned long long>>(0).val == 3);
static_assert(bead_eigenvalue<MaxPlus<unsigned long long>>(3).val == 3);

// Critical path value = 3 ⊗ 1 ⊗ 1 ⊗ 3 = 8 — the same value showcase_13's
// semiring_closure computes, with no FiniteNet and no edge sequence.
static_assert(necklace_value<MaxPlus<unsigned long long>>().val == 8);

// argmax = [up, up, down, down] — exactly showcase_13's critical chevron,
// recovered as the per-diamond ⊕-winners.
static_assert(necklace_argmax<MaxPlus<unsigned long long>>() ==
              std::array<std::size_t, 4>{0, 0, 1, 1});

TEST_CASE("necklace: materialize(argmax(cost)) reproduces the CPM optimum",
          "[linear_algebra][transfer][necklace]") {
  using MPu = MaxPlus<unsigned long long>;

  // The value: transfer_chain ⊗-folds the four rank-1 eigenvalues to 8,
  // streamed with a single-scalar memo.
  CHECK(necklace_value<MPu>().val == 8);

  // The witness: argmax realizes the optimal path — the up,up,down,down
  // chevron of showcase_13.
  const auto path = necklace_argmax<MPu>();
  CHECK(path == std::array<std::size_t, 4>{0, 0, 1, 1});
}

TEST_CASE("necklace: the semiring is the choice of problem (transfer form)",
          "[linear_algebra][transfer][necklace][semiring-swap]") {
  // MinPlus ⟹ shortest necklace: each diamond takes its cheaper branch
  // (min(peak, trough) = 0 for every diamond), so the shortest path is 0.
  using MPl = MinPlus<unsigned long long>;
  CHECK(bead_eigenvalue<MPl>(0).val == 0);  // min(3, 0)
  CHECK(necklace_value<MPl>().val == 0);    // 0 ⊗ 0 ⊗ 0 ⊗ 0

  // Its argmax picks the cheap branch of each diamond: [down, down, up, up]
  // (the trough where the peak is dearer, then the peak where the trough is).
  CHECK(necklace_argmax<MPl>() == std::array<std::size_t, 4>{1, 1, 0, 0});
}
