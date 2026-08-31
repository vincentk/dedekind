#include <catch2/catch_test_macros.hpp>
#include <cstddef>

import dedekind.linear_algebra;
import dedekind.algebra;
import dedekind.category;

// Runtime companions to the in-module static_asserts of :transfer (which are
// invisible to coverage).  We exercise the semiring bra·ket, the ⊕/⊗ matmul
// kernel, and the rank-1 collapse M² = λ·M across three semirings — the SAME
// dyad, one semiring apart: MaxPlus ⟹ critical path, MinPlus ⟹ shortest path,
// 𝔹 ⟹ reachability.

using namespace dedekind::linear_algebra;
using dedekind::algebra::MaxPlus;
using dedekind::algebra::MinPlus;

namespace {

// A diamond bead: entry→mid weights (3,5) as the ket, mid→exit weights (5,2)
// as the bra.  Parametrised on the tropical carrier so the same shape serves
// max-plus and min-plus.
template <typename MP>
struct entry_to_mid {
  using Domain = std::size_t;
  using Codomain = MP;
  constexpr MP operator()(std::size_t k) const {
    return k == 0 ? MP::of(3) : MP::of(5);
  }
};
template <typename MP>
struct mid_to_exit {
  using Domain = std::size_t;
  using Codomain = MP;
  constexpr MP operator()(std::size_t k) const {
    return k == 0 ? MP::of(5) : MP::of(2);
  }
};

}  // namespace

TEST_CASE("transfer: tropical bra·ket eigenvalue and rank-1 collapse",
          "[linear_algebra][transfer][tropical]") {
  using MP = MaxPlus<unsigned long long>;
  using Mult = typename dedekind::algebra::semiring_ops<MP>::mult;
  const entry_to_mid<MP> v{};
  const mid_to_exit<MP> w{};

  // λ = ⟨w|v⟩ = max(3+5, 5+2) = 8 — the long branch wins.
  const MP lambda = inner_product<2>(w, v);
  CHECK(lambda == MP::of(8));

  const OuterProduct<entry_to_mid<MP>, mid_to_exit<MP>, Mult> bead{};
  CHECK(bead(0, 0) == MP::of(8));   // 3 ⊗ 5
  CHECK(bead(1, 0) == MP::of(10));  // 5 ⊗ 5

  // The rank-1 collapse, at runtime: (M·M)(i,j) = λ ⊗ M(i,j).
  for (std::size_t i = 0; i < 2; ++i)
    for (std::size_t j = 0; j < 2; ++j)
      CHECK(matmul_entry<2>(bead, bead, i, j) == Mult{}(lambda, bead(i, j)));

  CHECK(eigenvalue<2>(bead) == lambda);
}

TEST_CASE("transfer: the semiring is the choice of problem",
          "[linear_algebra][transfer][semiring-swap]") {
  SECTION("MinPlus ⟹ shortest path: min(3+5, 5+2) = 7") {
    using MP = MinPlus<unsigned long long>;
    const entry_to_mid<MP> v{};
    const mid_to_exit<MP> w{};
    CHECK(inner_product<2>(w, v) == MP::of(7));  // the short branch wins
  }

  SECTION("𝔹 ⟹ reachability: is a branch present end to end?") {
    struct all_present {
      using Domain = std::size_t;
      using Codomain = bool;
      constexpr bool operator()(std::size_t) const { return true; }
    };
    struct only_first {
      using Domain = std::size_t;
      using Codomain = bool;
      constexpr bool operator()(std::size_t k) const { return k == 0; }
    };
    struct none_present {
      using Domain = std::size_t;
      using Codomain = bool;
      constexpr bool operator()(std::size_t) const { return false; }
    };
    CHECK(inner_product<2>(only_first{}, all_present{}));  // branch 0 through
    CHECK(!inner_product<2>(none_present{}, all_present{}));  // no branch
  }
}
