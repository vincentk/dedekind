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

TEST_CASE(
    "transfer: the matrix star R* — reachability (𝔹) and longest path "
    "(MaxPlus)",
    "[linear_algebra][transfer][star][closure]") {
  // The edge relation i → i+1 on the 4-node line graph, materialised to its
  // adjacency matrix then STARRED: R* = Δ ⊕ R ⊕ R² ⊕ … , the Kleene closure of
  // the matrix semiring Mat(S).  This exercises `star` / `materialise` at
  // runtime (the in-module witnesses are compile-time only).
  auto edge = [](std::size_t i, std::size_t j) { return j == i + 1; };

  SECTION("𝔹: the star is reachability, R*(i,j) = (i ≤ j)") {
    const auto Rstar = star<4>(materialise<4>(edge));
    CHECK(Rstar[0][0]);        // reflexive: 0 reaches 0 (empty path)
    CHECK(Rstar[0][3]);        // transitive: 0 reaches 3 along the path
    CHECK(Rstar[1][3]);        // 1 reaches 3
    CHECK_FALSE(Rstar[3][0]);  // acyclic: 3 does not reach 0
    CHECK_FALSE(Rstar[2][1]);  // no back-edge
  }

  SECTION(
      "MaxPlus: the SAME star is the longest path (algebraic path problem)") {
    using MP = MaxPlus<unsigned long long>;
    using Add = typename dedekind::algebra::semiring_ops<MP>::add;
    const MP bot = dedekind::category::identity_v<MP, Add>;
    auto wedge = [bot](std::size_t i, std::size_t j) {
      return (j == i + 1) ? MP::of(1) : bot;
    };
    const auto Tstar = star<4>(materialise<4>(wedge));
    CHECK(Tstar[0][3] == MP::of(3));  // longest path 0→1→2→3 costs 3
    CHECK(Tstar[1][3] == MP::of(2));  // 1→2→3 costs 2
    CHECK(Tstar[0][0] == MP::of(0));  // ⊗-identity on the diagonal (empty path)
    CHECK(Tstar[3][0] == bot);        // unreachable = the ⊕-identity
  }

  SECTION("MinPlus: one semiring apart, the star is the shortest path") {
    using MP = MinPlus<unsigned long long>;
    using Add = typename dedekind::algebra::semiring_ops<MP>::add;
    const MP top = dedekind::category::identity_v<MP, Add>;  // ⊕-id = +∞
    auto wedge = [top](std::size_t i, std::size_t j) {
      return (j == i + 1) ? MP::of(1) : top;
    };
    const auto Sstar = star<4>(materialise<4>(wedge));
    CHECK(Sstar[0][3] == MP::of(3));  // the only 0→3 path costs 3
    CHECK(Sstar[3][0] == top);        // unreachable = +∞
  }
}
