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

TEST_CASE(
    "transfer: Mat(S) — the matrix semiring and its bra·ket vectors at runtime",
    "[linear_algebra][matnxn][semiring]") {
  using MP = MaxPlus<unsigned long long>;
  using Mat = MatNxNV<MP, 2>;
  using Add =
      typename dedekind::algebra::semiring_ops<Mat>::add;  // ⊕ = MatPlus
  using Mult =
      typename dedekind::algebra::semiring_ops<Mat>::mult;  // ⊗ = MatTimes
  using SAdd = typename dedekind::algebra::semiring_ops<MP>::add;
  const MP bot = dedekind::category::identity_v<MP, SAdd>;  // ⊕-identity (−∞)

  auto mk = [](MP a, MP b, MP c, MP d) {
    Mat m{};
    m.e[0] = {a, b};
    m.e[1] = {c, d};
    return m;
  };
  const Mat A = mk(MP::of(1), MP::of(2), MP::of(3), MP::of(4));
  const Mat B = mk(MP::of(0), MP::of(5), MP::of(6), MP::of(0));

  SECTION("entry access, ==, and transpose (the extensional dagger)") {
    CHECK(A(0, 1) == MP::of(2));
    CHECK(A[1][0] == MP::of(3));
    CHECK(A == A);
    CHECK_FALSE(A == B);
    const Mat At = A.transpose();
    CHECK(At(1, 0) == A(0, 1));
    CHECK(At(0, 1) == A(1, 0));
  }

  SECTION("⊕ is elementwise max; ⊗ is the tropical matrix product") {
    const Mat sum = Add{}(A, B);
    CHECK(sum(0, 0) == MP::of(1));  // max(1, 0)
    CHECK(sum(0, 1) == MP::of(5));  // max(2, 5)
    const Mat prod = Mult{}(A, B);
    CHECK(prod(0, 0) == MP::of(8));  // max(1+0, 2+6) = 8
    CHECK(prod(1, 1) == MP::of(8));  // max(3+5, 4+0) = 8
  }

  SECTION("the identity and zero matrices") {
    const Mat I = identity_matrix<MP, 2>();
    CHECK(I(0, 0) == MP::of(0));  // ⊗-identity on the diagonal
    CHECK(I(0, 1) == bot);        // ⊕-identity off it
    const Mat Z = zero_matrix<MP, 2>();
    CHECK(Z(0, 0) == bot);
    CHECK(Mult{}(A, I) == A);  // Δ is the ⊗-unit
  }

  SECTION(
      "column / row ARE Ket / Bra, carrying the semimodule ⊕ and scalar ⊗") {
    const auto col = A.column(1);  // |v⟩ = [2, 4]
    CHECK(col[0] == MP::of(2));
    CHECK(col[1] == MP::of(4));
    const auto row = A.row(0);  // ⟨w| = [1, 2]
    CHECK(row[0] == MP::of(1));
    CHECK(row[1] == MP::of(2));

    const auto col0 = A.column(0);   // [1, 3]
    const auto joined = col + col0;  // elementwise max: [2, 4]
    CHECK(joined[0] == MP::of(2));
    CHECK(joined[1] == MP::of(4));
    const auto scaled = MP::of(3) * col;  // scalar ⊗ (= +): [5, 7]
    CHECK(scaled[0] == MP::of(5));
    CHECK(scaled[1] == MP::of(7));
    CHECK(col * MP::of(3) == scaled);  // the two-sided action agrees
  }
}

TEST_CASE(
    "transfer: the dagger surface — isometry / unitary as computed predicates",
    "[linear_algebra][involution][dagger]") {
  // Runtime companion to the compile-time witnesses in :transfer.  The dagger
  // predicates (dedekind::category, :involution) are computed on the ARROW
  // value: over Mat(𝔹) "the converse is the inverse" is exactly f° ; f = Δ.
  using Mat = MatNxNV<bool, 2>;
  using Dag = dedekind::linear_algebra::TransposeF<bool, 2>;
  using Mult = typename dedekind::algebra::semiring_ops<Mat>::mult;
  namespace cat = dedekind::category;

  auto mk = [](bool a, bool b, bool c, bool d) {
    Mat m{};
    m.e[0] = {a, b};
    m.e[1] = {c, d};
    return m;
  };
  const Mat swap = mk(false, true, true, false);  // the 2-cycle (a permutation)
  const Mat id2 = mk(true, false, false, true);   // the identity
  const Mat shear = mk(true, true, false, true);  // NOT a permutation

  SECTION("transpose is a certified dagger (an involution)") {
    CHECK(cat::IsDagger<Dag, Mat>);
    CHECK(Dag{}(swap) == swap);           // the swap is symmetric: sᵀ = s
    CHECK(Dag{}(Dag{}(shear)) == shear);  // Aᵀᵀ = A
  }
  SECTION("a permutation is UNITARY: s° ; s = Δ and s ; s° = Δ") {
    CHECK(cat::is_isometry<Dag, Mult>(swap));
    CHECK(cat::is_coisometry<Dag, Mult>(swap));
    CHECK(cat::is_unitary<Dag, Mult>(swap));
    CHECK(
        cat::is_unitary<Dag, Mult>(id2));  // the identity is trivially unitary
  }
  SECTION("a non-permutation is NOT unitary (the predicate is honest)") {
    CHECK_FALSE(cat::is_isometry<Dag, Mult>(shear));
    CHECK_FALSE(cat::is_unitary<Dag, Mult>(shear));
  }
}
