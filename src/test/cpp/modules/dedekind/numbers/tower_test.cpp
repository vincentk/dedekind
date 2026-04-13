/**
 * @file tower_test.cpp
 * @brief The numerical tower: embedding-mediated membership through ℕ ↪ ℤ ↪ ℚ ↪
 * ℝ ↪ ℂ.
 *
 * @section Design_Note
 * We deliberately model inclusions as *embedding arrows* rather than subset
 * declarations.  This avoids closure / totality commitments: each embedding
 * e: X -> Y is a user-declared monic arrow; membership in Y is tested via
 * χ_Y(e(x)), not by asserting X ⊆ Y in a categorical sense.
 *
 * See: etcs.cppm — `in_via(x, e, S)` for the underlying primitive.
 */
#include <catch2/catch_test_macros.hpp>

import dedekind.category;
import dedekind.numbers;
import dedekind.sets;

using namespace dedekind::category;
using namespace dedekind::numbers;
using namespace dedekind::sets;

// ---------------------------------------------------------------------------
// Compile-time monicity declarations are honoured
// ---------------------------------------------------------------------------

static_assert(IsMonicArrow<Identity<int>>);
static_assert(IsMonicArrow<std::decay_t<decltype(embed_B_N)>>);
static_assert(IsMonicArrow<std::decay_t<decltype(embed_N_Z)>>);
static_assert(IsMonicArrow<std::decay_t<decltype(embed_K3_Z)>>);
static_assert(IsMonicArrow<std::decay_t<decltype(embed_Z_Q)>>);
static_assert(IsMonicArrow<std::decay_t<decltype(embed_Q_R)>>);
static_assert(IsMonicArrow<std::decay_t<decltype(embed_R_C)>>);

// ---------------------------------------------------------------------------
// Arrow types are correct (compile-time)
// ---------------------------------------------------------------------------

static_assert(std::same_as<Dom<std::decay_t<decltype(embed_B_N)>>, bool>);
static_assert(std::same_as<Cod<std::decay_t<decltype(embed_B_N)>>, unsigned>);

static_assert(std::same_as<Dom<std::decay_t<decltype(embed_N_Z)>>, unsigned>);
static_assert(std::same_as<Cod<std::decay_t<decltype(embed_N_Z)>>, int>);

static_assert(std::same_as<Dom<std::decay_t<decltype(embed_K3_Z)>>, Ternary>);
static_assert(std::same_as<Cod<std::decay_t<decltype(embed_K3_Z)>>, int>);

static_assert(std::same_as<Dom<std::decay_t<decltype(embed_Z_Q)>>, int>);
static_assert(
    std::same_as<Cod<std::decay_t<decltype(embed_Z_Q)>>, Rational<int>>);

static_assert(
    std::same_as<Dom<std::decay_t<decltype(embed_Q_R)>>, Rational<int>>);
static_assert(
    std::same_as<Cod<std::decay_t<decltype(embed_Q_R)>>, Real<double>>);

static_assert(
    std::same_as<Dom<std::decay_t<decltype(embed_R_C)>>, Real<double>>);
static_assert(
    std::same_as<Cod<std::decay_t<decltype(embed_R_C)>>, Complex<double>>);

static_assert(IsSpecies<Rational<int>>);
static_assert(IsSpecies<Real<double>>);
static_assert(IsSpecies<Complex<double>>);

// ---------------------------------------------------------------------------
// 𝔹 ↪ ℕ
// ---------------------------------------------------------------------------

TEST_CASE("Tower: 𝔹 ↪ ℕ via embed_B_N", "[numbers][tower][embedding]") {
  const auto naturals_u =
      ambient_set<unsigned>([](const unsigned&) { return true; });

  CHECK(embed_B_N(false) == 0u);
  CHECK(embed_B_N(true) == 1u);

  CHECK(in_via(false, embed_B_N, naturals_u) == true);
  CHECK(in_via(true, embed_B_N, naturals_u) == true);
}

// ---------------------------------------------------------------------------
// ℕ ↪ ℤ
// ---------------------------------------------------------------------------

TEST_CASE("Tower: ℕ ↪ ℤ via embed_N_Z", "[numbers][tower][embedding]") {
  // Unsigned naturals always embed into integers successfully.
  const auto integers = ambient_set<int>([](const int&) { return true; });

  CHECK(in_via(0u, embed_N_Z, integers) == true);
  CHECK(in_via(42u, embed_N_Z, integers) == true);
  CHECK(in_via(1000u, embed_N_Z, integers) == true);

  // Membership in the naturals set respects sign.
  CHECK(N(0) == true);
  CHECK(N(7) == true);
  CHECK(N(-1) == false);

  // Unsigned values are always natural.
  CHECK(N(0u) == true);
  CHECK(N(42u) == true);
}

// ---------------------------------------------------------------------------
// K3 ↪ ℤ
// ---------------------------------------------------------------------------

TEST_CASE("Tower: K3 ↪ ℤ via embed_K3_Z", "[numbers][tower][embedding]") {
  const auto integers = ambient_set<int>([](const int&) { return true; });

  CHECK(embed_K3_Z(Ternary::False) == -1);
  CHECK(embed_K3_Z(Ternary::Unknown) == 0);
  CHECK(embed_K3_Z(Ternary::True) == 1);

  CHECK(in_via(Ternary::False, embed_K3_Z, integers) == true);
  CHECK(in_via(Ternary::Unknown, embed_K3_Z, integers) == true);
  CHECK(in_via(Ternary::True, embed_K3_Z, integers) == true);
}

// ---------------------------------------------------------------------------
// ℤ ↪ ℚ
// ---------------------------------------------------------------------------

TEST_CASE("Tower: ℤ ↪ ℚ via embed_Z_Q", "[numbers][tower][embedding]") {
  // Embedding preserves value: n -> n/1.
  CHECK(embed_Z_Q(3).num() == 3);
  CHECK(embed_Z_Q(3).den() == 1);
  CHECK(embed_Z_Q(-5).num() == -5);
}

// ---------------------------------------------------------------------------
// ℚ ↪ ℝ
// ---------------------------------------------------------------------------

TEST_CASE("Tower: ℚ ↪ ℝ via embed_Q_R", "[numbers][tower][embedding]") {
  // Embedding preserves value: 3/1 -> 3.0.
  CHECK(embed_Q_R(Rational<int>{3, 1}).resolve() == 3.0);
  CHECK(embed_Q_R(Rational<int>{1, 2}).resolve() == 0.5);
  CHECK(embed_Q_R(Rational<int>{-7, 4}).resolve() == -1.75);
}

// ---------------------------------------------------------------------------
// ℝ ↪ ℂ
// ---------------------------------------------------------------------------

TEST_CASE("Tower: ℝ ↪ ℂ via embed_R_C", "[numbers][tower][embedding]") {
  // Embedding: imaginary part is always 0.
  CHECK(embed_R_C(Real<double>{3.0}).real() == 3.0);
  CHECK(embed_R_C(Real<double>{3.0}).imag() == 0.0);
  CHECK(embed_R_C(Real<double>{-2.5}).real() == -2.5);
  CHECK(embed_R_C(Real<double>{-2.5}).imag() == 0.0);
}

// ---------------------------------------------------------------------------
// Composed tower: unsigned ↪ ℤ ↪ ℚ ↪ ℝ ↪ ℂ  (via >> composition)
// ---------------------------------------------------------------------------

TEST_CASE("Tower: composed chain unsigned -> ℤ -> ℚ -> ℝ -> ℂ",
          "[numbers][tower][embedding][composition]") {
  const auto embed_N_Q = embed_N_Z >> embed_Z_Q;
  const auto embed_N_R = embed_N_Q >> embed_Q_R;
  const auto embed_N_C = embed_N_R >> embed_R_C;

  // 3 travels the full chain and arrives in ℂ as (3 + 0i).
  const auto result = embed_N_C(3u);
  CHECK(result.real() == 3.0);
  CHECK(result.imag() == 0.0);
}
