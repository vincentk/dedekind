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

static_assert(IsMonicArrow<Identity<machine_integer>>);
static_assert(IsMonicArrow<std::decay_t<decltype(embed_𝔹_ℕ)>>);
static_assert(IsMonicArrow<std::decay_t<decltype(embed_ℕ_ℤ)>>);
static_assert(IsMonicArrow<std::decay_t<decltype(embed_K3_ℤ)>>);
static_assert(IsMonicArrow<std::decay_t<decltype(embed_ℤ_ℚ<>)>>);
static_assert(IsMonicArrow<std::decay_t<decltype(embed_ℚ_ℝ<>)>>);
static_assert(IsMonicArrow<std::decay_t<decltype(embed_ℝ_ℂ<>)>>);

// ---------------------------------------------------------------------------
// Arrow types are correct (compile-time)
// ---------------------------------------------------------------------------

static_assert(std::same_as<Dom<std::decay_t<decltype(embed_𝔹_ℕ)>>, bool>);
static_assert(std::same_as<Cod<std::decay_t<decltype(embed_𝔹_ℕ)>>, unsigned>);

static_assert(std::same_as<Dom<std::decay_t<decltype(embed_ℕ_ℤ)>>, unsigned>);
static_assert(
    std::same_as<Cod<std::decay_t<decltype(embed_ℕ_ℤ)>>, machine_integer>);

static_assert(std::same_as<Dom<std::decay_t<decltype(embed_K3_ℤ)>>, Ternary>);
static_assert(
    std::same_as<Cod<std::decay_t<decltype(embed_K3_ℤ)>>, machine_integer>);

static_assert(
    std::same_as<Dom<std::decay_t<decltype(embed_ℤ_ℚ<>)>>, machine_integer>);
static_assert(std::same_as<Cod<std::decay_t<decltype(embed_ℤ_ℚ<>)>>,
                           Rational<machine_integer>>);

static_assert(std::same_as<Dom<std::decay_t<decltype(embed_ℚ_ℝ<>)>>,
                           Rational<machine_integer>>);
static_assert(std::same_as<Cod<std::decay_t<decltype(embed_ℚ_ℝ<>)>>,
                           Real<machine_real_scalar>>);

static_assert(std::same_as<Dom<std::decay_t<decltype(embed_ℝ_ℂ<>)>>,
                           Real<machine_real_scalar>>);
static_assert(std::same_as<Cod<std::decay_t<decltype(embed_ℝ_ℂ<>)>>,
                           Complex<machine_real_scalar>>);

static_assert(IsSpecies<Rational<machine_integer>>);
static_assert(IsSpecies<Real<machine_real_scalar>>);
static_assert(IsSpecies<Complex<machine_real_scalar>>);

// ---------------------------------------------------------------------------
// 𝔹 ↪ ℕ
// ---------------------------------------------------------------------------

TEST_CASE("Tower: 𝔹 ↪ ℕ via embed_𝔹_ℕ", "[numbers][tower][embedding]") {
  const auto naturals_u =
      ambient_set<unsigned>([](const unsigned&) { return true; });

  CHECK(embed_𝔹_ℕ(false) == 0u);
  CHECK(embed_𝔹_ℕ(true) == 1u);

  CHECK(in_via(false, embed_𝔹_ℕ, naturals_u) == true);
  CHECK(in_via(true, embed_𝔹_ℕ, naturals_u) == true);
}

// ---------------------------------------------------------------------------
// ℕ ↪ ℤ
// ---------------------------------------------------------------------------

TEST_CASE("Tower: ℕ ↪ ℤ via embed_ℕ_ℤ", "[numbers][tower][embedding]") {
  // Unsigned naturals always embed into integers successfully.
  const auto integers = ambient_set<int>([](const int&) { return true; });

  CHECK(in_via(0u, embed_ℕ_ℤ, integers) == true);
  CHECK(in_via(42u, embed_ℕ_ℤ, integers) == true);
  CHECK(in_via(1000u, embed_ℕ_ℤ, integers) == true);

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

TEST_CASE("Tower: K3 ↪ ℤ via embed_K3_ℤ", "[numbers][tower][embedding]") {
  const auto integers = ambient_set<int>([](const int&) { return true; });

  CHECK(embed_K3_ℤ(Ternary::False) == -1);
  CHECK(embed_K3_ℤ(Ternary::Unknown) == 0);
  CHECK(embed_K3_ℤ(Ternary::True) == 1);

  CHECK(in_via(Ternary::False, embed_K3_ℤ, integers) == true);
  CHECK(in_via(Ternary::Unknown, embed_K3_ℤ, integers) == true);
  CHECK(in_via(Ternary::True, embed_K3_ℤ, integers) == true);
}

// ---------------------------------------------------------------------------
// ℤ ↪ ℚ
// ---------------------------------------------------------------------------

TEST_CASE("Tower: ℤ ↪ ℚ via embed_ℤ_ℚ", "[numbers][tower][embedding]") {
  // Embedding preserves value: n -> n/1.
  CHECK(embed_ℤ_ℚ<>(3).num() == 3);
  CHECK(embed_ℤ_ℚ<>(3).den() == 1);
  CHECK(embed_ℤ_ℚ<>(-5).num() == -5);
}

// ---------------------------------------------------------------------------
// ℚ ↪ ℝ
// ---------------------------------------------------------------------------

TEST_CASE("Tower: ℚ ↪ ℝ via embed_ℚ_ℝ", "[numbers][tower][embedding]") {
  // Embedding preserves value: 3/1 -> 3.0.
  CHECK(embed_ℚ_ℝ<>(Rational<machine_integer>{3, 1}).resolve() == 3.0);
  CHECK(embed_ℚ_ℝ<>(Rational<machine_integer>{1, 2}).resolve() == 0.5);
  CHECK(embed_ℚ_ℝ<>(Rational<machine_integer>{-7, 4}).resolve() == -1.75);
}

// ---------------------------------------------------------------------------
// ℝ ↪ ℂ
// ---------------------------------------------------------------------------

TEST_CASE("Tower: ℝ ↪ ℂ via embed_ℝ_ℂ", "[numbers][tower][embedding]") {
  // Embedding: imaginary part is always 0.
  CHECK(embed_ℝ_ℂ<>(Real<machine_real_scalar>{3.0}).real() == 3.0);
  CHECK(embed_ℝ_ℂ<>(Real<machine_real_scalar>{3.0}).imag() == 0.0);
  CHECK(embed_ℝ_ℂ<>(Real<machine_real_scalar>{-2.5}).real() == -2.5);
  CHECK(embed_ℝ_ℂ<>(Real<machine_real_scalar>{-2.5}).imag() == 0.0);
}

// ---------------------------------------------------------------------------
// Composed tower: unsigned ↪ ℤ ↪ ℚ ↪ ℝ ↪ ℂ  (via >> composition)
// ---------------------------------------------------------------------------

TEST_CASE("Tower: composed chain unsigned -> ℤ -> ℚ -> ℝ -> ℂ",
          "[numbers][tower][embedding][composition]") {
  const auto embed_ℕ_ℚ = embed_ℕ_ℤ >> embed_ℤ_ℚ<>;
  const auto embed_ℕ_ℝ = embed_ℕ_ℚ >> embed_ℚ_ℝ<>;
  const auto embed_ℕ_ℂ = embed_ℕ_ℝ >> embed_ℝ_ℂ<>;

  // 3 travels the full chain and arrives in ℂ as (3 + 0i).
  const auto result = embed_ℕ_ℂ(3u);
  CHECK(result.real() == 3.0);
  CHECK(result.imag() == 0.0);
}

// ---------------------------------------------------------------------------
// Stress test: Im(K3 -> ℤ) ∩ { z in ℂ | Re(z) > 0 }
// ---------------------------------------------------------------------------

TEST_CASE("Stress: Im(K3->ℤ) intersect positive-real ℂ",
          "[numbers][tower][embedding][intersection]") {
  // Image of K3 in ℤ under canonical embedding: {-1, 0, 1}.
  const int z_false = embed_K3_ℤ(Ternary::False);
  const int z_unknown = embed_K3_ℤ(Ternary::Unknown);
  const int z_true = embed_K3_ℤ(Ternary::True);

  const auto embed_ℤ_ℂ = embed_ℤ_ℚ<> >> embed_ℚ_ℝ<> >> embed_ℝ_ℂ<>;

  const auto c_false = embed_ℤ_ℂ(z_false);
  const auto c_unknown = embed_ℤ_ℂ(z_unknown);
  const auto c_true = embed_ℤ_ℂ(z_true);

  const auto has_positive_real = [](const Complex<machine_real_scalar>& z) {
    return z.real() > 0.0;
  };

  CHECK(has_positive_real(c_false) == false);
  CHECK(has_positive_real(c_unknown) == false);
  CHECK(has_positive_real(c_true) == true);

  // Therefore the intersection is exactly {1 + 0i}.
  CHECK(c_true.real() == 1.0);
  CHECK(c_true.imag() == 0.0);
}
