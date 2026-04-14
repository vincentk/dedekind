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

// ---------------------------------------------------------------------------
// Partial Arithmetic and Ternary Logic Integration
// ---------------------------------------------------------------------------

TEST_CASE("Partial Arithmetic: Rational<I>",
          "[numbers][tower][partial][rational]") {
  const auto add_op = PartialAddRational<machine_integer>{};
  const auto mul_op = PartialMulRational<machine_integer>{};

  // Partial addition on rationals is exact (Ternary::True)
  const auto q1 = Rational<machine_integer>(1, 2);
  const auto q2 = Rational<machine_integer>(1, 3);

  const auto add_result = add_op(std::make_pair(q1, q2));
  CHECK(add_result.status == Ternary::True);  // Exact arithmetic
  CHECK(add_result.value == Rational<machine_integer>(5, 6));

  // Partial multiplication is also exact
  const auto mul_result = mul_op(std::make_pair(q1, q2));
  CHECK(mul_result.status == Ternary::True);
  CHECK(mul_result.value == Rational<machine_integer>(1, 6));
}

TEST_CASE("Partial Arithmetic: Real<S>", "[numbers][tower][partial][real]") {
  const auto add_op = PartialAddReal<machine_real_scalar>{};
  const auto mul_op = PartialMulReal<machine_real_scalar>{};
  const auto div_op = PartialDivReal<machine_real_scalar>{};

  const auto r1 = Real<machine_real_scalar>{1.5};
  const auto r2 = Real<machine_real_scalar>{2.0};

  // Partial addition on reals always succeeds
  const auto add_result = add_op(std::make_pair(r1, r2));
  CHECK(add_result.status == Ternary::True);
  CHECK(add_result.value.resolve() == 3.5);

  // Partial multiplication succeeds
  const auto mul_result = mul_op(std::make_pair(r1, r2));
  CHECK(mul_result.status == Ternary::True);
  CHECK(mul_result.value.resolve() == 3.0);

  // Division by non-zero succeeds
  const auto div_result = div_op(std::make_pair(r1, r2));
  CHECK(div_result.status == Ternary::True);

  // Division by zero fails (returns False status)
  const auto div_zero =
      div_op(std::make_pair(r1, Real<machine_real_scalar>{0}));
  CHECK(div_zero.status == Ternary::False);
}

TEST_CASE("Partial Arithmetic: Complex<R>",
          "[numbers][tower][partial][complex]") {
  const auto add_op = PartialAddComplex<machine_real_scalar>{};
  const auto mul_op = PartialMulComplex<machine_real_scalar>{};

  const auto c1 = Complex<machine_real_scalar>{1.0, 2.0};
  const auto c2 = Complex<machine_real_scalar>{3.0, 4.0};

  // Partial addition: (1+2i) + (3+4i) = (4+6i)
  const auto add_result = add_op(std::make_pair(c1, c2));
  CHECK(add_result.status == Ternary::True);
  CHECK(add_result.value.real() == 4.0);
  CHECK(add_result.value.imag() == 6.0);

  // Partial multiplication: (1+2i)(3+4i) = 3+4i+6i+8i^2 = -5+10i
  const auto mul_result = mul_op(std::make_pair(c1, c2));
  CHECK(mul_result.status == Ternary::True);
  CHECK(mul_result.value.real() == -5.0);
  CHECK(mul_result.value.imag() == 10.0);
}

TEST_CASE("Partial Embeddings with Ternary Status",
          "[numbers][tower][partial][embeddings]") {
  // ℤ ↪ ℚ: exact embedding (Ternary::True)
  const auto embed_z_to_q = PartialEmbedIntegerToRational<machine_integer>{};
  const auto z_result = embed_z_to_q(42);
  CHECK(z_result.status == Ternary::True);
  CHECK(z_result.value.num() == 42);
  CHECK(z_result.value.den() == 1);

  // ℚ ↪ ℝ: lossy embedding (Ternary::Unknown)
  const auto embed_q_to_r =
      PartialEmbedRationalToReal<machine_integer, machine_real_scalar>{};
  const auto q = Rational<machine_integer>(1, 3);
  const auto q_result = embed_q_to_r(q);
  CHECK(q_result.status == Ternary::Unknown);  // Flagged as potentially lossy
  CHECK(q_result.value.resolve() ==
        static_cast<machine_real_scalar>(1.0 / 3.0));

  // ℝ ↪ ℂ: exact embedding (Ternary::True)
  const auto embed_r_to_c = PartialEmbedRealToComplex<machine_real_scalar>{};
  const auto r = Real<machine_real_scalar>{2.5};
  const auto r_result = embed_r_to_c(r);
  CHECK(r_result.status == Ternary::True);
  CHECK(r_result.value.real() == 2.5);
  CHECK(r_result.value.imag() == 0.0);
}

// Static asserts for Kleene traits
static_assert(is_kleene_associative_v<Rational<machine_integer>,
                                      PartialAddRational<machine_integer>>);
static_assert(is_kleene_commutative_v<Rational<machine_integer>,
                                      PartialAddRational<machine_integer>>);
static_assert(partial_identity_v<Rational<machine_integer>,
                                 PartialAddRational<machine_integer>>.num() ==
              0);

static_assert(is_kleene_associative_v<Rational<machine_integer>,
                                      PartialMulRational<machine_integer>>);
static_assert(is_kleene_commutative_v<Rational<machine_integer>,
                                      PartialMulRational<machine_integer>>);
static_assert(partial_identity_v<Rational<machine_integer>,
                                 PartialMulRational<machine_integer>>.num() ==
              1);

// Floating-point is commutative but NOT associative: (a+b)+c != a+(b+c) due to
// rounding. Associativity-by-fiat is reserved for the explicit
// dedekind::ieee::IEEE<F> opt-in.
static_assert(!is_kleene_associative_v<Real<machine_real_scalar>,
                                       PartialAddReal<machine_real_scalar>>);
static_assert(is_kleene_commutative_v<Real<machine_real_scalar>,
                                      PartialAddReal<machine_real_scalar>>);

static_assert(!is_kleene_associative_v<Real<machine_real_scalar>,
                                       PartialMulReal<machine_real_scalar>>);
static_assert(is_kleene_commutative_v<Real<machine_real_scalar>,
                                      PartialMulReal<machine_real_scalar>>);

static_assert(!is_kleene_associative_v<Complex<machine_real_scalar>,
                                       PartialAddComplex<machine_real_scalar>>);
static_assert(is_kleene_commutative_v<Complex<machine_real_scalar>,
                                      PartialAddComplex<machine_real_scalar>>);

static_assert(!is_kleene_associative_v<Complex<machine_real_scalar>,
                                       PartialMulComplex<machine_real_scalar>>);
static_assert(is_kleene_commutative_v<Complex<machine_real_scalar>,
                                      PartialMulComplex<machine_real_scalar>>);
