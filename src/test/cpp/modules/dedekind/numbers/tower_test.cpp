/**
 * @file tower_test.cpp
 * @brief The partial arithmetic of @c :numbers: ℤ ↪ ℚ and the
 * Ternary-status transforms on ℚ and ℂ.
 *
 * The lower rungs (𝔹 ↪ ℕ, 𝕂3 ↪ ℤ) are tested in their own partitions'
 * suites (morphologies/uint_test, numbers/integer_test).
 */
#include <catch2/catch_test_macros.hpp>

import dedekind.category;
import dedekind.numbers;

using namespace dedekind::category;
using namespace dedekind::numbers;

static_assert(IsSpecies<Rational<machine_integer>>);
static_assert(IsSpecies<Complex<machine_real_scalar>>);

TEST_CASE("Partial Arithmetic: Rational<I>",
          "[numbers][tower][partial][rational]") {
  const auto add_op = PartialAddRational<machine_integer>{};
  const auto mul_op = PartialMulRational<machine_integer>{};
  const auto div_op = HonestDivRational<machine_integer>{};

  const auto q1 = Rational<machine_integer>(1, 2);
  const auto q2 = Rational<machine_integer>(1, 3);

  const auto add_result = add_op(std::make_pair(q1, q2));
  CHECK(add_result.status == Ternary::True);
  CHECK(add_result.value == Rational<machine_integer>(5, 6));

  const auto mul_result = mul_op(std::make_pair(q1, q2));
  CHECK(mul_result.status == Ternary::True);
  CHECK(mul_result.value == Rational<machine_integer>(1, 6));

  const auto div_result = div_op(std::make_pair(q1, q2));
  CHECK(div_result.status == Ternary::True);
  CHECK(div_result.value == Rational<machine_integer>(3, 2));

  const auto div_zero =
      div_op(std::make_pair(q1, Rational<machine_integer>(0, 1)));
  CHECK(div_zero.status == Ternary::False);
}

TEST_CASE("Partial Arithmetic: Complex<R>",
          "[numbers][tower][partial][complex]") {
  const auto add_op = PartialAddComplex<machine_real_scalar>{};
  const auto mul_op = PartialMulComplex<machine_real_scalar>{};

  const auto c1 = Complex<machine_real_scalar>{1.0, 2.0};
  const auto c2 = Complex<machine_real_scalar>{3.0, 4.0};

  // (1+2i) + (3+4i) = (4+6i)
  const auto add_result = add_op(std::make_pair(c1, c2));
  CHECK(add_result.status == Ternary::True);
  CHECK(add_result.value.real() == 4.0);
  CHECK(add_result.value.imag() == 6.0);

  // (1+2i)(3+4i) = -5+10i
  const auto mul_result = mul_op(std::make_pair(c1, c2));
  CHECK(mul_result.status == Ternary::True);
  CHECK(mul_result.value.real() == -5.0);
  CHECK(mul_result.value.imag() == 10.0);
}

TEST_CASE("Partial Embedding ℤ ↪ ℚ with Ternary status",
          "[numbers][tower][partial][embeddings]") {
  const auto embed_z_to_q = PartialEmbedIntegerToRational<machine_integer>{};
  const auto z_result = embed_z_to_q(42);
  CHECK(z_result.status == Ternary::True);
  CHECK(z_result.value.num() == 42);
  CHECK(z_result.value.den() == 1);
}

// Kleene traits: exact ℚ arithmetic is associative and commutative.
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

// Floating-point is commutative but NOT associative: (a+b)+c != a+(b+c) under
// rounding.  Associativity-by-fiat is reserved for dedekind::ieee::IEEE<F>.
static_assert(!is_kleene_associative_v<Complex<machine_real_scalar>,
                                       PartialAddComplex<machine_real_scalar>>);
static_assert(is_kleene_commutative_v<Complex<machine_real_scalar>,
                                      PartialAddComplex<machine_real_scalar>>);
static_assert(!is_kleene_associative_v<Complex<machine_real_scalar>,
                                       PartialMulComplex<machine_real_scalar>>);
static_assert(is_kleene_commutative_v<Complex<machine_real_scalar>,
                                      PartialMulComplex<machine_real_scalar>>);
