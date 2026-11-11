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
using dedekind::sets::Integer;
using dedekind::sets::Numeral;

static_assert(IsSpecies<Rational>);
static_assert(IsSpecies<Complex<Rational>>);

TEST_CASE("Partial Arithmetic: Rational",
          "[numbers][tower][partial][rational]") {
  const auto add_op = PartialAddRational{};
  const auto mul_op = PartialMulRational{};
  const auto div_op = HonestDivRational{};

  const auto q1 = Rational(1, 2);
  const auto q2 = Rational(1, 3);

  const auto add_result = add_op(std::make_pair(q1, q2));
  CHECK(add_result.status == Ternary::True);
  CHECK(add_result.value == Rational(5, 6));

  const auto mul_result = mul_op(std::make_pair(q1, q2));
  CHECK(mul_result.status == Ternary::True);
  CHECK(mul_result.value == Rational(1, 6));

  const auto div_result = div_op(std::make_pair(q1, q2));
  CHECK(div_result.status == Ternary::True);
  CHECK(div_result.value == Rational(3, 2));

  const auto div_zero = div_op(std::make_pair(q1, Rational(0, 1)));
  CHECK(div_zero.status == Ternary::False);
}

TEST_CASE("Partial Arithmetic: Complex<R>",
          "[numbers][tower][partial][complex]") {
  using Q = Rational;
  const auto add_op = PartialAddComplex<Q>{};
  const auto mul_op = PartialMulComplex<Q>{};

  const auto c1 = Complex<Q>{Q{1}, Q{2}};
  const auto c2 = Complex<Q>{Q{3}, Q{4}};

  // (1+2i) + (3+4i) = (4+6i)
  const auto add_result = add_op(std::make_pair(c1, c2));
  CHECK(add_result.status == Ternary::True);
  CHECK(add_result.value.real() == Q{4});
  CHECK(add_result.value.imag() == Q{6});

  // (1+2i)(3+4i) = -5+10i
  const auto mul_result = mul_op(std::make_pair(c1, c2));
  CHECK(mul_result.status == Ternary::True);
  CHECK(mul_result.value.real() == Q{-5});
  CHECK(mul_result.value.imag() == Q{10});
}

TEST_CASE("Partial Embedding ℤ ↪ ℚ with Ternary status",
          "[numbers][tower][partial][embeddings]") {
  const auto embed_z_to_q = PartialEmbedIntegerToRational{};
  const auto z_result = embed_z_to_q(Integer{42});
  CHECK(z_result.status == Ternary::True);
  CHECK(z_result.value.num() == Integer{42});
  CHECK(z_result.value.den() == Numeral{1u});
}

// Kleene traits: exact ℚ arithmetic is associative and commutative.
static_assert(is_kleene_associative_v<Rational, PartialAddRational>);
static_assert(is_kleene_commutative_v<Rational, PartialAddRational>);
static_assert(partial_unit_v<Rational, PartialAddRational>.num() == Integer{});
static_assert(is_kleene_associative_v<Rational, PartialMulRational>);
static_assert(is_kleene_commutative_v<Rational, PartialMulRational>);
static_assert(partial_unit_v<Rational, PartialMulRational>.num() == Integer{1});

// Complex arithmetic is commutative, registered for every exact scalar.
static_assert(
    is_kleene_commutative_v<Complex<Rational>, PartialAddComplex<Rational>>);
static_assert(
    is_kleene_commutative_v<Complex<Rational>, PartialMulComplex<Rational>>);
