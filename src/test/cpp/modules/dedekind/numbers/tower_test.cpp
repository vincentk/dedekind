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
using dedekind::sets::IntExpr;
using dedekind::sets::RatExpr;

static_assert(IsSpecies<RatExpr<>>);
static_assert(IsSpecies<Complex<RatExpr<>>>);

TEST_CASE("Partial Arithmetic: RatExpr<>",
          "[numbers][tower][partial][rational]") {
  const auto add_op = PartialAddRational{};
  const auto mul_op = PartialMulRational{};
  const auto div_op = HonestDivRational{};

  const auto q1 = RatExpr<>(1, 2);
  const auto q2 = RatExpr<>(1, 3);

  const auto add_result = add_op(std::make_pair(q1, q2));
  CHECK(add_result.status == Ternary::True);
  CHECK(add_result.value == RatExpr<>(5, 6));

  const auto mul_result = mul_op(std::make_pair(q1, q2));
  CHECK(mul_result.status == Ternary::True);
  CHECK(mul_result.value == RatExpr<>(1, 6));

  const auto div_result = div_op(std::make_pair(q1, q2));
  CHECK(div_result.status == Ternary::True);
  CHECK(div_result.value == RatExpr<>(3, 2));

  const auto div_zero = div_op(std::make_pair(q1, RatExpr<>(0, 1)));
  CHECK(div_zero.status == Ternary::False);
}

TEST_CASE("Partial Arithmetic: Complex<R>",
          "[numbers][tower][partial][complex]") {
  using Q = RatExpr<>;
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
  const auto z_result = embed_z_to_q(IntExpr<>{42});
  CHECK(z_result.status == Ternary::True);
  CHECK(z_result.value == RatExpr<>{42});
}

// Kleene traits: exact ℚ arithmetic is associative and commutative.
static_assert(is_kleene_associative_v<RatExpr<>, PartialAddRational>);
static_assert(is_kleene_commutative_v<RatExpr<>, PartialAddRational>);
static_assert(partial_unit_v<RatExpr<>, PartialAddRational> == RatExpr<>{});
static_assert(is_kleene_associative_v<RatExpr<>, PartialMulRational>);
static_assert(is_kleene_commutative_v<RatExpr<>, PartialMulRational>);
static_assert(partial_unit_v<RatExpr<>, PartialMulRational> == RatExpr<>{1});

// Complex arithmetic is commutative, registered for every exact scalar.
static_assert(
    is_kleene_commutative_v<Complex<RatExpr<>>, PartialAddComplex<RatExpr<>>>);
static_assert(
    is_kleene_commutative_v<Complex<RatExpr<>>, PartialMulComplex<RatExpr<>>>);
