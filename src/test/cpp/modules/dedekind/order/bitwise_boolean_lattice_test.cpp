/** @file dedekind/order/bitwise_boolean_lattice_test.cpp
 *
 * Unit coverage for #710 — the bitwise Boolean lattice on integral
 * carriers under the bit-subset relation
 * @c (a @c & @c b) @c == @c a.  Lives in @c :order::lattice (the
 * order-theoretic-specialisations home for integral carriers); the
 * Form-chain row-7 concept it instantiates lives upstream in
 * @c :category::lattice.
 *
 * Cross-reference: @c algebra/galois.cppm carries the Galois field
 * structure on @c bool and @c 𝔽64 — a different algebraic structure
 * (field, not lattice) on the same/related underlying carriers.
 * The bitwise Boolean lattice here is the power-set lattice of bit
 * positions; the Galois field on @c bool (= @c 𝔽_2) is the smallest
 * non-trivial field.  Same underlying carriers, parallel algebraic
 * readings.
 */

#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <cstddef>
#include <functional>

import dedekind.category;
import dedekind.order;

using namespace dedekind::category;
using dedekind::order::bit_subset_eq;

TEST_CASE(
    "order:bitwise-boolean — size_t fires Form-chain rows 1–7 under "
    "bit_subset_eq",
    "[order][lattice][bitwise][boolean][size_t]") {
  /** @brief @c size_t under the bitwise Boolean algebra:
   *         (@c bit_subset_eq, @c |, @c &, @c ~).  All seven Form-chain
   *         rows fire — this is the power-set lattice of
   *         @c std::numeric_limits<std::size_t>::digits bit positions
   *         (64 on LP64 / LLP64; 32 on 32-bit targets), a finite
   *         Boolean algebra of that dimension. */
  STATIC_CHECK(
      IsThinCategory<std::size_t, bit_subset_eq<std::size_t>, ClassicalLogic>);
  STATIC_CHECK(
      IsPosetal<std::size_t, bit_subset_eq<std::size_t>, ClassicalLogic>);
  STATIC_CHECK(IsFilteredCategory<std::size_t, bit_subset_eq<std::size_t>,
                                  ClassicalLogic>);
  STATIC_CHECK(
      IsBooleanLatticeCategory<
          std::size_t, bit_subset_eq<std::size_t>, std::bit_or<std::size_t>,
          std::bit_and<std::size_t>, std::bit_not<std::size_t>>);
}

TEST_CASE("order:bitwise-boolean — unsigned int also fires",
          "[order][lattice][bitwise][boolean][unsigned]") {
  STATIC_CHECK(
      IsBooleanLatticeCategory<unsigned, bit_subset_eq<unsigned>,
                               std::bit_or<unsigned>, std::bit_and<unsigned>,
                               std::bit_not<unsigned>>);
}

TEST_CASE("order:bitwise-boolean — bit_subset_eq computes correctly",
          "[order][lattice][bitwise][boolean][value-level]") {
  /** @brief Value-level witness of the bit-subset relation.
   *         @c 0b0101 @c ⊆_bit @c 0b1111 because @c (0b0101 @c & @c
   *         0b1111) @c = @c 0b0101.  @c 0b0110 @c ⊄_bit @c 0b1001
   *         because they share no bits. */
  constexpr bit_subset_eq<unsigned> rel{};
  STATIC_CHECK(rel(0b0000u, 0b1111u));        // bottom ⊆ anything
  STATIC_CHECK(rel(0b0101u, 0b1111u));        // bits a subset of bits of b
  STATIC_CHECK(rel(0b0101u, 0b0101u));        // reflexivity
  STATIC_CHECK_FALSE(rel(0b0110u, 0b1001u));  // disjoint bit patterns
  STATIC_CHECK_FALSE(rel(0b1111u, 0b0101u));  // a strictly more bits than b
}

TEST_CASE("order:bitwise-boolean — complement laws hold at value level",
          "[order][lattice][bitwise][boolean][complement-laws]") {
  /** @brief Boolean complement laws on the bitwise lattice:
   *         @c a @c & @c ~a @c = @c 0 (bottom) and @c a @c | @c ~a
   *         @c = @c ~0 (top). */
  constexpr std::size_t a = 0b101010ULL;
  STATIC_CHECK((a & ~a) == std::size_t{0});
  STATIC_CHECK((a | ~a) == ~std::size_t{0});
}

TEST_CASE("order:bitwise-boolean — lattice bottom and top recover correctly",
          "[order][lattice][bitwise][boolean][bounds]") {
  STATIC_CHECK(LatticeBottom<std::size_t, bit_subset_eq<std::size_t>>::value ==
               std::size_t{0});
  STATIC_CHECK(LatticeTop<std::size_t, bit_subset_eq<std::size_t>>::value ==
               ~std::size_t{0});
}

TEST_CASE(
    "order:bitwise-boolean — order-theoretic chain on same carrier stays "
    "Heyting-only (Slice 7 honest rejection)",
    "[order][lattice][bitwise][order][negative]") {
  /** @brief Cross-check: the SAME @c size_t carrier under the
   *         @b order-theoretic reading (@c std::less_equal,
   *         @c std::ranges::min / @c max, @c std::bit_not) does NOT
   *         participate in @c IsBooleanLatticeCategory.  Two distinct
   *         algebraic structures, two distinct lattice witnesses. */
  STATIC_CHECK(IsHeytingLatticeCategory<std::size_t>);
  STATIC_CHECK_FALSE(
      IsBooleanLatticeCategory<
          std::size_t, std::less_equal<std::size_t>, decltype(std::ranges::max),
          decltype(std::ranges::min), std::bit_not<std::size_t>>);
}
