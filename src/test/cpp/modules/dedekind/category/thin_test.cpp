/** @file dedekind/category/thin_test.cpp
 *
 * Unit coverage for @c :category::thin (the lattice-Form-chain row 1, #698).
 *
 * @c IsThinCategory<T, Rel, L> reifies "category whose hom-sets have at most
 * one morphism" as a preorder over @c T (reflexive + transitive, no
 * antisymmetry).  Witnesses below pin the canonical entry points and the
 * relationship to @c :posetal (the row-2 refinement).
 */

#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <functional>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE("category:thin — Bool is the canonical 2-element thin category",
          "[category][thin][bool][canonical]") {
  /** @brief @c (false @c ≤ @c true) under @c std::less_equal is the
   *         smallest non-trivial thin category — and also the canonical
   *         Boolean algebra / subobject classifier in Set. */
  STATIC_CHECK(IsThinCategory<bool>);
  STATIC_CHECK(IsThinCategory<bool, std::less_equal<bool>, ClassicalLogic>);
}

TEST_CASE("category:thin — integral carriers are thin",
          "[category][thin][numeric]") {
  /** @brief Standard integral carriers form thin categories under the
   *         usual order — they are also totally ordered, but @c
   *         IsThinCategory pins only the preorder content (no total-order
   *         requirement).
   *
   *  @note Floating-point carriers (@c double, @c float) intentionally
   *  do @b not satisfy @c IsThinCategory under @c std::less_equal: the
   *  @c is_transitive_v trait in @c :species is specialised only for
   *  integral and bool carriers, because @c NaN-tainted IEEE 754
   *  comparisons break transitivity (@c NaN @c <= x is always false).
   *  Honest rejection — see @c numbers::approx / @c :ieee for the
   *  partial-arithmetic surface that handles this. */
  STATIC_CHECK(IsThinCategory<int>);
  STATIC_CHECK(IsThinCategory<unsigned>);
  STATIC_CHECK(IsThinCategory<std::size_t>);
}

TEST_CASE("category:thin — IsPosetal strictly refines IsThinCategory",
          "[category][thin][posetal][faithful]") {
  /** @brief Faithful inclusion: every @c IsPosetal carrier is @c
   *         IsThinCategory (poset = thin + antisymmetric).  The inclusion
   *         is *currently* implicit because @c IsPosetal builds on @c
   *         IsPartialOrder which already enforces reflexivity +
   *         transitivity; the explicit signature-level encoding lands in
   *         the @c :posetal refactor slice (#698 Slice 1). */
  STATIC_CHECK(IsThinCategory<bool>);
  STATIC_CHECK(IsPosetal<bool>);

  STATIC_CHECK(IsThinCategory<int>);
  STATIC_CHECK(IsPosetal<int>);
}
