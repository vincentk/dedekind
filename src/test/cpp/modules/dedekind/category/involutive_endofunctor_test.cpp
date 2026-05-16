/** @file dedekind/category/involutive_endofunctor_test.cpp
 *
 * Unit coverage for @c :category::lattice::IsInvolutiveEndofunctor
 * (#698 Slice 5).
 *
 * An involutive endofunctor on @c T is a callable @c F @c : @c T @c → @c T
 * with @c F² @c ≅ @c Id.  Canonical witnesses:
 *
 *   - @c std::logical_not<bool> on @c bool ( @c !!x @c = @c x ).
 *   - @c std::bit_not<T> on integral @c T ( @c ~~x @c = @c x ).
 *
 * Used by @c IsBooleanLatticeCategory (#698 Slice 7) with @c F the
 * lattice complement.
 *
 * Sollbruchstelle: lives inline in @c :lattice per #698 Q2; extraction
 * to @c :involution or @c :monad deferred until a second consumer
 * arrives.
 */

#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <functional>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE("category:lattice — std::logical_not<bool> is the Boolean involution",
          "[category][lattice][involution][bool]") {
  /** @brief The element-level complement on @c bool: @c !!x @c = @c x
   *         for all @c x @c ∈ @c {false, @c true}.  This is the
   *         canonical 2-element Boolean involution. */
  STATIC_CHECK(IsInvolutiveEndofunctor<std::logical_not<bool>, bool>);
  STATIC_CHECK(is_involutive_v<std::logical_not<bool>, bool>);

  /** @brief Runtime witnesses — @c !!x @c = @c x for both values. */
  constexpr std::logical_not<bool> not_op{};
  STATIC_CHECK(not_op(not_op(false)) == false);
  STATIC_CHECK(not_op(not_op(true)) == true);
}

TEST_CASE("category:lattice — std::bit_not is involutive on integral carriers",
          "[category][lattice][involution][bitwise]") {
  /** @brief Bitwise complement: @c ~~x @c = @c x for all integral
   *         @c T.  This is the involution on the bitwise Boolean
   *         algebra on @c size_t / @c int / @c unsigned. */
  STATIC_CHECK(IsInvolutiveEndofunctor<std::bit_not<int>, int>);
  STATIC_CHECK(IsInvolutiveEndofunctor<std::bit_not<unsigned>, unsigned>);
  STATIC_CHECK(IsInvolutiveEndofunctor<std::bit_not<std::size_t>, std::size_t>);

  /** @brief Runtime witnesses. */
  constexpr std::bit_not<int> bit_not_int{};
  STATIC_CHECK(bit_not_int(bit_not_int(42)) == 42);
  STATIC_CHECK(bit_not_int(bit_not_int(-1)) == -1);
  STATIC_CHECK(bit_not_int(bit_not_int(0)) == 0);
}

TEST_CASE(
    "category:lattice — opt-in registration is required (no default true)",
    "[category][lattice][involution][negative][opt-in]") {
  /** @brief Sanity: the @c is_involutive trait is @b opt-in.  Absence
   *         of an explicit specialisation means @c is_involutive_v is
   *         @c false_type by default, and @c IsInvolutiveEndofunctor
   *         fails closed.
   *
   *  @note @c std::negate<unsigned> @b is actually involutive (unsigned
   *  arithmetic is modulo, so @c -(-x) @c == @c x), but no
   *  specialisation has been registered for it — this test demonstrates
   *  the opt-in pattern, not non-involutiveness.  A genuinely
   *  non-involutive callable would also fail, but for a different
   *  structural reason. */
  STATIC_CHECK_FALSE(IsInvolutiveEndofunctor<std::negate<unsigned>, unsigned>);
  STATIC_CHECK_FALSE(is_involutive_v<std::negate<unsigned>, unsigned>);
}
