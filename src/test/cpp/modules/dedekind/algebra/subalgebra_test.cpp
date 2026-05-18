/** @file dedekind/algebra/subalgebra_test.cpp
 *
 * Unit coverage for @c IsSubalgebra<S, A, Op> in @c :algebra::quotient
 * (#718 Slice 3) — the S leg of Birkhoff's HSP closure that completes
 * the triple with the existing H (IsQuotientAlgebra) and P
 * (IsProductAlgebra) legs already in the partition.
 *
 * Coverage targets:
 *  - Structural witness: a subobject closed under @c std::plus<int>
 *    satisfies @c IsSubalgebra<...>.
 *  - Negative gate (closure): a subobject @b not registered as closed
 *    under @c Op honestly rejects.
 *  - Negative gate (Op shape): an operation that doesn't have the
 *    @c V @c × @c V @c → @c V signature fails the requires clause.
 *  - Runtime exercise of the witness's @c χ and @c ι (codecov).
 */

#include <catch2/catch_test_macros.hpp>
#include <functional>
#include <string>

import dedekind.algebra;
import dedekind.category;

namespace dedekind::category {
namespace _subalgebra_witnesses {

/** @brief Characteristic predicate of "even integer".  Closed under
 *         @c std::plus<int>: even + even = even (textbook). */
struct even_chi {
  using Domain = int;
  using Codomain = bool;
  constexpr bool operator()(int x) const noexcept { return (x & 1) == 0; }
};

/** @brief The even-integers subobject of @c int.  Mirrors the
 *         @c :topoi::Subobject<A, Chi> shape with explicit
 *         @c Ambient / @c Member / @c ι / @c operator() fields, so
 *         it satisfies @c IsSubobject<EvenInts, int> directly
 *         without going through the @c Subobject struct (the test
 *         exercises the @c IsSubalgebra concept body, not the
 *         downstream @c Subobject machinery). */
struct even_ints {
  using Ambient = int;
  struct Member {
    int value;
  };
  even_chi χ;
  constexpr int ι(const Member& m) const noexcept { return m.value; }
  constexpr bool operator()(int a) const noexcept { return χ(a); }
};

/** @brief "Malformed" Op witness: takes a @c double, not a binary
 *         @c int × @c int operation.  Used below to exercise the
 *         @c IsSubalgebra concept body's @c { op(a, a) } @c
 *         @c -> @c std::convertible_to<A> Op-shape gate.
 *         @c std::convertible_to<int> still admits @c double via
 *         narrowing conversion, so we deliberately return a
 *         @c std::string — a type that has no conversion to @c int. */
struct malformed_op {
  constexpr std::string operator()(int, int) const { return {}; }
};

}  // namespace _subalgebra_witnesses

/** @brief Closure registration: the even integers are closed under
 *         @c std::plus<int>.  Textbook subalgebra of @c (ℤ, +). */
template <>
inline constexpr bool
    is_closed_under_v<_subalgebra_witnesses::even_ints, int, std::plus<int>> =
        true;

/** @brief Op-shape-gate test: closure is force-registered for the
 *         malformed Op, so the Op-shape @c requires clause in the
 *         @c IsSubalgebra concept body is the @b only thing
 *         preventing the concept from firing.  A regression that
 *         removes the gate would now fail this static_assert. */
template <>
inline constexpr bool is_closed_under_v<_subalgebra_witnesses::even_ints, int,
                                        _subalgebra_witnesses::malformed_op> =
    true;

}  // namespace dedekind::category

using namespace dedekind::category;

TEST_CASE("algebra:subalgebra — even integers are a subalgebra of (ℤ, +)",
          "[algebra][subalgebra][HSP-S][canonical]") {
  /** @brief Even integers @c {…, -2, 0, 2, 4, …} form a subalgebra of
   *         @c ℤ under @c +: the sum of two even integers is even.
   *         This is the canonical textbook subalgebra (Burris-Sank
   *         §II.5) — the S leg of HSP. */
  STATIC_CHECK(IsSubobject<_subalgebra_witnesses::even_ints, int>);
  STATIC_CHECK(
      IsSubalgebra<_subalgebra_witnesses::even_ints, int, std::plus<int>>);
}

TEST_CASE(
    "algebra:subalgebra — negative gate: unregistered subobjects honestly "
    "reject",
    "[algebra][subalgebra][HSP-S][negative][closure-gate]") {
  /** @brief A subobject that hasn't opted into @c is_closed_under_v<…,
   *         Op> for a given @c Op honestly rejects @c IsSubalgebra
   *         — the closure obligation must be declared.  Here we use
   *         @c std::multiplies<int> (no closure registration above)
   *         to demonstrate the rejection. */
  STATIC_CHECK_FALSE(IsSubalgebra<_subalgebra_witnesses::even_ints, int,
                                  std::multiplies<int>>);
  // (For the record: even integers ARE closed under multiplication too —
  // even * anything = even.  But this test demonstrates that without an
  // explicit opt-in registration the concept honestly rejects, which is
  // the project's Honest-Rejection discipline.)
}

TEST_CASE(
    "algebra:subalgebra — negative gate: Op-shape requires clause rejects "
    "malformed operations even when closure is registered",
    "[algebra][subalgebra][HSP-S][negative][op-shape-gate]") {
  /** @brief Op-shape gate is the second negative cover: a
   *         @c malformed_op (returns @c std::string from a binary
   *         @c (int, int) call) cannot satisfy
   *         @c { op(a, a) } @c -> @c std::convertible_to<A>.
   *         Closure is force-registered to true above, so the
   *         @c requires clause is the @b only thing preventing the
   *         concept from firing.  A regression that removes the gate
   *         would surface here at compile time. */
  STATIC_CHECK_FALSE(IsSubalgebra<_subalgebra_witnesses::even_ints, int,
                                  _subalgebra_witnesses::malformed_op>);
}

TEST_CASE("algebra:subalgebra — runtime exercise of the even-ints witness",
          "[algebra][subalgebra][runtime]") {
  /** @brief Runtime exercise of the canonical witness's @c χ predicate
   *         and @c ι inclusion arrow — keeps codecov happy and pins
   *         the operational shape (the witness is a real working
   *         subobject, not just a type-level marker). */
  _subalgebra_witnesses::even_ints e{};
  CHECK(e(0));        // 0 is even
  CHECK(e(2));        // 2 is even
  CHECK(e(-4));       // -4 is even
  CHECK_FALSE(e(1));  // 1 is odd
  CHECK_FALSE(e(7));  // 7 is odd

  // Inclusion arrow ι : Member ↣ int.
  _subalgebra_witnesses::even_ints::Member m{42};
  CHECK(e.ι(m) == 42);
}
