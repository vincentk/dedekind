/** @file dedekind/algebra/subalgebra_test.cpp
 *
 * Unit coverage for @c IsSubalgebra<S, A, Op> in @c :algebra::quotient
 * (#718 Slice 3) — the S leg of Birkhoff's HSP closure that completes
 * the triple with the existing H (IsQuotientAlgebra) and P
 * (IsProductAlgebra) legs already in the partition.
 *
 * The ambient is @c unsigned = @c ℤ/2^wℤ, a @b total ambient algebra
 * (@c IsMagma<unsigned, std::plus<unsigned>> holds; wraparound is
 * defined).  @c (int, +) is deliberately @b not used: signed overflow
 * is undefined, so @c !IsMagma<int, std::plus<int>> and @c int is not
 * a bona fide ambient under the project's Honest-Rejection policy
 * (Copilot #802).  The even residues @c {0, 2, …, 2^w−2} are closed
 * under @c + (even + even = even, mod the even modulus @c 2^w), a
 * genuine subalgebra of the additive group.
 *
 * Coverage targets:
 *  - Structural witness: a subobject closed under @c std::plus<unsigned>
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

/** @brief Characteristic predicate of "even" over the total ambient
 *         @c unsigned.  Closed under @c std::plus<unsigned>: even + even
 *         = even (mod the even modulus @c 2^w). */
struct even_chi {
  using Domain = unsigned;
  using Codomain = bool;
  constexpr bool operator()(unsigned x) const noexcept {
    return (x & 1u) == 0u;
  }
};

/** @brief The even-residues subobject of @c unsigned.  Mirrors the
 *         @c :topoi::Subobject<A, Chi> shape with explicit
 *         @c Domain / @c Member / @c ι / @c operator() fields, so
 *         it satisfies @c IsSubobject<even_unsigned, unsigned> directly
 *         without going through the @c Subobject struct (the test
 *         exercises the @c IsSubalgebra concept body, not the
 *         downstream @c Subobject machinery). */
struct even_unsigned {
  using Domain = unsigned;
  struct Member {
    unsigned value;
  };
  even_chi χ;
  constexpr unsigned ι(const Member& m) const noexcept { return m.value; }
  constexpr bool operator()(unsigned a) const noexcept { return χ(a); }
};

/** @brief "Malformed" Op witness: returns a @c std::string from a binary
 *         @c (unsigned, unsigned) call, not a @c V @c × @c V @c → @c V
 *         operation.  Used below to exercise the @c IsSubalgebra concept
 *         body's @c { op(a, a) } @c -> @c std::convertible_to<A> Op-shape
 *         gate: @c std::string has no conversion to @c unsigned. */
struct malformed_op {
  constexpr std::string operator()(unsigned, unsigned) const { return {}; }
};

}  // namespace _subalgebra_witnesses

/** @brief Closure registration: the even residues are closed under
 *         @c std::plus<unsigned>.  Subalgebra of the additive group
 *         @c (ℤ/2^wℤ, +). */
template <>
inline constexpr bool is_closed_under_v<_subalgebra_witnesses::even_unsigned,
                                        unsigned, std::plus<unsigned>> = true;

/** @brief Op-shape-gate test: closure is force-registered for the
 *         malformed Op, so the Op-shape @c requires clause in the
 *         @c IsSubalgebra concept body is the @b only thing
 *         preventing the concept from firing.  A regression that
 *         removes the gate would now fail this static_assert. */
template <>
inline constexpr bool
    is_closed_under_v<_subalgebra_witnesses::even_unsigned, unsigned,
                      _subalgebra_witnesses::malformed_op> = true;

}  // namespace dedekind::category

using namespace dedekind::category;

TEST_CASE("algebra:subalgebra — even residues are a subalgebra of (ℤ/2^wℤ, +)",
          "[algebra][subalgebra][HSP-S][canonical]") {
  /** @brief The even residues @c {0, 2, 4, …} form a subalgebra of the
   *         total ambient @c unsigned @c = @c ℤ/2^wℤ under @c +: the sum
   *         of two even residues is even (Burris-Sank §II.5) — the S leg
   *         of HSP, over a bona fide ambient (unlike @c (int, +), whose
   *         signed overflow is undefined). */
  STATIC_CHECK(IsSubobject<_subalgebra_witnesses::even_unsigned, unsigned>);
  STATIC_CHECK(IsSubalgebra<_subalgebra_witnesses::even_unsigned, unsigned,
                            std::plus<unsigned>>);
}

TEST_CASE(
    "algebra:subalgebra — negative gate: unregistered subobjects honestly "
    "reject",
    "[algebra][subalgebra][HSP-S][negative][closure-gate]") {
  /** @brief A subobject that hasn't opted into @c is_closed_under_v<…,
   *         Op> for a given @c Op honestly rejects @c IsSubalgebra
   *         — the closure obligation must be declared.  Here we use
   *         @c std::multiplies<unsigned> (no closure registration above)
   *         to demonstrate the rejection. */
  STATIC_CHECK_FALSE(IsSubalgebra<_subalgebra_witnesses::even_unsigned,
                                  unsigned, std::multiplies<unsigned>>);
  // (For the record: even residues ARE closed under multiplication too —
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
   *         @c (unsigned, unsigned) call) cannot satisfy
   *         @c { op(a, a) } @c -> @c std::convertible_to<A>.
   *         Closure is force-registered to true above, so the
   *         @c requires clause is the @b only thing preventing the
   *         concept from firing.  A regression that removes the gate
   *         would surface here at compile time. */
  STATIC_CHECK_FALSE(
      IsSubalgebra<_subalgebra_witnesses::even_unsigned, unsigned,
                   _subalgebra_witnesses::malformed_op>);
}

TEST_CASE("algebra:subalgebra — runtime exercise of the even-residues witness",
          "[algebra][subalgebra][runtime]") {
  /** @brief Runtime exercise of the canonical witness's @c χ predicate
   *         and @c ι inclusion arrow — keeps codecov happy and pins
   *         the operational shape (the witness is a real working
   *         subobject, not just a type-level marker). */
  _subalgebra_witnesses::even_unsigned e{};
  CHECK(e(0u));        // 0 is even
  CHECK(e(2u));        // 2 is even
  CHECK(e(4u));        // 4 is even
  CHECK_FALSE(e(1u));  // 1 is odd
  CHECK_FALSE(e(7u));  // 7 is odd

  // Inclusion arrow ι : Member ↣ unsigned.
  _subalgebra_witnesses::even_unsigned::Member m{42u};
  CHECK(e.ι(m) == 42u);
}
