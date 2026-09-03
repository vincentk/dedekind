/** @file dedekind/algebra/congruence_quotient_test.cpp
 *
 * Unit coverage for @c IsCongruenceQuotient<Q, Op> in @c :algebra::quotient
 * (#801) — the @b relational reading of the H leg of Birkhoff's HSP closure.
 * Where the existing @c IsQuotientAlgebra rides a bare
 * @c quotient_algebra_base<Q>::type pointer, @c IsCongruenceQuotient requires
 * the quotient to be witnessed by a @b congruence relation
 * (@c quotient_congruence<Q>::type), an equivalence preserved by @c Op
 * (@c :cartesian's @c IsCongruence).  This is the H-leg analogue of the S-leg's
 * @c IsSubalgebra (see @c subalgebra_test.cpp), closing the asymmetry where S
 * carried a relation but H carried only a type.
 *
 * Coverage targets:
 *  - Structural witness: a carrier declaring both hooks with a genuine
 *    congruence (@c int/(=), the diagonal congruence) satisfies the concept.
 *  - Negative gate (missing congruence): a carrier on the type-declaration
 *    path (@c quotient_algebra_base only, as @c Dual/@c Complex/@c Rational
 * use) honestly rejects — the relational reading requires the congruence.
 *  - Negative gate (not a congruence): a declared relation that is not an
 *    @c IsCongruence honestly rejects.
 *  - Runtime exercise of the congruence relation (codecov).
 */

#include <catch2/catch_test_macros.hpp>
#include <functional>

import dedekind.algebra;
import dedekind.category;

namespace dedekind::category {
namespace _congruence_quotient_witnesses {

/** @brief Carrier declaring @b both hooks: a quotient of @c int by the
 *         diagonal congruence @c (=).  @c std::equal_to is a registered
 *         congruence for @c + and @c × (@c :cartesian), so this is the
 *         canonical positive: the identity quotient @c int/(=). */
struct int_by_equality {};

/** @brief Carrier declaring @b only the trait-propagation base
 *         (@c quotient_algebra_base), no congruence — the type-declaration
 *         path that @c Dual / @c Complex / @c Rational currently use. */
struct int_type_only {};

/** @brief A relation type @b not registered as an equivalence/congruence,
 *         used to exercise the @c IsCongruence gate inside the concept. */
struct bogus_relation {};

/** @brief Carrier declaring a congruence hook that names a non-congruence. */
struct int_by_bogus {};

}  // namespace _congruence_quotient_witnesses

// --- Positive witness: int/(=) declares both hooks with a real congruence. --
template <>
struct quotient_algebra_base<_congruence_quotient_witnesses::int_by_equality> {
  using type = int;
};
template <>
struct quotient_congruence<_congruence_quotient_witnesses::int_by_equality> {
  using type = std::equal_to<int>;
};

// --- Type-declaration path: base only, no congruence declared. --------------
template <>
struct quotient_algebra_base<_congruence_quotient_witnesses::int_type_only> {
  using type = int;
};

// --- Non-congruence relation declared as the congruence hook. ---------------
template <>
struct quotient_algebra_base<_congruence_quotient_witnesses::int_by_bogus> {
  using type = int;
};
template <>
struct quotient_congruence<_congruence_quotient_witnesses::int_by_bogus> {
  using type = _congruence_quotient_witnesses::bogus_relation;
};

}  // namespace dedekind::category

using namespace dedekind::category;

TEST_CASE(
    "algebra:congruence-quotient — int/(=) is a congruence quotient under +, ×",
    "[algebra][quotient][HSP-H][canonical]") {
  /** @brief The identity quotient @c int/(=) is witnessed relationally: the
   *         diagonal congruence @c std::equal_to<int> is a genuine
   *         @c IsCongruence for both @c + and @c ×, so @c IsCongruenceQuotient
   *         fires — the H-leg's Birkhoff justification is now a relation, not
   *         a bare type pointer. */
  STATIC_CHECK(
      IsCongruenceQuotient<_congruence_quotient_witnesses::int_by_equality,
                           std::plus<int>>);
  STATIC_CHECK(
      IsCongruenceQuotient<_congruence_quotient_witnesses::int_by_equality,
                           std::multiplies<int>>);
}

TEST_CASE(
    "algebra:congruence-quotient — negative gate: the type-declaration path "
    "has no relational reading",
    "[algebra][quotient][HSP-H][negative][congruence-gate]") {
  /** @brief A carrier that declares only @c quotient_algebra_base (the
   *         trait-propagation base) but @b not a congruence honestly rejects
   *         @c IsCongruenceQuotient: the relational reading requires the
   *         congruence to be exhibited.  This is exactly the boundary between
   *         the type-declaration path (@c Dual/@c Complex/@c Rational today)
   *         and the relational path. */
  STATIC_CHECK_FALSE(
      IsCongruenceQuotient<_congruence_quotient_witnesses::int_type_only,
                           std::plus<int>>);
}

TEST_CASE(
    "algebra:congruence-quotient — negative gate: a declared non-congruence "
    "honestly rejects",
    "[algebra][quotient][HSP-H][negative][not-a-congruence]") {
  /** @brief Declaring @c quotient_congruence with a relation that is not an
   *         @c IsCongruence (here an unregistered @c bogus_relation, neither an
   *         equivalence nor a registered congruence) is honestly rejected —
   *         the congruence obligation is real, not a formality. */
  STATIC_CHECK_FALSE(
      IsCongruenceQuotient<_congruence_quotient_witnesses::int_by_bogus,
                           std::plus<int>>);
}

TEST_CASE("algebra:congruence-quotient — runtime exercise of the congruence",
          "[algebra][quotient][runtime]") {
  /** @brief Runtime exercise of the diagonal congruence relation that
   *         witnesses the positive case, and a runtime read of the concept
   *         bool — keeps codecov happy and pins that the witness is a real
   *         working relation, not just a type-level marker. */
  std::equal_to<int> cong{};
  CHECK(cong(3, 3));        // reflexive: 3 ~ 3
  CHECK_FALSE(cong(3, 4));  // distinct: 3 ≁ 4

  CHECK(IsCongruenceQuotient<_congruence_quotient_witnesses::int_by_equality,
                             std::plus<int>>);
  CHECK_FALSE(
      IsCongruenceQuotient<_congruence_quotient_witnesses::int_type_only,
                           std::plus<int>>);
}
