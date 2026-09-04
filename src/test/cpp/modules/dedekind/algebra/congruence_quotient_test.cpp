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
 *  - Negative gate (no projection): a genuine congruence on a non-reducing
 *    empty marker rejects — the concept ties @c Q to @c V/R via the canonical
 *    projection @f$V\to Q@f$ (Copilot #803), so a free-floating congruence is
 *    not enough.
 *  - Runtime exercise of the congruence relation (codecov).
 */

#include <catch2/catch_test_macros.hpp>
#include <functional>

import dedekind.algebra;
import dedekind.category;

namespace dedekind::category {
namespace _congruence_quotient_witnesses {

/** @brief Positive: a quotient of @c int by the diagonal congruence @c (=),
 *         @b reducing from @c int (the projection is the identity).
 *         @c std::equal_to is a registered congruence for @c + and @c ×
 *         (@c :cartesian), so this is the canonical identity quotient
 *         @c int/(=). */
struct int_by_equality {
  int value;
  constexpr explicit int_by_equality(int v) : value(v) {}
};

/** @brief Negative (no congruence): declares @b only the trait-propagation
 *         base (@c quotient_algebra_base), no congruence — the type-declaration
 *         path that @c Dual / @c Complex / @c Rational currently use. */
struct int_type_only {};

/** @brief A relation type @b not registered as an equivalence/congruence,
 *         used to exercise the @c IsCongruence gate inside the concept. */
struct bogus_relation {};

/** @brief Negative (not a congruence): reduces from @c int but names a
 *         non-congruence relation — isolates the @c IsCongruence gate. */
struct int_by_bogus {
  int value;
  constexpr explicit int_by_bogus(int v) : value(v) {}
};

/** @brief Negative (no projection): declares a @b genuine congruence but is an
 *         empty marker @b not constructible from its carrier @c int, so it is
 *         not tied to @c V/R.  This is the Copilot #803 case: a free-floating
 *         congruence on an unrelated type must be rejected by clause (2). */
struct int_marker_no_proj {};

}  // namespace _congruence_quotient_witnesses

// --- Positive witness: int/(=) declares the congruence (relation + carrier). -
template <>
struct quotient_congruence<_congruence_quotient_witnesses::int_by_equality> {
  using type = std::equal_to<int>;
  using carrier = int;
};

// --- Type-declaration path: a trait-propagation base but no congruence. ------
template <>
struct quotient_algebra_base<_congruence_quotient_witnesses::int_type_only> {
  using type = int;
};

// --- Non-congruence relation declared as the congruence. --------------------
template <>
struct quotient_congruence<_congruence_quotient_witnesses::int_by_bogus> {
  using type = _congruence_quotient_witnesses::bogus_relation;
  using carrier = int;
};

// --- Genuine congruence, but the carrier is a non-reducing empty marker. -----
template <>
struct quotient_congruence<_congruence_quotient_witnesses::int_marker_no_proj> {
  using type = std::equal_to<int>;
  using carrier = int;
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

TEST_CASE(
    "algebra:congruence-quotient — negative gate: a genuine congruence on a "
    "non-reducing marker is not tied to V/R",
    "[algebra][quotient][HSP-H][negative][projection-gate]") {
  /** @brief The Copilot #803 case: @c int_marker_no_proj declares a @b genuine
   *         congruence (@c int/(=)) but is an empty type not constructible from
   *         its carrier @c int, so the canonical projection @f$V\to Q@f$ is
   *         absent and the concept honestly rejects.  This pins clause (2): a
   *         free-floating congruence on an unrelated type is not a congruence
   *         quotient. */
  STATIC_CHECK_FALSE(
      IsCongruenceQuotient<_congruence_quotient_witnesses::int_marker_no_proj,
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
