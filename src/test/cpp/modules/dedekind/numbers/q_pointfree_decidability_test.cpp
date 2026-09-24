/**
 * @file q_pointfree_decidability_test.cpp
 * @brief Point-free decidability-parity witness for ℚ (#848/#927).
 *
 * @section q_pointfree_decidability_test__Scope
 *
 * Extracted from the retired @c q_scout_algebra_test when the test-only
 * symbolic scout-algebra layer (GroupScout / affine @c element<ℚ> @c +
 * @c bound<k> factories) was sunset under #895.  The GroupScout
 * multiplicative/affine TEST_CASEs went with that layer; this
 * point-free block --- which never depended on GroupScout --- stays.
 *
 * The claim: a point-free ℚ halfspace @c ℚ @c | @c (π @c > @c fix(5_c))
 * classifies IDENTICALLY to the ambient ℚ.  ℚ is countable, so the cut
 * is decidable (Boole), not Kleene.  The @c Halfspace lives in @c :order,
 * which is upstream of @c :numbers and cannot see that @c Rational is
 * countable structurally (@c IsRingIntegral<Rational> is false);
 * @c Rational self-declares its @c cardinality_type @c = @c ℵ_0, which
 * @c order::carrier_cardinality reads.  Before the fix the point-free
 * path fell to the @c IsRingIntegral fallback (ℶ_1 → Kleene),
 * disagreeing with the ℵ_0/Boole verdict.
 */
#include <concepts>     // std::same_as
#include <type_traits>  // std::remove_cvref_t

import dedekind.category;
import dedekind.numbers;
import dedekind.order;
import dedekind.sets;

using namespace dedekind::numbers;

namespace {
using dedekind::order::fix;
using dedekind::order::operator""_c;
// Derive the halfspace type from the PUBLIC point-free expression
// `ℚ | (π > fix(5_c))`, not a hand-built Halfspace<Rational>: this way the
// witness fails if `ℚ | pred` stops binding to the Rational carrier or its
// Boole logic (the actual surface under test), per #927 review.
using QHalfspace =
    std::remove_cvref_t<decltype(ℚ | (dedekind::sets::π > fix(5_c)))>;
// The public expression really does bind the ℚ carrier and the Above<5> cut.
static_assert(
    std::same_as<
        QHalfspace,
        dedekind::order::Halfspace<
            Rational<default_integer>, 5, dedekind::order::Direction::Upward,
            dedekind::order::Strictness::Strict, dedekind::category::Boole>>,
    "ℚ | (π > fix(5_c)) binds to the Above<5> halfspace over Rational.");
// Carrier-axis magnitude is countable ℵ_0, matching the ambient's own C.
static_assert(
    std::same_as<typename QHalfspace::cardinality_type, dedekind::sets::ℵ_0>,
    "ℚ halfspace inherits the countable ℵ_0 the carrier self-declares.");
// The NaturalLogic verdict therefore matches the ambient ℚ: decidable Boole.
static_assert(
    std::same_as<typename dedekind::sets::NaturalLogic<QHalfspace>::type,
                 typename dedekind::sets::NaturalLogic<
                     std::remove_cvref_t<decltype(ℚ)>>::type>,
    "point-free ℚ halfspace classifies as the ambient ℚ does (parity).");
static_assert(
    std::same_as<typename dedekind::sets::NaturalLogic<QHalfspace>::type,
                 dedekind::category::Boole>,
    "a countable ℚ cut is decidable (Boole), not Kleene.");
}  // namespace
