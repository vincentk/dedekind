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
// `ℚ | (π > fix(5_c))`, not a hand-built UpRay<Rational, ...>: this way the
// witness fails if `ℚ | pred` stops binding to the Rational carrier or its
// Boole logic (the actual surface under test), per #927 review.
using QHalfspace =
    std::remove_cvref_t<decltype(ℚ | (dedekind::sets::π > fix(5_c)))>;
// The public expression really does bind the ℚ carrier and the Above cut
// (value-carrying, so the pivot 5 rides in the instance, not the type).
static_assert(
    std::same_as<QHalfspace,
                 dedekind::order::UpRay<Rational<default_integer>,
                                        dedekind::order::Strictness::Strict,
                                        dedekind::category::Boole>>,
    "ℚ | (π > fix(5_c)) binds to the Above halfspace over Rational.");
// Carrier-axis magnitude is countable ℵ_0, matching the ambient's own C.
static_assert(
    std::same_as<typename QHalfspace::cardinality_type, dedekind::sets::ℵ_0>,
    "ℚ halfspace inherits the countable ℵ_0 the carrier self-declares.");
// The cut answers in the ambient's species, Boole: decidable membership.
static_assert(
    std::same_as<typename QHalfspace::logic_species,
                 typename std::remove_cvref_t<decltype(ℚ)>::logic_species>,
    "point-free ℚ halfspace answers in the species of the ambient ℚ.");
static_assert(dedekind::sets::HasDecidableMembership<QHalfspace>,
              "a ℚ cut is decidable (Boole).");
}  // namespace
