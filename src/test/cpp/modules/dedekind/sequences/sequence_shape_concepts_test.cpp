/** @file dedekind/sequences/sequence_shape_concepts_test.cpp
 *
 * Unit coverage for the sequence-shape concepts (#719 Slice 1):
 * @c IsBoundedSequence, @c IsMonotoneSequence, @c IsPeriodicSequence,
 * @c IsAbsorptiveSequence, @c IsSubsequence.
 *
 * Each property is a property of the @b value (which sequence), not
 * the carrier type, so the concepts gate on opt-in traits a tagged
 * carrier registers when it guarantees the property.  The toy tagged
 * paths below derive from @c Path<double> (inheriting the
 * @c IsSequence shape) and opt into the relevant trait.
 *
 * Coverage:
 *  - Each concept fires on its tagged witness.
 *  - Each concept honestly rejects a plain @c Path<double> (no opt-in).
 *  - @c IsSubsequence fires on a (sub, super) tagged pair; rejects a
 *    cross-Codomain pair.
 */

#include <catch2/catch_test_macros.hpp>

import dedekind.sequences;

using namespace dedekind::sequences;

namespace seq_shape_witnesses {

/** @brief Tagged toy paths — derive from Path<double> to inherit the
 *         IsSequence shape; the opt-in trait registrations below mark
 *         each as guaranteeing its shape property. */
struct bounded_path : Path<double> {
  using Path<double>::Path;
};
struct monotone_path : Path<double> {
  using Path<double>::Path;
};
struct periodic_path : Path<double> {
  using Path<double>::Path;
};
struct absorptive_path : Path<double> {
  using Path<double>::Path;
};
struct sub_path : Path<double> {
  using Path<double>::Path;
};
struct super_path : Path<double> {
  using Path<double>::Path;
};
struct int_path : Path<int> {
  using Path<int>::Path;
};

}  // namespace seq_shape_witnesses

namespace dedekind::sequences {
template <>
inline constexpr bool is_bounded_sequence_v<seq_shape_witnesses::bounded_path> =
    true;
template <>
inline constexpr bool
    is_monotone_sequence_v<seq_shape_witnesses::monotone_path> = true;
template <>
inline constexpr bool
    is_periodic_sequence_v<seq_shape_witnesses::periodic_path, 3> = true;
template <>
inline constexpr bool
    is_absorptive_sequence_v<seq_shape_witnesses::absorptive_path> = true;
template <>
inline constexpr bool is_subsequence_v<seq_shape_witnesses::sub_path,
                                       seq_shape_witnesses::super_path> = true;
}  // namespace dedekind::sequences

TEST_CASE("sequences:shape — IsBoundedSequence fires on the tagged witness",
          "[sequences][shape][bounded]") {
  STATIC_CHECK(IsBoundedSequence<seq_shape_witnesses::bounded_path>);
  STATIC_CHECK_FALSE(IsBoundedSequence<Path<double>>);
}

TEST_CASE("sequences:shape — IsMonotoneSequence fires on the tagged witness",
          "[sequences][shape][monotone]") {
  STATIC_CHECK(IsMonotoneSequence<seq_shape_witnesses::monotone_path>);
  STATIC_CHECK_FALSE(IsMonotoneSequence<Path<double>>);
}

TEST_CASE(
    "sequences:shape — IsPeriodicSequence<S, N> fires on the period-3 witness",
    "[sequences][shape][periodic]") {
  /** @brief Period-3 tagged path.  Index-side compression: factors
   *         through ℕ/3ℤ.  The period N is part of the concept's
   *         type-level signature. */
  STATIC_CHECK(IsPeriodicSequence<seq_shape_witnesses::periodic_path, 3>);
  // Wrong period N honestly rejects (no opt-in for N=2):
  STATIC_CHECK_FALSE(IsPeriodicSequence<seq_shape_witnesses::periodic_path, 2>);
  STATIC_CHECK_FALSE(IsPeriodicSequence<Path<double>, 3>);
}

TEST_CASE(
    "sequences:shape — IsAbsorptiveSequence fires on the eventually-constant "
    "witness",
    "[sequences][shape][absorptive]") {
  /** @brief Codomain-side compression: eventually constant; the dual
   *         of periodic. */
  STATIC_CHECK(IsAbsorptiveSequence<seq_shape_witnesses::absorptive_path>);
  STATIC_CHECK_FALSE(IsAbsorptiveSequence<Path<double>>);
}

TEST_CASE("sequences:shape — IsSubsequence fires on the (sub, super) pair",
          "[sequences][shape][subsequence]") {
  STATIC_CHECK(IsSubsequence<seq_shape_witnesses::sub_path,
                             seq_shape_witnesses::super_path>);
  // Cross-Codomain pair honestly rejects (double vs int):
  STATIC_CHECK_FALSE(IsSubsequence<seq_shape_witnesses::sub_path,
                                   seq_shape_witnesses::int_path>);
  // No opt-in ⇒ honest reject even with matching Codomain:
  STATIC_CHECK_FALSE(IsSubsequence<Path<double>, Path<double>>);
}
