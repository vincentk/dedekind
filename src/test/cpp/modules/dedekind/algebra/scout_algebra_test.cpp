/**
 * @file scout_algebra_test.cpp
 * @brief Witnesses for the @c :algebra:scout_algebra partition (#664 Slice 1).
 *
 * @section scout_algebra_test__Scope
 *
 * Slice 1 establishes the algebraic foundation:
 *  - The partition compiles and exports @c GroupScout / @c operator+ /
 *    @c operator-.
 *  - Honest Rejection: the operator is removed from the candidate set
 *    when the @c IsAdditiveGroup gate fails (e.g.\ machine @c int,
 *    where overflow is UB and the group axioms therefore fail).
 *
 * Concrete positive witnesses (a registered @c IsAdditiveGroup carrier
 * being used end-to-end in a comprehension) land in Slice 2 alongside
 * the @c MembershipBinding routing and Set CTAD work.
 */
#include <catch2/catch_test_macros.hpp>

import dedekind.algebra;
import dedekind.category;
import dedekind.order;
import dedekind.sets;

namespace {

// SFINAE probe: can a `BoundScout<UniversalSet<T>{}>` be combined with a
// `bound<K>` via operator+ / operator-?  Returns @c true iff the
// algebraic gate (@c IsAdditiveGroup<T>) is satisfied.
//
// We use a function template (not a concept-level @c requires) to keep
// the SFINAE-narrow surface: only the operator's requires-clause is
// evaluated, no incidental concept-tree side effects from the call
// site's scope.
template <typename T, auto K>
concept ScoutAdditiveShiftIsCallable =
    requires(dedekind::sets::BoundScout<dedekind::sets::UniversalSet<T>{}> s,
             dedekind::order::Bound<K> b) { s + b; };

template <typename T, auto K>
concept ScoutAdditiveSubtractIsCallable =
    requires(dedekind::sets::BoundScout<dedekind::sets::UniversalSet<T>{}> s,
             dedekind::order::Bound<K> b) { s - b; };

}  // namespace

// ---------------------------------------------------------------------------
// Smoke check: the partition compiles and is reachable from a downstream test.
// ---------------------------------------------------------------------------

TEST_CASE("scout_algebra: partition compiles, exports are reachable (#664)",
          "[algebra][scout_algebra][smoke]") {
  // If the partition didn't compile or didn't export GroupScout /
  // operator+ / operator-, this test file would have failed to build.
  // The presence of this TEST_CASE is the runtime witness; the structural
  // witness is the compilation success itself.
  SUCCEED("scout_algebra partition built and imported successfully.");
}

// ---------------------------------------------------------------------------
// Positive control: operator+ / operator- DO compile when IsAdditiveGroup
// is satisfied (unsigned int → Z/2ⁿZ modular group; cf. category/total.cppm
// remark "unsigned int with addition is a Group").  Without a positive
// witness, the Honest Rejection static_asserts below could pass even if the
// operators were unreachable (e.g., wrong namespace, ADL miss).  The
// positive control proves the operators are wired correctly @b and the
// gate engages on the right axis.
// ---------------------------------------------------------------------------

static_assert(ScoutAdditiveShiftIsCallable<unsigned int, 3u>,
              "operator+(BoundScout<unsigned>, Bound<3u>) must be callable: "
              "unsigned int is an additive group under modular wrap "
              "(Z/2^N Z), so IsAdditiveGroup<unsigned> holds.");

static_assert(ScoutAdditiveSubtractIsCallable<unsigned int, 3u>,
              "operator-(BoundScout<unsigned>, Bound<3u>) must be callable on "
              "an additive group (modular subtraction is well-defined).");

// ---------------------------------------------------------------------------
// Honest Rejection: operator+ / operator- are removed when IsAdditiveGroup
// fails (machine `int`; overflow is UB → no group structure).
// ---------------------------------------------------------------------------

// Machine `int` is intentionally NOT modelled as an additive group in
// :algebra:group (cf. the commented-out static_assert at
// algebra/group.cppm:271 and the rationale in :category:total:275-280).
// Therefore the operator overloads in :algebra:scout_algebra must be
// removed from the candidate set for `BoundScout<int> + Bound<k>` and
// `BoundScout<int> - Bound<k>`.  The compile-time check below witnesses
// the Honest Rejection.
static_assert(!ScoutAdditiveShiftIsCallable<int, 3>,
              "operator+(BoundScout<int>, Bound<k>) must be removed from the "
              "candidate set: machine int is not an additive group "
              "(overflow is UB).");

static_assert(!ScoutAdditiveSubtractIsCallable<int, 3>,
              "operator-(BoundScout<int>, Bound<k>) must be removed from the "
              "candidate set on a carrier that is not an additive group.");

// ---------------------------------------------------------------------------
// Slice 2 Honest Rejection on the comprehension pipe: modular carriers
// (unsigned int as Z/2^N Z) inhabit IsAdditiveGroup but the order is NOT
// translation-invariant (wraparound at the boundary flips order).  The
// halfspace-pivot transport would be semantically wrong, so the pipe
// must reject the comprehension at compile time.
//
// Note that operator+/- DO compile on unsigned (Slice 1's gate is just
// IsAdditiveGroup --- creating a GroupScout doesn't depend on order).
// Only operator| (the halfspace pipe) tightens the gate to
// IsOrderedAdditiveGroup, which excludes modular carriers via the
// is_translation_invariant_ordered marker default of false.
// ---------------------------------------------------------------------------

namespace {

template <typename T, auto K, auto P>
concept ScoutCompositesWithHalfspacePipe =
    requires(dedekind::sets::BoundScout<dedekind::sets::UniversalSet<T>{}> s,
             dedekind::order::Bound<K> k,
             dedekind::order::Bound<P> p) { (s + k) | (s > p); };

}  // namespace

static_assert(
    !ScoutCompositesWithHalfspacePipe<unsigned int, 3u, 5u>,
    "Halfspace pipe must reject modular unsigned: Z/2^N Z's order is not "
    "translation-invariant, so the pivot transport would be wrong.");

// ---------------------------------------------------------------------------
// Slice 2: end-to-end halfspace-pivot transport on the project's ℤ.
//
// The canonical witness uses @c sets::SignedExtensionalCardinal<> --- the
// project's exact ℤ carrier --- which inhabits IsOrderedAdditiveGroup
// (unbounded ordered abelian group; the is_translation_invariant_ordered
// marker is specialised to @c true for this carrier in scout_algebra.cppm).
// ---------------------------------------------------------------------------

TEST_CASE("scout_algebra: in-line shift transports halfspace pivot on ℤ (#664)",
          "[algebra][scout_algebra][comprehension][slice2]") {
  using Z = dedekind::sets::SignedExtensionalCardinal<>;

  constexpr auto x = dedekind::sets::element<dedekind::sets::Ω<Z>>;
  constexpr auto S = dedekind::sets::Set{x + dedekind::order::bound<3> |
                                         (x > dedekind::order::bound<5>)};

  // The result IS `Set<Z, TernaryLogic, Halfspace<Z, 8, Upward, Strict>>`:
  // the source halfspace (pivot 5) is transported through the +3 shift
  // to land at pivot 8, direction and strictness preserved.  Pivot
  // arithmetic happens in the structural NTTP type (`int`), not in `Z`
  // (which may not itself be NTTP-usable as a structural type).
  // The Set's logic is `TernaryLogic` because `NaturalLogic<Ω<Z>>`
  // routes through transfinite cardinality.  Type-directed collapse
  // at compile time.
  using SetL = typename dedekind::sets::NaturalLogic<
      dedekind::sets::UniversalSet<Z>>::type;
  using ExpectedPredicate =
      dedekind::order::Halfspace<Z, 8, dedekind::order::Direction::Upward,
                                 dedekind::order::Strictness::Strict>;
  using ExpectedSet = dedekind::sets::Set<Z, SetL, ExpectedPredicate>;
  using ResultType = std::remove_cvref_t<decltype(S)>;
  STATIC_CHECK(std::same_as<ResultType, ExpectedSet>);
}
