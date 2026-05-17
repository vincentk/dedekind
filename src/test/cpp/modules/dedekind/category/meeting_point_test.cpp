/** @file dedekind/category/meeting_point_test.cpp
 *
 * #698 Slice 10 — the meeting-point test.
 *
 * The closing slice for the lattice Form-chain (#698): end-to-end
 * witnesses that the Slice-0-through-9 machinery fires consistently
 * across three structurally distinct canonical carriers:
 *
 *   - @c bool — primitive, smallest non-trivial Boolean topos.  Sub(1) ≅
 *     Bool.  Fires all Form-chain rows 1–7 plus the L-parametric
 *     Boolean refinement.
 *   - @c size_t — primitive, large finite carrier.  Two lattice
 *     structures: order-theoretic (@c min / @c max) and bitwise
 *     (@c & / @c |).  Fires rows 1–6 under the order-theoretic reading;
 *     row 7 is honestly rejected under @c std::less_equal because the
 *     bitwise complement doesn't match the order (#710 tracks the
 *     bitwise route as a follow-up canonical witness).
 *   - @c std::ranges::iota_view — @b non-primitive, structurally novel.
 *     Per #703, iota_view is simultaneously a carrier, a subobject of
 *     the ambient integral type, and a halfspace; the project-side
 *     iota_view-as-Form-chain-carrier reading is deferred to its own
 *     slice (carrier shape is genuinely different).  This file
 *     witnesses what's tractable for Slice 10: iota_view's element
 *     type (integral) participates in the Form-chain via @c std::ranges
 *     niebloids, and value-level lattice ops on iota_views fire via
 *     @c std::ranges::set_intersection / @c set_union.
 *
 * The meeting-point structure: each canonical carrier listed against
 * each row.  Where a carrier doesn't fire a row, @c STATIC_CHECK_FALSE
 * is used for Honest Rejection — same machinery, different participation.
 */

#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <cstddef>
#include <functional>
#include <ranges>

import dedekind.category;

using namespace dedekind::category;

// ---------------------------------------------------------------------------
// Canonical carrier 1: bool — the smallest non-trivial Boolean topos.
// ---------------------------------------------------------------------------

TEST_CASE("category:meeting-point — bool fires Form-chain rows 1–7",
          "[category][lattice][meeting-point][bool]") {
  /** @brief @c bool is the smallest non-trivial topos: @c Sub(1) ≅
   *         @c Bool, classical Ω, two-element Boolean lattice. */
  STATIC_CHECK(IsThinCategory<bool>);            // row 1
  STATIC_CHECK(IsPosetal<bool>);                 // row 2
  STATIC_CHECK(IsFilteredCategory<bool>);        // row 3
  STATIC_CHECK(IsLatticeCategory<bool>);         // row 4
  STATIC_CHECK(IsBoundedLatticeCategory<bool>);  // row 5
  STATIC_CHECK(IsHeytingLatticeCategory<bool>);  // row 6
  STATIC_CHECK(IsBooleanLatticeCategory<bool>);  // row 7
}

// ---------------------------------------------------------------------------
// Canonical carrier 2: size_t — large finite carrier, totally ordered.
// ---------------------------------------------------------------------------

TEST_CASE(
    "category:meeting-point — size_t fires rows 1–6, rejects row 7 honestly",
    "[category][lattice][meeting-point][size_t][honest-rejection]") {
  /** @brief @c size_t under @c std::less_equal is a totally-ordered
   *         Heyting algebra (a chain).  Rows 1–6 fire.  Row 7 (Boolean)
   *         requires a complement matching the order; @c std::bit_not
   *         is the natural complement but doesn't match @c min / @c max
   *         (e.g.\ @c min(5, ~5) @c ≠ @c numeric_limits::min).  Honest
   *         Rejection via @c is_complement_v opt-in; the bitwise
   *         Boolean lattice under bit-subset @c Rel is tracked under
   *         #710 as a separate canonical witness. */
  STATIC_CHECK(IsThinCategory<std::size_t>);
  STATIC_CHECK(IsPosetal<std::size_t>);
  STATIC_CHECK(IsFilteredCategory<std::size_t>);
  STATIC_CHECK(IsLatticeCategory<std::size_t>);
  STATIC_CHECK(IsBoundedLatticeCategory<std::size_t>);
  STATIC_CHECK(IsHeytingLatticeCategory<std::size_t>);

  // Row 7 honest rejection under (std::less_equal + std::bit_not).
  STATIC_CHECK_FALSE(
      IsBooleanLatticeCategory<
          std::size_t, std::less_equal<std::size_t>, decltype(std::ranges::max),
          decltype(std::ranges::min), std::bit_not<std::size_t>>);
}

// ---------------------------------------------------------------------------
// Canonical carrier 3: std::ranges::iota_view — non-primitive, structurally
// novel (per #703).
// ---------------------------------------------------------------------------

TEST_CASE(
    "category:meeting-point — iota_view's element type fires Form-chain via "
    "std::ranges niebloids",
    "[category][lattice][meeting-point][std-ranges][iota-view]") {
  /** @brief @c std::ranges::iota_view<int, int> is a non-primitive
   *         carrier — finite, parametrised, range-typed.  Per #703 the
   *         iota_view-as-Form-chain-carrier reading needs its own slice
   *         (carrier shape differs from @c bool / @c size_t).  What's
   *         tractable here in Slice 10: the iota_view's element type
   *         (@c int / integral) participates in the Form-chain through
   *         the canonical @c std::ranges niebloid defaults
   *         (@c std::ranges::max / @c std::ranges::min).  This pins the
   *         element-level meeting-point. */
  using ElementType =
      std::ranges::range_value_t<std::ranges::iota_view<int, int>>;
  static_assert(std::same_as<ElementType, int>);

  STATIC_CHECK(IsThinCategory<int>);
  STATIC_CHECK(IsPosetal<int>);
  STATIC_CHECK(IsFilteredCategory<int>);
  STATIC_CHECK(IsLatticeCategory<int>);
  STATIC_CHECK(IsBoundedLatticeCategory<int>);
  STATIC_CHECK(IsHeytingLatticeCategory<int>);

  /** @brief Niebloid identity: the Form-chain @c Meet / @c Join slots
   *         default to @c decltype(std::ranges::min) and
   *         @c decltype(std::ranges::max) — i.e.\ the std::ranges
   *         niebloids ARE the Form-chain's canonical lattice ops. */
  STATIC_CHECK(
      IsLatticeCategory<int, std::less_equal<int>, decltype(std::ranges::max),
                        decltype(std::ranges::min)>);
}

TEST_CASE(
    "category:meeting-point — iota_view value-level lattice ops compute "
    "correctly",
    "[category][lattice][meeting-point][std-ranges][iota-view][runtime]") {
  /** @brief Runtime witness: @c std::ranges::iota_view supports lattice
   *         operations on its elements via @c std::ranges algorithms.
   *         @c iota_view<0, 5> ∩ @c iota_view<3, 8> @c = @c [3, 5)
   *         element-wise (set_intersection).
   *         @c iota_view<0, 5> ∪ @c iota_view<3, 8> @c = @c [0, 8)
   *         element-wise (set_union).  These ARE the canonical set-
   *         algebra lattice ops on the carrier's element-level subobject
   *         lattice.  Per #703 the type-level lifting (iota_view's
   *         family-typed Form-chain participation) is the deferred
   *         work. */
  constexpr auto left = std::views::iota(0, 5);
  constexpr auto right = std::views::iota(3, 8);

  // Element-wise meet via std::ranges::set_intersection.
  std::vector<int> meet_result;
  std::ranges::set_intersection(left, right, std::back_inserter(meet_result));
  CHECK(meet_result == std::vector<int>{3, 4});

  // Element-wise join via std::ranges::set_union.
  std::vector<int> join_result;
  std::ranges::set_union(left, right, std::back_inserter(join_result));
  CHECK(join_result == std::vector<int>{0, 1, 2, 3, 4, 5, 6, 7});
}

// ---------------------------------------------------------------------------
// Meeting-point matrix: the three carriers side-by-side at each row.
// ---------------------------------------------------------------------------

TEST_CASE(
    "category:meeting-point — three-carrier matrix at every Form-chain row",
    "[category][lattice][meeting-point][matrix][summary]") {
  /** @brief Summary matrix — bool, size_t, and (iota_view's element
   *         type) int side-by-side at each Form-chain row.  Shows the
   *         abstraction is genuinely L-parametric and carrier-uniform
   *         across the three structurally distinct readings. */

  // Row 1 — thin (preorder).
  STATIC_CHECK(IsThinCategory<bool>);
  STATIC_CHECK(IsThinCategory<std::size_t>);
  STATIC_CHECK(IsThinCategory<int>);

  // Row 2 — posetal (poset).
  STATIC_CHECK(IsPosetal<bool>);
  STATIC_CHECK(IsPosetal<std::size_t>);
  STATIC_CHECK(IsPosetal<int>);

  // Row 3 — filtered (directed).
  STATIC_CHECK(IsFilteredCategory<bool>);
  STATIC_CHECK(IsFilteredCategory<std::size_t>);
  STATIC_CHECK(IsFilteredCategory<int>);

  // Row 4 — lattice (bicartesian thin).
  STATIC_CHECK(IsLatticeCategory<bool>);
  STATIC_CHECK(IsLatticeCategory<std::size_t>);
  STATIC_CHECK(IsLatticeCategory<int>);

  // Row 5 — bounded (initial + terminal).
  STATIC_CHECK(IsBoundedLatticeCategory<bool>);
  STATIC_CHECK(IsBoundedLatticeCategory<std::size_t>);
  STATIC_CHECK(IsBoundedLatticeCategory<int>);

  // Row 6 — Heyting (exponentials).
  STATIC_CHECK(IsHeytingLatticeCategory<bool>);
  STATIC_CHECK(IsHeytingLatticeCategory<std::size_t>);
  STATIC_CHECK(IsHeytingLatticeCategory<int>);

  // Row 7 — Boolean (complement involution + complement laws).
  // Only bool fires under (std::less_equal + std::logical_not); size_t
  // and int honestly fail (the bitwise route is #710's territory).
  STATIC_CHECK(IsBooleanLatticeCategory<bool>);
  STATIC_CHECK_FALSE(IsBooleanLatticeCategory<std::size_t>);
  STATIC_CHECK_FALSE(IsBooleanLatticeCategory<int>);
}
