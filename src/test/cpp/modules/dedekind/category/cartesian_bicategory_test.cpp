#include <catch2/catch_test_macros.hpp>

import dedekind.category;

using namespace dedekind::category;

// Negative witnesses moved out of the module (#1061): each type here breaks
// Rule 1 on purpose, to prove that a concept rejects it.  A witness of a
// rejection is a test, so it lives in the tests, where the Jlt linter's gate
// does not judge it.

namespace cartesian_bicategory_witness {
// Dually, a MOVE-ONLY carrier is rejected at the concept surface: the diagonal
// duplicates its input, so the @c std::copy_constructible<A> guard on
// @c Copy::operator() removes it from the overload set and @c IsArrow (hence
// @c IsCopy) fails --- not deferred to the @c {a,a} body (#950 review).
struct CopyMoveOnly {  // move-only: the diagonal cannot duplicate it
  CopyMoveOnly() = default;
  CopyMoveOnly(CopyMoveOnly&&) = default;
  CopyMoveOnly(const CopyMoveOnly&) = delete;
};
static_assert(!IsArrow<Copy<CopyMoveOnly>>,
              "a move-only carrier must NOT model IsArrow via Copy: the "
              "diagonal a↦(a,a) copies, so copy-constructibility is required.");
}  // namespace cartesian_bicategory_witness

TEST_CASE("Cartesian bicategory: negative witnesses compile",
          "[category][cartesian_bicategory]") {
  SUCCEED("the static_asserts above are the test");
}
