#include <catch2/catch_test_macros.hpp>
#include <compare>
#include <functional>
#include <type_traits>

import dedekind.category;

// Negative witnesses moved out of the module (#1061): each type here breaks
// Rule 1 on purpose, to prove that a concept rejects it.  A witness of a
// rejection is a test, so it lives in the tests, where the Jlt checker's gate
// does not judge it.

namespace dedekind::category {

namespace mereology_witness {
/** @brief A certified relation callable only on a mutable object: the law
 *  concepts' const-callability contract rejects it under every reference
 *  qualification. */
struct MutableOnly {
  bool operator()(int, int) { return true; }
};
}  // namespace mereology_witness
template <>
struct is_reflexive<int, mereology_witness::MutableOnly> : std::true_type {};
static_assert(!IsReflexive<int, mereology_witness::MutableOnly> &&
                  !IsReflexive<int, mereology_witness::MutableOnly&>,
              "a mutable-only call is no relation, by value or by reference");

namespace mereology_witness {
/** @brief A four-value chain local to the test (the module's own is not
 *  exported): a cover step and two bounds, nothing registered about its
 *  relations. */
struct Dial4 {
  int v;
  friend constexpr auto operator<=>(const Dial4&, const Dial4&) = default;
  friend constexpr Dial4 successor(Dial4 d) {
    return {d.v < 3 ? d.v + 1 : d.v};
  }
};
}  // namespace mereology_witness
template <>
struct unit_trait<mereology_witness::Dial4, Sup> {
  static constexpr mereology_witness::Dial4 value{0};
};
template <>
struct unit_trait<mereology_witness::Dial4, Inf> {
  static constexpr mereology_witness::Dial4 value{3};
};
static_assert(IsReflexive<mereology_witness::Dial4,
                          std::less_equal<mereology_witness::Dial4>>,
              "the test's chain is decided under a constexpr order, so the "
              "rejection below is the relation's doing, not the chain's");

namespace mereology_witness {
/** @brief A relation the compiler cannot evaluate: its call is not constexpr.
 */
struct RuntimeLe {
  bool operator()(const Dial4& a, const Dial4& b) const { return a <= b; }
};
}  // namespace mereology_witness
static_assert(
    !IsReflexive<mereology_witness::Dial4, mereology_witness::RuntimeLe> &&
        !IsTransitive<mereology_witness::Dial4, mereology_witness::RuntimeLe>,
    "a relation that is no constant expression is not decided by "
    "the law: uncertified, and no compile error");

}  // namespace dedekind::category

TEST_CASE("Mereology: negative witnesses compile", "[category][mereology]") {
  SUCCEED("the static_asserts above are the test");
}
