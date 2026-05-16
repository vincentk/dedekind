/** @file dedekind/category/heyting_lattice_test.cpp
 *
 * Unit coverage for @c :category::lattice::IsHeytingLatticeCategory
 * (row 6 of the lattice Form-chain, #698 Slice 6).
 *
 * Slice 6 also generalises @c :cartesian::IsExponential to a pure
 * structural call-shape recogniser:
 *
 *     template <typename E, typename A, typename B>
 *     concept IsExponential = requires(E e, A a) {
 *       { e(a) } -> std::same_as<B>;
 *     };
 *
 * The unification: function-space exponentials (Set / Cpp) and
 * lattice-internal value exponentials (Heyting) satisfy the @b same
 * concept body — Juliet posture, no tag, no CPO.  Witnesses below
 * exercise both flavours plus the Form-chain inclusion chain.
 */

#include <algorithm>  // std::ranges::min — Meet template arg in HeytingExponential
#include <catch2/catch_test_macros.hpp>
#include <climits>
#include <concepts>
#include <functional>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE("category:heyting — Bool is the canonical 2-element Heyting lattice",
          "[category][lattice][heyting][bool][canonical]") {
  /** @brief @c bool under @c std::less_equal is a Heyting lattice (in
   *         fact Boolean).  @c HeytingExponential<bool, …>::operator()(x)
   *         computes @c min(value, x), which is the eval morphism of the
   *         Boolean exponential. */
  STATIC_CHECK(IsHeytingLatticeCategory<bool>);

  // Concrete eval values: e.g. value = true (= top), operator()(false) = false.
  constexpr HeytingExponential<bool, std::less_equal<bool>,
                               decltype(std::ranges::min)>
      e_top{true};
  STATIC_CHECK(e_top(false) == false);
  STATIC_CHECK(e_top(true) == true);
  // value = false (= bottom), operator()(x) = false for every x.
  constexpr HeytingExponential<bool, std::less_equal<bool>,
                               decltype(std::ranges::min)>
      e_bot{false};
  STATIC_CHECK(e_bot(false) == false);
  STATIC_CHECK(e_bot(true) == false);
}

TEST_CASE("category:heyting — integral carriers are Heyting lattices",
          "[category][lattice][heyting][numeric]") {
  /** @brief Integral carriers under @c std::less_equal are Heyting:
   *         eval is min, which corresponds to the totally-ordered Heyting
   *         implication a → b = top if a ≤ b else b. */
  STATIC_CHECK(IsHeytingLatticeCategory<int>);
  STATIC_CHECK(IsHeytingLatticeCategory<unsigned>);
  STATIC_CHECK(IsHeytingLatticeCategory<std::size_t>);

  constexpr HeytingExponential<int, std::less_equal<int>,
                               decltype(std::ranges::min)>
      e_int{42};
  STATIC_CHECK(e_int(10) == 10);   // min(42, 10) = 10
  STATIC_CHECK(e_int(100) == 42);  // min(42, 100) = 42
  STATIC_CHECK(e_int(42) == 42);   // min(42, 42) = 42
}

TEST_CASE(
    "category:heyting — HeytingExponential aligns structurally with "
    ":cartesian::IsExponential",
    "[category][lattice][heyting][cartesian][unification]") {
  /** @brief #698 Slice 6's structural unification: function-space
   *         exponentials and lattice value-exponentials satisfy the
   *         @b same @c IsExponential concept body.  Juliet posture —
   *         pure call-shape recognition, no tag / CPO / wrapper-for-tag. */

  // Function-space side: std::function and lambdas still fire.
  STATIC_CHECK(IsExponential<std::function<bool(int)>, int, bool>);
  STATIC_CHECK(IsExponential<std::function<int(int)>, int, int>);

  // Honest Rejection still fires under std::same_as (preserves the
  // pre-Slice-6 negative-witness behaviour).
  STATIC_CHECK_FALSE(IsExponential<std::function<int(int)>, int, bool>);

  // Lattice-value side: HeytingExponential satisfies the same concept.
  STATIC_CHECK(IsExponential<HeytingExponential<bool, std::less_equal<bool>,
                                                decltype(std::ranges::min)>,
                             bool, bool>);
  STATIC_CHECK(IsExponential<HeytingExponential<int, std::less_equal<int>,
                                                decltype(std::ranges::min)>,
                             int, int>);
}

TEST_CASE(
    "category:heyting — Form-chain faithful inclusions hold through row 6",
    "[category][lattice][heyting][faithful][form-chain]") {
  /** @brief Every Heyting lattice category is a bounded lattice, a
   *         lattice, filtered, posetal, and thin — all encoded
   *         definitionally through @c IsHeytingLatticeCategory's
   *         signature chain. */
  STATIC_CHECK(IsThinCategory<bool>);
  STATIC_CHECK(IsPosetal<bool>);
  STATIC_CHECK(IsFilteredCategory<bool>);
  STATIC_CHECK(IsLatticeCategory<bool>);
  STATIC_CHECK(IsBoundedLatticeCategory<bool>);
  STATIC_CHECK(IsHeytingLatticeCategory<bool>);

  STATIC_CHECK(IsThinCategory<int>);
  STATIC_CHECK(IsPosetal<int>);
  STATIC_CHECK(IsFilteredCategory<int>);
  STATIC_CHECK(IsLatticeCategory<int>);
  STATIC_CHECK(IsBoundedLatticeCategory<int>);
  STATIC_CHECK(IsHeytingLatticeCategory<int>);
}
