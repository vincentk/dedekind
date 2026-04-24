/** @file dedekind/category/field_test.cpp
 *
 * Tests for @c IsField and @c IsCommutativeRing in
 * @c dedekind.category:total.  Exercises the opt-in trait
 * @c is_field_v<T, Add, Mult> that gates the concept: default-false
 * carriers must fail the concept even when they structurally look
 * field-shaped, and carriers that explicitly specialise the trait
 * must pass.
 *
 * The actual paper-facing retargeting of @c IsFieldLikeScalar call
 * sites to @c IsField happens once @c Rational<Z>, @c Complex<R>,
 * etc. gain their trait specialisations (tracked under epic #374,
 * children #371 axiom-hook auto-lifter and #379 paper sweep).
 */

#include <catch2/catch_test_macros.hpp>
#include <functional>

import dedekind.category;

using dedekind::category::IsCommutativeRing;
using dedekind::category::IsField;
using dedekind::category::IsRing;

namespace {

// A tiny field-shaped mock: a single-element carrier that we declare
// a commutative ring with division by opting into every structural
// trait plus the @c is_field_v opt-in.  This keeps the test hermetic
// (no dependency on real number carriers) while exercising the
// concept's positive path.
struct TrivialField {
  // Operators are referenced only inside `requires` expressions in
  // the concept checks below; mark them [[maybe_unused]] so
  // -Werror=unused-function on the test build does not reject them.
  [[maybe_unused]] constexpr friend bool operator==(
      const TrivialField&, const TrivialField&) = default;
  [[maybe_unused]] constexpr friend TrivialField operator+(
      TrivialField, TrivialField) noexcept {
    return {};
  }
  [[maybe_unused]] constexpr friend TrivialField operator-(
      TrivialField, TrivialField) noexcept {
    return {};
  }
  [[maybe_unused]] constexpr friend TrivialField operator-(
      TrivialField) noexcept {
    return {};
  }
  [[maybe_unused]] constexpr friend TrivialField operator*(
      TrivialField, TrivialField) noexcept {
    return {};
  }
  [[maybe_unused]] constexpr friend TrivialField operator/(
      TrivialField, TrivialField) noexcept {
    return {};
  }
};

}  // namespace

namespace dedekind::category {

template <>
struct identity_trait<TrivialField, std::plus<TrivialField>> {
  using value_type = TrivialField;
  static constexpr value_type value{};
};
template <>
struct identity_trait<TrivialField, std::multiplies<TrivialField>> {
  using value_type = TrivialField;
  static constexpr value_type value{};
};

template <>
inline constexpr bool is_associative_v<TrivialField, std::plus<TrivialField>> =
    true;
template <>
inline constexpr bool
    is_associative_v<TrivialField, std::multiplies<TrivialField>> = true;
template <>
inline constexpr bool is_commutative_v<TrivialField, std::plus<TrivialField>> =
    true;
template <>
inline constexpr bool
    is_commutative_v<TrivialField, std::multiplies<TrivialField>> = true;
template <>
inline constexpr bool is_distributive_v<
    TrivialField, std::multiplies<TrivialField>, std::plus<TrivialField>> =
    true;
template <>
struct is_periodic<TrivialField, std::plus<TrivialField>> : std::true_type {};
template <>
struct is_periodic<TrivialField, std::multiplies<TrivialField>>
    : std::true_type {};
template <>
inline constexpr bool is_invertible_v<TrivialField, std::plus<TrivialField>> =
    true;

// THE CRITICAL OPT-IN: declare TrivialField as a field.
template <>
inline constexpr bool is_field_v<TrivialField, std::plus<TrivialField>,
                                 std::multiplies<TrivialField>> = true;

}  // namespace dedekind::category

TEST_CASE("IsCommutativeRing — baseline existing carriers",
          "[category][commutative-ring]") {
  // The modular-ring test types the library already certifies as IsRing
  // are also commutative, so IsCommutativeRing must hold.
  using M = dedekind::category::Modular<256>;
  STATIC_CHECK(IsCommutativeRing<M, std::plus<M>, std::multiplies<M>>);
  STATIC_CHECK(IsCommutativeRing<unsigned int, std::plus<unsigned int>,
                                 std::multiplies<unsigned int>>);
}

TEST_CASE("IsField — opt-in gates the concept", "[category][field]") {
  // TrivialField has declared is_field_v = true and carries the
  // required arithmetic surface (including operator/).  Must pass.
  STATIC_CHECK(IsField<TrivialField, std::plus<TrivialField>,
                       std::multiplies<TrivialField>>);
}

TEST_CASE("IsField — rejects non-field commutative rings without opt-in",
          "[category][field]") {
  // unsigned int under (+, *) is a commutative ring and C++ even
  // defines `a / b` on it.  Without an is_field_v opt-in, the concept
  // must still reject it (unsigned int is ℤ/2^Nℤ, not a field).
  STATIC_CHECK_FALSE(IsField<unsigned int, std::plus<unsigned int>,
                             std::multiplies<unsigned int>>);

  // Same for the library's own Modular<256>: commutative ring, not a
  // field (non-units lack multiplicative inverses).  Modular<256>
  // happens to lack operator/ as well, so IsField fails for *two*
  // reasons; either is sufficient.
  using M = dedekind::category::Modular<256>;
  STATIC_CHECK_FALSE(IsField<M, std::plus<M>, std::multiplies<M>>);
}
