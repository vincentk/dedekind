/** @file dedekind/category/field_test.cpp
 *
 * Tests for @c IsField and @c IsCommutativeRing in
 * @c dedekind.category:total.  The category-layer concepts here are
 * purely axiomatic (species-trait based, no operator requirements);
 * the operator-level witness @c dedekind::algebra::IsField builds on
 * this concept by additionally requiring the division surface via
 * @c IsDivisionRing.
 *
 * @c IsField composes @c IsCommutativeRing<T, Add, Mult> with
 * @c IsAbelianGroup<T, Mult>.  The multiplicative-inverse witness
 * the latter requires is the library's standard opt-in pair:
 * @c is_invertible_v<T, Mult> / @c inverse_trait<T, Mult>.  A
 * carrier asserts the field-level claim by specialising the same
 * trait the additive side already uses (e.g.
 * @c is_invertible_v<Modular<N>, std::plus> = true); zero is
 * understood excluded by convention, since concepts cannot quantify
 * over values.  Default-false carriers must fail the concept even
 * when their ring-trait chain otherwise looks field-shaped.
 *
 * The paper-facing retargeting of @c HasFieldOperators call sites
 * to @c IsField happens once @c Rational<Z>, @c Complex<R>, etc.
 * gain their trait specialisations (tracked under epic #374,
 * children #371 axiom-hook auto-lifter and #379 paper sweep).
 */

#include <catch2/catch_test_macros.hpp>
#include <functional>

import dedekind.category;

using dedekind::category::IsCommutativeRing;
using dedekind::category::IsField;

namespace {

// A tiny field-shaped mock: a single-element carrier we opt in as a
// commutative-ring field at the species-trait level.  The category-
// layer concept is axiomatic --- no operator requirements --- so
// this test type exposes only the minimum surface the underlying
// category concepts (@c IsMagma, @c IsAssociative, @c IsCommutative,
// etc.) reach through.
struct TrivialField {
  // Operators are referenced only inside `requires` expressions in
  // the category-layer concept bodies (e.g. IsMagma's operator
  // closure); mark them [[maybe_unused]] so -Werror=unused-function
  // does not reject them.
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

// THE CRITICAL OPT-IN: the multiplicative structure is an abelian group.
// This is what lifts TrivialField from IsCommutativeRing to IsField: every
// (non-zero) element admits a multiplicative inverse.  The trivial carrier
// has a single element that is simultaneously additive zero and
// multiplicative one, so the inverse claim holds vacuously.
template <>
inline constexpr bool
    is_invertible_v<TrivialField, std::multiplies<TrivialField>> = true;

}  // namespace dedekind::category

TEST_CASE("IsCommutativeRing — baseline existing carriers",
          "[category][commutative-ring]") {
  // unsigned int is the canonical primitive commutative ring (Z/2^N Z).
  // The stronger Modular<N> witness lives in morphologies:cyclic and is
  // covered by its own tests in the algebra test layer.
  STATIC_CHECK(IsCommutativeRing<unsigned int, std::plus<unsigned int>,
                                 std::multiplies<unsigned int>>);
}

TEST_CASE("IsField — opt-in gates the concept", "[category][field]") {
  // TrivialField registers the full species-trait chain and specialises
  // is_invertible_v<_, std::multiplies<_>> = true, so the multiplicative
  // side is an abelian group.  The axiomatic category-layer concept
  // passes.
  STATIC_CHECK(IsField<TrivialField, std::plus<TrivialField>,
                       std::multiplies<TrivialField>>);
}

TEST_CASE("IsField — rejects non-field commutative rings without opt-in",
          "[category][field]") {
  // unsigned int under (+, *) is a commutative ring.  The library does
  // not specialise is_invertible_v<unsigned int, std::multiplies> (and
  // no ADL inverse(x, std::multiplies<>{}) exists), so the multiplicative
  // side fails IsAbelianGroup and IsField must reject: unsigned int is
  // ℤ/2^Nℤ, a ring but not a field (non-units lack multiplicative
  // inverses).  This is precisely the false-positive the opt-in
  // prevents.  (The Modular<N> variant of this negative witness is
  // exercised in the morphologies test layer at
  // morphologies/modular_test.cpp, where the carrier is visible.)
  STATIC_CHECK_FALSE(IsField<unsigned int, std::plus<unsigned int>,
                             std::multiplies<unsigned int>>);
}
