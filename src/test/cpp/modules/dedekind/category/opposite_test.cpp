/**
 * @file opposite_test.cpp
 * @brief The opposite operation, an involution on operations (#1104): its
 *        laws read backwards, a left action folded along a walk as a right
 *        action of the opposite monoid, and a dagger as an involution that is
 *        a homomorphism into the opposite.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */
#include <array>
#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <cstddef>
#include <functional>
#include <ranges>

import dedekind.category;

namespace opposite_witness {
using std::size_t;
/** @brief The affine maps @f$x \mapsto a x + b@f$ on the counts, under
 *  composition @f$f \circ g@f$: a monoid that does not commute. */
struct Affine {
  size_t a;
  size_t b;
  constexpr size_t operator()(size_t x) const { return a * x + b; }
  friend constexpr Affine operator*(Affine f, Affine g) {  // f ∘ g
    return {f.a * g.a, f.a * g.b + f.b};
  }
  constexpr bool operator==(const Affine&) const = default;
};
using Compose = std::multiplies<Affine>;
/** @brief A map applied to a count: a left action, @f$f \cdot x = f(x)@f$,
 *  spelled with the count first, as a fold calls it. */
struct Apply {
  constexpr size_t operator()(size_t x, Affine f) const { return f(x); }
};
/** @brief The identity on the counts, callable only at run time: its unit
 *  law cannot be computed, so it is refused, not an error. */
struct AtRunTime {
  size_t operator()(size_t x) const { return x; }
};
}  // namespace opposite_witness

namespace dedekind::category {
template <>
struct is_associative<opposite_witness::Affine, opposite_witness::Compose>
    : std::true_type {};
template <>  // size_t arithmetic wraps, so composition is total
struct is_periodic<opposite_witness::Affine, opposite_witness::Compose>
    : std::true_type {};
template <>
struct unit_trait<opposite_witness::Affine, opposite_witness::Compose> {
  using value_type = opposite_witness::Affine;
  static constexpr value_type value{1, 0};
};
template <>  // g(f(x)) = (g ∘ f)(x): along the walk, the opposite product
struct acting_monoid<opposite_witness::Apply> {
  using type = Opposite<opposite_witness::Compose>;
};
template <>  // a homomorphism, but no constexpr function object
struct is_homomorphism<opposite_witness::AtRunTime, std::size_t,
                       std::plus<std::size_t>> : std::true_type {};
template <>  // -(a + b) = -a + -b, modulo the word
struct is_homomorphism<std::negate<std::size_t>, std::size_t,
                       std::plus<std::size_t>> : std::true_type {};
}  // namespace dedekind::category

namespace opposite_witness {
using dedekind::category::fold;
using dedekind::category::IsCommutative;
using dedekind::category::IsMonoid;
using dedekind::category::IsRightAction;
using dedekind::category::Opposite;
using Op = Opposite<Compose>;

constexpr Affine twice{2, 0};
constexpr Affine next{1, 1};

// An involution on operations, and the laws read backwards.
static_assert(
    std::same_as<Opposite<Op>, Compose> &&
    std::same_as<Opposite<Opposite<std::plus<size_t>>>, std::plus<size_t>>);
static_assert(
    IsMonoid<Affine, Op> && !IsCommutative<Affine, Op> &&
    IsMonoid<size_t, Opposite<std::plus<size_t>>> &&
    IsCommutative<size_t, Opposite<std::plus<size_t>>> &&
    dedekind::category::IsTotal<size_t, const Opposite<std::plus<size_t>>>);
static_assert(twice * next != next * twice &&
              Op{}(twice, next) == next * twice);

// A left action is a functor out of the opposite category: the delooping of
// the opposite operation, where twice then next composes to next ∘ twice.
using LeftActs = dedekind::category::ActionFunctor<Apply, size_t, Affine>;
using Map = dedekind::category::Element<Affine, Op>;
static_assert(dedekind::category::IsFunctor<LeftActs> &&
              (Map{twice} >> Map{next}).value == next * twice &&
              LeftActs{}.φ(Map{twice} >> Map{next})(3) == 7 &&
              (LeftActs{}.φ(Map{twice}) >> LeftActs{}.φ(Map{next}))(3) == 7);

// A left action along a walk acts by the opposite product: twice, then next,
// is next ∘ twice; and n copies are one power, by squaring.
constexpr std::array<Affine, 2> walk{twice, next};
static_assert(IsRightAction<Apply, size_t, Affine> &&
              fold(walk, size_t{3}, Apply{}) == Op{}(twice, next)(3) &&
              fold(walk, size_t{3}, Apply{}) == 7 &&
              fold(std::views::repeat(next, 100'000'000uz), size_t{0},
                   Apply{}) == 100'000'000);

// A dagger is an involution that reverses the operation.  Negation on the
// counts is a homomorphism of +, which commutes, so it is one into the
// opposite as well, and a dagger; the unit comes along, -0 = 0.  It is no
// homomorphism of *: (-a)(-b) = ab.
using dedekind::category::IsAntiHomomorphism;
using dedekind::category::IsDagger;
using dedekind::category::IsMonoidHomomorphism;
using Negate = std::negate<size_t>;
static_assert(IsMonoidHomomorphism<Negate, size_t, std::plus<size_t>> &&
              IsAntiHomomorphism<Negate, size_t, std::plus<size_t>> &&
              IsDagger<Negate, size_t, std::plus<size_t>> &&
              !IsDagger<Negate, size_t, std::multiplies<size_t>> &&
              !IsDagger<std::bit_not<size_t>, size_t, std::plus<size_t>>);
// A qualified operation names the same law.
static_assert(IsDagger<Negate, size_t, const std::plus<size_t>>);
static_assert(dedekind::category::IsMagmaHomomorphism<AtRunTime, size_t,
                                                      std::plus<size_t>> &&
              !IsMonoidHomomorphism<AtRunTime, size_t, std::plus<size_t>>);
// Lifted to an arrow, endo<size_t>(-), it keeps both certificates.
using NegateArrow = decltype(dedekind::category::endo<size_t>(Negate{}));
static_assert(IsDagger<NegateArrow, size_t, std::plus<size_t>> &&
              IsMonoidHomomorphism<NegateArrow, size_t, std::plus<size_t>>);
}  // namespace opposite_witness

TEST_CASE("Opposite: a left action folded along a walk",
          "[category][opposite]") {
  using opposite_witness::Affine;
  const std::array<Affine, 3> walk{Affine{2, 0}, Affine{1, 1}, Affine{3, 0}};
  CHECK(dedekind::category::fold(walk, std::size_t{1},
                                 opposite_witness::Apply{}) == 9);
}
