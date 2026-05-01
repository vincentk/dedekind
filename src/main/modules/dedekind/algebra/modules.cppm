/**
 * @file dedekind/algebra/modules.cppm
 * @partition :modules
 * @brief The Linear Synthesis (Modules and Vector Spaces).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.

 *
 * @section modules__Taxonomy_of_Influence
 * A Module is the reification of 'Structural Action'. It defines how
 * a Ring (the Scalar) acts upon an Additive Group (the Vector),
 * creating the 'Linear Action' that serves as the engine for all
 * Vector Spaces and Metric Geometry in the Dedekind topos.
 *
 * @note „Die Arithmetik, Algebra und Analysis sind nur eine einzige
 Wissenschaft,
 *  die Wissenschaft der Zahlen.“
 *  (Arithmetic, algebra, and analysis are but a single science,
 *   the science of numbers.)
 *  — Emmy Noether
 */
module;

#include <compare>     // for std::strong_ordering
#include <concepts>    // for std::integral, std::floating_point
#include <cstddef>     // for std::size_t
#include <functional>  // for std::plus, std::multiplies
#include <limits>      // for std::numeric_limits

export module dedekind.algebra:modules;

import dedekind.category;
import dedekind.ieee;
import :field;
import :group;
import :polynomial;
import :ring;  // HasRingOperators (operator-shape claim on int / unsigned int)

namespace dedekind::algebra {
using namespace dedekind::category;

/**
 * @concept IsIntegralScalar
 * @brief Internal bridge for std::integral scalar carriers.
 */
export template <typename S>
concept IsIntegralScalar = std::integral<S>;

/**
 * @concept IsUnsignedIntegralScalar
 * @brief Internal bridge for std::unsigned_integral scalar carriers.
 */
export template <typename S>
concept IsUnsignedIntegralScalar = std::unsigned_integral<S>;

/**
 * @concept IsFloatingScalar
 * @brief Internal bridge for std::floating_point scalar carriers.
 */
export template <typename S>
concept IsFloatingScalar = std::floating_point<S>;

// HasFieldOperators<S> --- the operator-shape predicate "S has field-shaped
// arithmetic" --- lives in @c dedekind.algebra:field as
// @c HasRingOperators<S> @c && @c HasGroupOperatorsMul<S>.  This partition
// (formerly :modules) used to ship a duplicate @c IsFieldLikeScalar with the
// same intent; that duplicate was retired alongside the rest of the operational
// @c *Like family (Stream 3 of #374).  Downstream code in @c :modules and
// @c :vectorspace consumes the canonical @c HasFieldOperators directly.

/**
 * @concept IsSemimodule
 * @brief A commutative additive monoid participating in a Linear Action.
 * @tparam M The module carrier.
 * @tparam S The scalar carrier.
 * @tparam AddM Additive law on M (defaults to `std::plus<M>`).
 * @tparam AddS Additive law on S (defaults to `std::plus<S>`).
 * @tparam MultS Multiplicative law on S (defaults to `std::multiplies<S>`).
 * @tparam Act External action witness S x M -> M
 * (defaults to `std::multiplies<>`).
 */
export template <typename M, typename S, typename AddM = std::plus<M>,
                 typename AddS = std::plus<S>,
                 typename MultS = std::multiplies<S>,
                 typename Act = std::multiplies<>>
concept IsSemimodule =
    IsCommutativeMonoid<M, AddM> && IsSemiring<S, AddS, MultS> &&
    IsLinearAction<S, M, Act, AddS>;

/**
 * @concept IsModule
 * @brief An Abelian Group participating in a Linear Action by a Ring.
 * @tparam M The module carrier.
 * @tparam S The scalar carrier.
 * @tparam AddM Additive law on M (defaults to `std::plus<M>`).
 * @tparam AddS Additive law on S (defaults to `std::plus<S>`).
 * @tparam MultS Multiplicative law on S (defaults to `std::multiplies<S>`).
 * @tparam Act External action witness S x M -> M
 * (defaults to `std::multiplies<>`).
 */
export template <typename M, typename S, typename AddM = std::plus<M>,
                 typename AddS = std::plus<S>,
                 typename MultS = std::multiplies<S>,
                 typename Act = std::multiplies<>>
concept IsModule = IsAdditiveGroup<M, AddM> && IsRing<S, AddS, MultS> &&
                   IsSemimodule<M, S, AddM, AddS, MultS, Act>;

/**
 * @concept IsScalar
 * @brief A bona fide scalar is a 1D semi-module over itself.
 * @details The scalar carrier S acts on itself by its own multiplication,
 *          forming the canonical 1D semi-module S over S. This is the
 *          minimal structural claim a type earns by being called a "scalar":
 *          it participates in its own linear action.
 *
 *          The field-level strengthening `IsFieldElement<F> = IsVectorSpace<F,
 * F>` lives in `algebra:vectorspace`, alongside `IsVectorSpace` itself.
 */
export template <typename S>
concept IsScalar = IsSemimodule<S, S>;

/**
 * @struct OneDimensionalVector
 * @brief Reified carrier for a 1D module/vector-space coordinate.
 * @details This is the concrete baseline for issue #125. The coordinate lives
 *          in the scalar species S, with pointwise addition and scalar action.
 */
export template <typename S, typename Tag = void>
  requires std::equality_comparable<S> && std::default_initializable<S>
struct OneDimensionalVector {
  using scalar_type = S;

  S x{};

  constexpr OneDimensionalVector() = default;
  constexpr explicit OneDimensionalVector(S value) : x(value) {}
  template <typename U>
    requires std::constructible_from<S, U>
  constexpr explicit OneDimensionalVector(U value) : x(S(value)) {}

  constexpr S coordinate() const noexcept { return x; }

  friend constexpr bool operator==(const OneDimensionalVector&,
                                   const OneDimensionalVector&) = default;

  friend constexpr OneDimensionalVector operator+(const OneDimensionalVector& a,
                                                  const OneDimensionalVector& b)
    requires requires(S s) {
      { s + s } -> std::same_as<S>;
    }
  {
    return OneDimensionalVector(a.x + b.x);
  }

  friend constexpr OneDimensionalVector operator-(const OneDimensionalVector& a,
                                                  const OneDimensionalVector& b)
    requires requires(S s) {
      { s - s } -> std::same_as<S>;
    }
  {
    return OneDimensionalVector(a.x - b.x);
  }

  friend constexpr OneDimensionalVector operator-(const OneDimensionalVector& v)
    requires requires(S s) {
      { -s } -> std::same_as<S>;
    }
  {
    return OneDimensionalVector(-v.x);
  }

  friend constexpr OneDimensionalVector operator*(const S& s,
                                                  const OneDimensionalVector& v)
    requires requires(S scalar) {
      { scalar * scalar } -> std::same_as<S>;
    }
  {
    return OneDimensionalVector(s * v.x);
  }

  friend constexpr OneDimensionalVector operator*(const OneDimensionalVector& v,
                                                  const S& s)
    requires requires(S scalar) {
      { scalar * scalar } -> std::same_as<S>;
    }
  {
    return OneDimensionalVector(v.x * s);
  }

  template <typename Op>
  static constexpr auto identity_v = []() {
    if constexpr (std::same_as<Op, std::plus<OneDimensionalVector>> ||
                  std::same_as<Op, std::plus<void>>) {
      return OneDimensionalVector{};
    }
  }();

  template <typename Op>
  static constexpr bool is_associative_v =
      std::same_as<Op, std::plus<OneDimensionalVector>> ||
      std::same_as<Op, std::plus<void>>;

  template <typename Op>
  static constexpr bool is_commutative_v =
      std::same_as<Op, std::plus<OneDimensionalVector>> ||
      std::same_as<Op, std::plus<void>>;
};

/** @brief Alias: reals seen as a canonical 1D vector carrier. */
export using RealLine = OneDimensionalVector<dedekind::ieee::IEEE<double>>;
export using UnsignedLine = OneDimensionalVector<unsigned int>;
export using BoolLine = OneDimensionalVector<bool>;

// HasSemimoduleOperators / HasScalarOperators were briefly exposed as
// derived shape predicates over (AddM, AddS, MultS, Act) functor tuples.
// Per #510 review: they are too generic to earn a name --- the closure
// claim ``these operations are functor-instantiable on T'' is mechanical
// substitution of the same operator-shape checks already covered by
// @c HasRingOperators / @c HasGroupOperatorsAdd / @c HasGroupOperatorsMul,
// and the algebraic claim is the strict @c IsSemimodule / @c IsScalar in
// @c category:total.  Either spelling --- pure operator shape, or the
// strict algebraic concept --- is more honest than a middle ``Operators''
// concept that adds nothing.  The retired aliases were wide enough that
// no consumer actually needed the multi-functor form; downstream code
// now binds either to the operator-shape predicates directly or to the
// strict algebraic concepts.

/** @brief Additive join on 1D Boolean vectors (OR). */
export struct BoolLineJoin {
  constexpr BoolLine operator()(BoolLine a, BoolLine b) const noexcept {
    return BoolLine(static_cast<bool>(a.coordinate() || b.coordinate()));
  }
};

/** @brief Scalar action on 1D Boolean vectors via meet (AND). */
export struct BoolLineMeetAction {
  constexpr BoolLine operator()(bool s, BoolLine v) const noexcept {
    return BoolLine(static_cast<bool>(s && v.coordinate()));
  }
};

/**
 * @brief Strength-reduced scaling for unsigned 1D vectors.
 * @details Multiplication by powers of two is lowered to left-shift when safe.
 */
export template <IsUnsignedIntegralScalar S, typename Tag>
constexpr OneDimensionalVector<S, Tag> scale_strength_reduced(
    const OneDimensionalVector<S, Tag>& v, S factor) {
  if (factor == S{0}) return OneDimensionalVector<S, Tag>(S{0});

  // Power-of-two factors can be reduced to a shift.
  if ((factor & (factor - S{1})) == S{0}) {
    std::size_t shift = 0;
    S tmp = factor;
    while (tmp > S{1}) {
      ++shift;
      tmp >>= 1;
    }

    if (shift >= static_cast<std::size_t>(std::numeric_limits<S>::digits)) {
      return OneDimensionalVector<S, Tag>(S{0});
    }
    return OneDimensionalVector<S, Tag>(
        static_cast<S>(v.coordinate() << shift));
  }

  return OneDimensionalVector<S, Tag>(static_cast<S>(v.coordinate() * factor));
}

/**
 * @struct PolynomialOperator
 * @brief The "Enhanced" Polynomial: A Formal Sum reified as a Morphic Action.
 *
 * @details
 * In the Dedekind hierarchy, this transforms the static 'Formal Polynomial'
 * into a dynamic 'Operator' on a Module M.
 *
 * @tparam R The Scalar Ring (Coefficients).
 * @tparam M The Target Module (The Space).
 */
export template <typename R, typename M>
  requires IsModule<M, R>
struct PolynomialOperator {
  RigPolynomial<R> p;

  /** @section modules__The_Action_Axiom */
  constexpr M operator()(const M& x) const {
    // Horner's Method implementation lives here,
    // where IsModule is fully visible.
    M result = dedekind::category::identity_v<M, std::plus<M>>;
    for (auto it = p.coeffs().rbegin(); it != p.coeffs().rend(); ++it) {
      result = (result * x) + (*it);
    }
    return result;
  }
};

export using RealLineScalar = decltype(RealLine{}.coordinate());

// RealLine's vector-space witnesses live in `:vectorspace` (they use
// `HasVectorSpaceOperators` / `SatisfiesVectorSpaceAxioms`, which moved
// there along with the rest of the field-level concept surface).

/** @section modules__Operator_Shape_Witnesses_For_Bona_Fide_Scalars
 *
 *  "A scalar is a 1D semi-module over itself; an element of a field is a 1D
 *   vector space over itself."  The operator-shape side of those slogans is
 *   captured by the existing @c HasRingOperators predicate from @c :ring;
 *   the strict-algebraic side is @c IsScalar / @c IsFieldElement in
 *   @c category:total / @c algebra:vectorspace.  No middle ``ScalarOperators''
 *   alias is shipped --- callers pick the spelling that matches what they
 *   actually need.
 */
static_assert(HasRingOperators<unsigned int>,
              "unsigned int closes the ring-operator surface (+, -, unary -, "
              "*); the strict algebraic claim is intentionally not specialised "
              "(modular-wrap, not associative).");
static_assert(HasRingOperators<int>,
              "int closes the ring-operator surface (+, -, unary -, *); the "
              "strict algebraic claim is intentionally not specialised "
              "(signed-overflow UB).");

}  // namespace dedekind::algebra

namespace dedekind::category {

template <typename S, typename Tag>
struct SpeciesTraits<dedekind::algebra::OneDimensionalVector<S, Tag>> {
  using Domain = dedekind::algebra::OneDimensionalVector<S, Tag>;
  using machine_type = S;
};

template <typename S, typename Tag>
  requires requires(S s) {
    { -s } -> std::same_as<S>;
  }
inline constexpr dedekind::algebra::OneDimensionalVector<S, Tag> inverse(
    dedekind::algebra::OneDimensionalVector<S, Tag> v,
    std::plus<dedekind::algebra::OneDimensionalVector<S, Tag>>) {
  return dedekind::algebra::OneDimensionalVector<S, Tag>(-v.x);
}

}  // namespace dedekind::category

namespace dedekind::algebra {

/** @section modules__Trait_Registry (NEW-A, #498/#499)
 *
 * The recursive enrichment relations Figure~1 of the paper depicts
 * (ℚ as ℤ-module, 𝔻 as ℝ-module, ℂ as ℝ-module, …) are pinned
 * mechanically as a single concept-based trait variable.  Unlike a
 * per-carrier opt-in pattern, the trait fires automatically for any
 * carrier @c (M, @c R) whose operators satisfy @c IsModule<M, @c R> —
 * no consumer-side registration is required.  The strict gating
 * (Module ⟹ Ring) is honoured because @c IsModule itself composes
 * @c IsRing<S>; carriers whose scalar fails @c IsRing won't fire the
 * trait, which is the correct algebraic statement.
 *
 * Sister traits with structural-metadata payload (rank, endomorphism
 * identity) — @c is_free_module_v and @c is_endomorphism_ring_v —
 * live in @c dedekind.linear_algebra:basis where the metadata is at
 * home.
 */

/** @brief @c is_module_v<M, R>: @c M is an @c R-module.  Concept-based
 *         dispatch — fires automatically when @c IsModule<M, @c R>
 *         holds. */
export template <typename M, typename R>
inline constexpr bool is_module_v = IsModule<M, R>;

}  // namespace dedekind::algebra
