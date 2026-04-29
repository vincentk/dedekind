/**
 * @file dedekind/algebra/modules.cppm
 * @partition :modules
 * @brief Level 3.6: The Linear Synthesis (Modules and Vector Spaces).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.

 *
 * @section Taxonomy_of_Influence
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

/**
 * @concept IsFieldLikeScalar
 * @brief Operational scalar witness for field-like arithmetic under policy.
 * @details This concept captures the permissive path used for machine-backed
 *          arithmetic carriers such as `dedekind::ieee::IEEE<double>`, where
 *          addition, subtraction, multiplication, division, and unary negation
 *          are available and closed in the carrier, even if the stricter
 *          categorical `IsField` proof is intentionally withheld.
 *
 * @note @b Design @b choice (audit #393): @c IsFieldLikeScalar is the
 *       deliberate operational variant for IEEE-edge-case carriers that
 *       intentionally do not carry strict axiomatic proofs.  It is
 *       \emph{not} a stopgap for the missing strict @c IsField --- the
 *       strict concept exists in @c category:total and is used at every
 *       callsite that genuinely needs the field axioms.  Where a callsite
 *       depends on the operator surface acting field-shaped under the
 *       active numeric policy (Kleene three-valued, partial traits, IEEE
 *       admissibility), this is the correct concept.  For pure
 *       syntactic-shape requirements with no algebraic claim, see the
 *       @c Has*Operators family (@c dedekind::algebra::HasRingOperators
 *       and siblings).
 */
export template <typename S>
concept IsFieldLikeScalar = requires(S a, S b) {
  { a + b } -> std::same_as<S>;
  { a - b } -> std::same_as<S>;
  { -a } -> std::same_as<S>;
  { a * b } -> std::same_as<S>;
  { a / b } -> std::same_as<S>;
};

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

/**
 * @concept IsSemimoduleLike
 * @brief Operational semimodule witness for concrete machine-level carriers.
 * @details This concept is intentionally lightweight and checks closure and
 *          compatibility signatures for additive/multiplicative/action laws.
 */
export template <typename M, typename S, typename AddM, typename AddS,
                 typename MultS, typename Act>
concept IsSemimoduleLike = requires(M m1, M m2, S s1, S s2) {
  { AddM{}(m1, m2) } -> std::same_as<M>;
  { AddS{}(s1, s2) } -> std::same_as<S>;
  { MultS{}(s1, s2) } -> std::same_as<S>;
  { Act{}(s1, m1) } -> std::same_as<M>;
  { Act{}(MultS{}(s1, s2), m1) } -> std::same_as<M>;
  { Act{}(AddS{}(s1, s2), m1) } -> std::same_as<M>;
  { Act{}(s1, AddM{}(m1, m2)) } -> std::same_as<M>;
};

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
 * @concept IsScalarLike
 * @brief Operational counterpart of `IsScalar` for machine-backed carriers.
 * @details Reads "S is a 1D semimodule-like over itself" using the pragmatic
 *          closure signatures rather than the strict categorical proof. This
 *          is the concept a type like `Rational<long>` or `IEEE<double>` can
 *          carry under the active numeric policy (cf. `IsFieldLikeScalar`).
 *
 *          The vector-space-level operational witnesses
 *          (`IsVectorSpaceLike`, `IsFieldElementLike`,
 *          `SatisfiesVectorSpaceAxioms`) live in
 *          `algebra:vectorspace`, alongside the strict `IsVectorSpace`.
 */
export template <typename S>
concept IsScalarLike = IsSemimoduleLike<S, S, std::plus<S>, std::plus<S>,
                                        std::multiplies<S>, std::multiplies<>>;

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

  /** @section The_Action_Axiom */
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
// `IsVectorSpaceLike` / `SatisfiesVectorSpaceAxioms`, which moved
// there along with the rest of the field-level concept surface).
static_assert(
    IsSemimoduleLike<BoolLine, bool, BoolLineJoin, std::logical_or<bool>,
                     std::logical_and<bool>, BoolLineMeetAction>,
    "BoolLine should satisfy the 1D boolean semimodule-like witness.");

/** @section Bona_Fide_Scalar_and_Field_Element_Witnesses
 *
 *  "A scalar is a 1D semi-module over itself; an element of a field is a 1D
 *   vector space over itself." These operational witnesses anchor the slogan
 *   for concrete carriers under the active numeric policy.
 */
static_assert(IsScalarLike<unsigned int>,
              "unsigned int is a bona fide scalar (1D semi-module over self).");
static_assert(IsScalarLike<int>,
              "int is a bona fide scalar (1D semi-module over self).");

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

namespace dedekind::algebra {}  // namespace dedekind::algebra
