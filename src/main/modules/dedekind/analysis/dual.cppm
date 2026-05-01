/**
 * @file dedekind/analysis/dual.cppm
 * @partition :dual
 * @brief Dual numbers 𝔻 = a + bε with ε² = 0 — forward-mode AD carrier.
 *
 * @section dual__Partition_Move
 * This file relocated from @c dedekind.numbers:dual to
 * @c dedekind.analysis:dual at PR #513: the construction is algebraically
 * a quotient ring, but its structural meaning is differential ---
 * dual numbers @b are forward-mode automatic differentiation, and
 * the partition's natural neighbours are @c :ftc (numerical
 * derivative bridge), @c :forms (differential one-forms), and
 * @c :hamilton (Hamiltonian flow on Dual-carrier symplectic state).
 * The @c :analysis layer is downstream of @c :numbers in the build
 * graph, so all numeric-carrier consumers (@c Rational<Z>,
 * @c Complex<R>, @c IEEE<F>) remain reachable.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section dual__The_Nilpotent_Basis
 * Reifies Dual Numbers a + bε where ε² = 0.
 * This provides the algebraic foundation for Forward-Mode
 * Automatic Differentiation (AD).
 *
 * @section dual__Carrier_Generality
 * The construction Dual(R) = R[ε]/(ε²) is well-defined over any
 * commutative ring R, not only over floating-point fields.  The current
 * Dual<F> constraint `std::regular<F>` reflects this: integer carriers
 * (Dual<int>, Dual<SignedExtensionalCardinal<>>) and the modular ring
 * (Dual<unsigned int>) instantiate cleanly and close the ring-operator
 * surface.
 *
 * Note on division semantics: @c Dual<F>::operator/ and @c inverse()
 * delegate to @c F's division (@c F{1}/val plus @c -der/(val*val)), so
 * the operator surface @b syntactically compiles for any F that
 * supplies a @c / operator --- including @c F=int, where the
 * integer-division semantics give truncating, non-field results.  The
 * syntactic predicate @c algebra::HasFieldOperators<Dual<F>> is therefore
 * NOT a guarantee that @c Dual<F> is field-shaped at the axiomatic level;
 * it merely reports that the operator surface is closed.  The
 * field-axiomatic distinction is a job for the strict @c category::IsField
 * concept (which is intentionally not specialised on @c Dual<int>);
 * this file pins @c HasFieldOperators<Dual<double>> as a positive
 * witness and the ring-shape concepts (@c HasRingOperators,
 * @c IsAlgebra) as the load-bearing claims for the integer-carrier
 * extensions.
 *
 * The algebraic-geometric reading of dual numbers over an arbitrary
 * base ring --- Spec(R[ε]/(ε²)) as the scheme of tangent vectors at a
 * point --- is the textbook anchor for this generality (Hartshorne,
 * @em Algebraic @em Geometry, Exercise II.2.8; Eisenbud, @em Commutative
 * @em Algebra @em with @em a @em View @em Toward @em Algebraic
 * @em Geometry, §16.5).
 *
 * @section dual__Coherence_With_Numerical_FTC_Bridge
 * Forward-mode AD on Dual<F> is the @b symbolic / @b exact route from
 * a function to its derivative: @c f(x + ε) yields the primal in the
 * value component and the derivative in the tangent component,
 * mechanically, without any numerical approximation.  The @b numerical
 * counterpart is @c dedekind::analysis::ftc::derivative_at (central
 * difference) in @c dedekind.analysis:ftc, which uses a small step
 * @c h on a @c std::floating_point carrier.  The two routes converge
 * on smooth functions and IEEE-edge carriers.  Their structural
 * divergence: @c :ftc currently gates on
 * @c IsNumericalBridgeScalar @c = @c HasFieldOperators<R> @c && @c
 * std::floating_point<resolved_value_t<R>>, so the analytic side is
 * floating-point-only by design; Dual<F> generalises to discrete
 * carriers without that restriction (the witnesses below pin one
 * rung per supported carrier).  An open architectural question is
 * whether a downstream partition (e.g.\ @c dedekind.analysis:dual_ad)
 * should host the AD-meaning of dual numbers and cross-link both
 * @c :dual and @c :ftc as parallel implementations of "compute the
 * derivative of f at x" --- tracked separately, not in scope for the
 * carrier-witness slice this file currently lands.
 *
 * @note "Musica est exercitium arithmeticae occultum nescientis se numerare
 * animi."
 *       ("Music is the pleasure the human mind experiences from counting
 * without being aware that it is counting.")
 *       -- Gottfried Wilhelm Leibniz, letter to Christian Goldbach (1712)
 */

module;
#include <concepts>
#include <functional>  // std::plus / std::multiplies in IsAlgebra witnesses

export module dedekind.analysis:dual;

import dedekind.algebra;
import dedekind.category;
import dedekind.geometry; // IsTangentBundle (flat-case tangent-bundle concept)
import dedekind.sets;

namespace dedekind::analysis {

using namespace dedekind::algebra;
using namespace dedekind::category;
using namespace dedekind::sets;

/**
 * @class Dual
 * @brief Represents f(x) + f'(x)ε.
 *
 * @details Structural C++20 NTTP-compatible layout: primal / tangent fields
 *          are public so the type can be used as a non-type template
 *          parameter (e.g. `Halfspace2D<Dual<Rat>, …>` for parametric LP).
 *          Backward-compatible accessors `value()` / `derivative()` are
 *          kept for existing call sites.
 */
export template <typename F>
  requires std::regular<F>
struct Dual {
  using value_type = F;

  F val{};  ///< Primal   f(x). Public so the type is NTTP-structural.
  F der{};  ///< Tangent  f'(x). Public so the type is NTTP-structural.

  constexpr Dual() = default;
  constexpr Dual(F v, F d = F{}) : val(v), der(d) {}

  constexpr F value() const { return val; }
  constexpr F derivative() const { return der; }

  /** @section dual__Dual_Arithmetic: ε² = 0 */

  friend constexpr bool operator==(const Dual&, const Dual&) = default;

  /**
   * @brief Primal-lex ordering: `a < b` iff `a.val < b.val`.
   *
   *  This is a partial order on `Dual<F>` (ties under primal equality
   *  are incomparable — we return `false`), projecting to the natural
   *  total order on the primal component `F`. Used by downstream
   *  reductions (e.g. argmax in `dedekind.optimization:lp`) whose
   *  ordering semantics are defined on the primal part only. Tangents
   *  are not part of the ordering — they ride along via the chain rule
   *  on arithmetic.
   */
  friend constexpr bool operator<(const Dual& a, const Dual& b) {
    return a.val < b.val;
  }

  friend constexpr Dual operator+(const Dual& a, const Dual& b) {
    return {a.val + b.val, a.der + b.der};
  }

  friend constexpr Dual operator-(const Dual& a, const Dual& b) {
    return {a.val - b.val, a.der - b.der};
  }

  constexpr Dual operator-() const { return {-val, -der}; }

  friend constexpr Dual operator*(const Dual& a, const Dual& b) {
    // (a + bε)(c + dε) = ac + (ad + bc)ε + bdε²(→0)
    return {a.val * b.val, (a.val * b.der) + (a.der * b.val)};
  }

  /**
   * @brief Multiplicative inverse: (a + bε)⁻¹ = (1/a) - (b/a²)ε.
   * Valid when a ≠ 0.
   */
  constexpr Dual inverse() const { return {F{1} / val, -der / (val * val)}; }

  friend constexpr Dual operator/(const Dual& a, const Dual& b) {
    return a * b.inverse();
  }
};

// IsTangentBundle (the flat-case tangent-bundle concept) is defined
// upstream in @c dedekind.geometry:tangent --- co-located with the
// trivial @c TangentVector / @c CotangentVector aliases in
// @c :linear_map, where future non-flat manifold-bundle structure
// (issue \#185) will also land.  @c Dual<F> is the canonical witness;
// the @c static_assert below pins that identification using the
// imported concept (relocated from @c :analysis:dual at PR #513
// reviewer request: a more discoverable home for tangent-bundle
// vocabulary).

/** @section dual__Formal_Verification */

// Basis element ε = Dual(0, 1); the nilpotent axiom ε² = 0.
inline constexpr Dual<double> eps{0.0, 1.0};
static_assert(eps * eps == Dual<double>{0.0, 0.0}, "Nilpotent axiom: ε² = 0.");

// Forward-mode AD correctness: d/dx(x²)|_{x=3} = 6.
// Dual(3, 1) seeds x with derivative 1; squaring gives value 9, derivative 6.
inline constexpr Dual<double> x_seed{3.0, 1.0};
static_assert(x_seed * x_seed == Dual<double>{9.0, 6.0},
              "AD rule: d/dx(x²)|_{x=3} = 6.");

// Dual<double> is field-like: +, -, unary -, *, / are all defined and closed.
static_assert(dedekind::algebra::HasFieldOperators<Dual<double>>,
              "Dual<double> must satisfy the operational field-like witness.");

// Dual(R) = R[ε]/(ε²) is well-defined for any commutative ring R; nothing in
// the +, -, unary -, * fragment needs R to be a field or to be a floating-
// point carrier (cf. issue #504).  The current `Dual<F>` constraint is
// `std::regular<F>`, which is permissive enough to admit integer carriers
// and exact rationals; division (`operator/`, `inverse()`) additionally
// requires F to admit `F{1}/val`, which is why `HasFieldOperators<Dual<...>>`
// only fires when F itself is field-shaped.
//
// The ring-shape witnesses below pin one rung of the carrier-strength chain
// per rung the construction supports.  Each is a closure-tier claim under
// the universal-algebra (A, F) reading (algebra:universal): no axioms
// claimed; just that std::plus / std::multiplies on Dual<F> close on the
// carrier.

// Dual<int> — machine-integer-coefficient AD on machine ints.  Pinned at
// the ring shape (+, -, unary -, *) and the universal-algebra (A, F)
// pattern; the field-shape predicate HasFieldOperators is intentionally
// NOT pinned here even though it would syntactically fire (operator/
// compiles via integer division), because the integer-division
// semantics are not field-axiomatic --- the field-shape claim belongs
// only on field-shaped carriers (Dual<double>, Dual<Rat>).
//
// Note: ``machine-integer-coefficient'' is the honest framing here.
// Plain @c int is NOT axiomatic-ring-safe in this project (signed-
// overflow UB; cf.\ the @c IsRing rejection on @c int across
// @c algebra:ring and the paper §3.4 footnote a).  For exact
// integer-coefficient AD the right carrier is
// @c Dual<SignedExtensionalCardinal<>> (the variant ℤ-proxy) or
// @c Dual<Rational<...>> rather than @c Dual<int> --- the witness
// here is structural ("Dual respects whatever the base carrier
// supplies") rather than a claim about ℤ-faithful semantics on
// machine ints.
static_assert(dedekind::algebra::HasRingOperators<Dual<int>>,
              "Dual<int> closes the ring-operator surface.");
static_assert(dedekind::algebra::IsAlgebra<Dual<int>, std::plus<Dual<int>>,
                                           std::multiplies<Dual<int>>>,
              "Dual<int> closes the universal-algebra (A, F) pattern under "
              "(+, *) at the closure tier.");

// Dual<unsigned int> — non-negative AD on the modular ring ℤ/2ⁿℤ.  Closes
// the ring-operator surface under modular wrap.  Useful as a structural
// exhibit: Dual respects the underlying ring structure, including modular.
static_assert(
    dedekind::algebra::HasRingOperators<Dual<unsigned int>>,
    "Dual<unsigned int> closes the ring-operator surface under modular wrap.");

// Nilpotent axiom ε² = 0 carries to any ring carrier — the defining
// relation of Dual is independent of F.
inline constexpr Dual<int> eps_int{0, 1};
static_assert(eps_int * eps_int == Dual<int>{0, 0},
              "Nilpotent axiom ε² = 0 holds on Dual<int>.");

// IsTangentBundle structural identification: Dual<F> IS a first-order
// tangent-bundle carrier over F (Hartshorne, Ex. II.2.8).  Pin one
// witness per shipped carrier so the concept's primary instances are
// mechanical at translation time.
static_assert(dedekind::geometry::IsTangentBundle<Dual<double>>,
              "Dual<double> is a first-order tangent-bundle carrier over the "
              "machine-real proxy.");
static_assert(
    dedekind::geometry::IsTangentBundle<Dual<int>>,
    "Dual<int> is the discrete-side tangent-bundle (finite-difference) "
    "carrier over the machine integers.");

// NEW-A trait registry note (#498/#499): @c Dual<F> is a module over
// @c F (textbook reading: the quotient ring @c F[ε]/(ε²) is rank-2
// over @c F as a free module; Lang §III.1, Eisenbud §16.5).  The
// strict concept-based default in @c dedekind::algebra:modules
// requires @c algebra::IsRing<F> to fire; for shipping carriers
// @c double / @c int / @c Rational<default_integer> the strict gating
// does not fire (IEEE-754 / signed-overflow UB / variant ℤ
// saturation).  No witness pinned here until the algebraic-axioms
// gap closes — see #498 follow-up.

export template <typename F = double, typename L = ClassicalLogic,
                 typename C = ℶ_1>
using DualSetOf = Ω<Dual<F>, L, C>;

export using DualSet = DualSetOf<>;
export using 𝔻 = DualSet;

export inline constexpr 𝔻 D{};

}  // namespace dedekind::analysis
