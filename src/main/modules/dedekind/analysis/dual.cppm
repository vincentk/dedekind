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
#include <limits>      // saturating componentwise + for unsigned carriers

export module dedekind.analysis:dual;

import dedekind.algebra;
import dedekind.category;
import dedekind.geometry; // IsTangentBundle (flat-case tangent-bundle concept)
import dedekind.numbers; // Rational<default_integer> for the trait-registry witness
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
   * @brief Lexicographic total order: primal first, tangent breaks ties.
   *
   *  `a < b` iff `a.val < b.val`, or equal primal with `a.der < b.der`.
   *  This is a @b total order whose induced equality is @c operator==
   *  (which compares both components), so trichotomy holds and any
   *  order-extremum over @c Dual --- e.g. the tropical @c ⊕ = @c max ---
   *  is @b commutative (a genuine semiring join, not an argument-order
   *  artefact).  On a primal tie the extremum carries the larger tangent,
   *  the @b upper subgradient: a deterministic, valid selection at a point
   *  where @c max is non-differentiable.  The primal alone still fixes the
   *  optimum's @b value (a tie is equal-primal); the tangent only decides
   *  @b which representative is carried out of the tie, so downstream
   *  argmax (e.g. @c dedekind.optimization:lp) keeps the same optimal
   *  value, with degenerate ties now broken deterministically.
   */
  friend constexpr bool operator<(const Dual& a, const Dual& b) {
    return a.val < b.val || (a.val == b.val && a.der < b.der);
  }

  friend constexpr Dual operator+(const Dual& a, const Dual& b) {
    if constexpr (std::unsigned_integral<F>) {
      // Saturate each component so + stays monotone over the lex order (hence
      // a distributive tropical ⊗ when Dual is a MaxPlus carrier): clamp at max
      // on wraparound rather than wrapping.
      const F sv = a.val + b.val, sd = a.der + b.der;
      return {sv < a.val ? std::numeric_limits<F>::max() : sv,
              sd < a.der ? std::numeric_limits<F>::max() : sd};
    } else {
      return {a.val + b.val, a.der + b.der};
    }
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

export template <typename F = dedekind::numbers::machine_real_scalar,
                 typename L = ClassicalLogic, typename C = ℶ_1>
using DualSetOf = UniversalSet<Dual<F>, L, C>;

export using DualSet = DualSetOf<>;

/** @brief The canonical dual-number universe 𝔻 = Ω<Dual<QuadraticReal<2>>,
 *         ClassicalLogic, ℶ_1> — the coat-hanger 𝔻 = Dual(ℝ) over the
 *         genuine ℝ = ℚ(√2) (mirroring ℝ and ℂ).
 *
 *  @details Per #559's chosen direction (option A): the named species
 *  symbols denote @b universe values (constexpr instances of
 *  @c UniversalSet over the carrier), not classifier-alias types.  All
 *  seven species symbols (𝔹, ℕ, ℤ, ℚ, ℝ, ℂ, 𝔻) carry the canonical
 *  @c element<𝔻> scout spelling.
 *
 *  Post-HSP retarget: the carrier of @c 𝔻 is @c Dual<QuadraticReal<2>> —
 *  the 2nd-order quotient ℝ[ε]/(ε²) over the coat-hanger ℝ, NOT
 *  @c Dual<double>.  Machine-double forward-mode AD lives on the
 *  materialisable ambient @c 𝔻_d = Ω<Dual<machine_real_scalar>> below
 *  (mirroring ℝ_d / ℂ_d).  The classifier is reachable via
 *  @c DualSet @c = @c DualSetOf<>.
 *
 *  Cardinality is set explicitly to @c ℶ_1 (continuum) — @c 𝔻 is in
 *  bijection with ℝ × ℝ via the @c (a, @c b) coefficient pair (the
 *  same shape that gives @c ℂ its @c ℶ_1) — overriding the
 *  @c Ω<...> variable template's @c ℵ_0 default.
 *
 *  Pre-#559 the spelling was @c using @c 𝔻 @c = @c DualSet (the
 *  classifier alias); type-context sites in concept gates and member
 *  extractions (@c typename @c 𝔻::Domain etc.) were migrated to
 *  @c DualSet directly in step 1 of this slice.
 *
 *  Textbook construction: @c 𝔻 @c = @c ℝ[ε]/(ε²) — a polynomial
 *  quotient observable via the @c quotient operator from
 *  @c sets:quotient (the same DSL primitive ℚ rides on under #567's
 *  textbook quotient exhibit).  The structural-trait propagation
 *  through @c quotient_algebra_base<Dual<F>>::type @c = @c F (below)
 *  is the universal-algebra side of that same construction.
 */
export inline constexpr auto 𝔻 =
    dedekind::sets::Ω<Dual<dedekind::numbers::QuadraticReal<2>>, ClassicalLogic,
                      ℶ_1>;

static_assert(
    std::same_as<
        std::remove_cvref_t<decltype(𝔻)>,
        dedekind::sets::UniversalSet<Dual<dedekind::numbers::QuadraticReal<2>>,
                                     ClassicalLogic, ℶ_1>>,
    "𝔻 is the universe Ω<Dual<QuadraticReal<2>>, ClassicalLogic, ℶ_1> — the "
    "coat-hanger 𝔻 = Dual(ℝ) = ℝ[ε]/(ε²) over the genuine ℝ = ℚ(√2), mirroring "
    "ℝ and ℂ.  Not Dual<double>.");
static_assert(std::same_as<typename std::remove_cvref_t<decltype(𝔻)>::Domain,
                           Dual<dedekind::numbers::QuadraticReal<2>>>,
              "𝔻's carrier IS Dual<QuadraticReal<2>> — the 2nd-order quotient "
              "ℝ[ε]/(ε²) over the coat-hanger ℝ.");

/** @brief The materialisable machine ambient @c 𝔻_d = @c Ω<Dual<double>>,
 *  mirroring @c ℝ_d / @c ℂ_d.  Machine-double forward-mode AD lives here; the
 *  abstract @c 𝔻 is the coat-hanger. */
export inline constexpr auto 𝔻_d =
    dedekind::sets::Ω<Dual<dedekind::numbers::machine_real_scalar>,
                      ClassicalLogic, ℶ_1>;
static_assert(std::same_as<typename std::remove_cvref_t<decltype(𝔻_d)>::Domain,
                           Dual<dedekind::numbers::machine_real_scalar>>,
              "𝔻_d's carrier is Dual<machine_real_scalar> (machine ambient).");

export inline constexpr DualSet D{};

/**
 * @brief The Birkhoff @b S leg @f$\mathbb{R}\hookrightarrow\mathbb{D}@f$ over
 * the coat-hanger: @c QuadraticReal<2> → @c Dual<QuadraticReal<2>>,
 *        @f$r\mapsto r+0\varepsilon@f$ (primal @c r, tangent @c 0).
 *
 * @details A genuine monic @b ring embedding (an @c EmbedsAsSubalgebra S-leg,
 * the 𝔻 sibling of @c embed_ℝ_ℂ / @c embed_ℚ_ℝ): ℝ is the constant subring
 * @f$\{\,\varepsilon\text{-part}=0\,\}\subset\mathbb{D}@f$ (the value-0-tangent
 * duals).  Witnessed by computation below. */
export inline constexpr auto embed_ℝ_𝔻 =
    dedekind::category::arrow<dedekind::numbers::QuadraticReal<2>,
                              Dual<dedekind::numbers::QuadraticReal<2>>>(
        [](const dedekind::numbers::QuadraticReal<2>& r) noexcept {
          return Dual<dedekind::numbers::QuadraticReal<2>>{
              r, dedekind::numbers::QuadraticReal<2>{}};
        });

}  // namespace dedekind::analysis

// ---------------------------------------------------------------------------
// Quotient-algebra registration for Dual<F> (#498/#499 NEW-A).
//
// Dual<F> = F[ε]/(ε²) is a polynomial-quotient construction (the
// nilpotent ε generator collapses to zero squared).  The single
// declaration below — `quotient_algebra_base<Dual<F>>::type = F` —
// fires the structural-trait propagation through
// `dedekind.algebra:quotient`: associativity, commutativity,
// distributivity, and the full IsTotal disjunction (periodic /
// idempotent / saturating) all lift from F to Dual<F> uniformly.  The carrier-
// specific bits (additive identity, additive inverse) live next to it
// as identity_trait / inverse_trait specialisations.
//
// Textbook references: Lang §III.1; Eisenbud §16.5.
// ---------------------------------------------------------------------------

namespace dedekind::category {

template <typename F>
  requires std::regular<F>
struct quotient_algebra_base<dedekind::analysis::Dual<F>> {
  using type = F;
};

template <typename F>
  requires std::regular<F>
struct identity_trait<dedekind::analysis::Dual<F>,
                      std::plus<dedekind::analysis::Dual<F>>> {
  using value_type = dedekind::analysis::Dual<F>;
  static constexpr value_type value = value_type{F{}, F{}};
};

template <typename F>
  requires std::regular<F>
struct identity_trait<dedekind::analysis::Dual<F>,
                      std::multiplies<dedekind::analysis::Dual<F>>> {
  using value_type = dedekind::analysis::Dual<F>;
  static constexpr value_type value = value_type{F{1}, F{}};
};

template <typename F>
  requires std::regular<F>
inline constexpr bool is_invertible_v<dedekind::analysis::Dual<F>,
                                      std::plus<dedekind::analysis::Dual<F>>> =
    true;

template <typename F>
  requires std::regular<F>
struct inverse_trait<dedekind::analysis::Dual<F>,
                     std::plus<dedekind::analysis::Dual<F>>> {
  static constexpr bool exists = true;
  using value_type = dedekind::analysis::Dual<F>;
  static constexpr value_type compute(
      const dedekind::analysis::Dual<F>& d) noexcept {
    return -d;
  }
};

// ── S-leg ℝ ↪ 𝔻 (monic) + the HSP legs of the coat-hanger 𝔻 = ℝ[ε]/(ε²) ──────
template <>
inline constexpr bool
    is_monic_arrow_v<std::decay_t<decltype(dedekind::analysis::embed_ℝ_𝔻)>> =
        true;
// P-leg: 𝔻 ≅ ℝ × ℝ via the (val, der) coefficient pair.  ℂ carries the
// pair-like IsProduct (its storage is literally .first/.second, what
// IsPairLikeProduct reads); 𝔻's storage is the semantic .val/.der, so its
// product iso is witnessed directly --- projections + reconstruction --- in the
// computed section below (dual__ℝ_𝔻_S_Leg_Witnesses).  Both ARE ℝ×ℝ. H-leg: 𝔻 =
// ℝ[ε]/(ε²) is a quotient algebra over ℝ (quotient_algebra_base above).
static_assert(
    IsQuotientAlgebra<
        dedekind::analysis::Dual<dedekind::numbers::QuadraticReal<2>>>,
    "H-leg: the coat-hanger 𝔻 = ℝ[ε]/(ε²) is a quotient algebra over ℝ.");

// Composability: the 2nd-order quotient functors nest over any ring.  Dual is
// the outer functor here, so the cross-functor D<C<Q>> / D<C<R>> and the plain
// D<R> / D<Q> instances are all bona-fide quotient algebras.
static_assert(
    IsQuotientAlgebra<dedekind::analysis::Dual<
        dedekind::numbers::Complex<dedekind::numbers::QuadraticReal<2>>>>,
    "D<C<R>>: Dual over Complex over the coat-hanger ℝ is a quotient algebra.");
static_assert(
    IsQuotientAlgebra<dedekind::analysis::Dual<dedekind::numbers::Complex<
        dedekind::numbers::Rational<dedekind::numbers::default_integer>>>>,
    "D<C<Q>>: Dual over Complex over ℚ is a quotient algebra (functors nest).");
// ...and the other way round: Dual<R> is IsComplexScalar (has {}, +, -, *), so
// Complex<Dual<R>> instantiates too --- the two 2nd-order functors compose in
// EITHER order.
static_assert(
    IsQuotientAlgebra<dedekind::numbers::Complex<
        dedekind::analysis::Dual<dedekind::numbers::QuadraticReal<2>>>>,
    "C<D<R>>: Complex over Dual over the coat-hanger ℝ is a quotient algebra.");

}  // namespace dedekind::category

namespace dedekind::algebra {
// S-leg: ℝ ↪ 𝔻 is a ring homomorphism (r ↦ r+0ε preserves +,×,0,1) — backed by
// the computed witnesses below.
template <>
inline constexpr bool
    is_homomorphism_v<std::decay_t<decltype(dedekind::analysis::embed_ℝ_𝔻)>> =
        true;
}  // namespace dedekind::algebra

namespace dedekind::analysis {

// NEW-A trait registry witness (#498/#499): @c Dual<F> is a module
// over @c F.  The witness fires through the quotient-algebra
// propagation in @c dedekind.algebra:quotient.
static_assert(
    dedekind::algebra::is_module_v<
        Dual<dedekind::numbers::Rational<dedekind::numbers::default_integer>>,
        dedekind::numbers::Rational<dedekind::numbers::default_integer>>,
    "Dual<ℚ> is a module over ℚ — the exact-arithmetic AD instance.");

/** @section dual__ℝ_𝔻_S_Leg_Witnesses
 *  The S-leg ℝ ↪ 𝔻 (r ↦ r+0ε) is a genuine ring homomorphism, @b computed. */
namespace {
using R2_d = dedekind::numbers::QuadraticReal<2>;
using D2_d = Dual<dedekind::numbers::QuadraticReal<2>>;
constexpr R2_d a_d = R2_d{2};
constexpr R2_d b_d = R2_d{3};
static_assert(embed_ℝ_𝔻(a_d + b_d) == embed_ℝ_𝔻(a_d) + embed_ℝ_𝔻(b_d),
              "ℝ ↪ 𝔻 preserves +.");
static_assert(embed_ℝ_𝔻(a_d* b_d) == embed_ℝ_𝔻(a_d) * embed_ℝ_𝔻(b_d),
              "ℝ ↪ 𝔻 preserves ×.");
static_assert(embed_ℝ_𝔻(R2_d{}) == D2_d{}, "ℝ ↪ 𝔻 preserves 0.");
static_assert(embed_ℝ_𝔻(R2_d{1}) == D2_d{R2_d{1}, R2_d{}},
              "ℝ ↪ 𝔻 preserves 1.");
static_assert(a_d != b_d && embed_ℝ_𝔻(a_d) != embed_ℝ_𝔻(b_d),
              "ℝ ↪ 𝔻 is injective (monic).");
static_assert(embed_ℝ_𝔻(a_d).derivative() == R2_d{},
              "image of ℝ ↪ 𝔻 lies in the constant subring {ε-part = 0} ⊂ 𝔻.");
static_assert(
    dedekind::algebra::EmbedsAsSubalgebra<std::decay_t<decltype(embed_ℝ_𝔻)>>,
    "S-leg: ℝ ↪ 𝔻 (embed_ℝ_𝔻) is a Birkhoff S-leg — a monic ring embedding.");

// --- P-leg (computed): 𝔻 ≅ ℝ × ℝ.  The two projections recover the
// coefficients
// --- and reconstruction from them is the identity — the product iso, run. ---
constexpr D2_d d_pair = D2_d{a_d, b_d};
static_assert(d_pair.value() == a_d, "P-leg: π₁ (value) recovers the ℝ val.");
static_assert(d_pair.derivative() == b_d,
              "P-leg: π₂ (derivative) recovers the ℝ der.");
static_assert(D2_d{d_pair.value(), d_pair.derivative()} == d_pair,
              "P-leg: ⟨π₁, π₂⟩ reconstruction is the identity — 𝔻 ≅ ℝ × ℝ.");
}  // namespace

}  // namespace dedekind::analysis
