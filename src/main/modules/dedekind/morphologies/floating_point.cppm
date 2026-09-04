/**
 * @file dedekind/numbers/floating_point.cppm
 * @partition :floating_point
 * @brief std::floating_point — IEEE 754 Honest Rejection.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section floating_point__Honest_Stance
 * Each @c std::floating_point instance ( @c float, @c double, @c long
 * @c double ) carries the @b operator @b surface of a field: @c +, @c -,
 * @c *, @c / all close on the carrier; @c <=> realises the IEEE 754
 * partial-order spaceship.  The carrier @b also satisfies the operational
 * ordered-field reading via @c IsOrderedField (in @c morphologies/
 * archimedean.cppm; pinned for @c double upstream).  Where the @c
 * std::floating_point carrier @b stops carrying structure is at the
 * @b axiomatic level:
 *
 *   1. @b Associativity @b fails @b under @b rounding.  In general
 *      @c (a @c + @c b) @c + @c c @c != @c a @c + @c (b @c + @c c) ---
 *      catastrophic cancellation is the canonical witness.  The
 *      @c IsRing<T, std::plus, std::multiplies> witness would be a false
 *      claim.
 *
 *   2. @b NaN @b breaks @b reflexivity.  @c NaN @c <= @c NaN evaluates to
 *      @c false in IEEE 754, so @c is_reflexive_v<double, @c std::less_equal<>>
 *      is deliberately not registered (see @c category/species.cppm).
 *      The @c IsTotallyOrdered<T> witness fails accordingly --- a fact
 *      cited in @c order/total.cppm but @b not previously pinned
 *      mechanically.
 *
 *   3. @b Field @b laws fail downstream of (1).  Without a ring the
 *      field witness cannot fire.
 *
 * The exact-real reading lives one carrier step deeper, on @c Real<Q>
 * (Dedekind cuts over @c Q ; PR #397).  IEEE 754's @b operational
 * acceptability for the project's downstream code is delegated to
 * @c IsOrderedField / @c IsArchimedeanField + the @c IsLipschitzBoundaryPolicy
 * machinery in @c category/numeric.cppm, @b not to a false axiomatic
 * claim.
 *
 * @section floating_point__Honesty_Obligation
 * The witness blocks below pin the umbrella's @b negative axiomatic
 * claims at the partition boundary so future drift in either the
 * trait registry or the concept definitions trips the build.  The
 * positive operator-shape and operational ordered-field readings are
 * cross-referenced upstream (per the project's no-vacuous-restatement
 * memo): @c HasFieldOperators / @c HasRingOperators are exercised on
 * @c double across the tower (e.g.\ in @c :complex, @c :dual,
 * @c morphologies/archimedean ); @c IsOrderedField<double> is pinned in
 * @c morphologies/archimedean.cppm.
 *
 * Curry--Howard reading per Wadler 2015; professional grounding per the
 * NSPE @c Code @c of @c Ethics @c for @c Engineers (canons III + IV).
 * See the @c §"poor man's theorem prover" footnote in @c report.tex /
 * @c paper.tex for the full literature anchor.
 *
 * @note "Niemand kann zwei Herren dienen."
 *       ("No one can serve two masters.")
 *       — Matthew 6:24, Lutherbibel 1545.
 *       The IEEE 754 carrier serves two masters --- the engineer's
 *       runtime (rounding-fast, NaN-permissive) and the textbook's
 *       structure (ring-axiomatic, total-order-reflexive).  This
 *       partition pins where the second master is refused.
 */
module;

#include <cmath>  // std::isfinite (the finite-subset gate)
#include <concepts>
#include <functional>
#include <optional>  // Kleisli lift F -> Maybe<safe_float<F>>

export module dedekind.morphologies:floating_point;

import dedekind.algebra;  // HasFieldOperators / HasRingOperators
                          // (operator-surface positives)
import dedekind.category; // category::IsRing / category::IsField --- the
                          // axiomatic forms whose negation is the umbrella
                          // claim (algebra::IsField composes these with
                          // IsDivisionRing's operator surface, so its
                          // negation is too strong: it would also fire on
                          // a missing operator/, not just on broken axioms).
import dedekind.order;    // HasPartialOrderOperators / HasTotalOrderOperators
                          // / IsTotallyOrdered (negation pinned here)

namespace dedekind::morphologies {

/** @section floating_point__Formal_Verification std::floating_point family
 * witnesses */

// (1) Operator surface — every std::floating_point carrier carries the
//     full field-operator shape and the partial- / total-order-operator
//     surface.  These are positive mechanical pins; the corresponding
//     trait specialisations live in :algebra and :order.

static_assert(dedekind::algebra::HasFieldOperators<float>,
              "float carries the field operator surface (+, -, *, /).");
static_assert(dedekind::algebra::HasFieldOperators<double>,
              "double carries the field operator surface (+, -, *, /).");
static_assert(dedekind::algebra::HasFieldOperators<long double>,
              "long double carries the field operator surface (+, -, *, /).");

static_assert(dedekind::algebra::HasRingOperators<float>,
              "float carries the ring operator surface (+, -, *).");
static_assert(dedekind::algebra::HasRingOperators<double>,
              "double carries the ring operator surface (+, -, *).");
static_assert(dedekind::algebra::HasRingOperators<long double>,
              "long double carries the ring operator surface (+, -, *).");

static_assert(
    dedekind::order::HasPartialOrderOperators<float>,
    "float carries the partial-order operator surface (<, <=, >, >=).");
static_assert(dedekind::order::HasPartialOrderOperators<double>,
              "double carries the partial-order operator surface.");
static_assert(dedekind::order::HasPartialOrderOperators<long double>,
              "long double carries the partial-order operator surface.");

static_assert(dedekind::order::HasTotalOrderOperators<float>,
              "float carries the total-order operator surface (the IEEE 754 "
              "spaceship operator); semantically partial under NaN, but the "
              "operator surface itself is full.");
static_assert(dedekind::order::HasTotalOrderOperators<double>,
              "double carries the total-order operator surface (IEEE 754 "
              "spaceship); semantically partial under NaN.");
static_assert(dedekind::order::HasTotalOrderOperators<long double>,
              "long double carries the total-order operator surface.");

// (2) Axiomatic rejections — none of the std::floating_point carriers
//     witness the strict ring / field / total-order concepts.
//     Rounding defeats associativity (so IsRing fails); NaN defeats
//     reflexivity of <= (so IsTotallyOrdered fails); IsField inherits
//     the IsRing rejection.  These pins are filed here at the umbrella
//     level so the std::floating_point reader sees the dichotomy in
//     one place.

static_assert(
    !dedekind::category::IsRing<float, std::plus<float>,
                                std::multiplies<float>>,
    "float is NOT an axiomatic ring under std::plus / std::multiplies: "
    "IEEE 754 rounding defeats associativity ((a+b)+c != a+(b+c) in general).");
static_assert(!dedekind::category::IsRing<double, std::plus<double>,
                                          std::multiplies<double>>,
              "double is NOT an axiomatic ring under std::plus / "
              "std::multiplies: IEEE 754 rounding defeats associativity.");
static_assert(!dedekind::category::IsRing<long double, std::plus<long double>,
                                          std::multiplies<long double>>,
              "long double is NOT an axiomatic ring: same reason as float / "
              "double (IEEE 754 rounding).");

// Pin the @b axiomatic field rejection via @c category::IsField (which
// requires the species-trait structure including @c IsAbelianGroup<T,
// Mult>) rather than @c algebra::IsField (which composes the axiomatic
// concept with @c IsDivisionRing's operator surface --- @c .inverse() /
// @c std::divides). The umbrella claim is that the field @b axioms
// don't hold; pinning the algebra:: form would also fire on a missing
// operator/ surface, which is the wrong kind of rejection here.
static_assert(!dedekind::category::IsField<float, std::plus<float>,
                                           std::multiplies<float>>,
              "float is NOT an axiomatic field under std::plus / "
              "std::multiplies: the field axioms inherit ring closure, "
              "which fails under IEEE 754 rounding.");
static_assert(!dedekind::category::IsField<double, std::plus<double>,
                                           std::multiplies<double>>,
              "double is NOT an axiomatic field under std::plus / "
              "std::multiplies: same reason as float (rounding defeats "
              "ring axioms).");
static_assert(!dedekind::category::IsField<long double, std::plus<long double>,
                                           std::multiplies<long double>>,
              "long double is NOT an axiomatic field: same reason as "
              "float / double.");

static_assert(!dedekind::order::IsTotallyOrdered<float>,
              "float is NOT IsTotallyOrdered: IEEE 754 NaN <= NaN evaluates "
              "to false, so reflexivity of <= fails.  Use IsOrderedField "
              "(morphologies/archimedean.cppm) for the operational reading.");
static_assert(!dedekind::order::IsTotallyOrdered<double>,
              "double is NOT IsTotallyOrdered: NaN breaks reflexivity.  "
              "Use IsOrderedField for the operational reading.");
static_assert(!dedekind::order::IsTotallyOrdered<long double>,
              "long double is NOT IsTotallyOrdered: NaN breaks reflexivity.");

// (3) Positive reconstruction on the finite subset -------------------------
//
// Claim (2) isolates NaN as the sole obstruction to reflexivity of <=.
// Filter it out (finite = neither NaN nor +/-inf) and the order is total
// again, so the subset is a genuine distributive lattice under (min, max):
// the "double is sound after all" certificate, complementary to the
// negative axiomatic pins above.  min / max select an operand -- no
// arithmetic, no rounding -- so unlike (+, x) they close on the subset and
// are exact and associative (a selection, not arithmetic; minsd / maxsd
// -eligible).  This is the first
// slice of the safe-float refined-type discipline (#496); the lattice pins
// realise the carrier-lattice showcase (#452) / carrier-reflex sweep (#544)
// for the IEEE carrier whose exact-real tension #191 tracks.
//
// (safe_float carries the FINITE invariant of #496; the lattice needs only
//  !isnan, so a bare-NaN-free variant would retain +/-inf as top / bottom.)

/** @brief Finite IEEE-754 subset: the invariant @c std::isfinite makes the
 *  order total, so (unlike raw @c F) this carrier reflexively compares.
 *
 *  @details The representative is @b private and the sole construction path
 *  is @c lift / @c try_safe_float: aggregate-initialising a NaN is ill-formed,
 *  so the reflexivity certificate below rests on a @b guarantee, not a
 *  convention.  This is the enforced form of the #496 @c safe_float.
 *
 *  @note @b Signed @b zeros.  @c -0.0 and @c +0.0 are distinct bit patterns
 *  that compare @b equal, so @c <= is antisymmetric only @e up @e to @c ==,
 *  which the defaulted @c operator== absorbs (they collapse to one lattice
 *  element).  A hostile reader will poke here; the collapse is sound. */
export template <std::floating_point F>
class safe_float {
  F value_;  ///< invariant: std::isfinite(value_), enforced by the private ctor
  constexpr explicit safe_float(F x) noexcept : value_(x) {}  // the gate

 public:
  /** @brief Kleisli lift @f$F \to
   *  \mathrm{Maybe}\langle\mathtt{safe\_float}\rangle@f$: @c nullopt on the
   *  rejected boundary (NaN / +/-inf), else the finite value.  Sole entry. */
  [[nodiscard]] static constexpr std::optional<safe_float> lift(F x) noexcept {
    if (std::isfinite(x)) return safe_float(x);
    return std::nullopt;
  }

  /** @brief Read the finite representative. */
  [[nodiscard]] constexpr F value() const noexcept { return value_; }

  friend constexpr auto operator<=>(const safe_float&,
                                    const safe_float&) noexcept = default;
  friend constexpr bool operator==(const safe_float&,
                                   const safe_float&) noexcept = default;
};

/** @brief Free-function spelling of @c safe_float<F>::lift. */
export template <std::floating_point F>
[[nodiscard]] constexpr std::optional<safe_float<F>> try_safe_float(
    F x) noexcept {
  return safe_float<F>::lift(x);
}

/** @brief @f$\mathbb{L}\langle F\rangle@f$ --- the algebraic handle for the
 *  finite-float @b lattice.  Names the @c (min, max) lattice @b reduct only;
 *  the carrier's @c (+, x) arithmetic is deliberately @b not claimed (it is
 *  non-associative under rounding).  @c safe_float<F> is the ASCII spelling. */
export template <std::floating_point F>
using 𝕃 = safe_float<F>;

}  // namespace dedekind::morphologies

namespace dedekind::category {

// The finite invariant restores the order axioms that raw @c double lacks
// (NaN breaks reflexivity); on @c safe_float they hold SOUNDLY.  Mirrors the
// Rational<I> registration in numbers/rational.cppm.
template <std::floating_point F>
inline constexpr bool
    is_reflexive_v<dedekind::morphologies::safe_float<F>, std::less_equal<>> =
        true;
template <std::floating_point F>
inline constexpr bool
    is_transitive_v<dedekind::morphologies::safe_float<F>, std::less_equal<>> =
        true;
template <std::floating_point F>
inline constexpr bool is_antisymmetric_v<dedekind::morphologies::safe_float<F>,
                                         std::less_equal<>> = true;

}  // namespace dedekind::category

namespace dedekind::morphologies {

// The positive lattice certificates for the finite double subset.
static_assert(dedekind::order::IsTotallyOrdered<safe_float<double>>,
              "safe_float<double> IS totally ordered: excluding NaN restores "
              "reflexivity of <=, the sole obstruction on raw double.");
// Pin the full DISTRIBUTIVE lattice, not the two semilattices separately: this
// bundles meet + join + absorption + both distributivity directions, so a
// future concept/trait change cannot leave 𝕃 passing only the weaker
// semilattice checks (min/max distributivity + absorption ride the blanket
// registrations in category/species.cppm; a chain is distributive).  min/max
// are selections -- exact and associative -- so minsd/maxsd-eligible.
static_assert(dedekind::order::IsOrderDistributiveLattice<safe_float<double>>,
              "safe_float<double> is a distributive lattice under (min, max).");

// The 𝕃 handle in action: 𝕃<double> IS the finite-double distributive lattice.
static_assert(
    dedekind::order::IsOrderDistributiveLattice<𝕃<double>>,
    "𝕃<double> is the finite-double distributive lattice (min, max).");

}  // namespace dedekind::morphologies
