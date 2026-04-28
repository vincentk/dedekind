/**
 * @file dedekind/numbers/floating_point.cppm
 * @partition :floating_point
 * @module dedekind.numbers:floating_point
 * @brief Level 4: The std::floating_point family — operator surface ✓ +
 *        operational ordered-field reading ✓, axiomatic ring / field /
 *        total order ✗ (IEEE 754 Honest Rejection: rounding breaks
 *        associativity; NaN breaks reflexivity).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Honest_Stance
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
 * @section Honesty_Obligation
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

#include <concepts>
#include <functional>

export module dedekind.numbers:floating_point;

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

namespace dedekind::numbers {

/** @section Formal_Verification — std::floating_point family witnesses */

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

}  // namespace dedekind::numbers
