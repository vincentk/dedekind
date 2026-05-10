/**
 * @file
 * src/test/cpp/modules/dedekind/python/showcase_10_image_intensional_naturals.cpp
 * @brief Showcase 10 — image of an intensional Set over ℕ under an arrow.
 *
 *   image( s : ℕ → ℕ , {n ∈ ℕ | n > 5} )
 *      ≡   {y ∈ ℕ | TernaryLogic::Unknown}
 *
 * Layer-1 entry per #602: the @c image overload for intensional
 * predicate-defined Sets completes the API surface alongside the
 * extensional cases (SingletonSet, std::set, std::unordered_set).
 *
 * Categorically the image of @c {x | P(x)} under @c f: T → U is
 * @c {y | ∃x ∈ T. P(x) ∧ y == f(x)}.  On a transfinite carrier the
 * existential is generally undecidable without further structural input
 * (a partial inverse for @c f, or finiteness of @c T); this overload
 * honestly tags the result as @c TernaryLogic and returns @c Unknown
 * for every membership query.  The indecision is a compile-time
 * observable rather than a hidden assumption.
 *
 * Specialised overloads for monic arrows with structural inverses, or
 * for finite source carriers, can decide membership and are layer 2 of
 * #602 (per-arrow / per-carrier dispatch).  This showcase exercises
 * the layer-1 default.
 *
 * Expected LLVM IR: @c ret @c i1 @c false for the Unknown-vs-True
 * comparison in @c witness_image_intensional_unknown.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */

#include <concepts>

import dedekind.category;
import dedekind.numbers;
import dedekind.order; // For bound<V> (NTTP-pivoted halfspace builder)
import dedekind.sets;

using namespace dedekind::category;
using namespace dedekind::numbers;
using namespace dedekind::order;
using namespace dedekind::sets;

// Source: an intensional predicate-defined Set over ℕ.  The set
// {n ∈ ℕ | n > 5} is structurally a Halfspace with NTTP pivot 5; the
// underlying carrier is Cardinality (the variant ℕ-proxy post-#402).
constexpr auto n = element<ℕ>;
constexpr auto gt_5 = Set{n | (n > bound<5>)};

// Arrow ℕ → ℕ wrapped via the project's typed-arrow factory so it
// satisfies @c IsArrow (which gates on @c Domain / @c Codomain
// typedefs that the bare @c cardinality_succ struct does not expose).
// Semantically this is @c cardinality_succ — the NNO successor on
// @c Cardinality, the canonical ℕ → ℕ arrow.
constexpr auto succ_arrow = arrow<Cardinality, Cardinality>(
    [](const Cardinality& m) noexcept { return m + finite_cardinality(1); });

// Image of an intensional Set under the successor arrow.  Result is
// itself an intensional Set on Cardinality with @c TernaryLogic as the
// logic species and the always-Unknown @c SymbolicImagePredicate as
// predicate.
constexpr auto img = image(succ_arrow, gt_5);

// Type-shape witnesses: the result IS a Set on the codomain
// (Cardinality) with TernaryLogic.  These pin the layer-1 API surface
// at the type level — @c image is well-formed on intensional inputs and
// returns the right structural shape regardless of the (un)decidability
// of the membership query.
static_assert(std::same_as<typename decltype(img)::Domain, Cardinality>,
              "image(f, intensional Set<T>) lands on Cod<f> = ℕ.");
static_assert(std::same_as<typename decltype(img)::logic_species, TernaryLogic>,
              "image of an intensional Set lifts the result to TernaryLogic — "
              "the existential ∃x ∈ T. P(x) ∧ y == f(x) is generally "
              "undecidable on transfinite carriers without further structural "
              "input.");

// Computability classification: the layer-1 image preserves NEITHER
// extensionality NOR decidable membership — the predicate is symbolic.
// Specialisations (layer 2) can decide for monic arrows with structural
// inverses, or for finite source carriers; here we exercise the default.
static_assert(!HasDecidableMembership<decltype(img)>,
              "Layer-1 image of an intensional Set is symbolic — "
              "membership query is honestly TernaryLogic::Unknown.");
static_assert(!IsExtensional<decltype(img)>,
              "Layer-1 image preserves the intensional shape; the result "
              "remains predicate-defined on the codomain carrier.");

// Membership-query witnesses: any value lands at @c TernaryLogic::Unknown.
// The existential @c ∃x ∈ ℕ. x > 5 ∧ y == x + 1 is undecidable on the
// transfinite carrier without further structural input — the symbolic
// predicate honestly admits the indecision.
static_assert(img(finite_cardinality(7)) == Ternary::Unknown,
              "img(7) is honestly Unknown — even though set-theoretically "
              "7 ∈ image(succ, {n>5}) (since 6>5 and succ(6)=7), the "
              "layer-1 symbolic predicate cannot decide this.");
static_assert(img(finite_cardinality(0)) == Ternary::Unknown,
              "img(0) is honestly Unknown — even though set-theoretically "
              "0 ∉ image(succ, {n>5}) (since succ has no zero in the image "
              "of a set bounded below by 5), the layer-1 symbolic "
              "predicate cannot decide this either.");

/**
 * @brief Showcase 10: image-of-intensional-Set on ℕ honestly returns
 *        Unknown.
 *
 * The image's membership query is statically @c TernaryLogic::Unknown,
 * so comparing against @c TernaryLogic::True folds to constant @c false
 * at compile time.  This is the structural payoff of the layer-1 API
 * surface — the indecision is a compile-time observable rather than a
 * hidden assumption (a runtime exception, an UB, a silent wrong
 * answer).
 *
 * Expected IR: @c ret @c i1 @c false
 */
extern "C" __attribute__((noinline)) bool witness_image_intensional_unknown() {
  return img(finite_cardinality(7)) == Ternary::True;
}
