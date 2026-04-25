/**
 * @file ontology:morphologies.cppm
 * @partition :archimedean
 * @brief Level 3.5b: Convergence morphology stubs for experimental
 * reintegration.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note चतुराधिकं शतमष्टगुणं द्वा षष्ठीस्तथा सहस्रद्वयम्। — Aryabhata,
 * *Aryabhatiya*, Ganitapada 10 (499 CE). [Sanskrit original; Trans: "Add four
 * to 100, multiply by eight, and then add 62,000. This is the measure of the
 * circumference of a circle whose diameter is 20,000." Yields π ≈ 3.1416,
 * accurate to 4 decimal places.]
 */
module;

#include <cmath>
#include <concepts>

export module dedekind.morphologies:archimedean;

import dedekind.sequences;

namespace dedekind::morphologies {
using namespace dedekind::sequences;

/**
 * @concept IsCyclic
 * @brief \emph{Operational} duck-typed check that @c T carries the
 *        member API of a cyclic structure: @c T::Domain alias,
 *        @c T::generator() returning a Domain element, and
 *        @c T::successor(a) walking the chain.
 *
 * @details This is the syntactic counterpart of the categorical
 * @c dedekind::category::IsCyclicGroup<T, Op> in the strict /
 * operational pattern the project uses elsewhere
 * (cf.\ @c IsRingLike vs @c IsRing).  The two concepts are
 * complementary:
 *
 *   - @c IsCyclic<T> --- "this carrier exposes the shape" (member
 *     API for generator + successor).  Cheap to check; useful for
 *     duck-typed templated algorithms that walk a cyclic chain.
 *   - @c category::IsCyclicGroup<T, Op> --- "this carrier under
 *     this operation forms a cyclic abelian group" (axiomatic, via
 *     @c IsAbelianGroup<T, Op> + the @c is_cyclic_group_v<T, Op>
 *     opt-in trait).  More demanding; certifies the laws.
 *
 * Carriers that satisfy both --- e.g.\ @c morphologies::Modular<N>
 * --- are positively certified at both layers.  Other carriers,
 * such as @c morphologies::CyclicRing<T, N>, may satisfy only the
 * operational layer unless separately registered with the
 * categorical trait machinery.
 */
export template <typename T>
concept IsCyclic = requires {
  typename T::Domain;
  { T::generator() } -> std::same_as<typename T::Domain>;
} && requires(typename T::Domain a) {
  { T::successor(a) } -> std::same_as<typename T::Domain>;
};

export template <typename T>
concept IsSimplyInfinite =
    IsCyclic<T> && requires { typename T::cardinality_type; };

export template <typename T>
concept IsCyclicRing = IsCyclic<T> && requires(T a, T b) {
  { a + b } -> std::same_as<T>;
  { a * b } -> std::same_as<T>;
};

export template <typename T>
concept IsOrderedField = std::regular<T> && std::totally_ordered<T>;

export template <typename T>
concept IsArchimedeanField = IsOrderedField<T> && requires(T a) {
  { a + T{1} } -> std::same_as<T>;
};

export template <typename T>
concept IsDedekindCompleteField = IsArchimedeanField<T>;

export template <typename S>
concept IsMinkowskiSummable = requires(S a, S b) {
  { a + b } -> std::same_as<S>;
};

export template <typename Seq>
concept IsCauchy = IsSequence<Seq> && requires(Seq s) {
  { s.at(0) } -> std::same_as<typename Seq::Codomain>;
  { std::abs(s.at(0) - s.at(0)) };
};

export template <typename Seq>
concept IsConvergent = IsCauchy<Seq> && requires(Seq s) {
  { limit(s) } -> std::same_as<typename Seq::Codomain>;
};

export template <typename T>
struct CauchyPath : public Path<T> {
  using Path<T>::Path;
  using is_cauchy_tag = void;
};

/** @section Formal_Verification */

// double is the canonical ordered Archimedean field under machine semantics.
static_assert(
    IsOrderedField<double>,
    "double must satisfy IsOrderedField (regular + totally ordered).");
static_assert(IsArchimedeanField<double>,
              "double must satisfy IsArchimedeanField (x + 1 is defined).");

}  // namespace dedekind::morphologies
