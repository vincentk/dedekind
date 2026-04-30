/**
 * @file dedekind/morphologies/archimedean.cppm
 * @partition :archimedean
 * @brief Archimedean / ordered-field convergence stubs.
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
 * (cf.\ @c HasRingOperators vs @c IsRing).  The two concepts are
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
 *
 * @section archimedean__Vocabulary_Notes_388
 *
 * The @c T::successor(x) member API exposed by @c IsCyclic carriers
 * coincides numerically with the Peano successor @f$S(x) = x + 1@f$
 * but is a different beast in the type system from the axiomatic
 * @c dedekind::order::IsSuccessor<T> concept (in
 * @c dedekind.order:completeness).  The pairing is:
 *
 *   - @b Carrier-level: a static member @c T::successor(x) returning
 *     a Domain element.  Operational; what the @c IsCyclic shape
 *     concept reaches for when walking a cyclic chain.
 *   - @b Concept-level: @c order::IsSuccessor<T>, asserting @c x +
 *     identity_v<T, std::multiplies<T>> is well-typed and that @c T
 *     is a partial magma + multiplicatively pointed.  Axiomatic;
 *     the trait the @c order::IsArchimedean composition demands.
 *
 * The two coincide on carriers like @c Modular<N> that satisfy both
 * surfaces.  The coherence witness
 * @c Modular<N>::successor(x) == x + Modular<N>{1} is pinned at the
 * type level in @c src/main/modules/dedekind/morphologies/cyclic.cppm
 * (alongside the @c IsCyclic / @c IsCyclicGroup chains for the same
 * carrier).  Re-expressing @c order::IsArchimedean purely in terms
 * of @c :order concepts is deliberately not done here: the additive
 * partial-magma + pointed multiplicative-identity composition is the
 * algebraic content the Archimedean axiom genuinely needs, and the
 * order layer is the right home for it.
 *
 * @section archimedean__Modern_Bridge_Archimedean_Generators
 *
 * The Peano successor and the Archimedean property are unified in
 * modern probability through the @b Archimedean @b generator of
 * copula theory: a strictly decreasing convex function
 * @f$\varphi : [0, 1] \to [0, \infty]@f$ with the additive law
 * @f[
 *   \varphi(C(u, v)) \;=\; \varphi(u) \,+\, \varphi(v),
 * @f]
 * where @c C is an Archimedean copula.  Two threads of the library
 * meet in this framing:
 *
 *   - @b Peano: the successor recursion @f$S(n) = n + 1@f$ is the
 *     finite analogue of @f$\varphi@f$'s additive layer-stacking
 *     under @c C.  Adding 1 once corresponds to composing one more
 *     dependency layer via @f$\varphi^{-1}@f$.
 *   - @b Archimedean: the order-theoretic scale axiom (every element
 *     reachable from a base point by finite iteration of @c S) is
 *     the discrete analogue of the copula-theoretic d-monotonicity
 *     condition --- @f$\varphi@f$'s pseudo-inverse must be
 *     "sufficiently large" for the dependency to scale across any
 *     number of variables.
 *
 * This is the deeper motivation behind the project's twin
 * @c successor / @c generator vocabulary: the @c IsCyclic carrier
 * supplies a discrete generator @c g and a successor walk
 * @c g, S(g), S(S(g)), \ldots; an @c order::IsArchimedean carrier
 * supplies the same shape at the axiom level; an Archimedean copula
 * lifts the same shape into the continuous unit interval.  A
 * dedicated @c :copula partition is out of scope here, but the
 * framing is recorded so future work can connect the discrete and
 * continuous Archimedean stories without re-deriving the bridge.
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

/** @section archimedean__Formal_Verification */

// double is the canonical ordered Archimedean field under machine semantics.
static_assert(
    IsOrderedField<double>,
    "double must satisfy IsOrderedField (regular + totally ordered).");
static_assert(IsArchimedeanField<double>,
              "double must satisfy IsArchimedeanField (x + 1 is defined).");

}  // namespace dedekind::morphologies
