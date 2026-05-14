/**
 * @file dedekind/algebra/scout_algebra.cppm
 * @partition :scout_algebra
 * @brief Symbolic scout type for the in-line comprehension surface,
 *        gated on algebraic structure of the carrier (#664).
 *
 * @section scout_algebra__Motivation
 *
 * The algebraic constraints give rise to the symbolic scout type.  A
 * @c GroupScout<T,Op,Element,Inner> exists when @c (T, @c Op) is at
 * least a monoid; per-pipe @c requires-clauses additionally check the
 * structural algebraic claim (group / abelian group / field / ring)
 * needed for that pipe's halfspace-transport semantics.  The gate is
 * purely algebraic, in textbook vocabulary, not in implementation-
 * specific concepts.  This partition is the
 * algebraic foundation for #664's in-line scout-algebra surface
 * @c Set{f(x) @c | @c P(x)}: once a scout is a valid algebraic
 * expression, the comprehension pipe transports the source predicate
 * structurally and yields a typed @c Set with the @b shifted predicate.
 *
 * @section scout_algebra__Design
 *
 * One basic type, gated on @c IsMonoid<T,Op> at the class level
 * (relaxed from @c IsGroup<T,Op> for #664 Slice 5's ring-retract
 * case --- @c (ℤ, std::multiplies) is a monoid not a group, and the
 * ring-retract pipe certifies the algebraic claim it actually needs).
 * Group-shaped structure is still enforced per-pipe via
 * @c requires-clauses on @c IsOrderedAdditiveGroup /
 * @c IsOrderedMultiplicativeGroup; the ring-retract pipe instead
 * gates on @c IsOrderedCommutativeRing.  Named specialisations via
 * operator overloads --- not template specialisations of the type
 * itself, but factory operator overloads that produce specific
 * scout instances:
 *
 *  - @c operator+(BoundScout<T>, @c Bound<k>) for
 *    @c IsAdditiveGroup<T> --- produces a
 *    @c GroupScout<T, @c std::plus<T>, @c k, @c BoundScout<T>>.
 *  - @c operator-(BoundScout<T>, @c Bound<k>) for
 *    @c IsAdditiveGroup<T> --- produces the group-inverse shift:
 *    @c GroupScout<T, @c std::plus<T>, @c -k, @c BoundScout<T>>
 *    (subtraction encoded via the additive inverse).
 *  - @c operator*(BoundScout<T>, @c Bound<k>) for
 *    @c IsAbelianGroup<T, @c std::multiplies<T>> (i.e.\ a field
 *    carrier, gated on PR #674's @c IsField cert) --- produces
 *    @c GroupScout<T, @c std::multiplies<T>, @c k, @c BoundScout<T>>.
 *  - @c operator+(GroupScout, @c Bound<k>) and
 *    @c operator*(GroupScout, @c Bound<k>) (Slice 4): @b composition
 *    factories lifting the above to a @c GroupScout LHS, so the
 *    canonical @b affine syntax @c (in<T> @c * @c bound<m>) @c + @c
 *    bound<k> = @c λx.\,m*x @c + @c k compiles.  Composition is
 *    @b recursive: nesting any depth of @c GroupScout layers (e.g.\
 *    @c ((x @c + @c 1) @c * @c 2) @c + @c 3) constructs and pipes
 *    correctly --- each recursive pipe call defers to @c Inner{} @c |
 *    @c hs, which terminates at the @c BoundScout base case.
 *  - @c operator*(BoundScout<T>, @c Bound<k>) for
 *    @c IsOrderedCommutativeRing<T> @b without
 *    @c IsOrderedMultiplicativeGroup<T> (Slice 5, ring-retract):
 *    activates on @c ℤ (the initial ring, not a field).  Produces
 *    the same @c GroupScout shape, but the @b ring-retract @b pipe
 *    overload produces an @c AffineImageOfHalfspace predicate
 *    (divisibility check + source halfspace on the preimage) ---
 *    @c {2n @c | @c n @c > @c 5} on @c ℤ has membership
 *    @c y @c ∈ @c image @c iff @c y @c % @c 2 @c == @c 0 @c &&
 *    @c y/2 @c > @c 5.  Composition with @c + on ring-retract
 *    scouts is deferred (would extend @c AffineImageOfHalfspace with
 *    an offset; out of scope for this slice).
 *
 * The comprehension pipe @c GroupScout::operator|(Halfspace) performs
 * @b halfspace-pivot @b transport.  The additive pipe shifts the
 * source pivot by the scout's @c Element along the additive group
 * action.  The multiplicative pipe (Slice 3) scales the source pivot,
 * preserves direction when @c k > 0, flips direction when @c k < 0,
 * and Honest-Rejects @c k = 0 (collapsing-halfspace case).  Slice 4's
 * @b composition pipes recurse: inner-first transport, then outer's
 * pipe on top, with the same direction-flip rules.  All yield a
 * @c Comprehension that Set CTAD picks up and resolves to a @c Set
 * with the transported halfspace as predicate.  No Set CTAD changes
 * needed; the existing @c Comprehension path in @c :sets:expressions
 * is the consumer.
 *
 * @section scout_algebra__Honest_Rejection
 *
 * Carriers that do @b not satisfy @c IsAdditiveGroup --- e.g.\
 * @c sets::Cardinality (ℕ has no additive inverses), signed @c int
 * (overflow is UB so the group axioms fail) --- cause the operator
 * overload to be removed from the candidate set.  The compile error
 * names the missing textbook axiom, not a library-specific failure.
 *
 * @section scout_algebra__Out_Of_Scope_For_This_Partition_Today
 *
 *  - Composition of a ring-retract scout with an additive shift
 *    (e.g.\ @c bound<2> @c * @c in<ℤ> @c + @c bound<1> for the
 *    issue's canonical witness @c {2n+1 @c | @c n @c > @c 5}):
 *    the additive composition pipe today expects the inner's
 *    result predicate to be a @c Halfspace exposing @c pivot @c /
 *    @c direction @c / @c strictness static members.  The
 *    ring-retract pipe produces an @c AffineImageOfHalfspace
 *    instead.  Lifting requires either (a) extending
 *    @c AffineImageOfHalfspace with an offset @c B and adding a
 *    composition overload that updates @c B, or (b) a separate
 *    affine-on-ring predicate type.
 *  - Division @c operator/(BoundScout<T>, @c Bound<k>) for fields:
 *    semantically @c x @c / @c k @c = @c x @c * @c k^{-1}.  Computing
 *    @c k^{-1} at the factory site embeds a non-trivial @c T value
 *    (e.g.\ @c Rational{1,k}) into the resulting scout's @c Element,
 *    which forces @c T to be a structural NTTP carrier ---
 *    incompatible with the variant-carrier ℚ today.  Follow-up.
 *  - (No further composition-depth limitation: the Slice-4 recursive
 *    pipes handle 2-layer @b and deeper nesting through the same
 *    @c Inner{} @c | @c hs recursion.  Pinned by the 3-layer test in
 *    @c q_scout_algebra_test.cpp.)
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */
module;

#include <concepts>
#include <cstddef>
#include <functional>
#include <type_traits>

export module dedekind.algebra:scout_algebra;

import dedekind.category; // IsGroup<T, Op>
import dedekind.sets;     // BoundScout<auto>, SignedExtensionalCardinal
import dedekind.order;    // Bound<auto>, IsTotallyOrdered
import :field;            // IsField<T, Add, Mult> (the multiplicative-group +
                          // ordered-field gate)
import :group;            // IsAdditiveGroup<T, Add>

namespace dedekind::algebra {

/**
 * @brief Marker trait: @c T is a non-cyclic ordered additive group
 *        whose order is translation-invariant, with the saturating
 *        escape contract for values that would exceed representable
 *        capacity.
 *
 * @details
 * Defaults to @c false.  Carriers opt in via specialisation when their
 * arithmetic is @b not @b cyclic --- i.e.\ they @b saturate or
 * @b escalate (e.g.\ to @f$\pm \aleph_0@f$ sentinels) rather than
 * wrapping at the representable bound.  Saturation preserves the
 * partial order @f$\le@f$ under translation (if @c a @c <= @c b then
 * @c a+c @c <= @c b+c, including the case where both saturate to the
 * same sentinel); wrapping does @b not (the modular wrap reverses the
 * order at the boundary).
 *
 * @note The structural fact this marker discriminates is @b not
 *       boundedness (every C++ carrier is bounded), but @b cyclicity.
 *       A saturating carrier is bounded but non-cyclic: arithmetic
 *       past the bound escalates to a distinguishable sentinel
 *       (@f$\pm \aleph_0@f$ or @c NaZ for indeterminate forms in the
 *       project's @c SignedCardinality variant), keeping
 *       translation-invariance honest @b including at the boundary.
 *       Modular carriers (@c unsigned @c int, the finite
 *       @c sets::SignedExtensionalCardinal<N>) wrap, reversing order;
 *       they fail the marker by default.
 *
 * @note The project put deliberate effort into the saturating
 *       variants (@c sets::SignedCardinality, @c sets::Cardinality)
 *       precisely so they would be bona-fide proxies for
 *       @f$\mathbb{Z}@f$ / @f$\mathbb{N}@f$ rather than cyclic
 *       (cf.\ @c cardinality.cppm:878-967 --- "@c SignedCardinality
 *       is the signed counterpart of @c Cardinality (the ℕ ∪ ℵ_0
 *       variant): ... saturating ... not periodic ... the library's
 *       bona-fide proxy for ℤ modulo physical limits").  The marker
 *       opt-in below honours that effort: it is specialised for
 *       @c SignedCardinality, @b not for the cyclic finite-fragment
 *       @c SignedExtensionalCardinal<N>.
 */
export template <typename T>
struct is_translation_invariant_ordered : std::false_type {};

/** @brief @c sets::SignedCardinality is the project's bona-fide
 *  saturating proxy for @f$\mathbb{Z}@f$: arithmetic escalates to
 *  @f$\pm \aleph_0@f$ on overflow and propagates @c NaZ on
 *  indeterminate forms, rather than wrapping modulo capacity
 *  (cf.\ @c cardinality.cppm:923-967).  This is the structural
 *  guarantee the marker certifies: translation preserves the partial
 *  order @f$\le@f$ on the finite fragment; saturation collapses
 *  ordering past the boundary into the @f$\pm \aleph_0@f$ sentinel
 *  (the meet still holds: both sides land at the same sentinel).  The
 *  finite-fragment carrier @c SignedExtensionalCardinal<N> is
 *  intentionally @b not opted in: it is cyclic
 *  (mod @f$2^{N \cdot 64}@f$), so its addition would reverse the
 *  order at the wrap boundary.  The variant @c SignedCardinality is
 *  what callers should use whenever they want translation-invariance
 *  at the type level (i.e.\ the halfspace pipe). */
export template <>
struct is_translation_invariant_ordered<dedekind::sets::SignedCardinality>
    : std::true_type {};

export template <typename T>
inline constexpr bool is_translation_invariant_ordered_v =
    is_translation_invariant_ordered<T>::value;

// No multiplicative-side marker is exported: unlike the additive case
// (where the @c is_translation_invariant_ordered marker carries the
// order claim past @c SignedCardinality 's partial-ordering @c <=>),
// the multiplicative case's consumers are ordered-field carriers
// (e.g.\ @c Rational<I>) whose @c <=> is @c std::strong_ordering and
// which therefore satisfy @c std::totally_ordered structurally.  The
// composition @c algebra::IsField<T> @c && @c order::IsTotallyOrdered<T>
// in @c IsOrderedMultiplicativeGroup below is the upstream-rooted
// expression of "ordered multiplicative group on the non-zero cone";
// a separate marker would only re-state what the upstream concepts
// already prove.

/**
 * @concept IsOrderedAdditiveGroup
 * @brief An additive group whose order is translation-invariant.
 *
 * @details
 * Combines the algebraic gate (@c IsAdditiveGroup) with the @b axiom
 * marker (@c is_translation_invariant_ordered_v).  This is the right
 * precondition for halfspace-pivot transport: shifting
 * @f$\{x \mid x > k\}@f$ by @c +c yields @f$\{y \mid y > k+c\}@f$
 * exactly when @c c-translation preserves the order, which is what
 * the marker certifies.
 *
 * @note Earlier drafts also required @c std::totally_ordered<T>.
 *       Dropped because the project's saturating ℤ proxy
 *       (@c sets::SignedCardinality, a @c std::variant) uses
 *       custom comparison operators (specialised on the saturating
 *       semantics and on @c NaZ propagation), which do not satisfy
 *       the @c std::totally_ordered structural concept --- yet the
 *       carrier @b is the right one for translation-invariant
 *       ordered-group semantics.  The marker carries the order claim;
 *       a separate @c std::totally_ordered structural check would
 *       Honest-Reject the very carriers the marker is supposed to
 *       accept.  The structural order check belongs at the marker
 *       opt-in site, not at the concept.
 *
 * Modular carriers (@c unsigned @c int as @f$\mathbb{Z}/2^N\mathbb{Z}@f$)
 * satisfy @c IsAdditiveGroup but NOT this concept --- they fail the
 * marker by default.  Honest Rejection on halfspace-pipe attempts with
 * modular carriers.
 */
export template <typename T>
concept IsOrderedAdditiveGroup =
    dedekind::category::IsAbelianGroup<T, std::plus<T>> &&
    is_translation_invariant_ordered_v<T>;

/**
 * @concept IsOrderedMultiplicativeGroup
 * @brief An ordered field --- the textbook home of multiplicative
 *        scaling of halfspaces.
 *
 * @details
 * Composes two upstream concepts directly:
 *   * @c algebra::IsField<T> --- @c T is a field (post-PR #674's
 *     @c IsField cert on @c Rational<I>), so the multiplicative
 *     group on the non-zero cone @c (T \\ {0}, @c *) is an abelian
 *     group with multiplicative inverses.
 *   * @c order::IsTotallyOrdered<T> --- @c T's @c <=> decides every
 *     pair via @c std::totally_ordered.  Combined with the field
 *     axiom, this gives an @b ordered @b field; the order is
 *     automatically compatible with @c * via the textbook
 *     positive/negative dichotomy.
 *
 * This is the precondition for halfspace-pivot transport under
 * scaling: scaling @f$\{x \mid x > k\}@f$ by @c k_E yields
 * @f$\{y \mid y > k \cdot k_E\}@f$ when @c k_E > 0 and the
 * direction-flipped @f$\{y \mid y < k \cdot k_E\}@f$ when @c k_E < 0.
 *
 * No carrier-promise marker (unlike the additive sibling): the
 * upstream concepts cover everything.  @c Rational<I> satisfies both
 * (@c IsField via PR #674's @c is_invertible_v registration,
 * @c IsTotallyOrdered via @c Rational 's @c std::strong_ordering
 * @c <=>); @c bool / 𝔽₂ satisfies @c IsTotallyOrdered trivially on
 * the 2-element set but the halfspace pivot-transport pattern doesn't
 * apply (no continuous-scaling interpretation on a 2-point carrier);
 * @c Complex<R> correctly fails @c IsTotallyOrdered (no order
 * compatible with the complex multiplication).
 *
 * @note The @c k_E = 0 case is degenerate (the scout function
 *       collapses to the constant @c x @c ↦ @c 0; the image is the
 *       singleton @c {0} or @c ∅, not a halfspace); handled at the
 *       pipe's @c Element != zero gate, not here.
 */
export template <typename T>
concept IsOrderedMultiplicativeGroup =
    dedekind::algebra::IsField<T> && dedekind::order::IsTotallyOrdered<T>;

/**
 * @concept IsOrderedCommutativeRing
 * @brief A commutative ring whose carrier is ordered under the
 *        additive translation marker.  The gate for ring-retract
 *        scaling (#664 Slice 5): @c bound<m> @c * @c in<ℤ> when @c T
 *        is a ring but not a field.
 *
 * @details
 * Composes:
 *   * @c category::IsCommutativeRing<T, +, *> --- @c T is an
 *     axiomatic commutative ring.
 *   * @c is_translation_invariant_ordered_v<T> --- @c T's order is
 *     compatible with the additive group (re-use of the additive
 *     marker; in a commutative ring with ordered addition,
 *     positive-scalar multiplication automatically preserves order
 *     and negative-scalar multiplication automatically reverses).
 *
 * @c ℤ (@c SignedCardinality) satisfies this; @c ℚ also does, but
 * the ring-retract pipe is shadowed by the @c IsOrderedMultiplicativeGroup
 * field-iso pipe via the @c !IsOrderedMultiplicativeGroup<T>
 * disambiguator at the pipe / factory site.
 */
export template <typename T>
concept IsOrderedCommutativeRing =
    dedekind::category::IsCommutativeRing<T, std::plus<T>,
                                          std::multiplies<T>> &&
    is_translation_invariant_ordered_v<T>;

// Local helper: structural probe for the @c Halfspace shape.  Used in
// the composition-pipe @c requires-clauses to Honest-Reject the case
// where the inner pipe produces a non-@c Halfspace predicate (e.g.\
// the Slice-5 ring-retract pipe produces @c AffineImageOfHalfspace,
// which lacks @c pivot @c / @c direction @c / @c strictness static
// members; a composition pipe assuming those members would otherwise
// fail with an opaque member-access error inside the body).
namespace detail {
template <typename P>
concept HasHalfspaceShape = requires {
  P::pivot;
  P::direction;
  P::strictness;
};

// Sibling probe for the @c AffineImageOfHalfspace shape.  Used in the
// Slice-B canonical-witness composition pipe to detect when the inner
// scout's pipe produced an affine-image predicate (the Slice-5 ring-
// retract output) rather than a plain @c Halfspace --- the two
// composition routes are structurally distinct and the requires-
// clauses dispatch on shape.
template <typename P>
concept HasAffineImageShape = requires {
  P::multiplier;
  P::offset;
  typename P::source_halfspace_type;
};
}  // namespace detail

// Forward declaration of @c AffineImageOfHalfspace --- the ring-retract
// pipe's result predicate type.  Defined after @c GroupScout below
// because its operator() invokes @c SourceHalfspace's operator(),
// which depends only on the @c :order:halfspace types already
// imported.  The forward declaration here is enough for two-phase
// name lookup inside @c GroupScout's ring-retract pipe to bind.
//
// Slice-5-canonical extension (post-PR #664-Slice-B): a fourth NTTP
// @c B carries the additive offset of the affine map @f$y = M @c * x +
// B@f$.  The pre-extension form @c AffineImageOfHalfspace<T, M, SrcHS>
// is now @c AffineImageOfHalfspace<T, M, /*B=*/T-typed-zero, SrcHS>; the
// Slice-5 ring-retract pipe instantiates it that way (no offset), and a
// new composition pipe folds outer additive shifts into the offset so
// the canonical witness @c Set{bound<2> @c * @c in<ℤ> @c + @c bound<1>
// @c | @c (in<ℤ> @c > @c bound<5>)} reaches a single predicate.
export template <typename T, auto M, auto B, typename SourceHalfspace>
struct AffineImageOfHalfspace;

/**
 * @brief Symbolic scout parameterised by carrier @c T, group operation
 *        @c Op, an element of the group (NTTP), and an inner
 *        expression.
 *
 * @details
 * The class-level algebraic constraint @c IsMonoid<T,Op> (relaxed
 * from @c IsGroup<T,Op> under #664 Slice 5 to admit the ring-retract
 * case for @c (ℤ, std::multiplies)) makes the scout type instantiable.
 * Each pipe overload below additionally enforces the stronger
 * algebraic claim it needs (@c IsOrderedAdditiveGroup /
 * @c IsOrderedMultiplicativeGroup / @c IsOrderedCommutativeRing)
 * at the @c requires-clause: a callsite that produces a
 * @c GroupScout whose @c (T, Op) is a monoid but not a group simply
 * won't find a pipe overload to fire @b unless the ring-retract path
 * applies.  This is the "algebraic-over-architectural" stance
 * applied to the comprehension surface: gates are textbook concepts,
 * not implementation-specific markers.
 *
 * Per-operator factories (@c operator+, @c operator-, [future
 * @c operator*]) populate the @c Op and @c Element fields.  The type
 * itself stays uniform across the additive and multiplicative
 * specialisations.
 *
 * @c GroupScout is @b purely symbolic --- a type-level marker used by
 * the Set CTAD (Slice 2) to perform halfspace-pivot transport.  It has
 * @b no @c operator() and no @c Domain typedef, mirroring the
 * @c BoundScout convention (cf.\ @c sets/expressions.cppm:168-178:
 * "the scout is type-only --- it has no runtime state and no
 * operator() on plain values").  The carrier is recoverable as
 * @c GroupScout::carrier_type.
 *
 * @tparam T       The carrier type.
 * @tparam Op      The group operation (a callable type, e.g.\
 *                 @c std::plus<T> or @c std::multiplies<T>).
 * @tparam Element The group element (NTTP).
 * @tparam Inner   The inner expression type
 *                 (e.g.\ @c BoundScout<Ambient>).
 */
// Class-level gate relaxed to @c IsMonoid<T, Op> to admit the
// @b ring-retract case (#664 Slice 5): @c (ℤ, std::multiplies<ℤ>) is
// a multiplicative monoid (not a group --- non-units lack inverses),
// and the ring-retract pipe overload below produces an
// @c AffineImageOfHalfspace predicate that correctly handles
// divisibility.  The strict group claim still applies at the pipe
// site for the group-based paths (@c IsOrderedAdditiveGroup ,
// @c IsOrderedMultiplicativeGroup): if the carrier isn't a group
// under @c Op, those pipes simply don't fire.  Naming "GroupScout"
// retained for backward compatibility; the per-pipe constraints
// carry the precise algebraic claim.
export template <typename T, typename Op, auto Element, typename Inner>
  requires dedekind::category::IsMonoid<T, Op>
struct GroupScout {
  using carrier_type = T;
  using op_type = Op;
  using inner_type = Inner;

  static constexpr auto element = Element;

  /**
   * @brief Comprehension pipe: @c GroupScout @c | @c Halfspace →
   *        @c Comprehension with the halfspace's pivot shifted by
   *        @c Element.
   *
   * @details
   * Halfspace-pivot transport along the additive group action: a source
   * predicate @f$\{x \in T \mid x \bowtie k\}@f$ pushed through the
   * scout function @f$\lambda x.\,k_E + x@f$ (where @f$k_E@f$ is this
   * scout's @c Element) yields the image halfspace
   * @f$\{y \in T \mid y \bowtie k + k_E\}@f$ --- the pivot shifts by
   * @c Element, the direction and strictness are preserved (group
   * translation is monotone).
   *
   * The result is a @c Comprehension over the inner scout's @c Ambient,
   * with the @b shifted halfspace as predicate.  Downstream @c Set CTAD
   * (already present in @c :sets:expressions) picks this up unchanged
   * and produces a @c Set<T,L,Halfspace<T,k+k_E,...>> --- no Set CTAD
   * changes needed.
   *
   * @section Specialisation gate
   *
   * Currently restricted to @c Op @c = @c std::plus<T> (additive group
   * action) and @c Inner exposing @c AmbientType / @c ambient (i.e.\
   * a @c BoundScout).  Slice 3 adds the multiplicative case
   * (@c Op @c = @c std::multiplies<T>); composition of scouts
   * (e.g.\ @c bound<2> @c * @c in<ℤ> @c + @c bound<1>) is further
   * follow-up.
   */
  template <auto Pivot, dedekind::order::Direction D,
            dedekind::order::Strictness S, typename L>
    requires std::same_as<Op, std::plus<T>> && IsOrderedAdditiveGroup<T> &&
             requires {
               typename Inner::AmbientType;
               Inner::ambient;
             }
  constexpr auto operator|(
      dedekind::order::Halfspace<T, Pivot, D, S, L>) const {
    // Compute the shifted pivot in its natural arithmetic --- not via a
    // forced cast to @c T.  This preserves the original pivot's
    // structural NTTP type (e.g.\ @c int when @c T is a non-structural
    // proxy) and follows the @c Halfspace contract that @c Pivot may
    // legitimately differ in type from @c T (cf.\ @c halfspace.cppm:189
    // "Pivot may be a different structural type than T ... the carrier's
    // converting ctor / overload set handles the comparison").
    constexpr auto new_pivot = Pivot + Element;
    using NewHalfspace = dedekind::order::Halfspace<T, new_pivot, D, S, L>;
    using AmbientType = typename Inner::AmbientType;
    return dedekind::sets::Comprehension<AmbientType, NewHalfspace>{
        Inner::ambient, NewHalfspace{}};
  }

  /**
   * @brief Comprehension pipe (multiplicative): @c GroupScout @c |
   *        @c Halfspace → @c Comprehension with the halfspace's pivot
   *        scaled by @c Element, and the direction flipped if
   *        @c Element < 0 (#664 Slice 3).
   *
   * @details
   * Halfspace-pivot transport along the multiplicative-group action: a
   * source predicate @f$\{x \in T \mid x \bowtie k\}@f$ pushed through
   * the scout function @f$\lambda x.\,k_E \cdot x@f$ (where @f$k_E@f$
   * is this scout's @c Element) yields the image halfspace
   * @f$\{y \in T \mid y \bowtie' k \cdot k_E\}@f$, where:
   *   * if @c k_E > 0: direction and strictness preserved
   *     (@c ⋈' @c = @c ⋈);
   *   * if @c k_E < 0: direction reversed
   *     (@c >  @c ↔ @c <,  @c ≥ @c ↔ @c ≤); strictness preserved.
   *
   * The @c k_E = 0 case is Honest-Rejected by the @c Element @c !=
   * zero gate in the requires-clause: scaling by @c 0 collapses the
   * scout function to the @b constant map @c x @c ↦ @c 0, so the
   * image of the source halfspace is the singleton @c {0} (if the
   * source is satisfiable) or @c ∅ (if not) --- neither of which is a
   * halfspace, so the halfspace-pivot transport pattern doesn't apply.
   * Callers wanting that image should re-spell it as a @c Singleton
   * (or empty Set) directly.
   *
   * @section Specialisation gate
   *
   * Currently restricted to @c Op @c = @c std::multiplies<T> and
   * @c Inner exposing @c AmbientType / @c ambient (i.e.\ a
   * @c BoundScout).  Mirrors the additive specialisation above.
   */
  template <auto Pivot, dedekind::order::Direction D,
            dedekind::order::Strictness S, typename L>
    requires std::same_as<Op, std::multiplies<T>> &&
             IsOrderedMultiplicativeGroup<T> &&
             // Honest Rejection on the degenerate @c k_E = 0 case.
             (Element != decltype(Element){}) && requires {
               typename Inner::AmbientType;
               Inner::ambient;
             }
  constexpr auto operator|(
      dedekind::order::Halfspace<T, Pivot, D, S, L>) const {
    // Same natural-arithmetic principle as the additive pipe: the
    // scaled pivot keeps @c Pivot 's NTTP type when feasible (e.g.\
    // @c int * @c int → @c int when both come from @c bound<int>),
    // sidestepping non-structural carriers.
    constexpr auto new_pivot = Element * Pivot;
    using AmbientType = typename Inner::AmbientType;
    if constexpr (Element > decltype(Element){}) {
      // @c k_E > 0: direction and strictness preserved.
      using NewHalfspace = dedekind::order::Halfspace<T, new_pivot, D, S, L>;
      return dedekind::sets::Comprehension<AmbientType, NewHalfspace>{
          Inner::ambient, NewHalfspace{}};
    } else {
      // @c k_E < 0: direction flipped (Upward ↔ Downward), strictness
      // preserved.
      constexpr auto flipped = (D == dedekind::order::Direction::Upward)
                                   ? dedekind::order::Direction::Downward
                                   : dedekind::order::Direction::Upward;
      using NewHalfspace =
          dedekind::order::Halfspace<T, new_pivot, flipped, S, L>;
      return dedekind::sets::Comprehension<AmbientType, NewHalfspace>{
          Inner::ambient, NewHalfspace{}};
    }
  }

  /**
   * @brief Comprehension pipe (composition): @c GroupScout @c |
   *        @c Halfspace when @c Inner is itself a @c GroupScout ---
   *        recursive halfspace-pivot transport across a composed
   *        affine map @c λx.\,f(g(x)) (#664 Slice 4).
   *
   * @details
   * Composition reads inside-out: @c (@c m @c * @c x @c ) @c + @c b
   * = @c λx.\,b @c + @c (m @c * @c x) is the outer @c GroupScout
   * (@c Op @c = @c std::plus, @c Element @c = @c b) wrapping the
   * inner @c GroupScout (@c Op @c = @c std::multiplies,
   * @c Element @c = @c m).  Halfspace transport applies the @b inner
   * transport first (it's the inside of the function composition),
   * then this scout's transport on top.  The result is the same
   * @c Comprehension shape the base pipes produce; the recursion
   * terminates at a @c BoundScout-inner leaf via the base-case
   * overload above.
   *
   * The new pivot, direction, and strictness are extracted from
   * @c Inner 's pipe result (@c Comprehension<..., InnerHalfspace>),
   * then re-transported under @c std::plus<T> .  Mirrors the
   * non-composed additive pipe's pivot arithmetic.
   *
   * @section Specialisation gate
   *
   * Outer is additive (@c Op @c = @c std::plus<T>) over an Inner that
   * is a @c GroupScout (not a @c BoundScout).  The multiplicative-
   * outer composition case (@c (@c x @c + @c b @c ) @c * @c m) is
   * the dual overload below; the inner-leg recursion makes both
   * cases compose freely.  Deeper composition (3+ layers, e.g.\
   * @c ((x @c + @c 1) @c * @c 2) @c + @c 3) works through the same
   * @c Inner{} @c | @c hs recursion; the @c requires-clause matches
   * any @c GroupScout @c Inner, not just @c GroupScout-of-@c BoundScout.
   */
  template <auto Pivot, dedekind::order::Direction D,
            dedekind::order::Strictness S, typename L>
    requires std::same_as<Op, std::plus<T>> && IsOrderedAdditiveGroup<T> &&
             // Inner is a @c GroupScout (composition); has @c op_type
             // and @c element, but not @c AmbientType / @c ambient ---
             // the latter live on the @c BoundScout at the bottom of
             // the chain.
             requires {
               typename Inner::op_type;
               Inner::element;
             } &&
             // Inner's pipe must produce a Halfspace-shaped predicate.
             // Honest-Rejects the case where inner is a ring-retract
             // pipe (Slice 5) whose result is @c AffineImageOfHalfspace;
             // the composition pipe assumes Halfspace's @c pivot @c /
             // @c direction @c / @c strictness static members.
             detail::HasHalfspaceShape<std::remove_cvref_t<
                 decltype((Inner{} | std::declval<dedekind::order::Halfspace<
                                         T, Pivot, D, S, L>>())
                              .predicate)>>
  constexpr auto operator|(
      dedekind::order::Halfspace<T, Pivot, D, S, L> hs) const {
    constexpr auto inner_comp = Inner{} | hs;
    using InnerPredicate = std::remove_cvref_t<decltype(inner_comp.predicate)>;
    constexpr auto new_pivot = InnerPredicate::pivot + Element;
    using NewHalfspace =
        dedekind::order::Halfspace<T, new_pivot, InnerPredicate::direction,
                                   InnerPredicate::strictness, L>;
    using AmbientType = std::remove_cvref_t<decltype(inner_comp.base)>;
    return dedekind::sets::Comprehension<AmbientType, NewHalfspace>{
        inner_comp.base, NewHalfspace{}};
  }

  /**
   * @brief Comprehension pipe (canonical-witness composition, #664
   *        Slice B): outer additive shift @c (@c f(x) @c ) @c + @c K
   *        over an inner ring-retract scaling @c f(x) @c = @c M
   *        @c * @c g(x) @c + @c B_inner.
   *
   * @details
   * The canonical-witness shape: @c Set{bound<M> @c * @c in<ℤ> @c +
   * @c bound<K> @c | @c (in<ℤ> @c ⋈ @c bound<p>)} on @c ℤ.  Inner
   * ring-retract produces @c AffineImageOfHalfspace<T, M, B_inner,
   * SrcHS>; outer additive shift by @c Element folds into the offset:
   *
   * @code
   *   inner:        x ↦ M @c * @c x @c + @c B_inner
   *   outer shift:  y ↦ y @c + @c Element
   *   composed:     x ↦ M @c * @c x @c + @c (B_inner @c + @c Element)
   * @endcode
   *
   * Membership of @c z in the composed image: @c (z @c - @c B_new)
   * is divisible by @c M AND @c src((z @c - @c B_new) @c / @c M),
   * with @c B_new @c = @c B_inner @c + @c Element.  The result type
   * is again @c AffineImageOfHalfspace --- the divisibility witness
   * doesn't collapse under additive shift (the divisor stays @c M).
   *
   * Dispatch: shape-detected via @c detail::HasAffineImageShape on
   * the inner pipe's predicate type, which is the sibling probe to
   * @c HasHalfspaceShape used by the standard additive-composition
   * pipe above.  The two pipes are mutually exclusive (a predicate
   * exposes @b either @c pivot @b or @c multiplier, never both), so
   * the requires-clauses partition cleanly without overload
   * ambiguity.
   */
  template <auto Pivot, dedekind::order::Direction D,
            dedekind::order::Strictness S, typename L>
    requires std::same_as<Op, std::plus<T>> && IsOrderedAdditiveGroup<T> &&
             requires {
               typename Inner::op_type;
               Inner::element;
             } &&
             // Inner's pipe produces an affine-image predicate (the
             // Slice-5 ring-retract output).  Honest-Rejects the
             // Slice-4 Halfspace case, which the sibling pipe above
             // handles.
             detail::HasAffineImageShape<std::remove_cvref_t<
                 decltype((Inner{} | std::declval<dedekind::order::Halfspace<
                                         T, Pivot, D, S, L>>())
                              .predicate)>>
  constexpr auto operator|(
      dedekind::order::Halfspace<T, Pivot, D, S, L> hs) const {
    constexpr auto inner_comp = Inner{} | hs;
    using InnerPredicate = std::remove_cvref_t<decltype(inner_comp.predicate)>;
    constexpr auto new_offset = InnerPredicate::offset + Element;
    using SrcHalfspace = typename InnerPredicate::source_halfspace_type;
    using NewPredicate =
        AffineImageOfHalfspace<T, InnerPredicate::multiplier, new_offset,
                               SrcHalfspace>;
    using AmbientType = std::remove_cvref_t<decltype(inner_comp.base)>;
    return dedekind::sets::Comprehension<AmbientType, NewPredicate>{
        inner_comp.base, NewPredicate{}};
  }

  /**
   * @brief Comprehension pipe (composition, multiplicative outer):
   *        @c (g(x)) @c * @c m where @c g is an inner scout.
   *
   * @details
   * Multiplicative-outer twin of the composition pipe above.  Reads
   * @c (@c x @c + @c b @c ) @c * @c m = @c λx.\,m @c * @c (x @c + @c b)
   * --- inner additive shift, outer multiplicative scale.  Inner's
   * pipe transports first; this scout multiplies the result's pivot
   * by @c Element and flips direction if @c Element @c < @c 0
   * (same direction-flip rule as the non-composed multiplicative
   * pipe).  Honest-Rejects @c Element @c = @c 0.
   */
  template <auto Pivot, dedekind::order::Direction D,
            dedekind::order::Strictness S, typename L>
    requires std::same_as<Op, std::multiplies<T>> &&
             IsOrderedMultiplicativeGroup<T> &&
             (Element != decltype(Element){}) &&
             requires {
               typename Inner::op_type;
               Inner::element;
             } &&
             // Inner's pipe must produce a Halfspace-shaped predicate
             // (same Honest-Rejection as the additive composition pipe
             // above; a Slice-5 ring-retract inner producing
             // @c AffineImageOfHalfspace is refused here).
             detail::HasHalfspaceShape<std::remove_cvref_t<
                 decltype((Inner{} | std::declval<dedekind::order::Halfspace<
                                         T, Pivot, D, S, L>>())
                              .predicate)>>
  constexpr auto operator|(
      dedekind::order::Halfspace<T, Pivot, D, S, L> hs) const {
    constexpr auto inner_comp = Inner{} | hs;
    using InnerPredicate = std::remove_cvref_t<decltype(inner_comp.predicate)>;
    constexpr auto new_pivot = Element * InnerPredicate::pivot;
    using AmbientType = std::remove_cvref_t<decltype(inner_comp.base)>;
    if constexpr (Element > decltype(Element){}) {
      using NewHalfspace =
          dedekind::order::Halfspace<T, new_pivot, InnerPredicate::direction,
                                     InnerPredicate::strictness, L>;
      return dedekind::sets::Comprehension<AmbientType, NewHalfspace>{
          inner_comp.base, NewHalfspace{}};
    } else {
      constexpr auto flipped =
          (InnerPredicate::direction == dedekind::order::Direction::Upward)
              ? dedekind::order::Direction::Downward
              : dedekind::order::Direction::Upward;
      using NewHalfspace =
          dedekind::order::Halfspace<T, new_pivot, flipped,
                                     InnerPredicate::strictness, L>;
      return dedekind::sets::Comprehension<AmbientType, NewHalfspace>{
          inner_comp.base, NewHalfspace{}};
    }
  }

  /**
   * @brief Comprehension pipe (ring-retract): @c GroupScout @c |
   *        @c Halfspace producing an @c AffineImageOfHalfspace
   *        predicate, for carriers that are commutative rings but
   *        @b not fields (#664 Slice 5).
   *
   * @details
   * On a ring (not a field) the map @c x @c ↦ @c M*x is a @b retract,
   * not an iso: only multiples of @c M land in the image, so the
   * result type isn't a plain halfspace --- it's the conjunction of a
   * divisibility check and the source halfspace on the preimage @c y/M.
   * The @c AffineImageOfHalfspace predicate carries both.
   *
   * Canonical carrier: @c ℤ (@c SignedCardinality), which is the
   * initial ring (not a field).  Disambiguates from the field-iso
   * pipe (above) via @c !IsOrderedMultiplicativeGroup<T> in the
   * @c requires-clause --- ℚ takes the iso path, ℤ takes this
   * ring-retract path.
   */
  template <auto Pivot, dedekind::order::Direction D,
            dedekind::order::Strictness S, typename L>
    requires std::same_as<Op, std::multiplies<T>> &&
             IsOrderedCommutativeRing<T> &&
             (!IsOrderedMultiplicativeGroup<T>) &&
             (Element != decltype(Element){}) &&
             // The ring-retract pipe's @c AffineImageOfHalfspace
             // predicate calls @c y/M and @c M*(y/M) at membership
             // time; gate on the operator surface here so a future
             // ring carrier without @c / (e.g.\ a polynomial ring on
             // a non-Euclidean coefficient ring) is Honest-Rejected
             // at the requires-clause rather than failing inside the
             // predicate body.
             requires(T a, T b) {
               { a / b } -> std::same_as<T>;
               { a * b } -> std::same_as<T>;
             } && requires {
               typename Inner::AmbientType;
               Inner::ambient;
             }
  constexpr auto operator|(
      dedekind::order::Halfspace<T, Pivot, D, S, L>) const {
    using SourceHalfspace = dedekind::order::Halfspace<T, Pivot, D, S, L>;
    // Slice-5 base case: no outer additive shift, so the affine
    // offset @c B is zero.  Slice-B's outer-shift composition pipe
    // (below) folds non-zero offsets in by accumulating @c B_inner
    // @c + @c K when an outer @c + @c bound<K> wraps an inner
    // ring-retract.
    using ResultPredicate =
        AffineImageOfHalfspace<T, Element, decltype(Element){0},
                               SourceHalfspace>;
    using AmbientType = typename Inner::AmbientType;
    return dedekind::sets::Comprehension<AmbientType, ResultPredicate>{
        Inner::ambient, ResultPredicate{}};
  }
};

/**
 * @brief Image of a source halfspace under the affine map @f$y = M
 *        \cdot x + B@f$ on a commutative ring carrier (#664 Slice 5
 *        ring-retract scaling + Slice-B canonical-witness extension).
 *
 * @details
 * Membership of @c y in the image is the conjunction of two
 * decidable checks (after the affine inversion @c y @c ↦ @c (y @c -
 * @c B) @c / @c M):
 *
 *   (1) @b Divisibility @c M @c | @c (y @c - @c B) --- there exists
 *       an integer @c x with @c M*x @c = @c y @c - @c B; on a ring
 *       carrier this is computable as @c ((y @c - @c B) @c % @c M)
 *       @c == @c 0 (or the identity-based form used in the body
 *       below, which propagates correctly through variant sentinels).
 *   (2) @b Source @b predicate @c source((y @c - @c B) @c / @c M)
 *       --- the (unique modulo sign) preimage satisfies the source
 *       halfspace.
 *
 * On a field (@c ℚ) the divisibility check is vacuous (every @c y
 * has a preimage); the @c !IsOrderedMultiplicativeGroup<T> gate at
 * the factory / pipe sites Honest-Rejects this predicate on fields
 * --- callers on fields take the iso path producing a plain
 * @c Halfspace, which folds at compile time without the
 * runtime modular check.
 *
 * @b Slice-B @b canonical @b witness: the offset @c B carries the
 * outer additive shift folded into the predicate by the
 * @c GroupScout<T, std::plus<T>, K, AffineImageInner> composition
 * pipe below.  The pre-extension Slice-5 form (no outer shift) sets
 * @c B @c = @c 0; the composition pipe accumulates @c B_new @c =
 * @c B_inner @c + @c K.
 *
 * @tparam T               Carrier (ordered commutative ring).
 * @tparam M               Affine multiplier (NTTP, @c != @c 0).
 * @tparam B               Affine offset (NTTP; @c 0 in the base
 *                         Slice-5 ring-retract pipe; non-zero only
 *                         when the canonical-witness composition
 *                         folds an outer additive shift in).
 * @tparam SourceHalfspace Source halfspace type.
 */
export template <typename T, auto M, auto B, typename SourceHalfspace>
struct AffineImageOfHalfspace {
  using Domain = T;
  using Codomain = typename SourceHalfspace::Codomain;
  using logic_species = typename SourceHalfspace::logic_species;
  using cardinality_type = dedekind::sets::ℵ_0;
  using source_halfspace_type = SourceHalfspace;
  static constexpr auto multiplier = M;
  static constexpr auto offset = B;

  constexpr Codomain operator()(const T& y) const {
    using L = logic_species;
    const T m_in_t = T{M};
    const T b_in_t = T{B};
    // Invert the affine map @c y @c = @c M*x @c + @c B by computing
    // @c y_minus_b @c = @c y @c - @c B and then the preimage
    // @c x @c = @c y_minus_b @c / @c M.  Divisibility-via-identity:
    // @c y @c in @c image @c iff @c M @c * @c (y_minus_b @c / @c M)
    // @c == @c y_minus_b.  This formulation is preferred over the
    // modular @c y_minus_b @c % @c M @c == @c 0 form because it
    // propagates correctly through variant sentinels (@c \pm @c
    // \aleph_0, @c NaZ) on the saturating ℤ proxy @c
    // SignedCardinality: @c \pm @c \aleph_0 @c / @c m @c = @c \pm @c
    // \aleph_0 and @c m @c * @c \pm @c \aleph_0 @c = @c \pm @c
    // \aleph_0, so the identity holds at the sentinel and the
    // predicate correctly defers to the source halfspace's sentinel-
    // handling.  (The modular form would return @c NaZ for sentinel
    // mod, mis-rejecting sentinels that are actually in the image.)
    const T y_minus_b = y - b_in_t;
    const T x_preimage = y_minus_b / m_in_t;
    if (!((m_in_t * x_preimage) == y_minus_b)) {
      return L::False;  // No preimage --- y not in image.
    }
    return SourceHalfspace{}(x_preimage);
  }
};

}  // namespace dedekind::algebra

// The factory operator overloads are declared in @c dedekind::sets ---
// the namespace of the @b first operand @c BoundScout --- so they
// participate in ADL when callers write @c in<T> @c + @c bound<k>
// without an explicit @c using @c dedekind::algebra::operator+
// directive.  This mirrors @c :order:halfspace 's placement of
// @c operator>(BoundScout,Bound) in @c dedekind::order (the namespace
// of @c Bound), reachable via ADL on either operand.
namespace dedekind::sets {

/**
 * @brief Additive specialisation: @c in<T> @c + @c bound<k> →
 *        @c GroupScout<T, @c std::plus<T>, @c k, @c BoundScout<T>>.
 *
 * @details
 * Gated on @c IsAdditiveGroup<T> @b and @c std::convertible_to<K,T> ---
 * the algebraic axioms guarantee the shift is well-defined, and the
 * convertibility check makes invalid @c K values fail in the
 * requires-clause rather than as hard errors during instantiation.
 *
 * Honest Rejection: writing @c in<ℕ> @c + @c bound<3> fails (ℕ is not
 * an additive group --- @c IsAdditiveGroup<Cardinality> @c ==
 * @c false), with the diagnostic naming the missing group axiom.
 *
 * The scout represents the function @c λx.k+x where @c + is the group
 * operation on @c T.
 */
export template <auto Ambient, auto K>
  requires dedekind::category::IsAbelianGroup<
               typename BoundScout<Ambient>::T,
               std::plus<typename BoundScout<Ambient>::T>> &&
           std::convertible_to<decltype(K), typename BoundScout<Ambient>::T>
constexpr auto operator+(BoundScout<Ambient>, dedekind::order::Bound<K>) {
  using T = typename BoundScout<Ambient>::T;
  return dedekind::algebra::GroupScout<T, std::plus<T>, K,
                                       BoundScout<Ambient>>{};
}

/**
 * @brief Additive group subtraction: @c in<T> @c - @c bound<k> →
 *        @c GroupScout<T, @c std::plus<T>, @c -k, @c BoundScout<T>>.
 *
 * @details
 * Encoded as the additive inverse: @c x @c - @c k @c = @c x @c + @c (-k).
 * Group axioms guarantee @c (-k) ∈ T (additive inverse exists for
 * every element).  The convertibility constraint applies to @c (-K)
 * since that is the value embedded in the resulting scout's
 * @c Element field.
 *
 * Honest Rejection on non-groups (ℕ): the negation @c -K cannot be
 * expressed in the carrier without an inverse, so the gate
 * @c IsAdditiveGroup<T> fails and the overload is removed.
 */
export template <auto Ambient, auto K>
  requires dedekind::category::IsAbelianGroup<
               typename BoundScout<Ambient>::T,
               std::plus<typename BoundScout<Ambient>::T>> &&
           std::convertible_to<decltype(-K), typename BoundScout<Ambient>::T>
constexpr auto operator-(BoundScout<Ambient>, dedekind::order::Bound<K>) {
  using T = typename BoundScout<Ambient>::T;
  return dedekind::algebra::GroupScout<T, std::plus<T>, -K,
                                       BoundScout<Ambient>>{};
}

/**
 * @brief Multiplicative specialisation: @c in<T> @c * @c bound<k> →
 *        @c GroupScout<T, @c std::multiplies<T>, @c k, @c BoundScout<T>>
 *        (#664 Slice 3).
 *
 * @details
 * Gated on @c IsAbelianGroup<T, std::multiplies<T>> @b and
 * @c std::convertible_to<K, T>.  Multiplicative-group axioms (every
 * non-zero element invertible) certify that scaling is well-defined;
 * the convertibility check makes invalid @c K values fail at the
 * requires-clause rather than as hard errors during instantiation.
 *
 * The scout represents the function @c λx.k @c * @c x where @c * is
 * the group operation on @c T.  The downstream
 * @c GroupScout::operator|(Halfspace) pipe (above) does the
 * direction-aware halfspace-pivot transport: @c k > 0 preserves the
 * direction, @c k < 0 flips it (Upward ↔ Downward), @c k = 0 is
 * Honest-Rejected at the pipe.
 *
 * Honest Rejection: writing @c in<ℤ> @c * @c bound<3> fails (post-#674,
 * @c IsAbelianGroup<SignedCardinality, std::multiplies> is @b false
 * --- the saturating ℤ proxy is a ring but @b not a field; ℤ has no
 * multiplicative inverses for non-units).  On ℚ the gate fires:
 * @c IsField<Rational<default_integer>> is the post-#674 witness that
 * @c (ℚ\\{0}, @c *) is a multiplicative abelian group.
 */
export template <auto Ambient, auto K>
  requires dedekind::category::IsAbelianGroup<
               typename BoundScout<Ambient>::T,
               std::multiplies<typename BoundScout<Ambient>::T>> &&
           std::convertible_to<decltype(K), typename BoundScout<Ambient>::T>
constexpr auto operator*(BoundScout<Ambient>, dedekind::order::Bound<K>) {
  using T = typename BoundScout<Ambient>::T;
  return dedekind::algebra::GroupScout<T, std::multiplies<T>, K,
                                       BoundScout<Ambient>>{};
}

/**
 * @brief Ring-retract multiplicative scaling: @c in<T> @c * @c bound<k>
 *        for carriers that are commutative rings but @b not fields
 *        (#664 Slice 5).
 *
 * @details
 * Disambiguator from the field-iso factory above: this overload
 * fires on @c ℤ (@c IsOrderedCommutativeRing fires, but
 * @c IsOrderedMultiplicativeGroup does not because non-units lack
 * multiplicative inverses).  On @c ℚ the field-iso factory wins
 * (the @c !IsOrderedMultiplicativeGroup gate disables this one).
 *
 * Produces the same @c GroupScout type as the field-iso factory ---
 * the discriminating ring-retract @b pipe overload in @c GroupScout
 * picks the @c AffineImageOfHalfspace result predicate at the pipe
 * site (rather than @c Halfspace) based on the same algebraic
 * structure check.
 */
export template <auto Ambient, auto K>
  requires dedekind::algebra::IsOrderedCommutativeRing<
               typename BoundScout<Ambient>::T> &&
           (!dedekind::algebra::IsOrderedMultiplicativeGroup<
               typename BoundScout<Ambient>::T>) &&
           // Match the ring-retract pipe's @c /, @c * requirement so
           // factory and pipe accept/reject the same carriers.
           requires(typename BoundScout<Ambient>::T a,
                    typename BoundScout<Ambient>::T b) {
             { a / b } -> std::same_as<typename BoundScout<Ambient>::T>;
             { a * b } -> std::same_as<typename BoundScout<Ambient>::T>;
           } &&
           std::convertible_to<decltype(K), typename BoundScout<Ambient>::T>
constexpr auto operator*(BoundScout<Ambient>, dedekind::order::Bound<K>) {
  using T = typename BoundScout<Ambient>::T;
  return dedekind::algebra::GroupScout<T, std::multiplies<T>, K,
                                       BoundScout<Ambient>>{};
}

/**
 * @brief Composition (additive outer): @c (g(x)) @c + @c bound<k> for
 *        any @c GroupScout @c g(x) (#664 Slice 4).
 *
 * @details
 * Lifts the @c operator+ factory from @c BoundScout to any
 * @c GroupScout LHS, enabling the canonical @b affine syntax
 * @c (in<ℚ> @c * @c bound<m>) @c + @c bound<k> = @c λx.\,m*x @c + @c k
 * (or its multiplicative-outer twin below).  The result is a
 * @c GroupScout with the LHS as @c Inner --- recursion handled by
 * the composition pipe in @c GroupScout::operator|(Halfspace) above.
 *
 * Honest Rejection: writing @c (g(x)) @c + @c bound<k> on a non-
 * additive-group carrier @c T fails at the same gate as the base
 * factory (@c IsAbelianGroup<T, std::plus<T>>).
 */
export template <typename T, typename InnerOp, auto InnerElement,
                 typename InnerInner, auto K>
  requires dedekind::category::IsAbelianGroup<T, std::plus<T>> &&
           std::convertible_to<decltype(K), T>
constexpr auto operator+(
    dedekind::algebra::GroupScout<T, InnerOp, InnerElement, InnerInner>,
    dedekind::order::Bound<K>) {
  return dedekind::algebra::GroupScout<
      T, std::plus<T>, K,
      dedekind::algebra::GroupScout<T, InnerOp, InnerElement, InnerInner>>{};
}

/**
 * @brief Composition (multiplicative outer): @c (g(x)) @c * @c bound<k>
 *        for any @c GroupScout @c g(x).  Multiplicative twin of the
 *        additive composition factory above.
 */
export template <typename T, typename InnerOp, auto InnerElement,
                 typename InnerInner, auto K>
  requires dedekind::category::IsAbelianGroup<T, std::multiplies<T>> &&
           std::convertible_to<decltype(K), T>
constexpr auto operator*(
    dedekind::algebra::GroupScout<T, InnerOp, InnerElement, InnerInner>,
    dedekind::order::Bound<K>) {
  return dedekind::algebra::GroupScout<
      T, std::multiplies<T>, K,
      dedekind::algebra::GroupScout<T, InnerOp, InnerElement, InnerInner>>{};
}

}  // namespace dedekind::sets
