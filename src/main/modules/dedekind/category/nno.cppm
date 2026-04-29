/**
 * @file dedekind/category/nno.cppm
 * @partition :nno
 * @brief Level 2: The Natural Numbers Object — Lawvere's ETCS Axiom 9.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section NNO_Universal_Property
 * In Lawvere's ETCS the natural numbers are not @b constructed --- they
 * are defined by a @b universal @b property.  An NNO consists of an
 * object @c N together with two arrows
 *
 *   @c z @c : @c 1 @c → @c N            (the zero element)
 *   @c s @c : @c N @c → @c N            (the successor map)
 *
 * such that for any object @c A together with arrows @c a₀ @c : @c 1
 * @c → @c A and @c g @c : @c A @c → @c A there exists a @b unique
 * arrow @c f @c : @c N @c → @c A making the recursion diagram commute:
 *
 *   @c f @c ∘ @c z @c = @c a₀          (initial-value clause)
 *   @c f @c ∘ @c s @c = @c g @c ∘ @c f  (step clause)
 *
 * Existence + uniqueness of @c f for any @c (A, a₀, g) is the universal
 * property; it is the "engineer's honesty obligation" in this
 * codebase, since C++ concepts cannot quantify over the functorial
 * content.  The structural shape ( @c z and @c s as arrows of the
 * stated signatures) @b is what the type system can pin.
 *
 * @section This_Partition
 * Lifts ETCS Axiom 9 from the @c :etcs facade-level mention (where it
 * was previously cross-referenced as @c SpeciesTraits<unsigned>) to a
 * first-class @b concept that any carrier can witness.  Closes part of
 * #445.  Concrete carrier witnesses ( @c Cardinality from @c
 * sets:cardinality, @c Modular<N> from @c morphologies:cyclic) are
 * registered downstream where the carriers are available.
 *
 * @section Form_Bias_NNO_Cardinality_ℕ
 * Per the project's Platonist stance ("Forms precede carriers"), the
 * architecture is a three-layer chain:
 *
 *   @c NNO  →  @c Cardinality  →  @c ℕ
 *
 * @li @b NNO is the @b Form — the universal property defined in this
 *     partition.  Multiple carriers may witness it.
 * @li @b Cardinality (variant @c ℕ-proxy in @c sets:cardinality) is
 *     the @b canonical @b carrier inhabiting the NNO Form, certified
 *     by the @c IsNNO witness in @c numbers:natural.  Saturating
 *     semantics ( @c ℵ_0 escalation) is the @b carrier's extra
 *     behaviour beyond the textbook NNO — the abstract NNO is
 *     purely the Peano-style universal property over a bounded /
 *     finite presentation; @c Cardinality adds @c ℵ_0 as an
 *     overflow sentinel so a machine implementation can stay
 *     honest about values that exceed its representable range.
 * @li @b ℕ (textbook symbol, in @c sets:boundaries post-#427) is an
 *     alias that @b refers to @c Cardinality — the @b symbol
 *     practitioners use.  The chain reads "NNO supplies the Form;
 *     Cardinality is the canonical witness; ℕ is the name."
 *
 * Sibling carrier witnesses ( @c Modular<2^w> as the @b bounded NNO,
 * the textbook ℤ/2^wℤ; @c unsigned @c int with the bounded-NNO
 * caveat that closure under successor only holds away from
 * @c numeric_limits<unsigned>::max() ) are downstream candidates.
 *
 * @note "In the mathematical development of recent decades, the
 *        notion of set has not only played a fundamental role, but
 *        it has itself passed through a long process of refinement.
 *        ... The natural numbers, of course, recur in every
 *        development of the theory, since the set of natural numbers
 *        is by far the most important infinite set."
 *       — F. William Lawvere, "An Elementary Theory of the Category
 *         of Sets" (1964).
 */
module;

#include <concepts>
#include <cstddef>
#include <functional>  // std::invoke
#include <type_traits>

export module dedekind.category:nno;

import :morphism;

namespace dedekind::category {

/**
 * @concept IsNNO
 * @brief @b Structural @b shape: @c (N, Z, S) is a Natural Numbers
 *        Object — @c Z is the zero element @c 1 @c → @c N, @c S is
 *        the successor map @c N @c → @c N.
 *
 * @details C++ concepts cannot quantify over the universal property
 * (existence + uniqueness of @c f @c : @c N @c → @c A for arbitrary
 * @c (A, a₀, g)); that is the engineer's honesty obligation.  The
 * structural shape pinned here is the operational signatures — @c Z
 * is callable with no arguments and yields an @c N (the zero
 * element); @c S is callable on an @c N and yields an @c N (the
 * successor).  Together they generate the NNO's iterated
 * presentation: @c Z(), @c S(Z()), @c S(S(Z())), ...
 *
 * The dual shape concept on the @b Functor side (NNOs as initial
 * F-algebras of @c F(X) @c = @c 1 @c + @c X) is reified in the
 * sibling partition @c :f_algebra (closes the universal-property
 * layer for #449; Pierce §5.4, Mac Lane III–VI).  The two readings
 * are operationally equivalent — the @c (z, s) pair pinned here
 * @b induces the structure map @c [z, @c s] @c : @c 1 @c + @c N @c →
 * @c N via coproduct copairing on the standard injections
 * @c inl @c : @c 1 @c → @c 1 @c + @c N and
 * @c inr @c : @c N @c → @c 1 @c + @c N.  This partition focuses on
 * the @c (z, s) shape since it is the more directly recognisable
 * one to a working mathematician.
 *
 * @tparam N The carrier object.
 * @tparam Z A nullary callable @c Z : 1 → N (the zero element).
 * @tparam S A unary callable @c S : N → N (the successor).
 */
export template <typename N, typename Z, typename S>
concept IsNNO = requires(Z z, S s, N n) {
  { z() } -> std::convertible_to<N>;
  { s(n) } -> std::convertible_to<N>;
};

/**
 * @brief Recursion-via-universal-property (operational discharge).
 *
 * @details Given an NNO witness @c (N, Z, S), an initial value @c a₀
 * of type @c A, and a step function @c g : A → A, materialises the
 * unique morphism @c f : N → A in its iterated presentation:
 *
 *   @c f(0) @c = @c a₀
 *   @c f(n+1) @c = @c g(f(n))
 *
 * Operationally, this is bounded count-driven iteration: the caller
 * supplies a count @c n (machine @c std::size_t) and gets back
 * @c f(n) by composing @c g with itself @c n times starting from
 * @c a₀.  The count is the iteration driver — not the NNO element
 * itself, since materialising a transfinite NNO element through
 * machine-bounded iteration is structurally impossible.  The
 * universal-property claim that @c f exists and is unique remains
 * the honesty obligation; what this combinator does is realise @c f
 * @b at @b a @b finite @b index.
 *
 * For NNO carriers like @c Cardinality that admit a transfinite
 * @c ℵ_0 element, the recursion at the transfinite index is
 * deliberately not provided by this combinator — callers needing
 * that case state their own saturating semantics explicitly (a
 * proof obligation discharge separate from the count-bounded
 * recursion below).
 *
 * @tparam A The recursion target type.
 * @tparam G A unary callable @c G : A → A (the step function).
 *
 * @param a0  The initial value @c f(0).
 * @param g   The step function.
 * @param n   The count of iterations (the index into the NNO at
 *            which to evaluate @c f).
 *
 * @return @c f(n) — the result of applying @c g to @c a₀ exactly
 *         @c n times.
 */
export template <typename A, typename G>
  requires std::invocable<G&, A const&> &&
           std::convertible_to<std::invoke_result_t<G&, A const&>, A>
constexpr A nno_iterate(A a0, G g, std::size_t n) {
  A current = a0;
  for (std::size_t i = 0; i < n; ++i) {
    current = std::invoke(g, current);
  }
  return current;
}

}  // namespace dedekind::category
