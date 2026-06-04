/**
 * @file dedekind/sequences/samples.cppm
 * @partition :samples
 * @brief Named §3 page-2 sample sequences: @c is_even and @c fibonacci.
 *
 * @section samples__Purpose
 *
 * Paper §3 page-2 (the @em Sequences half of "Sets and Sequences") leans
 * on two named, non-trivial @c Path<T> sequences whose graphs can be
 * lifted via @c as_relation and joined relationally — the page-2 anchor
 * for the @em θ-join @em via @em σ @em ∘ @em × beat.  Naming them in
 * code (rather than inlining anonymous closures at the call sites) lets
 * the paper prose cite them by their exported names, and lets downstream
 * exhibits (the §3 page-2 listing, the §3 reshape PR) compose against a
 * stable surface.
 *
 * @subsection The_Sample_Pair
 *  - @c is_even @c : @c Path<Boolean> — gives the join row a heterogeneous
 *    codomain (@c ℕ @c × @c 𝔹 @c × @c ℕ ), exhibiting tuples-not-lists.
 *  - @c fibonacci_of<T> @c : @c Path<T> — the canonical recurrence,
 *    expressed via the n-ary @c iterate(op, @c seeds...) primitive in
 *    @c :sequences:path .  Fibonacci is the
 *    @c iterate(std::plus<T>{}, @c T(0u), @c T(1u)) instance — one
 *    named primitive, one binary operator, two seeds, no fanout /
 *    projection / explicit-product-type ceremony.  Parameterised on the
 *    same algebraic gate @c IsSequence imposes on its @c Domain
 *    (@c :order:halfspace:IsRingIntegral , the gate used by #755 ): any
 *    ring-integral carrier @c T whose seed @c T(0u) and @c T(1u) are
 *    well-formed.  The named default
 *    @c fibonacci @c = @c fibonacci_of<std::size_t> is what the paper
 *    listing cites; alternative @c IsRingIntegral carriers
 *    (@c unsigned, @c ExtensionalCardinal<N>, …) instantiate the same
 *    recurrence at no extra cost.  The variant @c Cardinality /
 *    @c SignedCardinality Forms require the
 *    @c finite_cardinality(0) factory rather than brace-init and are a
 *    Sollbruchstelle for a future arrow-based zero/one oracle (post
 *    #602's set-level embeddings).
 *
 *    The structural pattern @em "binary @em op @em applied @em
 *    recursively" is captured by the n-ary @c iterate primitive
 *    directly; Tribonacci is @c iterate(λ(a,b,c). @c a+b+c, @c ...) ,
 *    GCD-via-Euclid is @c iterate(modulus, @c ...) , etc.  @c iterate is
 *    the unique NNO morphism @c ℕ @c → @c carrier making Lawvere's NNO
 *    recursion diagram commute (Mac Lane @em CWM §V.5), generalised to
 *    @c n -dimensional state.
 *
 * @build_order 5
 * @dependency dedekind.category, dedekind.order, dedekind.sequences:path
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "Examples are the schoolmaster of fools."
 *       — Edmund Burke, @em Reflections @em on @em the @em Revolution
 *       @em in @em France (1790), turning the saw on its head: well-chosen
 *       examples are the schoolmaster of the wise, too.
 */

module;

#include <cstddef>
#include <functional>
#include <utility>

export module dedekind.sequences:samples;

import dedekind.category;
import dedekind.order;
import :path;

namespace dedekind::sequences {

using namespace dedekind::category;

/**
 * @brief @c is_even @c : @c ℕ @c → @c 𝔹 — the parity sequence as a
 *        @c Path<Boolean> .
 *
 * @details Boolean codomain by design: the §3 page-2 join exhibit
 * relationally joins @c is_even and @c fibonacci on the shared index
 * column, producing rows of type @c (ℕ, @c 𝔹, @c ℕ) — a heterogeneous
 * triple, not a list of one type.  The join's @em tuples-not-lists
 * character is the page-2 beat the paper prose lands on.
 */
export inline const auto is_even =
    Path<Boolean>{[](std::size_t n) -> Boolean { return Boolean{n % 2 == 0}; }};

/**
 * @brief @c fibonacci_of<T> @c : @c ℕ @c → @c T — the canonical Fibonacci
 *        sequence @c 0, @c 1, @c 1, @c 2, @c 3, @c 5, @c 8, @c 13, ... in
 *        any @c IsRingIntegral carrier @c T .
 *
 * @details Expressed via the n-ary @c iterate primitive (in
 * @c :sequences:path ): Fibonacci is the @c (op @c = @c +, @c seeds @c =
 * @c (0, @c 1)) instance of the "binary op applied recursively" pattern.
 * The carrier @c T is parameterised on the same algebraic gate
 * @c IsSequence imposes on its @c Domain (@c IsRingIntegral at
 * @c :order:halfspace:96 ); the @c requires-clause narrows to those
 * carriers where the unsigned-literal brace-init @c T(0u), @c T(1u) is
 * well-formed.
 *
 * @tparam T Any @c IsRingIntegral inhabitant constructible from the
 *           unsigned literals @c 0u and @c 1u — i.e.\ @c std::integral
 *           types and the @c ExtensionalCardinal<N> /
 *           @c SignedExtensionalCardinal<N> ring-integral carriers.
 *           The variant @c Cardinality / @c SignedCardinality Forms are
 *           excluded by the brace-init requirement; they need
 *           @c finite_cardinality factories rather than @c T(0u)
 *           (a Sollbruchstelle named in the partition docstring above).
 */
export template <dedekind::order::IsRingIntegral T>
  requires requires {
    T(0u);
    T(1u);
  }
inline const auto fibonacci_of = iterate(std::plus<T>{}, T(0u), T(1u));

/**
 * @brief @c fibonacci — named default at @c T @c = @c std::size_t for
 *        paper citation.
 *
 * @details The §3 page-2 listing cites this name directly:
 * @c fibonacci is the canonical operational instantiation, and the same
 * recurrence lifts to any other @c IsRingIntegral carrier via
 * @c fibonacci_of<T> .
 */
export inline const auto& fibonacci = fibonacci_of<std::size_t>;

}  // namespace dedekind::sequences
