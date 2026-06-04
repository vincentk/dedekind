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
 *  - @c fibonacci @c : @c Path<std::size_t> — the canonical recurrence,
 *    formulated as @c iterate(seed, @c step) on the @em categorical
 *    @em product carrier @c ℕ @c × @c ℕ .  Step is an @c O(1)
 *    endomorphism @c (a, @c b) @c ↦ @c (b, @c a+b) ; the @c iterate
 *    primitive is the unique NNO morphism @c ℕ @c → @c carrier making
 *    Lawvere's NNO recursion diagram commute (Mac Lane @em CWM §V.5).
 *
 * @build_order 5
 * @dependency dedekind.category, dedekind.sequences:path
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
#include <utility>

export module dedekind.sequences:samples;

import dedekind.category;
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
 * @brief The Fibonacci recurrence state stream: @c iterate on @c ℕ @c × @c ℕ .
 *
 * @details The state carrier is the @em categorical @em product
 * @c ℕ @c × @c ℕ — modelled by @c std::pair<std::size_t, @c std::size_t>
 * (witnessed by the @c IsProduct static_assert below, exactly as
 * @c Rational<Z> certifies @c IsProduct<Rational<Z>, @c Z, @c Z> and
 * @c Complex<R> certifies @c IsProduct<Complex<R>, @c R, @c R> ).  The
 * step @c (a, @c b) @c ↦ @c (b, @c a+b) is an @c O(1) endomorphism on
 * that product.  @c iterate(seed, @c step) is the unique morphism
 * @c ℕ @c → @c state-carrier making Lawvere's NNO recursion diagram
 * commute (Mac Lane @em CWM §V.5).
 */
static_assert(
    dedekind::category::IsProduct<std::pair<std::size_t, std::size_t>,
                                  std::size_t, std::size_t>,
    "fibonacci's recurrence state IS the categorical product ℕ × ℕ via "
    ".first / .second as the canonical projections π₁ and π₂.");

export inline const auto fibonacci_state = iterate(
    std::pair<std::size_t, std::size_t>{0u, 1u},
    [](const auto& p) { return std::pair{p.second, p.first + p.second}; });

/**
 * @brief @c fibonacci @c : @c ℕ @c → @c ℕ — the canonical Fibonacci
 *        sequence @c 0, @c 1, @c 1, @c 2, @c 3, @c 5, @c 8, @c 13, ... .
 *
 * @details Read off the canonical sequence by projecting @c π₁ from
 * @c fibonacci_state : @c fibonacci(n) @c = @c π₁(fibonacci_state.at(n)) .
 * The recurrence step lives in @c fibonacci_state ; this Path is just
 * the first-component readout.
 */
export inline const auto fibonacci = Path<std::size_t>{
    [](std::size_t n) -> std::size_t { return fibonacci_state.at(n).first; }};

}  // namespace dedekind::sequences
