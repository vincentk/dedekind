/**
 * @file dedekind/sequences/tail_equivalence.cppm
 * @partition :tail
 * @brief Tail-equivalence of paths: the congruence of "agreeing
 *        eventually", tying the sequence layer to the #718 quotient
 *        machinery.
 *
 * @section tail__The_Germ_At_Infinity
 * Two sequences are @b tail-equivalent when they agree from some index
 * on:
 * @f[
 *   s \sim_{\mathrm{tail}} t \;\;\Longleftrightarrow\;\;
 *     \exists N.\ \forall n \ge N.\ s(n) = t(n).
 * @f]
 * The equivalence class of @c s is its @b germ at infinity — everything
 * about @c s except a finite prefix.  This is the relation under which
 * the sequence-shape concepts become invariants: an
 * @c IsAbsorptiveSequence (eventually constant) is exactly a sequence
 * tail-equivalent to a constant path, and an eventually-periodic
 * sequence is tail-equivalent to a periodic one.
 *
 * @section tail__A_Congruence_Not_Just_An_Equivalence
 * Tail-equivalence is compatible with @c Path's pointwise addition: if
 * @c s ~ s' (agree past @c N₁) and @c t ~ t' (agree past @c N₂) then
 * @c s+t ~ s'+t' (agree past @c max(N₁,N₂)).  So it is a genuine
 * @c dedekind::category::IsCongruence with respect to
 * @c std::plus<Path<T>>, and the quotient @c Path<T>/~ inherits the
 * additive structure — a well-defined @b quotient @b algebra (the same
 * structural fact #718's @c IsCongruence machinery certifies for
 * @c ℤ/Nℤ and @c Frac).  Reifying the quotient @b carrier (with chosen
 * representatives) is deliberately deferred — the congruence is what
 * makes the quotient well-defined; the carrier struct would be added
 * only when a consumer needs representatives.
 *
 * @section tail__Honesty_Obligation
 * The full relation quantifies over an infinite tail (@c ∀n≥N), so it is
 * not decidable in general.  @c TailEquivalent's @c operator() therefore
 * gives the @b operational reading — agreement on a sampled tail window
 * — exactly as @c IsCauchySequence pins the operational subtraction
 * shape and the convergence hooks sample a tail.  The full
 * @c ∃N ∀n≥N criterion is the engineer's honesty obligation; the
 * type-level @c is_congruence_v registration carries the structural
 * claim that tail-equivalence @b is a congruence.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "Le silence éternel de ces espaces infinis m'effraie."
 *       — Blaise Pascal, *Pensées* (1670).
 *       [Trans: "The eternal silence of these infinite spaces frightens
 *        me." — the tail of a sequence is precisely the part that lives
 *        in those infinite spaces.]
 */
module;

#include <cstddef>
#include <functional>
#include <limits>

export module dedekind.sequences:tail;

import dedekind.category; // IsCongruence / IsEquivalenceRelation + relation
                          // and congruence opt-in traits (#718)
import :path;             // Path + pointwise operator+

namespace dedekind::sequences {

/**
 * @class TailEquivalent
 * @brief The tail-equivalence relation on @c Path<T>: agreement on a
 *        sampled tail window @c [start, start+span).
 *
 * @details Callable as a binary relation @c (Path<T>, Path<T>) → bool, so
 * it satisfies @c dedekind::category::IsBinaryRelation.  The sampled
 * window is the operational reading of the (undecidable) full relation
 * @c ∃N ∀n≥N. s(n)=t(n); see the partition's
 * @c tail__Honesty_Obligation note.  Reflexive, symmetric and transitive
 * on the window (and as the full relation), and a congruence w.r.t.
 * pointwise @c + — registered below.
 */
export template <typename T>
struct TailEquivalent {
  std::size_t window_start = 0;
  std::size_t window_span = 64;

  constexpr bool operator()(const Path<T>& s, const Path<T>& t) const {
    // Overflow-safe end: a large window_start must not wrap the bound and
    // make the loop vacuously report agreement.
    const std::size_t end =
        window_start > std::numeric_limits<std::size_t>::max() - window_span
            ? std::numeric_limits<std::size_t>::max()
            : window_start + window_span;
    for (std::size_t i = window_start; i < end; ++i) {
      if (s.at(i) != t.at(i)) {
        return false;
      }
    }
    return true;
  }
};

}  // namespace dedekind::sequences

namespace dedekind::category {

// Tail-equivalence inherits each relational property from the carrier's
// own equality: it is reflexive / symmetric / transitive exactly when
// @c std::equal_to<T> is.  This mirrors the codebase's equality policy
// (std::equal_to<V> is registered an equivalence only for reflexive
// carriers — std::integral V), so IEEE floats are correctly excluded:
// NaN ≠ NaN breaks reflexivity, so TailEquivalent<double> is NOT an
// equivalence relation (and hence not a congruence).
template <typename T>
  requires is_reflexive_relation_v<std::equal_to<T>>
inline constexpr bool
    is_reflexive_relation_v<dedekind::sequences::TailEquivalent<T>> = true;
template <typename T>
  requires is_symmetric_relation_v<std::equal_to<T>>
inline constexpr bool
    is_symmetric_relation_v<dedekind::sequences::TailEquivalent<T>> = true;
template <typename T>
  requires is_transitive_relation_v<std::equal_to<T>>
inline constexpr bool
    is_transitive_relation_v<dedekind::sequences::TailEquivalent<T>> = true;

// …and a congruence with respect to Path's pointwise addition (agreeing
// past N₁ and past N₂ ⇒ the sums agree past max(N₁,N₂)) — gated on the
// carrier's equality being a bona fide equivalence, same policy.
template <typename T>
  requires IsEquivalenceRelation<std::equal_to<T>, T>
inline constexpr bool is_congruence_v<dedekind::sequences::TailEquivalent<T>,
                                      dedekind::sequences::Path<T>,
                                      std::plus<dedekind::sequences::Path<T>>> =
    true;

}  // namespace dedekind::category

namespace dedekind::sequences {

/** @section tail__Formal_Verification */

// Tail-equivalence over Path<int> is an equivalence relation…
static_assert(
    dedekind::category::IsEquivalenceRelation<TailEquivalent<int>, Path<int>>,
    "Tail-equivalence must be an equivalence relation on Path<int>.");

// …and a congruence w.r.t. pointwise + — so Path<int>/~tail is a
// well-defined quotient algebra (the #718 IsCongruence link).
static_assert(
    dedekind::category::IsCongruence<TailEquivalent<int>, Path<int>,
                                     std::plus<Path<int>>>,
    "Tail-equivalence must be a congruence w.r.t. pointwise + on Path<int>, "
    "so the quotient Path<int>/~tail inherits the additive structure.");

// Honest-Rejection: over an IEEE float carrier the carrier's own equality
// is not reflexive (NaN ≠ NaN), so tail-equivalence is not an equivalence
// relation and Path<double>/~tail is not certified a congruence quotient.
// Same policy as std::equal_to<double>.
static_assert(
    !dedekind::category::IsCongruence<TailEquivalent<double>, Path<double>,
                                      std::plus<Path<double>>>,
    "Tail-equivalence over double must NOT be a congruence: double equality "
    "is not reflexive (NaN), mirroring the std::equal_to<double> policy.");

}  // namespace dedekind::sequences
