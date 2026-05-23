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

#include <algorithm>
#include <concepts>
#include <cstddef>
#include <functional>
#include <limits>
#include <numeric>
#include <type_traits>

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
    // Compare via the carrier's own equality (the registered policy), not a
    // bare !=, so types providing only operator== participate too.
    const std::equal_to<T> eq{};
    for (std::size_t i = window_start; i < end; ++i) {
      if (!eq(s.at(i), t.at(i))) {
        return false;
      }
    }
    return true;
  }
};

/**
 * @section tail__From_The_Undecidable_General_Case_To_A_Decidable_Bool
 *
 * Tail-equivalence is undecidable in general — deciding @c s ~ t means
 * consuming the infinite tail, i.e.\ @b general @b recursion over a
 * codata stream whose state space is unbounded; the honest verdict is
 * @c Ternary::Unknown.  It collapses to a decidable @c bool exactly when
 * both streams have a @b finite-state presentation — a @b lasso: a finite
 * prefix followed by a cycle, @f$s(n) = s(n + C)\ \forall n \ge P@f$.
 * On a lasso the decision is @b structural @b recursion over the finite
 * @c (prefix, cycle) witness: past @c max(Pₛ,Pₜ) both streams factor
 * through a common cycle of length @c lcm(Cₛ,Cₜ), so agreement on @b one
 * full period is agreement forever — a @b provably @b sufficient,
 * terminating check, not a sampled heuristic.
 *
 * This is the type-directed bridge from the general (Ternary) case to the
 * decidable (Boolean) case: the finite-presentation witness
 * @c IsEventuallyPeriodic is exactly what licenses structural recursion.
 * It unifies the @c :convergence shape concepts as lasso shapes —
 * @c IsAbsorptiveSequence is a prefix + @b 1-cycle (eventually constant),
 * @c IsPeriodicSequence<N> is a @b 0-prefix + N-cycle — and coheres with
 * the #719 Slice 5 collapse: a finite presentation is a countable /
 * @c ClassicalLogic object (decidable), its absence is @c TernaryLogic.
 */

/** @brief Opt-in finite-state ("lasso") presentation of an
 *         eventually-periodic sequence: a prefix of length @c pre_period
 *         followed by a cycle of length @c period
 *         (@c s(n) @c = @c s(n+period) for all @c n @c ≥ @c pre_period).
 *         The primary carries no presentation; a witness specialises it
 *         with the finite @c (pre_period, period) data — the engineer's
 *         honesty obligation (the presentation cannot be computed in
 *         general).  Absorptive ⇒ @c period @c = @c 1; periodic ⇒
 *         @c pre_period @c = @c 0. */
export template <typename Seq>
struct lasso_presentation {};

/**
 * @concept IsEventuallyPeriodic
 * @brief @c Seq has a finite-state (lasso) presentation: a
 *        @c lasso_presentation specialisation giving a positive @c period
 *        and a @c pre_period.  This is the @b finite-presentation witness
 *        that licenses a decidable, structural-recursive tail-equivalence
 *        test (see @c decide_tail_equivalence).
 */
export template <typename Seq>
concept IsEventuallyPeriodic = IsSequence<Seq> && requires {
  // Both members must be present and integral (so they are usable with
  // std::gcd / std::lcm and index arithmetic) — a non-integral period
  // could otherwise satisfy `> 0` yet break decide_tail_equivalence.
  requires std::integral<
      std::remove_cvref_t<decltype(lasso_presentation<Seq>::pre_period)>>;
  requires std::integral<
      std::remove_cvref_t<decltype(lasso_presentation<Seq>::period)>>;
  requires lasso_presentation<Seq>::period > 0;
};

/**
 * @brief Type-directed tail-equivalence decision: honest @c bool exactly
 *        on the finite-presentation domain, @c Ternary::Unknown otherwise.
 *
 * @details If both @c S and @c T are @c IsEventuallyPeriodic, the verdict
 * is computed by @b structural recursion over their lasso presentations —
 * a comparison on the provably-sufficient window
 * @c [max(Pₛ,Pₜ), max(Pₛ,Pₜ) + lcm(Cₛ,Cₜ)) — and returned as
 * @c Ternary::True / @c Ternary::False.  Otherwise the decision would
 * require @b general recursion over an unbounded tail, so it is honestly
 * rejected as @c Ternary::Unknown.  Contrast @c TailEquivalent::operator()
 * — the @c bool relation the congruence machinery consumes — whose
 * fixed-window read is operational/sampled; this is the decidable,
 * correct verdict where the type permits it.
 */
export template <typename S, typename T>
  requires(IsSequence<S> && IsSequence<T> &&
           std::same_as<typename S::Codomain, typename T::Codomain>)
constexpr dedekind::category::Ternary decide_tail_equivalence(const S& s,
                                                              const T& t) {
  using dedekind::category::Ternary;
  if constexpr (IsEventuallyPeriodic<S> && IsEventuallyPeriodic<T>) {
    constexpr auto max = std::numeric_limits<std::size_t>::max();
    const std::size_t ps =
        static_cast<std::size_t>(lasso_presentation<S>::pre_period);
    const std::size_t pt =
        static_cast<std::size_t>(lasso_presentation<T>::pre_period);
    const std::size_t cs =
        static_cast<std::size_t>(lasso_presentation<S>::period);
    const std::size_t ct =
        static_cast<std::size_t>(lasso_presentation<T>::period);
    const std::size_t start = std::max(ps, pt);
    // Compute lcm(cs, ct) without UB: lcm = cs/gcd * ct; bail to Unknown if
    // that product — or the start+len window — would overflow size_t.
    const std::size_t g = std::gcd(cs, ct);
    if (g == 0) {
      return Ternary::Unknown;
    }
    const std::size_t a = cs / g;
    if (a > max / ct) {
      return Ternary::Unknown;  // lcm overflow ⇒ undecidable within bounds
    }
    const std::size_t len = a * ct;
    if (len == 0 || start > max - len) {
      return Ternary::Unknown;  // window would overflow ⇒ undecidable here
    }
    // Compare on the IsArrow surface s(n) (IsSequence guarantees operator(),
    // not necessarily .at()) via the carrier's equality.
    const std::equal_to<typename S::Codomain> eq{};
    for (std::size_t k = 0; k < len; ++k) {
      const std::size_t n = start + k;
      if (!eq(s(static_cast<typename S::Domain>(n)),
              t(static_cast<typename T::Domain>(n)))) {
        return Ternary::False;
      }
    }
    return Ternary::True;
  } else {
    return dedekind::category::Ternary::Unknown;
  }
}

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

// A bare Path has no finite (lasso) presentation, so the general→decidable
// bridge does not fire — deciding tail-equivalence on it is general
// recursion (Ternary::Unknown).  Witness-based positive cases (decidable
// bool on lasso witnesses) live in the test, where the tagged carriers do.
static_assert(!IsEventuallyPeriodic<Path<int>>,
              "A bare Path<int> has no finite-state (lasso) presentation by "
              "default; the decidable tail-equivalence bridge requires one.");

}  // namespace dedekind::sequences
