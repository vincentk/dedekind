/**
 * @file dedekind/category/kleisli.cppm
 * @partition :kleisli
 * @brief The Kleisli Partition (The Extension Systems).
 *
 * @section kleisli__The_Kleisli_Unification
 * This partition defines the "Extension Systems" that serve as the functional
 * engine for the Dedekind ontology. By focusing on the Kleisli Triple (for
 * Monads) and the Co-Kleisli Triple (for Comonads), we sidestep C++ template
 * limitations regarding partial specialization of 'fmap'.
 *
 * @subsection The_Kleisli_Triple (Monadic Push)
 * A species provides a Kleisli system if it implements:
 * - η (Unit/Return): value injection into context.
 * - >>= (Bind)      : The 'extension' operator, chaining computations within
 * the context.
 *
 * @subsection The_Co_Kleisli_Triple (Comonadic Pull)
 * A species provides a Co-Kleisli system if it implements:
 * - ε (Counit/Extract): value extraction from context.
 * - <<= (Extend)       : The 'co-extension' operator, sampling the species into
 * a new context.
 *
 * @section kleisli__Notation_Mapping
 * Surface
 *
 * Core monadic/comonadic symbols and their implementation names:
 * - η : unit/pure (concrete overloads defined in `:natural`)
 * - μ : join/flatten (concrete overloads defined in `:natural`)
 * - ε : counit/extract (concrete overloads defined in `:natural`)
 * - δ : comultiplication/duplicate (concrete overloads defined in `:natural`)
 * - κ : Kleisli extension (`κ(ma, f)`), exported in this partition
 * - σ : co-Kleisli extension (`σ(wa, f)`), exported in this partition
 *
 * Operator and named aliases in this partition:
 * - `ma >>= f` and `bind(ma, f)` are aliases for κ(ma, f)
 * - `wa <<= f` and `extend(wa, f)` are aliases for σ(wa, f)
 * - fish aliases: `ma >> f` (Kleisli) and `wa << f` (co-Kleisli)
 *
 * @section kleisli__The_Bootstrapping_Mechanism
 * This partition is the prerequisite for the ':functorial' partition. Because
 * 'fmap' is an epi-phenomenon in this ontology, the presence of either a
 * Kleisli or Co-Kleisli structure allows the automatic derivation of functorial
 * mapping via the Highway Bridge.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "Important though the general concepts and propositions may be with
 * which the modern industrious passion for axiomatizing and generalizing has
 * presented us, in algebra perhaps more than anywhere else, nevertheless I am
 * convinced that the special problems in all their complexity constitute the
 * stock and core of mathematics."
 *       -- Hermann Weyl, The Classical Groups (1939)
 */
module;

#include <concepts>
#include <functional>
#include <ios>
#include <optional>  // std::nullopt — used by the _kleisli_arrow_witness_380 skeleton
#include <utility>  // std::move / std::forward (kept self-contained per #521 review)

export module dedekind.category:kleisli;

import :functor;  // maybe_functor (canonical Hub for unit_witness /
                  // counit_witness; #508 / #632)
import :natural;  // IsDefaultHubTag — type-checked constraint on the Kleisli M
                  // slot
import :monad;
import :small;
import :morphism;

namespace dedekind::category {

/** @section kleisli__The_Maybe_Action_Engine */

// Bind: Maybe<T> >>= (T -> Maybe<U>) -> Maybe<U>.
// Standard Maybe-monad bind: nullopt propagates; Some(x) feeds x into f.
export template <typename T, typename Func>
constexpr auto operator>>=(const Maybe<T>& m, Func&& f) {
  using Result = std::invoke_result_t<Func, T>;
  return m.has_value() ? std::forward<Func>(f)(*m) : Result{std::nullopt};
}

// Extend: Maybe<T> <<= (Maybe<T> -> U) -> Maybe<U>.
// The comonadic extend on Maybe.  Mathematically Maybe is not a true
// comonad (counit on nullopt has no honest answer), so this overload
// is well-defined only on the Some-fragment: when @c m is Some(x), the
// result is @c Some(f(m)); when @c m is nullopt, the result is also
// @c nullopt (the carrier propagates the indecision rather than
// fabricating a value).  Sister to the Box-as-trivial-comonad
// vestigial path retired in #632.
export template <typename T, typename Func>
constexpr auto operator<<=(const Maybe<T>& m, Func&& f) {
  using U = std::invoke_result_t<Func, Maybe<T>>;
  return m.has_value() ? Maybe<U>{std::forward<Func>(f)(m)}
                       : Maybe<U>{std::nullopt};
}

/**
 * @brief unit_witness<Hub, T>: Generic Kleisli unit witness.
 * Lifts a value into the Kleisli extension named by @c Hub::template Shape<T>.
 *
 * @tparam Hub A regular type carrying a nested @c Shape<U> alias; canonical
 *             example is @c maybe_functor<U> from @c :functor.
 * @tparam T   The value type.
 */
export template <typename Hub, typename T>
struct unit_witness;

export template <typename T>
struct unit_witness<maybe_functor<T>, T> final {
  constexpr auto operator()(T x) const { return Maybe<T>{std::move(x)}; }
};

/**
 * @brief counit_witness<Hub, T>: Generic Kleisli counit witness.
 * Samples a value from the co-Kleisli extension named by
 * @c Hub::template Shape<T>.
 *
 * @note For @c maybe_functor<T> the counit is well-defined only on the
 * Some-fragment of @c Maybe<T>: dereferencing @c std::nullopt has no
 * honest answer and produces UB.  Callers exercise the witness on
 * Some-values; the comonadic-extend / IsCoKleisliExtension /
 * IsFrobenius witnesses inherit this restriction.  See the
 * "kleisli__The_Maybe_Action_Engine" section header for the broader
 * mathematical concession (Maybe is a monad, not a true comonad).
 */
export template <typename Hub, typename T>
struct counit_witness;

export template <typename T>
struct counit_witness<maybe_functor<T>, T> final {
  constexpr T operator()(const Maybe<T>& m) const noexcept { return *m; }
};

/** @section kleisli__Extension_Concepts */

export template <typename Hub, typename T, typename U>
concept IsKleisliExtension =
    requires(T x, typename Hub::template Shape<T> box,
             std::function<typename Hub::template Shape<U>(T)> f) {
      {
        unit_witness<Hub, T>{}(x)
      } -> std::same_as<typename Hub::template Shape<T>>;
      { box >>= f } -> std::same_as<typename Hub::template Shape<U>>;
    };

export template <typename Hub, typename T, typename U>
concept IsCoKleisliExtension =
    requires(typename Hub::template Shape<T> box,
             std::function<U(typename Hub::template Shape<T>)> f) {
      { counit_witness<Hub, T>{}(box) } -> std::same_as<T>;
      { box <<= f } -> std::same_as<typename Hub::template Shape<U>>;
    };

export template <typename Hub, typename T, typename U>
concept IsFrobenius =
    IsKleisliExtension<Hub, T, U> && IsCoKleisliExtension<Hub, T, U>;

/** @section kleisli__Kleisli_Operators */

/**
 * @brief κ (kappa): Kleisli bind (monadic extension).
 * @details Given a monadic value ma: T<A> and a Kleisli arrow f: A → T<B>,
 * produces a value of type T<B> by:
 *   κ(ma, f) = μ(φ(ma, f))
 * This is the textbook Kleisli extension operation.
 *
 * @tparam MA The monadic type (e.g., Maybe<T>, Identity<T>)
 * @tparam F The function type to apply (typically A → MA<B>)
 *
 * @param ma The monadic value to bind
 * @param f The Kleisli arrow to extend
 * @return The result of applying μ(φ(ma, f))
 */
export template <typename MA, typename F>
  requires requires(MA const& ma, F&& f) { μ(φ(ma, std::forward<F>(f))); }
constexpr auto κ(MA const& ma, F&& f) {
  return μ(φ(ma, std::forward<F>(f)));
}

/**
 * @brief σ (sigma): Co-Kleisli extend (comonadic extension).
 * @details Given a comonadic value wa: W<A> and a co-Kleisli arrow f: W<A> → B,
 * produces a value of type W<B> by:
 *   σ(wa, f) = φ(δ(wa), f)
 * This is the textbook co-Kleisli extension operation (extend/cobind).
 *
 * @tparam WA The comonadic type (e.g., Identity<T>; Maybe<T> on the
 *             Some-fragment per the @c kleisli__The_Maybe_Action_Engine
 *             concession)
 * @tparam F The function type to apply (typically W<A> → B)
 *
 * @param wa The comonadic value to extend
 * @param f The co-Kleisli arrow to apply
 * @return The result of applying φ(δ(wa), f)
 */
export template <typename WA, typename F>
  requires requires(WA const& wa, F&& f) { φ(δ(wa), std::forward<F>(f)); }
constexpr auto σ(WA const& wa, F&& f) {
  return φ(δ(wa), std::forward<F>(f));
}

/**
 * @brief bind: named alias for monadic Kleisli extension.
 * @details Equivalent to κ(ma, f).
 */
export template <typename MA, typename F>
  requires requires(MA const& ma, F&& f) { κ(ma, std::forward<F>(f)); }
constexpr auto bind(MA const& ma, F&& f) {
  return κ(ma, std::forward<F>(f));
}

/**
 * @brief extend: named alias for comonadic co-Kleisli extension.
 * @details Equivalent to σ(wa, f).
 */
export template <typename WA, typename F>
  requires requires(WA const& wa, F&& f) { σ(wa, std::forward<F>(f)); }
constexpr auto extend(WA const& wa, F&& f) {
  return σ(wa, std::forward<F>(f));
}

/**
 * @brief bind with explicit hub-tag dispatch.
 * @details Default route: bind(tag, ma, f) = join(tag, φ(ma, f)).
 */
export template <typename HubTag, typename MA, typename F>
  requires IsDefaultHubTag<HubTag> &&
           requires(HubTag tag, MA const& ma, F&& f) {
             join(tag, φ(ma, std::forward<F>(f)));
           }
constexpr auto bind(HubTag tag, MA const& ma, F&& f) {
  return join(tag, φ(ma, std::forward<F>(f)));
}

/**
 * @brief extend with explicit hub-tag dispatch.
 * @details Default route: extend(tag, wa, f) = φ(duplicate(tag, wa), f).
 */
export template <typename HubTag, typename WA, typename F>
  requires IsDefaultHubTag<HubTag> &&
           requires(HubTag tag, WA const& wa, F&& f) {
             φ(duplicate(tag, wa), std::forward<F>(f));
           }
constexpr auto extend(HubTag tag, WA const& wa, F&& f) {
  return φ(duplicate(tag, wa), std::forward<F>(f));
}

/** @section kleisli__Kleisli_Bind_Operators */

/**
 * @brief operator>>= : Monadic bind (Kleisli arrow application).
 * @details Syntactic sugar for κ(ma, f).
 * Given ma: T<A> and f: A → T<B>, produces T<B>.
 */
export template <typename MA, typename F>
  requires requires(MA const& ma, F&& f) {
    { κ(ma, std::forward<F>(f)) };
  }
constexpr auto operator>>=(MA const& ma, F&& f) {
  return κ(ma, std::forward<F>(f));
}

/**
 * @brief operator>> : Fish alias for monadic bind.
 * @details Enables left-associative Kleisli pipelines:
 *   ma >> f >> g
 * which is parsed as (ma >> f) >> g and avoids assignment-operator
 * associativity of >>=.
 */
export template <typename MA, typename F>
  requires requires(MA const& ma, F&& f) {
    { φ(ma, std::forward<F>(f)) };
    { μ(φ(ma, std::forward<F>(f))) };
  }
constexpr auto operator>>(MA const& ma, F&& f) {
  return κ(ma, std::forward<F>(f));
}

/**
 * @brief operator<<= : Comonadic extend (co-Kleisli arrow application).
 * @details Syntactic sugar for σ(wa, f).
 * Given wa: W<A> and f: W<A> → B, produces W<B>.
 */
export template <typename WA, typename F>
  requires requires(WA const& wa, F&& f) {
    { σ(wa, std::forward<F>(f)) };
  }
constexpr auto operator<<=(WA const& wa, F&& f) {
  return σ(wa, std::forward<F>(f));
}

/**
 * @brief operator<< : Fish alias for comonadic extend.
 * @details Enables left-associative co-Kleisli pipelines:
 *   wa << f << g
 * which is parsed as (wa << f) << g and avoids assignment-operator
 * associativity of <<=.
 */
export template <typename WA, typename F>
  requires(!std::derived_from<std::remove_cvref_t<WA>, std::ios_base>) &&
          requires(WA const& wa, F&& f) {
            { δ(wa) };
            { φ(δ(wa), std::forward<F>(f)) };
          }
constexpr auto operator<<(WA const& wa, F&& f) {
  return σ(wa, std::forward<F>(f));
}

// ---------------------------------------------------------------------------
// Kleisli-bind / comonadic-extend operator-shape concepts (closes part of
// #450).  Sibling to @c HasArrowComposeOperators (in @c :morphism).
//
// Categorical reading:
//   * @c HasKleisliBindOperators<MA, F> classifies @c MA as supporting
//     composition in the Kleisli category 𝒞_M of a monad @c M (via @c
//     >>= ; @c >> is the fish alias).
//   * @c HasComonadicExtendOperators<WA, F> is the dual on the comonadic
//     side (via @c <<= ; @c << is the fish alias).
//
// The concepts pin only the @b syntactic surface; the monadic / comonadic
// laws (left/right unit, associativity for monads; counit, coassociativity
// for comonads) are the engineer's honesty obligation, structurally
// witnessed by the @c IsMonad / @c IsComonad concepts in @c :monad.
// ---------------------------------------------------------------------------

/**
 * @concept HasKleisliBindOperators
 * @brief A monadic carrier @c MA supports Kleisli bind via BOTH
 *        @c >>= AND its fish alias @c >> with a Kleisli arrow @c F.
 * @details Captures the operator-shape availability of monadic bind:
 *          given @c ma @c : @c M<A> and @c f @c : @c A @c → @c M<B>,
 *          @b both @c ma @c >>= @c f and @c ma @c >> @c f must
 *          compile.  Requiring both makes the concept faithful to the
 *          docstring's claim of fish-pair support; downstream code
 *          constrained by this concept may freely use either spelling.
 *          The laws (left/right unit, associativity) are not enforced
 *          here; they are pinned by @c IsMonad in @c :monad.
 */
export template <typename MA, typename F>
concept HasKleisliBindOperators = requires(MA const& ma, F&& f) {
  { ma >>= std::forward<F>(f) };
  { ma >> std::forward<F>(f) };
};

/**
 * @concept HasComonadicExtendOperators
 * @brief A comonadic carrier @c WA supports comonadic extend via
 *        BOTH @c <<= AND its fish alias @c << with a co-Kleisli
 *        arrow @c F.
 * @details Captures the operator-shape availability of comonadic
 *          extend: given @c wa @c : @c W<A> and @c f @c : @c W<A> @c
 *          → @c B, @b both @c wa @c <<= @c f and @c wa @c << @c f
 *          must compile.  Mirrors the existing @c operator<< guard
 *          on @c :kleisli that excludes @c std::ios_base-derived
 *          types so the concept doesn't accidentally fire on stream
 *          carriers; downstream code constrained by this concept may
 *          freely use either spelling.  The laws (counit,
 *          coassociativity) are pinned by @c IsComonad in @c :monad.
 */
export template <typename WA, typename F>
concept HasComonadicExtendOperators =
    (!std::derived_from<std::remove_cvref_t<WA>, std::ios_base>) &&
    requires(WA const& wa, F&& f) {
      { wa <<= std::forward<F>(f) };
      { wa << std::forward<F>(f) };
    };

// ---------------------------------------------------------------------------
// operator-> — Kleisli dereference / comonadic extract (#531, step 1).
//
// A C++ arrow-dereference expression @c m->member on a monadic /
// comonadic carrier has two CT readings depending on the carrier
// shape:
//
//   * @b Comonadic carrier (e.g. @c Identity<T>; future smart-pointer-
//     like comonads): @c m->member @c ≡ @c ε(m).member, where
//     @c ε: @c W<A> @c → @c A is the comonadic @b extract (Mac Lane
//     @em CWM §VI; Wadler 1992 @em Comprehending @em Monads).
//   * @b Monadic carrier with partial dereference (e.g. @c Maybe<T>):
//     @c m->member is Kleisli sugar — well-defined when the monadic
//     value is "present", behaviour on absence is carrier-specific
//     (UB / throw / sentinel).
//
// Both readings share the operator surface; the codebase reifies them
// in the two-layer @c Has*Operators / strict-concept pattern:
//
//   1. @c HasArrowDereferenceOperator<M> — purely syntactic.
//   2. @c IsKleisliDeref<M> — opt-in, witnesses the categorical
//      reading at the carrier site via @c is_kleisli_deref_v.
// ---------------------------------------------------------------------------

/** @concept HasArrowDereferenceOperator
 *  @brief Syntactic check: @c m.operator->() compiles for @c M.
 */
export template <typename M>
concept HasArrowDereferenceOperator = requires(M const& m) { m.operator->(); };

/** @brief @c is_kleisli_deref_v<M>: opt-in marker that the carrier's
 *         @c operator-> realises Kleisli dereference (for monadic
 *         carriers like @c Maybe<T>) or comonadic extract @c ε
 *         (for comonadic carriers like @c Identity<T>).  Default false;
 *         specialise to @c true at the carrier site. */
export template <typename M>
inline constexpr bool is_kleisli_deref_v = false;

/** @concept IsKleisliDeref
 *  @brief @c M exposes @c operator-> and the carrier site has
 *         declared it as Kleisli dereference / comonadic extract
 *         (Mac Lane CWM §VI; Wadler 1992).
 */
export template <typename M>
concept IsKleisliDeref =
    HasArrowDereferenceOperator<M> && is_kleisli_deref_v<M>;

/**
 * @brief User-declared "Kleisli arrow" witness (#380, step 1).
 * @details A Kleisli arrow lives in the Kleisli category indexed by a
 *          monad-hub tag @c M.  Its underlying map has shape
 *          @c e: @c A @c → @c T<B>, where @c T is the monadic carrier
 *          shape selected by the hub tag (@c Maybe for
 *          @c maybe_hub_tag, @c Identity for @c identity_hub_tag).  The @c M
 * slot is the
 *          @b monad-hub @b tag itself — a tag type that names which
 *          Kleisli category the arrow inhabits — not the class template.
 *          Constrained by @c IsDefaultHubTag to make the connection
 *          type-checked; downstream code can extend by adding new tags
 *          to the @c IsDefaultHubTag disjunction in @c :natural.
 *          Default @c false; opt-in by specialising to @c true at the
 *          @c (E, M) pair.  Pairs with the composition / fmap
 *          machinery above; this trait only declares membership in the
 *          Kleisli category, it does not implement composition.
 */
export template <typename E, typename M>
  requires IsDefaultHubTag<M>
inline constexpr bool is_kleisli_arrow_v = false;

/**
 * @concept IsKleisliArrow
 * @brief An arrow declared to live in the Kleisli category indexed by
 *        the monad-hub tag @c M (constrained by @c IsDefaultHubTag).
 * @details Opt-in via @c is_kleisli_arrow_v<E, M> @c = @c true.
 */
export template <typename E, typename M>
concept IsKleisliArrow =
    IsArrow<E> && IsDefaultHubTag<M> && is_kleisli_arrow_v<E, M>;

namespace _kleisli_arrow_witness_380 {

// Witness skeleton (#380 step 1): a hand-rolled arrow E with shape
// @c A @c → @c Maybe<B> opts in to @c IsKleisliArrow<E, maybe_hub_tag>
// via the @c is_kleisli_arrow_v specialisation below.  Demonstrates the
// end-to-end pattern step 2 of #380 will apply to real
// @c try_realize_to_<primitive> arrows: declare the arrow, register
// the trait, recover @c IsKleisliArrow at the use site.
struct toy_partial_realize {
  using ArrowKind = spoke_arrow_tag;
  using Domain = int;
  using Codomain = Maybe<int>;
  constexpr Maybe<int> operator()(int n) const {
    return n >= 0 ? Maybe<int>{n} : std::nullopt;
  }
};

}  // namespace _kleisli_arrow_witness_380

template <>
inline constexpr bool is_kleisli_arrow_v<
    _kleisli_arrow_witness_380::toy_partial_realize, maybe_hub_tag> = true;

namespace _kleisli_arrow_witness_380 {

static_assert(IsArrow<toy_partial_realize>,
              "Witness arrow shape: A → Maybe<B> qualifies as IsArrow.");
static_assert(
    IsKleisliArrow<toy_partial_realize, maybe_hub_tag>,
    "Opt-in via is_kleisli_arrow_v<E, maybe_hub_tag> = true lifts the "
    "arrow into the IsKleisliArrow concept.");
static_assert(
    !IsKleisliArrow<toy_partial_realize, identity_hub_tag>,
    "Opt-in is per-(E, M) pair: a Maybe-shaped Kleisli arrow does not "
    "automatically inhabit the Identity Kleisli category.");

}  // namespace _kleisli_arrow_witness_380

}  // namespace dedekind::category
