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
 * @section kleisli__Notation_Mapping Textbook Notation -> Implementation
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

export module dedekind.category:kleisli;

import :monad;
import :small;
import :morphism;

namespace dedekind::category {

/** @section kleisli__The_Box_Action_Engine */

// Bind: Box<T> >>= (T -> Box<U>) -> Box<U>
export template <typename T, typename Func>
constexpr auto operator>>=(const Box<T>& b, Func&& f) {
  return std::forward<Func>(f)(b.value);
}

// Extend: Box<T> <<= (Box<T> -> U) -> Box<U>
export template <typename T, typename Func>
constexpr auto operator<<=(const Box<T>& b, Func&& f) {
  using U = std::invoke_result_t<Func, Box<T>>;
  return Box<U>{std::forward<Func>(f)(b)};
}

/** @section kleisli__The_Kleisli_Witnesses */

/**
 * @brief unit_witness<F, T>: Generic Kleisli unit witness.
 * Lifts a value into the Kleisli extension.
 */
export template <template <typename...> typename F, typename T>
struct unit_witness;

export template <typename T>
struct unit_witness<Box, T> final {
  constexpr auto operator()(T x) const { return Box<T>{std::move(x)}; }
};

/**
 * @brief counit_witness<F, T>: Generic Kleisli counit witness.
 * Samples a value from the co-Kleisli extension.
 */
export template <template <typename...> typename F, typename T>
struct counit_witness;

export template <typename T>
struct counit_witness<Box, T> final {
  constexpr T operator()(const Box<T>& b) const noexcept { return b.value; }
};

/** @section kleisli__Extension_Concepts */

export template <template <typename...> typename F, typename T, typename U>
concept IsKleisliExtension = requires(T x, F<T> box, std::function<F<U>(T)> f) {
  { unit_witness<F, T>{}(x) } -> std::same_as<F<T>>;
  { box >>= f } -> std::same_as<F<U>>;
};

export template <template <typename...> typename F, typename T, typename U>
concept IsCoKleisliExtension = requires(F<T> box, std::function<U(F<T>)> f) {
  { counit_witness<F, T>{}(box) } -> std::same_as<T>;
  { box <<= f } -> std::same_as<F<U>>;
};

export template <template <typename...> typename F, typename T, typename U>
concept IsFrobenius =
    IsKleisliExtension<F, T, U> && IsCoKleisliExtension<F, T, U>;

/** @section kleisli__Kleisli_Operators */

/**
 * @brief κ (kappa): Kleisli bind (monadic extension).
 * @details Given a monadic value ma: T<A> and a Kleisli arrow f: A → T<B>,
 * produces a value of type T<B> by:
 *   κ(ma, f) = μ(φ(ma, f))
 * This is the textbook Kleisli extension operation.
 *
 * @tparam MA The monadic type (e.g., Box<T>, Maybe<T>, Identity<T>)
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
 * @tparam WA The comonadic type (e.g., Box<T>, Identity<T>)
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

}  // namespace dedekind::category
