/**
 * @file dedekind/category/kleisli.cppm
 * @partition :kleisli
 * @brief The Kleisli Partition (The Extension Systems).
 *
 * @section kleisli__The_Kleisli_Unification
 * This partition defines the "Extension Systems" that serve as the functional
 * engine for the Dedekind ontology. By focusing on the Kleisli Triple (for
 * Monads) and the Co-Kleisli Triple (for Comonads), extension reads the
 * functor's action on arrows, F(f), and needs no applicator of its own.
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
 * Kleisli extension is μ ∘ T(f) and co-Kleisli extension W(f) ∘ δ: both read
 * the functor's action on arrows, F(f), from @c :functor (#1114).
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
#include <tuple>        // std::tuple<T> — bona-fide Frobenius carrier (#632)
#include <type_traits>  // std::remove_cvref_t: a container to its functor
#include <utility>  // std::move / std::forward (kept self-contained per #521 review)

export module dedekind.category:kleisli;

import :functor;  // maybe_functor (canonical Functor for unit_witness,
                  // monad-only) and tuple_functor (canonical Functor for both
                  // unit_witness AND counit_witness; #508 / #632)
import :natural;  // IsDefaultMonadTag — type-checked constraint on the Kleisli
                  // M slot
import :monad;
import :small;
import :morphism;

namespace dedekind::category {

/** @section kleisli__The_Maybe_Action_Engine
 *
 * @details Maybe is a monad, not a comonad — counit on @c std::nullopt
 * has no honest answer.  This block defines only the monadic surface
 * for @c Maybe<T> (bind, unit_witness).  The Frobenius monad (a monad
 * and a comonad on one functor, μ and δ related by the Frobenius law) is
 * @c std::tuple<T> below; that's where counit / extend /
 * IsCoKleisliTriple / IsFrobenius witnesses live.
 */

/**
 * @brief unit_witness<Functor, T>: Generic Kleisli unit witness.
 * Lifts a value into the Kleisli extension named by @c Functor::template
 * Shape<T>.
 *
 * @tparam Functor A regular type carrying a nested @c Shape<U> alias; canonical
 *             examples are @c maybe_functor<U> (monad-only) and
 *             @c tuple_functor<U> (Frobenius) from @c :functor.
 * @tparam T   The value type.
 */
export template <typename Functor, typename T>
struct unit_witness;

export template <typename T>
struct unit_witness<maybe_functor<T>, T> final {
  using Domain = T;
  using Codomain = Maybe<T>;
  constexpr Maybe<T> operator()(T x) const { return Maybe<T>{std::move(x)}; }
};

/**
 * @brief counit_witness<Functor, T>: Generic Kleisli counit witness.
 * Samples a value from the co-Kleisli extension named by
 * @c Functor::template Shape<T>.  Defined for genuine comonadic carriers
 * only; @c maybe_functor is not specialised here because Maybe is a
 * monad, not a comonad (counit on nullopt has no honest answer).
 */
export template <typename Functor, typename T>
struct counit_witness;

/** @section kleisli__The_Tuple_Frobenius_Engine
 *
 * @details @c std::tuple<T> is a 1-tuple — always has a single
 * element, so @c std::get<0> is total.  This makes it the project's
 * Frobenius monad: both Kleisli and co-Kleisli laws hold without
 * concession, and μ and δ, each the identity up to the one-tuple's
 * wrapping, satisfy the Frobenius law.  Replaces the vestigial
 * Box-as-trivially-comonadic path retired in #632; the std-blessed
 * @c std::tuple wrapper carries the Frobenius role going forward.
 */

export template <typename T>
struct unit_witness<tuple_functor<T>, T> final {
  using Domain = T;
  using Codomain = std::tuple<T>;
  constexpr std::tuple<T> operator()(T x) const {
    return std::tuple<T>{std::move(x)};
  }
};

export template <typename T>
struct counit_witness<tuple_functor<T>, T> final {
  using Domain = std::tuple<T>;
  using Codomain = T;
  constexpr T operator()(const std::tuple<T>& t) const noexcept {
    return std::get<0>(t);
  }
};

/** @section kleisli__Extension_Concepts */

/** @brief Trait: @c Functor's unit and bind satisfy the Kleisli-triple laws
 *  (Manes): @f$\eta(x)^* f = f(x)@f$, @f$m^* \eta = m@f$ and
 *  @f$(m^* f)^* g = m^* (x \mapsto f(x)^* g)@f$.  A certificate, primary
 *  false, declared where a container's bind keeps them by construction.
 *  @tparam Functor the functor. */
export template <typename Functor>
struct is_kleisli_triple : std::false_type {};
/** @brief Trait: the co-Kleisli-triple laws, the duals, for @c Functor's
 *  counit and extend.  @tparam Functor the functor. */
export template <typename Functor>
struct is_cokleisli_triple : std::false_type {};
template <typename T>
struct is_kleisli_triple<maybe_functor<T>> : std::true_type {};
template <typename T>
struct is_kleisli_triple<tuple_functor<T>> : std::true_type {};
template <typename T>
struct is_cokleisli_triple<tuple_functor<T>> : std::true_type {};
/** @brief Trait: @c Functor's multiplication and comultiplication satisfy the
 *  Frobenius law (Street, "Frobenius monads and pseudomonoids", 2004):
 *  @f$T\mu \circ \delta T = \delta \circ \mu = \mu T \circ T\delta@f$.  A
 *  certificate, primary false.  Being a monad and a comonad is not enough:
 *  on streams (the diagonal monad, the suffix comonad) @f$\delta \circ \mu@f$
 *  reads @f$s_{i+j}(i+j)@f$ where @f$T\mu \circ \delta T@f$ reads
 *  @f$s_{i+j}(j)@f$.  @tparam Functor the functor. */
export template <typename Functor>
struct is_frobenius_monad : std::false_type {};
/** The one-tuple: μ and δ are the identity up to wrapping, both sides of the
 *  law the identity. */
template <typename T>
struct is_frobenius_monad<tuple_functor<T>> : std::true_type {};

/** @brief An arrow @c A → @c B the triple concepts probe bind and extend
 *  with: only its type is read, never its value.  In this namespace, not a
 *  detail one, so argument-dependent lookup finds the @c >>= and @c <<=
 *  defined below.  @tparam A @tparam B the ends. */
template <typename A, typename B>
struct kleisli_probe_arrow {
  using Domain = A;
  using Codomain = B;
  B operator()(const A&) const { std::unreachable(); }
};

/** @brief A Kleisli triple ⟨T, η, (−)*⟩ (Manes), the monad in its extension
 *  form: the unit and bind, their laws certified by @c is_kleisli_triple.
 *  @tparam Functor the functor.  @tparam T @tparam U the element types. */
export template <typename Functor, typename T, typename U>
concept IsKleisliTriple =
    is_kleisli_triple<Functor>::value &&
    requires(T x, typename Functor::template Shape<T> box,
             kleisli_probe_arrow<T, typename Functor::template Shape<U>> f) {
      {
        unit_witness<Functor, T>{}(x)
      } -> std::same_as<typename Functor::template Shape<T>>;
      { box >>= f } -> std::same_as<typename Functor::template Shape<U>>;
    };

/** @brief A co-Kleisli triple ⟨W, ε, (−)†⟩, the comonad in its extension
 *  form: the counit and extend, their laws certified by
 *  @c is_cokleisli_triple.  @tparam Functor the functor.  @tparam T @tparam U
 *  the element types. */
export template <typename Functor, typename T, typename U>
concept IsCoKleisliTriple =
    is_cokleisli_triple<Functor>::value &&
    requires(typename Functor::template Shape<T> box,
             kleisli_probe_arrow<typename Functor::template Shape<T>, U> f) {
      { counit_witness<Functor, T>{}(box) } -> std::same_as<T>;
      { box <<= f } -> std::same_as<typename Functor::template Shape<U>>;
    };

/** @brief A Frobenius monad (Street): a Kleisli and a co-Kleisli triple on one
 *  functor whose μ and δ satisfy the Frobenius law, certified by
 *  @c is_frobenius_monad.  @tparam Functor the functor.  @tparam T
 *  @tparam U the element types. */
export template <typename Functor, typename T, typename U>
concept IsFrobenius =
    IsKleisliTriple<Functor, T, U> && IsCoKleisliTriple<Functor, T, U> &&
    is_frobenius_monad<Functor>::value;

/** @section kleisli__Kleisli_Operators */

/** @brief The functor whose values a container is: @c Maybe<A> a value of
 *  @c maybe_functor<A>, the one-tuple of @c tuple_functor<A>.  Kleisli
 *  extension reads its action on arrows, @f$\mu \circ T(f)@f$.
 *  @tparam MA the container. */
export template <typename MA>
struct functor_of {};
template <typename A>
struct functor_of<Maybe<A>> {
  using type = maybe_functor<A>;
};
template <typename A>
struct functor_of<std::tuple<A>> {
  using type = tuple_functor<A>;
};
export template <typename MA>
using functor_of_t = typename functor_of<std::remove_cvref_t<MA>>::type;

namespace detail_kleisli {
/** @brief @f$T(f)@f$ applied to @c ma: the arrow @c f, from the value's
 *  element, under @c T's action, at the value.  @param ma the value.
 *  @param f the arrow. */
template <typename MA, IsArrow F>
  requires requires { typename functor_of_t<MA>; } &&
           std::same_as<Dom<F>, typename functor_of_t<MA>::Σ_cat::Species>
constexpr auto lifted(const MA& ma, F f) {
  return functor_of_t<MA>{}(std::move(f))(ma);
}
}  // namespace detail_kleisli

/**
 * @brief κ (kappa): Kleisli bind (monadic extension).
 * @details Given a monadic value ma: T<A> and a Kleisli arrow f: A → T<B>,
 * produces a value of type T<B> by:
 *   κ(ma, f) = μ(T(f)(ma))
 * This is the textbook Kleisli extension operation.
 *
 * @tparam MA The monadic type (e.g., Maybe<T>, Identity<T>)
 * @tparam F The function type to apply (typically A → MA<B>)
 *
 * @param ma The monadic value to bind
 * @param f The Kleisli arrow to extend
 * @return @f$\mu(T(f)(ma))@f$
 */
export template <typename MA, typename F>
  requires requires(MA const& ma, F&& f) {
    μ(detail_kleisli::lifted(ma, std::forward<F>(f)));
  }
constexpr auto κ(MA const& ma, F f) {
  return μ(detail_kleisli::lifted(ma, std::move(f)));  // μ ∘ T(f)
}

/**
 * @brief σ (sigma): Co-Kleisli extend (comonadic extension).
 * @details Given a comonadic value wa: W<A> and a co-Kleisli arrow f: W<A> → B,
 * produces a value of type W<B> by:
 *   σ(wa, f) = W(f)(δ(wa))
 * This is the textbook co-Kleisli extension operation (extend/cobind).
 *
 * @tparam WA The comonadic type (e.g., @c std::tuple<T> as the
 *             Frobenius monad per @c
 *             kleisli__The_Tuple_Frobenius_Engine, or the identity-
 *             arrow Identity).  Maybe is monad-only and intentionally
 *             not a comonadic carrier here (#632).
 * @tparam F The function type to apply (typically W<A> → B)
 *
 * @param wa The comonadic value to extend
 * @param f The co-Kleisli arrow to apply
 * @return @f$W(f)(\delta(wa))@f$
 */
export template <typename WA, typename F>
  requires requires(WA const& wa, F&& f) {
    detail_kleisli::lifted(δ(wa), std::forward<F>(f));
  }
constexpr auto σ(WA const& wa, F f) {
  return detail_kleisli::lifted(δ(wa), std::move(f));  // W(f) ∘ δ
}

/**
 * @brief bind: named alias for monadic Kleisli extension.
 * @details Equivalent to κ(ma, f).
 */
export template <typename MA, typename F>
  requires requires(MA const& ma, F&& f) { κ(ma, std::forward<F>(f)); }
constexpr auto bind(MA const& ma, F f) {
  return κ(ma, std::move(f));
}

/**
 * @brief extend: named alias for comonadic co-Kleisli extension.
 * @details Equivalent to σ(wa, f).
 */
export template <typename WA, typename F>
  requires requires(WA const& wa, F&& f) { σ(wa, std::forward<F>(f)); }
constexpr auto extend(WA const& wa, F f) {
  return σ(wa, std::move(f));
}

/**
 * @brief bind with explicit monad-tag dispatch.
 * @details Default route: bind(tag, ma, f) = join(tag, T(f)(ma)).
 */
export template <typename MonadTag, typename MA, typename F>
  requires IsDefaultMonadTag<MonadTag> &&
           requires(MonadTag tag, MA const& ma, F&& f) {
             join(tag, detail_kleisli::lifted(ma, std::forward<F>(f)));
           }
constexpr auto bind(MonadTag tag, MA const& ma, F f) {
  return join(tag, detail_kleisli::lifted(ma, std::move(f)));
}

/**
 * @brief extend with explicit monad-tag dispatch.
 * @details Default route: extend(tag, wa, f) = W(f)(duplicate(tag, wa)).
 */
export template <typename MonadTag, typename WA, typename F>
  requires IsDefaultMonadTag<MonadTag> &&
           requires(MonadTag tag, WA const& wa, F&& f) {
             detail_kleisli::lifted(duplicate(tag, wa), std::forward<F>(f));
           }
constexpr auto extend(MonadTag tag, WA const& wa, F f) {
  return detail_kleisli::lifted(duplicate(tag, wa), std::move(f));
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
constexpr auto operator>>=(MA const& ma, F f) {
  return κ(ma, std::move(f));
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
constexpr auto operator<<=(WA const& wa, F f) {
  return σ(wa, std::move(f));
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
 * @brief A monadic carrier @c MA supports Kleisli bind, @c >>=, with a
 *        Kleisli arrow @c F; @c >> composes arrows and nothing else.
 * @details Captures the operator-shape availability of monadic bind:
 *          given @c ma @c : @c M<A> and @c f @c : @c A @c → @c M<B>,
 *          @c ma @c >>= @c f must compile.
 *          The laws (left/right unit, associativity) are not enforced
 *          here; they are pinned by @c IsMonad in @c :monad.
 */
export template <typename MA, typename F>
concept HasKleisliBindOperators = requires(MA const& ma, F&& f) {
  { ma >>= std::forward<F>(f) };
};

/**
 * @concept HasComonadicExtendOperators
 * @brief A comonadic carrier @c WA supports comonadic extend, @c <<=,
 *        with a co-Kleisli arrow @c F.
 * @details Captures the operator-shape availability of comonadic
 *          extend: given @c wa @c : @c W<A> and @c f @c : @c W<A> @c
 *          → @c B, @c wa @c <<= @c f must compile.  It excludes
 *          @c std::ios_base-derived types, so that it does not fire on
 *          stream carriers.  The laws (counit,
 *          coassociativity) are pinned by @c IsComonad in @c :monad.
 */
export template <typename WA, typename F>
concept HasComonadicExtendOperators =
    (!std::derived_from<std::remove_cvref_t<WA>, std::ios_base>) &&
    requires(WA const& wa, F&& f) {
      { wa <<= std::forward<F>(f) };
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
 *          monad tag @c M.  Its underlying map has shape
 *          @c e: @c A @c → @c T<B>, where @c T is the monadic carrier
 *          shape selected by the monad tag (@c Maybe for @c maybe_monad_tag,
 *          @c Identity for @c identity_monad_tag).  The @c M slot is the
 *          @b monad @b tag itself — a tag type that names which
 *          Kleisli category the arrow inhabits — not the class template.
 *          Constrained by @c IsDefaultMonadTag to make the connection
 *          type-checked; downstream code can extend by adding new tags
 *          to the @c IsDefaultMonadTag disjunction in @c :natural.
 *          Default @c false; opt-in by specialising to @c true at the
 *          @c (E, M) pair.  Pairs with the Kleisli extension
 *          above; this trait only declares membership in the
 *          Kleisli category, it does not implement composition.
 */
export template <typename E, typename M>
  requires IsDefaultMonadTag<M>
inline constexpr bool is_kleisli_arrow_v = false;

/**
 * @concept IsKleisliArrow
 * @brief An arrow declared to live in the Kleisli category indexed by
 *        the monad tag @c M (constrained by @c IsDefaultMonadTag).
 * @details Opt-in via @c is_kleisli_arrow_v<E, M> @c = @c true.
 */
export template <typename E, typename M>
concept IsKleisliArrow =
    IsArrow<E> && IsDefaultMonadTag<M> && is_kleisli_arrow_v<E, M>;

namespace _kleisli_arrow_witness_380 {

// Witness skeleton (#380 step 1): a hand-rolled arrow E with shape
// @c A @c → @c Maybe<B> opts in to @c IsKleisliArrow<E, maybe_monad_tag>
// via the @c is_kleisli_arrow_v specialisation below.  Demonstrates the
// end-to-end pattern step 2 of #380 will apply to real
// @c try_realize_to_<primitive> arrows: declare the arrow, register
// the trait, recover @c IsKleisliArrow at the use site.
struct toy_partial_realize {
  using Domain = int;
  using Codomain = Maybe<int>;
  constexpr Maybe<int> operator()(int n) const {
    return n >= 0 ? Maybe<int>{n} : std::nullopt;
  }
};

}  // namespace _kleisli_arrow_witness_380

template <>
inline constexpr bool is_kleisli_arrow_v<
    _kleisli_arrow_witness_380::toy_partial_realize, maybe_monad_tag> = true;

namespace _kleisli_arrow_witness_380 {

static_assert(IsArrow<toy_partial_realize>,
              "Witness arrow shape: A → Maybe<B> qualifies as IsArrow.");
static_assert(
    IsKleisliArrow<toy_partial_realize, maybe_monad_tag>,
    "Opt-in via is_kleisli_arrow_v<E, maybe_monad_tag> = true lifts the "
    "arrow into the IsKleisliArrow concept.");
static_assert(
    !IsKleisliArrow<toy_partial_realize, identity_monad_tag>,
    "Opt-in is per-(E, M) pair: a Maybe-shaped Kleisli arrow does not "
    "automatically inhabit the Identity Kleisli category.");

}  // namespace _kleisli_arrow_witness_380

}  // namespace dedekind::category
