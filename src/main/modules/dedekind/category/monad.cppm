/**
 * @file dedekind/category/monad.cppm
 * @partition :monad
 * @brief Monads and Comonads (Context as Algebra).
 *
 * This partition internalizes monads and comonads as endofunctor-level
 * algebraic structure. Because categories in this library are specialized to a
 * single `Species` type, the object witness used by the laws is always
 * recovered through the identity arrow `id_c(x)` rather than through a
 * separate textbook object universe.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "The introduction of the digit 0 or the group concept was general
 * nonsense too, and mathematics was more or less stagnating for thousands of
 * years because nobody was around to take such childish steps."
 *       -- Alexander Grothendieck, letter to Ronald Brown (5 May 1982)
 */
module;

#include <concepts>
#include <functional>
#include <type_traits>  // the monad-law certificates

export module dedekind.category:monad;

import :natural;
import :morphism;  // IsIdentityArrow: the identity monad, decided

namespace dedekind::category {

/**
 * @section monad__Monadic_and_Comonadic_Components
 *
 * The concrete implementations of η (Unit), μ (Multiplication), ε (Counit),
 * and δ (Comultiplication) are defined in the :natural partition for each
 * supported monad/comonad type (e.g., η for Maybe, Identity, Box). These
 * are specific overloads, not generic template-template helpers.
 *
 * This design follows the tag dispatch of the :functor partition, avoiding C++
 * limitations with template-template parameter overloading and maintaining
 * clean partition boundaries.
 *
 * Textbook alignment:
 * - η (eta): unit/pure      [defined in :natural]
 * - μ (mu):  multiplication [defined in :natural]
 * - ε (epsilon): counit     [defined in :natural]
 * - δ (delta): comultiplication [defined in :natural]
 */

/**
 * @brief Value-level alias for η (eta) with explicit monad-tag dispatch.
 * @details Example: pure(maybe_monad, x), pure(identity_monad, x),
 *          pure(tuple_monad, x).
 */
export template <typename MonadTag, typename A>
  requires IsDefaultMonadTag<MonadTag> &&
           requires(MonadTag tag, A&& v) { η(tag, std::forward<A>(v)); }
constexpr auto pure(MonadTag tag, A value) {
  return η(tag, std::move(value));
}

/** @brief Value-level alias for μ (mu) with explicit monad-tag dispatch. */
export template <typename MonadTag, typename MMA>
  requires IsDefaultMonadTag<MonadTag> &&
           requires(MonadTag tag, MMA const& mma) { μ(tag, mma); }
constexpr auto join(MonadTag tag, MMA const& mma) {
  return μ(tag, mma);
}

/** @brief Value-level alias for ε (epsilon) with explicit monad-tag dispatch.
 */
export template <typename MonadTag, typename WA>
  requires IsDefaultMonadTag<MonadTag> &&
           requires(MonadTag tag, WA const& wa) { ε(tag, wa); }
constexpr auto extract(MonadTag tag, WA const& wa) {
  return ε(tag, wa);
}

/** @brief Value-level alias for δ (delta) with explicit monad-tag dispatch. */
export template <typename MonadTag, typename WA>
  requires IsDefaultMonadTag<MonadTag> &&
           requires(MonadTag tag, WA const& wa) { δ(tag, wa); }
constexpr auto duplicate(MonadTag tag, WA const& wa) {
  return δ(tag, wa);
}

/**
 * @section monad__Monad_as_Monoid
 * We bridge the gap:
 *   η (Unit)           <--> unit_v (Monoid Unit)
 *   μ (Multiplication) <--> Op         (Monoid Operation)
 *
 * @brief T : C -> C with η : Id ⟹ T and μ : T² ⟹ T
 * @details In the project's single-species setting, the component laws are
 *          checked against an object label `c`, then transported into the
 *          endofunctor context by lifting the identity arrow `id_c(c)`. This
 *          is how the type system recovers the object witnesses `T(c)` and
 *          `T(T(c))` needed for η and μ.
 *
 * Textbook note: this concept intentionally requires `T` to satisfy
 * @ref IsEndofunctor. Therefore, a functor that changes the ambient category
 * (for example `Set<T> -> Set<std::optional<T>>`) is not admitted as an
 * `IsMonad` witness in this formalization, even if value-level `η/μ/κ`
 * behavior exists for its carrier.
 */
/** @brief Trait: @c η and @c μ satisfy the monad laws on @c T,
 *  associativity @f$\mu \circ T\mu = \mu \circ \mu T@f$ and the two unit
 *  laws @f$\mu \circ T\eta = \mathrm{id} = \mu \circ \eta T@f$.  A
 *  certificate, primary false: the two sides of each law are different
 *  types that denote one arrow, so no type shows them.  Decided without it
 *  where both components are identities (the identity monad), by the unit
 *  law.  @tparam T the endofunctor.  @tparam η_t @tparam μ_t its unit and
 *  multiplication. */
export template <typename T, typename η_t, typename μ_t>
struct is_monadic : std::false_type {};
/** @brief Trait: the comonad laws for @c ε and @c δ on @c W, the duals.
 *  @tparam W the endofunctor.  @tparam ε_t @tparam δ_t its counit and
 *  comultiplication. */
export template <typename W, typename ε_t, typename δ_t>
struct is_comonadic : std::false_type {};

export template <typename T, typename η_t, typename μ_t>
concept IsMonad =
    IsEndofunctor<T> &&
    // η: Id_C ⟹ T (Unit)
    IsNaturalTransformation<η_t, identity_functor<typename T::Σ_cat>, T> &&
    // μ: T² ⟹ T (Multiplication/Join)
    IsNaturalTransformation<μ_t, composite_functor<T, T>, T> &&

    // the laws: decided where both components are identities, the unit law
    // at the type; certified otherwise
    (requires(η_t η, μ_t μ, typename T::Σ_cat::Arrow::Domain c) {
      requires IsIdentityArrow<decltype(η(c))> &&
                   IsIdentityArrow<decltype(μ(c))>;
    } || is_monadic<T, η_t, μ_t>::value);

/**
 * @section monad__Comonadic_Morphisms
 */

/**
 * @concept IsComonad
 * @brief The Unified Comonadic Proof: Co-Kleisli Action as Contextual Being.
 *
 * @details
 * This concept bridges the dual formal definitions:
 * 1. Co-Kleisli Triple (Action): Existence of ε (Extract) and <<= (Extend).
 * 2. Comonoid in Endofunctors (Structure): F is a Functor with δ (Duplicate).
 *
 * As with `IsMonad`, the laws are stated over a single object label from the
 * source category. The corresponding contextual objects are recovered through
 * identity lifting, which is the canonical witness mechanism in this library's
 * single-species categories.
 *
 * @section monad__The_Mereological_Pull
 * Following the Dedekind posture, we do not require δ (Duplicate/Coreturn)
 * to be explicitly defined if <<= (Extend) is present, as δ is the
 * "Self-Extend": δ(w) = w <<= id.
 */
export template <typename W, typename ε_t, typename δ_t>
concept IsComonad =
    IsEndofunctor<W> &&
    // ε: W ⟹ Id_C (Counit / Extract)
    IsNaturalTransformation<ε_t, W, identity_functor<typename W::Σ_cat>> &&
    // δ: W ⟹ W² (Comultiplication / Duplicate)
    IsNaturalTransformation<δ_t, W, composite_functor<W, W>> &&

    (requires(ε_t ε, δ_t δ, typename W::Σ_cat::Arrow::Domain c) {
      requires IsIdentityArrow<decltype(ε(c))> &&
                   IsIdentityArrow<decltype(δ(c))>;
    } || is_comonadic<W, ε_t, δ_t>::value);

}  // namespace dedekind::category
