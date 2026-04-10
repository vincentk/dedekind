/**
 * @file dedekind.category:monad
 * @partition :monad
 * @brief Level 2.5: Monads and Comonads (Context as Algebra).
 *
 * This partition internalizes monads and comonads as endofunctor-level
 * algebraic structure. Because categories in this library are specialized to a
 * single `Species` type, the object witness used by the laws is always
 * recovered through the identity spoke `id_c(x)` rather than through a
 * separate textbook object universe.
 */
module;

#include <concepts>
#include <functional>

export module dedekind.category:monad;

import :natural;

namespace dedekind::category {

/**
 * @section Monad_as_Monoid (Explicit Definition)
 * We bridge the gap:
 *   η (Unit)           <--> identity_v (Monoid Unit)
 *   μ (Multiplication) <--> Op         (Monoid Operation)
 *
 * @brief T : C -> C with η : Id ⟹ T and μ : T² ⟹ T
 * @details In the project's single-species setting, the component laws are
 *          checked against an object label `c`, then transported into the
 *          endofunctor context by lifting the identity spoke `id_c(c)`. This
 *          is how the type system recovers the object witnesses `T(c)` and
 *          `T(T(c))` needed for η and μ.
 */
export template <typename T, typename η_t, typename μ_t>
concept IsMonad =
    IsEndofunctor<T> &&
    // η: Id_C ⟹ T (Unit)
    IsNaturalTransformation<η_t, identity_functor<typename T::Σ_cat>, T> &&
    // μ: T² ⟹ T (Multiplication/Join)
    IsNaturalTransformation<μ_t, composite_functor<T, T>, T> &&

    requires(η_t η, μ_t μ, typename T::Σ_cat::Arrow::Domain c) {
      // `c` is the object label. The corresponding object in T and T² is
      // recovered by functorially lifting the identity spoke on c.
      // 1. Associativity Law: μ ∘ T(μ) == μ ∘ μ(T)
      { T{}.fmap(μ(c)) >> μ(c) } -> std::same_as<decltype(μ(c) >> μ(c))>;

      // 2. Left Unit Law: μ ∘ T(η) == id_T
      { T{}.fmap(η(c)) >> μ(c) } -> std::same_as<typename T::Τ_cat::Id>;

      // 3. Right Unit Law: μ ∘ η_T == id_T
      { η(c) >> μ(c) } -> std::same_as<typename T::Τ_cat::Id>;
    };

/**
 * @section Comonadic_Morphisms: Extract (ε) and Duplicate (δ)
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
 * @section The_Mereological_Pull
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

    requires(ε_t ε, δ_t δ, typename W::Σ_cat::Arrow::Domain w_obj) {
      // 1. Coassociativity Law: W(δ) ∘ δ == δ_W ∘ δ
      {
        δ(w_obj) >> W{}.fmap(δ(w_obj))
      } -> std::same_as<decltype(δ(w_obj) >> δ(w_obj))>;

      // 2. Left Counit Law: W(ε) ∘ δ == id_W
      { δ(w_obj) >> W{}.fmap(ε(w_obj)) } -> std::same_as<typename W::Σ_cat::Id>;

      // 3. Right Counit Law: ε_W ∘ δ == id_W
      // We move across the duplication, then extract using the component at the
      // result.
      { δ(w_obj) >> ε(w_obj) } -> std::same_as<typename W::Σ_cat::Id>;
    };

/** @section Generic_Monadic_Pipeline */

export template <typename W>
struct η_tag {};

export template <typename W>
struct μ_tag {};

// 1. Entry: value >> into<η_t>
// Since η is a transformation from Id -> T, we select its component using the
// raw object label directly. In the single-species setting that label is the
// canonical witness for the identity spoke at the same object.
export template <typename T, typename η_t>
constexpr auto operator>>(T&& val, η_tag<η_t>) {
  // η_t{}(val) produces the arrow, we then apply it to the value
  return η_t{}(val)(std::forward<T>(val));
}

// 2. Join: T<T<X>> >> join<μ_t>
// μ_t is the natural transformation T^2 => T.
// The nested context itself acts as the witness for selecting the appropriate
// component, mirroring the identity-based recovery strategy used in the laws.
export template <typename NestedContext, typename μ_t>
constexpr auto operator>>(NestedContext&& nested, μ_tag<μ_t>) {
  // The nested context itself acts as the witness for selecting the component.
  auto mu_x = μ_t{}(nested);
  return mu_x(std::forward<NestedContext>(nested));
}

}  // namespace dedekind::category
