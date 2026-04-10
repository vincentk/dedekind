/**
 * @file dedekind.category:mereology
 * @partition :mereology
 * @brief Level 0c: Embryonic Mereology (The Language of Parts).
 *
 * Following the formal axiomatization of Stanisław Leśniewski, this partition
 * introduces the "Part-Whole" relation as a foundational structural primitive.
 * In this "embryonic" stage, we define the axioms of a partial order that
 * govern any category with mereological structure.
 *
 * @section Axioms
 * 1. Reflexivity: Everything is a part of itself.
 * 2. Transitivity: A part of a part is a part of the whole.
 * 3. Antisymmetry: Two distinct things cannot be parts of each other.
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
 */
export template <typename T, typename η_t, typename μ_t>
concept IsMonad =
    IsEndofunctor<T> &&
    // η: Id_C ⟹ T (Unit)
    IsNaturalTransformation<η_t, identity_functor<typename T::Σ_cat>, T> &&
    // μ: T² ⟹ T (Multiplication/Join)
    IsNaturalTransformation<μ_t, composite_functor<T, T>, T> &&

    requires(η_t η, μ_t μ, typename T::Σ_cat::Arrow::Domain c) {
      // 1. Associativity Law: μ ∘ T(μ) == μ ∘ μ(T)
      {
        T{}.fmap(μ(c)) >> μ(c)
      } -> std::same_as<
          decltype(μ(T{}.fmap(T{}.fmap(typename T::Σ_cat::id_c(c))).vertex) >>
                   μ(c))>;

      // 2. Left Unit Law: μ ∘ T(η) == id_T
      { T{}.fmap(η(c)) >> μ(c) } -> std::same_as<typename T::Τ_cat::Id>;

      // 3. Right Unit Law: μ ∘ η_T == id_T
      {
        η(T{}.fmap(T::Σ_cat::id_c(c)).vertex) >> μ(c)
      } -> std::same_as<typename T::Τ_cat::Id>;
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
      } -> std::same_as<decltype(δ(w_obj) >> δ(δ(w_obj).vertex))>;

      // 2. Left Counit Law: W(ε) ∘ δ == id_W
      { δ(w_obj) >> W{}.fmap(ε(w_obj)) } -> std::same_as<typename W::Σ_cat::Id>;

      // 3. Right Counit Law: ε_W ∘ δ == id_W
      // We move across the duplication, then extract using the component at the
      // result.
      { δ(w_obj) >> ε(δ(w_obj).vertex) } -> std::same_as<typename W::Σ_cat::Id>;
    };

/** @section Generic_Monadic_Pipeline */

export template <typename W>
struct η_tag {};

export template <typename W>
struct μ_tag {};

// 1. Entry: value >> into<η_t>
// Since η is a transformation from Id -> T, we use its component at T
export template <typename T, typename η_t>
constexpr auto operator>>(T&& val, η_tag<η_t>) {
  // η_t{}(val) produces the arrow, we then apply it to the value
  return η_t{}(val)(std::forward<T>(val));
}

// 2. Join: T<T<X>> >> join<μ_t>
// μ_t is the natural transformation T^2 => T
export template <typename NestedContext, typename μ_t>
constexpr auto operator>>(NestedContext&& nested, μ_tag<μ_t>) {
  // The nested context itself acts as the witness for selecting the component.
  auto mu_x = μ_t{}(nested);
  return mu_x(std::forward<NestedContext>(nested));
}

}  // namespace dedekind::category
