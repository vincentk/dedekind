/**
 * @file dedekind.category:kleisli.cppm
 * @brief Level 0.5: The Kleisli Partition (The Extension Systems).
 *
 * @section The_Kleisli_Unification
 * This partition defines the "Extension Systems" that serve as the functional
 * engine for the Dedekind ontology. By focusing on the Kleisli Triple (for
 * Monads) and the Co-Kleisli Triple (for Comonads), we sidestep C++ template
 * limitations regarding partial specialization of 'fmap'.
 *
 * @subsection The_Kleisli_Triple (Monadic Push)
 * A species provides a Kleisli system if it implements:
 * - η (Unit/Return) : The 'into' operator (>>), lifting raw types into the
 * species.
 * - >>= (Bind)      : The 'extension' operator, chaining computations within
 * the context.
 *
 * @subsection The_Co_Kleisli_Triple (Comonadic Pull)
 * A species provides a Co-Kleisli system if it implements:
 * - ε (Counit/Extract) : The 'extract' operator (<<), sampling raw types from
 * the species.
 * - <<= (Extend)       : The 'co-extension' operator, sampling the species into
 * a new context.
 *
 * @section The_Bootstrapping_Mechanism
 * This partition is the prerequisite for the ':functorial' partition. Because
 * 'fmap' is an epi-phenomenon in this ontology, the presence of either a
 * Kleisli or Co-Kleisli structure allows the automatic derivation of functorial
 * mapping via the Highway Bridge.
 */

module;

#include <concepts>
#include <functional>

export module dedekind.category:kleisli;

import :species;

namespace dedekind::category {

/** @section The_Universal_Unit (η) */
export template <template <typename...> typename F, typename T,
                 typename... Context>
struct η;  // Primary template for all kinds of contexts

/** @section The_Kleisli_Triple_for_Box */
// η (Unit): Lifting a value into the Box
export template <typename T>
struct η<Box, T> final {
  constexpr auto operator()(T x) const { return Box<T>{x}; }
};

/** @section The_Universal_Counit (ε) */
export template <template <typename...> typename F, typename T,
                 typename... Context>
struct ε;

/** @section Box_Specialization_for_ε */
export template <typename T>
struct ε<Box, T> final {
  // For a Box, extraction is simply accessing the value.
  constexpr T operator()(const Box<T>& b) const noexcept { return b.value; }
};

/**
 * @section The_Kleisli_Extension_System
 * @brief Formal detection of the Monadic "Lift and Chain" action.
 *
 * @details
 * A species F satisfies this system if it provides:
 * 1. η (Unit): A way to lift a raw species into the context.
 * 2. >>= (Bind): A way to chain a context to a Kleisli Arrow (T -> F<U>).
 */
export template <template <typename...> typename F, typename T, typename U>
concept IsKleisliExtension = requires(T x, F<T> box, std::function<F<U>(T)> f) {
  { η<F, T>{}(x) } -> std::same_as<F<T>>;  // The Lift (η)
  { box >>= f } -> std::same_as<F<U>>;     // The Chain (Bind)
};

static_assert(
    IsKleisliExtension<Box, int, int>,
    "Skeletal Failure: Box does not satisfy the Kleisli Extension System.");

/**
 * @section The_Kleisli_Monad_Proof
 * @brief Elevates a Kleisli Extension to a formal Monad.
 *
 * @details
 * A species is a Kleisli Monad if it possesses the 'Action' (Extension)
 * and satisfies the 'Categorical Identity' (The Arrow Mapping).
 */
export template <template <typename...> typename F, typename T, typename U>
concept IsKleisli = IsKleisliExtension<F, T, U> &&
                    requires(T x, F<T> box, std::function<F<U>(T)> f) {
                      // We add the formal Categorical requirements here.
                      // E.g., The result must be a stable F<U>.
                      { box >>= f } -> std::same_as<F<U>>;
                    };

/** @section CoKleisli_Extension_System (The Pull) */
export template <template <typename...> typename F, typename T, typename U>
concept IsCoKleisliExtension = requires(F<T> box, std::function<U(F<T>)> f) {
  { ε<F, T>{}(box) } -> std::same_as<T>;  // The Extract
  { box <<= f } -> std::same_as<F<U>>;    // The Extend
};

/**
 * @section The_CoKleisli_Comonad_Proof
 * @brief Elevates a Co-Kleisli Extension to a formal Comonad.
 */
export template <template <typename...> typename F, typename T, typename U>
concept IsCoKleisli = IsCoKleisliExtension<F, T, U> &&
                      requires(F<T> box, std::function<U(F<T>)> f) {
                        { box <<= f } -> std::same_as<F<U>>;
                      };

static_assert(IsCoKleisliExtension<Box, int, int>,
              "Box does not satisfy the Co-Kleisli Extension System.");

/**
 * @brief Concept for Frobenius Structures (The Unified Highway).
 *
 * @section The_Frobenius_Isomorphism
 * A species is Frobenius if it provides both a Kleisli (Monadic) and
 * Co-Kleisli (Comonadic) extension system. In this state, the 'Push'
 * and 'Pull' highways are functionally equivalent for the derivation
 * of fmap, ensuring structural symmetry.
 *
 * @note Dedekind Requirement:
 * While a Frobenius monad in pure category theory requires specific
 * distributive laws, our ontology uses this concept to signify that
 * a species can be navigated from both directions (into and out of context).
 */
export template <template <typename...> typename F, typename T, typename U>
concept IsFrobenius =
    IsKleisliExtension<F, T, U> && IsCoKleisliExtension<F, T, U>;

static_assert(IsFrobenius<Box, int, int>,
              "Box does not satisfy the Co-Kleisli Extension System.");

}  // namespace dedekind::category
