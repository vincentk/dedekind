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

import :monad;
import :small;
import :morphism;

namespace dedekind::category {

/** @section The_Box_Action_Engine */

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

/** @section The_Kleisli_Witnesses */

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

/** @section Extension_Concepts */

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

/** @section Kleisli_Operators */

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
constexpr auto σ(WA const& wa, F&& f) {
  return φ(δ(wa), std::forward<F>(f));
}

/** @section Kleisli_Bind_Operators */

/**
 * @brief operator>>= : Monadic bind (Kleisli arrow application).
 * @details Syntactic sugar for κ(ma, f).
 * Given ma: T<A> and f: A → T<B>, produces T<B>.
 */
export template <typename MA, typename F>
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
  requires requires(WA const& wa, F&& f) {
    { δ(wa) };
    { φ(δ(wa), std::forward<F>(f)) };
  }
constexpr auto operator<<(WA const& wa, F&& f) {
  return σ(wa, std::forward<F>(f));
}

}  // namespace dedekind::category
