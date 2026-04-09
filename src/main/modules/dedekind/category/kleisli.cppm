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

// η (Unit): Lifting a value into the Box
export template <template <typename...> typename F, typename T>
struct η;

export template <typename T>
struct η<Box, T> final {
  constexpr auto operator()(T x) const { return Box<T>{std::move(x)}; }
};

// ε (Counit): Sampling a value from the Box
export template <template <typename...> typename F, typename T>
struct ε;

export template <typename T>
struct ε<Box, T> final {
  constexpr T operator()(const Box<T>& b) const noexcept { return b.value; }
};

/** @section Extension_Concepts */

export template <template <typename...> typename F, typename T, typename U>
concept IsKleisliExtension = requires(T x, F<T> box, std::function<F<U>(T)> f) {
  { η<F, T>{}(x) } -> std::same_as<F<T>>;
  { box >>= f } -> std::same_as<F<U>>;
};

export template <template <typename...> typename F, typename T, typename U>
concept IsCoKleisliExtension = requires(F<T> box, std::function<U(F<T>)> f) {
  { ε<F, T>{}(box) } -> std::same_as<T>;
  { box <<= f } -> std::same_as<F<U>>;
};

export template <template <typename...> typename F, typename T, typename U>
concept IsFrobenius =
    IsKleisliExtension<F, T, U> && IsCoKleisliExtension<F, T, U>;

/** @section Static_Axioms */

static_assert(IsKleisliExtension<Box, int, int>);
static_assert(IsCoKleisliExtension<Box, int, int>);
static_assert(IsFrobenius<Box, int, int>);

/** @section The_Unified_Highway_Bridge */

export template <template <typename...> typename F, typename Arrow>
  requires IsArrow<Arrow>
constexpr auto fmap(Arrow f) {
  using T = typename std::remove_cvref_t<Arrow>::Domain;
  using U = typename std::remove_cvref_t<Arrow>::Codomain;

  if constexpr (IsKleisliExtension<F, T, U>) {
    // Capture 'f' by value [f] to lift it into the monadic context
    return arrow([f](const F<T>& m) {
      return m >>= [f](const T& x) { return η<F, U>{}(f(x)); };
    });
  } else if constexpr (IsCoKleisliExtension<F, T, U>) {
    // Capture 'f' by value [f] to lift it into the comonadic context
    return arrow([f](const F<T>& w) {
      return w <<= [f](const F<T>& ctx) { return f(ε<F, T>{}(ctx)); };
    });
  }
}

/** @section Pipeline_Infrastructure */

export template <template <typename...> typename F>
struct ε_tag {};

// Duplicate (δ) defined as Self-Extend (w <<= id)
export template <typename T, template <typename...> typename F>
  requires IsCoKleisliExtension<F, T, T>
constexpr auto operator<<(const F<T>& box, ε_tag<F>) {
  return box <<= [](const F<T>& w) { return w; };
}

}  // namespace dedekind::category
