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

/**
 * @brief κ (kappa): Kleisli extension (bind).
 * Inferred as: κ(f) = μ ∘ φ(f)
 */
template <template <typename> typename T, typename A, typename F>
  requires IsMonad<T>
constexpr auto κ(T<A> const& ma, F&& f) {
  return μ(φ(ma, std::forward<F>(f)));
}

/**
 * @brief σ (sigma): Co-Kleisli extension (extend).
 * Inferred as: σ(f) = φ(f) ∘ δ
 */
template <template <typename> typename W, typename A, typename F>
  requires IsComonad<W>
constexpr auto σ(W<A> const& wa, F&& f) {
  return φ(δ(wa), std::forward<F>(f));
}

// Monadic Bind: ma >>= f
template <template <typename> typename T, typename A, typename F>
  requires IsMonad<T>
constexpr auto operator>>=(T<A> const& ma, F&& f) {
  return κ(ma, std::forward<F>(f));
}

// Comonadic Extend: wa <<= f
template <template <typename> typename W, typename A, typename F>
  requires IsComonad<W>
constexpr auto operator<<=(W<A> const& wa, F&& f) {
  return σ(wa, std::forward<F>(f));
}

/** @section The_Unified_Highway_Bridge (presumably
 * just another way to state the Kleisli operator kappa) */
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

/**
 * @brief operator >>= : Syntactic sugar for monadic bind.
 */
template <template <typename> typename T, typename A, typename F>
  requires IsMonad<T>
constexpr auto operator>>=(T<A> const& ma, F&& f) {
  return κ(ma, std::forward<F>(f));
}

/**
 * @brief operator <<= : Syntactic sugar for comonadic extension.
 */
template <template <typename> typename W, typename A, typename F>
  requires IsComonad<W>
constexpr auto operator<<=(W<A> const& wa, F&& f) {
  return σ(wa, std::forward<F>(f));
}

/**
 * @brief The "Fish" Operator (Kleisli Arrow Composition)
 * f >> g  =>  λx. κ(f(x), g)
 */
template <typename F, typename G>
constexpr auto operator>>(F&& f, G&& g) {
  return [f = std::forward<F>(f), g = std::forward<G>(g)](auto&& x) {
    return κ(f(std::forward<decltype(x)>(x)), g);
  };
}

/**
 * @brief The "Co-Fish" Operator (Co-Kleisli Arrow Composition).
 * wa << f  =>  σ(wa, f)
 */
template <template <typename> typename W, typename A, typename F>
  requires Comonad<W>
constexpr auto operator<<(W<A> const& wa, F&& f) {
  return σ(wa, std::forward<F>(f));
}

/**
 * @brief The "Downstream Fish" (Standard Composition)
 * g << f  =>  g ∘ f
 *
 * @note: while for plain endofunctors, the upstream and the downstream fish
 * operators are symmetric, the symmetry break vis-a-vis the "Downstream Fish"
 * is now intentional.
 */
template <typename F, typename G>
constexpr auto operator>>(F&& f, G&& g) {
  return [f = std::forward<F>(f), g = std::forward<G>(g)](auto&& x) {
    // Result of f(x) is T<b>. We feed that into g via κ.
    return κ(f(std::forward<decltype(x)>(x)), g);
  };

  /**
   * @brief The "Upstream Fish" (Standard Composition)
   * g << f  =>  g ∘ f
   *

   * @note: while for plain endofunctors, the upstream and the downstream fish
   operators are symmetric, the symmetry break vis-a-vis the "Downstream Fish"
   is now intentional.
   */
  template <typename F, typename G>
  constexpr auto operator<<(F&& f, G&& g) {
    return [f = std::forward<F>(f), g = std::forward<G>(g)](auto&& wa) {
      // Use σ to transform W<a> to W<b> using g, then apply f.
      return f(σ(wa, g));
    };
  }
}

// --- Static Constraints ---

// 1. Verify Maybe is a valid Functor
// This ensures φ satisfies the Functor concept for the Maybe type.
static_assert(Functor<Maybe>);

// 2. Verify η and μ are Natural Transformations
// Naturality requires that for any f: a -> b, the diagram commutes:
// For η: T f ∘ η_a = η_b ∘ f
// For μ: T f ∘ μ_a = μ_b ∘ T(T f)
static_assert(NaturalTransformation<η, Maybe>);
static_assert(NaturalTransformation<μ, Maybe>);

}  // namespace dedekind::category
