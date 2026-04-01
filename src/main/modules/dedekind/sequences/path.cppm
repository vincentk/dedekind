/**
 * @file dedekind/sequences/path.cppm
 * @partition :path
 * @brief Level 2.5a: The Frobenius Path (Morphism of Enumeration).
 *
 * @section The_Path_Morphism
 * Following the Dedekind structuralist tradition, a Path is a functional
 * mapping (λn. s_n) from the Countable Index (ℕ) to a Species (T).
 *
 * @subsection The_Frobenius_Highway
 * A Path is unique in the ontology as it satisfies both the Monadic (Push)
 * and Comonadic (Pull) extension systems:
 * - Kleisli (Monad)    : Enables 'flattening' of nested paths via diagonal
 *                        sampling (Bind).
 * - Co-Kleisli (Comonad): Enables 'contextual' transformations such as
 *                        running averages or finite differences (Extend).
 *
 * @build_order 5
 * @dependency dedekind.category, dedekind.sequences:sequences
 *
 * Wikipedia: Sequence, Monad (category theory), Comonad, Frobenius algebra
 */

module;

#include <concepts>
#include <functional>

export module dedekind.sequences:path;

import dedekind.category;
import :sequences;

namespace dedekind::sequences {

using namespace dedekind::category;

/**
 * @class Path
 * @brief A Frobenius-aware sequence F: ℕ → T.
 *
 * @tparam T The Species being enumerated.
 */
export template <typename T>
struct Path {
  using Domain = std::size_t;
  using Codomain = T;

  /** @brief The underlying mapping f(n). */
  std::function<T(std::size_t)> generator;

  constexpr Codomain operator()(Domain i) const { return generator(i); }

  /** @section Sequence_Interface */
  constexpr T at(std::size_t i) const { return generator(i); }

  /**
   * @section Kleisli_Triple (The Monadic Push)
   * @brief Bind (>>=): m >>= f.
   */
  template <typename F>
  friend constexpr auto operator>>=(const Path& m, F&& f) {
    using ResultPath = std::invoke_result_t<F, T>;
    using U = typename ResultPath::Codomain;

    return Path<U>{[m, f = std::forward<F>(f)](std::size_t n) {
      return f(m.at(n)).at(n);
    }};
  }

  /**
   * @section Co_Kleisli_Triple (The Comonadic Pull)
   * @brief Extend (<<=): w <<= f.
   */
  template <typename F>
  friend constexpr auto operator<<=(const Path& w, F&& f) {
    // Symmetry: F now maps Path<T> -> U.
    // We wrap that U back into a Path<U>.
    using U = std::invoke_result_t<F, Path<T>>;

    return Path<U>{[w, f = std::forward<F>(f)](std::size_t n) {
      // Create the "sub-path" (suffix) starting at n, then apply f
      return f(Path<T>{[w, n](std::size_t i) { return w.at(n + i); }});
    }};
  }
};

}  // namespace dedekind::sequences

/** @section Categorical_Anchors */

namespace dedekind::category {

/** @brief η (Unit) for Path: T -> Path<T> (The Constant Path). */
template <typename T>
struct η<dedekind::sequences::Path, T> {
  constexpr auto operator()(const T& x) const {
    return dedekind::sequences::Path<T>{[x](std::size_t) { return x; }};
  }
};

/** @brief ε (Counit) for Path: Path<T> -> T (Sampling the 'Now'). */
template <typename T>
struct ε<dedekind::sequences::Path, T> {
  constexpr T operator()(const dedekind::sequences::Path<T>& p) const {
    return p.at(0);
  }
};

}  // namespace dedekind::category

namespace dedekind::sequences {
using namespace dedekind::category;

/** @section Formal_Verification */

static_assert(IsSequence<Path<int>>,
              "Path must satisfy the Level 2.5 IsSequence concept.");

static_assert(IsKleisliExtension<Path, int, int>,
              "Morphism Error: Path must support the Monadic (>>=) extension.");

static_assert(
    IsCoKleisliExtension<Path, int, int>,
    "Morphism Error: Path must support the Comonadic (<<=) extension.");

static_assert(IsFrobenius<Path, int, int>,
              "Topos Error: Path must be a Frobenius structure (Push & Pull).");

static_assert(
    IsArrow<decltype(fmap<Path>(id<int>())), Path<int>, Path<int>>,
    "Functor Error: fmap discovery must resolve for the Path species.");

}  // namespace dedekind::sequences
