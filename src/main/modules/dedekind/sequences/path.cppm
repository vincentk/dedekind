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
import :net;

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

  /** @brief The Path is generally viewed as a mapping from N. */
  constexpr auto cardinality() const noexcept {
    // Return ℵ_0 for infinite paths,
    // or a Finite(n) wrapper if Path tracks its own length.
    return ℵ_0{};
  }
};

}  // namespace dedekind::sequences

namespace dedekind::sequences {
using namespace dedekind::category;

/** @section Formal_Verification
 * Deferred while path-level categorical bridges are being retargeted to the
 * current dedekind.category hub/spoke interfaces.
 */

}  // namespace dedekind::sequences
