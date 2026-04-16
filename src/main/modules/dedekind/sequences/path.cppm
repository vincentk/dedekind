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
#include <cstddef>
#include <functional>
#include <utility>

export module dedekind.sequences:path;

import dedekind.category;
import :net;

namespace dedekind::sequences {

using namespace dedekind::category;
using namespace dedekind::sets;

/**
 * @class Path
 * @brief A Frobenius-aware sequence F: ℕ → T.
 *
 * @tparam T The Species being enumerated.
 */
export template <typename T, typename Cardinality = ℵ_0>
struct Path {
  using Domain = std::size_t;
  using Codomain = T;
  using cardinality_type = Cardinality;

  /** @brief The underlying mapping f(n). */
  std::function<T(std::size_t)> generator;
  std::size_t extent = 0;

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

    auto bound_generator = [m, f = std::forward<F>(f)](std::size_t n) {
      return f(m.at(n)).at(n);
    };

    if constexpr (IsFiniteMagnitude<Cardinality>) {
      return Path<U, Cardinality>{bound_generator, m.size()};
    } else {
      return Path<U, Cardinality>{bound_generator};
    }
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

    auto extended_generator = [w, f = std::forward<F>(f)](std::size_t n) {
      // Create the "sub-path" (suffix) starting at n, then apply f
      return f(Path<T>{[w, n](std::size_t i) { return w.at(n + i); }});
    };

    if constexpr (IsFiniteMagnitude<Cardinality>) {
      return Path<U, Cardinality>{extended_generator, w.size()};
    } else {
      return Path<U, Cardinality>{extended_generator};
    }
  }

  constexpr std::size_t size() const noexcept
    requires IsFiniteMagnitude<Cardinality>
  {
    return extent;
  }

  /** @brief The Path is generally viewed as a mapping from N. */
  constexpr auto cardinality() const noexcept { return Cardinality{}; }
};

export template <typename T>
using FinitePath = Path<T, Finite>;

export template <typename T, typename Cardinality>
constexpr auto prefix(const Path<T, Cardinality>& path, std::size_t length) {
  return FinitePath<T>{[path](std::size_t i) { return path.at(i); }, length};
}

export template <typename T, typename Step>
  requires std::invocable<const std::decay_t<Step>&, const T&> &&
           std::same_as<
               std::invoke_result_t<const std::decay_t<Step>&, const T&>, T>
constexpr auto iterate(T seed, Step&& step) {
  return Path<T>{[seed, f = std::forward<Step>(step)](std::size_t n) {
    T value = seed;
    for (std::size_t i = 0; i < n; ++i) value = std::invoke(f, value);
    return value;
  }};
}

export template <typename T, typename Step>
  requires std::invocable<const std::decay_t<Step>&, const T&> &&
           std::same_as<
               std::invoke_result_t<const std::decay_t<Step>&, const T&>, T>
constexpr auto iterate(T seed, Step&& step, std::size_t length) {
  return prefix(iterate(std::move(seed), std::forward<Step>(step)), length);
}

export template <typename T, typename Cardinality, typename Pred>
  requires IsFiniteMagnitude<Cardinality> &&
           std::predicate<const std::decay_t<Pred>&, const T&>
constexpr std::size_t count_if(const Path<T, Cardinality>& path, Pred&& pred) {
  auto predicate = std::forward<Pred>(pred);
  std::size_t count = 0;

  for (std::size_t i = 0; i < path.size(); ++i) {
    if (std::invoke(predicate, path.at(i))) ++count;
  }

  return count;
}

export template <typename T, typename Cardinality, typename Pred>
  requires LogicalMap<Pred, T>
constexpr auto exists(const Path<T, Cardinality>& path, Pred&& pred) {
  using Omega = OmegaOf<Pred, T>;
  using Logic = LogicOf<Omega>;

  auto predicate = std::forward<Pred>(pred);
  Omega witness = Logic::False;
  for (std::size_t i = 0;; ++i) {
    if constexpr (IsFiniteMagnitude<Cardinality>) {
      if (i >= path.size()) break;
    }
    witness = Logic::OR(witness, std::invoke(predicate, path.at(i)));
    if (witness == Logic::True)
      break;  // short-circuit: absorbing element found
  }
  return witness;
}

export template <typename T, typename Cardinality, typename Pred>
  requires LogicalMap<Pred, T>
constexpr auto forall(const Path<T, Cardinality>& path, Pred&& pred) {
  using Omega = OmegaOf<Pred, T>;
  using Logic = LogicOf<Omega>;

  auto predicate = std::forward<Pred>(pred);
  Omega witness = Logic::True;
  for (std::size_t i = 0;; ++i) {
    if constexpr (IsFiniteMagnitude<Cardinality>) {
      if (i >= path.size()) break;
    }
    witness = Logic::AND(witness, std::invoke(predicate, path.at(i)));
    if (witness == Logic::False)
      break;  // short-circuit: absorbing element found
  }
  return witness;
}

}  // namespace dedekind::sequences

namespace dedekind::sequences {
using namespace dedekind::category;

/** @section Formal_Verification
 * Deferred while path-level categorical bridges are being retargeted to the
 * current dedekind.category hub/spoke interfaces.
 */

}  // namespace dedekind::sequences
