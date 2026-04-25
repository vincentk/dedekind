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
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "I am striking out a new path for myself."
 *       -- Srinivasa Ramanujan, letter to G. H. Hardy (16 January 1913)
 */

module;

#include <algorithm>
#include <cassert>
#include <compare>
#include <concepts>
#include <cstddef>
#include <functional>
#include <iterator>
#include <limits>
#include <memory>
#include <optional>
#include <ranges>
#include <type_traits>
#include <utility>
#include <vector>

export module dedekind.sequences:path;

import dedekind.category;
import dedekind.sets;
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

  struct const_iterator {
    using iterator_concept = std::random_access_iterator_tag;
    using iterator_category = std::random_access_iterator_tag;
    using value_type = T;
    using difference_type = std::ptrdiff_t;
    using reference = T;

    const Path* owner = nullptr;
    std::size_t index = 0;

    constexpr reference operator*() const {
      assert(owner != nullptr && "path iterator requires owner");
      return owner->at(index);
    }

    constexpr reference operator[](difference_type n) const {
      assert(owner != nullptr && "path iterator requires owner");
      return owner->at(
          static_cast<std::size_t>(static_cast<difference_type>(index) + n));
    }

    constexpr const_iterator& operator++() {
      ++index;
      return *this;
    }

    constexpr const_iterator operator++(int) {
      auto tmp = *this;
      ++(*this);
      return tmp;
    }

    constexpr const_iterator& operator--() {
      --index;
      return *this;
    }

    constexpr const_iterator operator--(int) {
      auto tmp = *this;
      --(*this);
      return tmp;
    }

    constexpr const_iterator& operator+=(difference_type n) {
      index = static_cast<std::size_t>(static_cast<difference_type>(index) + n);
      return *this;
    }

    constexpr const_iterator& operator-=(difference_type n) {
      return (*this += -n);
    }

    friend constexpr const_iterator operator+(const const_iterator& it,
                                              difference_type n) {
      auto copy = it;
      copy += n;
      return copy;
    }

    friend constexpr const_iterator operator+(difference_type n,
                                              const const_iterator& it) {
      return it + n;
    }

    friend constexpr const_iterator operator-(const const_iterator& it,
                                              difference_type n) {
      auto copy = it;
      copy -= n;
      return copy;
    }

    friend constexpr difference_type operator-(const const_iterator& lhs,
                                               const const_iterator& rhs) {
      assert(lhs.owner == rhs.owner && "cannot diff iterators from two paths");
      return static_cast<difference_type>(lhs.index) -
             static_cast<difference_type>(rhs.index);
    }

    friend constexpr bool operator==(const const_iterator& lhs,
                                     const const_iterator& rhs) {
      return lhs.owner == rhs.owner && lhs.index == rhs.index;
    }

    friend constexpr bool operator<(const const_iterator& lhs,
                                    const const_iterator& rhs) {
      assert(lhs.owner == rhs.owner &&
             "cannot compare iterators from two paths");
      return lhs.index < rhs.index;
    }

    friend constexpr bool operator>(const const_iterator& lhs,
                                    const const_iterator& rhs) {
      return rhs < lhs;
    }

    friend constexpr bool operator<=(const const_iterator& lhs,
                                     const const_iterator& rhs) {
      return !(rhs < lhs);
    }

    friend constexpr bool operator>=(const const_iterator& lhs,
                                     const const_iterator& rhs) {
      return !(lhs < rhs);
    }
  };

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
      // Create the "sub-path" (suffix) starting at n, preserving cardinality.
      if constexpr (IsFiniteMagnitude<Cardinality>) {
        const std::size_t suffix_size = (n < w.size()) ? w.size() - n : 0;
        return f(Path<T, Cardinality>{
            [w, n](std::size_t i) { return w.at(n + i); }, suffix_size});
      } else {
        return f(Path<T, Cardinality>{
            [w, n](std::size_t i) { return w.at(n + i); }});
      }
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

  constexpr auto begin() const noexcept
    requires IsFiniteMagnitude<Cardinality>
  {
    return const_iterator{.owner = this, .index = 0};
  }

  constexpr auto end() const noexcept
    requires IsFiniteMagnitude<Cardinality>
  {
    return const_iterator{.owner = this, .index = size()};
  }

  /** @brief The Path is generally viewed as a mapping from N. */
  constexpr auto cardinality() const noexcept { return Cardinality{}; }
};

export template <typename T>
using FinitePath = Path<T, Finite>;

export template <typename T>
constexpr const FinitePath<T>& as_range(const FinitePath<T>& path) {
  return path;
}

export template <typename R>
  requires std::ranges::input_range<R>
constexpr auto from_range(R&& range) {
  using U = std::ranges::range_value_t<R>;

  auto values = std::make_shared<std::vector<U>>();
  if constexpr (std::ranges::sized_range<R>) {
    values->reserve(static_cast<typename std::vector<U>::size_type>(
        std::ranges::size(range)));
  }
  for (auto&& value : range) values->push_back(static_cast<U>(value));

  return FinitePath<U>{
      [values](std::size_t i) {
        assert(i < values->size() && "from_range: index out of range");
        return (*values)[i];
      },
      values->size()};
}

export template <typename T, typename Cardinality>
constexpr auto prefix(const Path<T, Cardinality>& path, std::size_t length) {
  if constexpr (IsFiniteMagnitude<Cardinality>) {
    const std::size_t clamped = std::min(length, path.size());
    return FinitePath<T>{[path](std::size_t i) { return path.at(i); }, clamped};
  } else {
    return FinitePath<T>{[path](std::size_t i) { return path.at(i); }, length};
  }
}

/**
 * Return the infinite tail of a path, shifted by n indices.
 *
 * drop(path, n) = λi. path(n + i)
 *
 * @tparam T The element type.
 * @tparam Cardinality The cardinality of the input path (must be infinite).
 * @param path The input path (must have infinite cardinality).
 * @param n The offset to drop. Precondition: for each accessed index i, the
 *          sum n + i must be representable in std::size_t.
 * @return An infinite path with the same cardinality, representing the shifted
 *         tail.
 */
export template <typename T, typename Cardinality>
  requires(!IsFiniteMagnitude<Cardinality>)
constexpr auto drop(const Path<T, Cardinality>& path, std::size_t n) {
  return Path<T, Cardinality>{[path, n](std::size_t i) {
    assert(i <= std::numeric_limits<std::size_t>::max() - n &&
           "drop index overflow: n + i exceeds size_t");
    return path.at(n + i);
  }};
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
  if (length == 0) {
    return FinitePath<T>{[](std::size_t) -> T {
                           assert(false && "at() on empty iterate path");
                           std::unreachable();
                         },
                         0};
  }

  auto values = std::make_shared<std::vector<T>>();
  values->reserve(length);
  values->push_back(std::move(seed));

  auto f = std::forward<Step>(step);
  for (std::size_t i = 1; i < length; ++i)
    values->push_back(std::invoke(f, values->back()));

  return FinitePath<T>{
      [values](std::size_t i) {
        assert(i < values->size() && "iterate: index out of range");
        return (*values)[i];
      },
      length};
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

/**
 * Find the first index where a predicate holds, up to an inclusive budget.
 *
 * first_where(path, pred, n) ≡ min { k ≤ n | pred(path(k)) }, or nullopt.
 *
 * Two overloads: infinite path with explicit budget, finite path self-bounded.
 */
export template <typename T, typename Pred>
  requires std::copy_constructible<std::decay_t<Pred>> &&
           std::predicate<const std::decay_t<Pred>&, const T&>
constexpr std::optional<std::size_t> first_where(const Path<T>& path,
                                                 Pred&& pred,
                                                 std::size_t budget) {
  for (std::size_t i = 0; i <= budget; ++i) {
    if (std::invoke(pred, path.at(i))) return i;
  }
  return std::nullopt;
}

export template <typename T, typename Pred>
  requires std::copy_constructible<std::decay_t<Pred>> &&
           std::predicate<const std::decay_t<Pred>&, const T&>
constexpr std::optional<std::size_t> first_where(const FinitePath<T>& path,
                                                 Pred&& pred) {
  for (std::size_t i = 0; i < path.size(); ++i) {
    if (std::invoke(pred, path.at(i))) return i;
  }
  return std::nullopt;
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

/**
 * @brief Co-Kleisli scan: apply a prefix aggregate to each initial segment.
 * @details Produces an infinite path where element i is f(prefix(path, i+1)).
 *
 * This is the canonical SQL window-function pattern over an ordered sequence:
 *   scan(sum,           path)(i) == sum of path[0..i]
 *   scan(exists(pred),  path)(i) == exists(prefix(path, i+1), pred)
 *
 * Relation to operator<<=: scan(f, path) is the co-Kleisli extension of
 * [f](ctx) { return f(prefix(ctx, 1)); }, expressed without the suffix-path
 * construction. Note that operator<<= passes a suffix (drop) as context,
 * while scan passes a prefix — these are the two canonical window shapes.
 *
 * @param f    Aggregate: FinitePath<T> → U (e.g. exists, forall, count_if).
 * @param path Source infinite path.
 * @return     Infinite Path<U> where element i == f(prefix(path, i+1)).
 */
export template <typename T, typename F>
  requires std::copy_constructible<std::decay_t<F>> &&
           std::invocable<const std::decay_t<F>&, const FinitePath<T>&>
constexpr auto scan(F&& f, const Path<T>& path) -> Path<
    std::invoke_result_t<const std::decay_t<F>&, const FinitePath<T>&>> {
  using Fn = std::decay_t<F>;
  using U = std::invoke_result_t<const Fn&, const FinitePath<T>&>;
  return Path<U>{[f = Fn(std::forward<F>(f)), path](std::size_t i) {
    assert(i < std::numeric_limits<std::size_t>::max() &&
           "scan index overflow: i+1 exceeds size_t");
    return std::invoke(f, prefix(path, i + 1));
  }};
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

namespace dedekind::category {

// Path participates in the Kleisli witness framework as an infinite constant
// path for η and head sampling for ε.
export template <typename T>
struct unit_witness<dedekind::sequences::Path, T> final {
  constexpr auto operator()(T x) const {
    return dedekind::sequences::Path<T>{[x](std::size_t) { return x; }};
  }
};

export template <typename T>
struct counit_witness<dedekind::sequences::Path, T> final {
  constexpr T operator()(const dedekind::sequences::Path<T>& p) const {
    return p.at(0);
  }
};

}  // namespace dedekind::category

namespace dedekind::sequences {
using namespace dedekind::category;

/** @section Formal_Verification */
static_assert(IsSequence<Path<int>>, "Path must satisfy the sequence concept.");

static_assert(IsFiniteSequence<FinitePath<int>>,
              "FinitePath must satisfy the finite sequence concept.");

// Bidirectional anchor (math ↔ stdlib) for #388: the library's
// IsFiniteSequence carrier presents as a std::ranges::input_range
// out of the box (FinitePath::begin / end yield std::input_iterator
// instances), and the inverse direction is provided by from_range
// just above, which lifts any std::ranges::input_range into a
// FinitePath.  Anchoring at the input_range concept makes library
// sequences flow into std::ranges algorithms (transform / filter /
// accumulate / for_each) without a bespoke adapter, and keeps the
// :sequences vocabulary connected to standard C++ idioms while the
// strictly stronger std::generator coroutine is awaited from libc++.
static_assert(std::ranges::input_range<FinitePath<int>>,
              "FinitePath<T> must satisfy std::ranges::input_range so "
              "library sequences flow into std::ranges algorithms.");
static_assert(
    std::input_iterator<decltype(std::declval<FinitePath<int>>().begin())>,
    "FinitePath<T>::begin() must yield a std::input_iterator.");

static_assert(IsKleisliExtension<Path, int, long>,
              "Path must satisfy the Kleisli extension witness.");

static_assert(IsCoKleisliExtension<Path, int, long>,
              "Path must satisfy the co-Kleisli extension witness.");

static_assert(
    IsFrobenius<Path, int, long>,
    "Path must satisfy the Frobenius witness (Kleisli + co-Kleisli).");

}  // namespace dedekind::sequences
