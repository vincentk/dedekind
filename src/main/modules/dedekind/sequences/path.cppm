/**
 * @file dedekind/sequences/path.cppm
 * @partition :path
 * @brief The Frobenius Path (Morphism of Enumeration).
 *
 * @section path__The_Path_Morphism
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

  /** @section path__Sequence_Interface */
  constexpr T at(std::size_t i) const { return generator(i); }

  /** @section path__Pointwise_Operators
   *
   *  @c Path<T, @c Cardinality> is a function space @c T^ℕ (or @c T^[0..N)
   *  in the finite case): pointwise @c +, scalar @c ·, and unary @c -
   *  give the vector-space surface a function space carries.  This is
   *  the operator anchor for #537's @c IsFunctionSpace<W, @c D, @c T>
   *  concept.  Each operation produces a fresh @c Path whose
   *  generator captures the operands intensionally — no eager
   *  materialisation of values.
   */

  friend constexpr Path operator+(Path const& a, Path const& b) {
    auto gen = [a, b](std::size_t i) { return a.at(i) + b.at(i); };
    if constexpr (IsFiniteMagnitude<Cardinality>) {
      return Path{gen, std::min(a.size(), b.size())};
    } else {
      return Path{gen};
    }
  }

  friend constexpr Path operator-(Path const& a, Path const& b) {
    auto gen = [a, b](std::size_t i) { return a.at(i) - b.at(i); };
    if constexpr (IsFiniteMagnitude<Cardinality>) {
      return Path{gen, std::min(a.size(), b.size())};
    } else {
      return Path{gen};
    }
  }

  friend constexpr Path operator-(Path const& a) {
    auto gen = [a](std::size_t i) { return -a.at(i); };
    if constexpr (IsFiniteMagnitude<Cardinality>) {
      return Path{gen, a.size()};
    } else {
      return Path{gen};
    }
  }

  friend constexpr Path operator*(T const& s, Path const& a) {
    auto gen = [s, a](std::size_t i) { return s * a.at(i); };
    if constexpr (IsFiniteMagnitude<Cardinality>) {
      return Path{gen, a.size()};
    } else {
      return Path{gen};
    }
  }

  friend constexpr Path operator*(Path const& a, T const& s) {
    auto gen = [s, a](std::size_t i) { return a.at(i) * s; };
    if constexpr (IsFiniteMagnitude<Cardinality>) {
      return Path{gen, a.size()};
    } else {
      return Path{gen};
    }
  }

  /**
   * @section path__Kleisli_Triple
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
   * @section path__Co_Kleisli_Triple
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

/**
 * @brief The Path Hub for the @c Path<·> shape (#508 / PR #521 review).
 *
 * @details Carries the @c Shape<U> @c = @c Path<U> alias used by the
 * Kleisli witness registry (@c unit_witness / @c counit_witness /
 * @c IsKleisliExtension / @c IsCoKleisliExtension / @c IsFrobenius).
 * Named @c path_functor for naming-convention parity with the sibling
 * @c box_functor / @c maybe_functor / @c vec2_functor /
 * @c matrix2x2_functor Hubs in other partitions.
 *
 * @section path__Functor_Limitation
 * Unlike @c box_functor / @c maybe_functor — which DO satisfy
 * @c dedekind::category::IsFunctor — @c path_functor deliberately
 * does NOT.  Reason: @c Path<T> is itself an Arrow at the type level
 * (carries @c Domain @c = @c std::size_t and @c Codomain @c = @c T,
 * realising the textbook reading @c Path: @c ℕ @c → @c T).  This
 * makes @c Morphism<Path<T>, Path<T>, ...> fail @c IsSpokeArrow
 * (because @c Path<T> is an @c IsArrow domain), which in turn makes
 * @c Set<Path<T>>::operator>>(Arrow, Arrow) — needed by
 * @c IsCategory<Set<Path<T>>> — not resolve cleanly.  The standard
 * Functor-on-Set machinery does not apply.  This is structural to
 * Path's "is itself an arrow" nature and not a defect to be papered
 * over.
 *
 * @section path__Frobenius_Reading
 * Path is @b Frobenius — it carries both monadic and comonadic
 * structure (per the partition's header note).  Under the categorical
 * reading at the Kleisli-witness level:
 *   * @c path_functor is the Hub-tag for the underlying functor T,
 *     namely the type-constructor @c Path<·> on objects (no @c φ
 *     reified at this level for the structural reason above).
 *   * @c unit_witness<path_functor<T>, T> is the @c T-component of
 *     the natural transformation @c η: @c Id @c ⇒ @c T (constant
 *     path @c λn.x).
 *   * @c counit_witness<path_functor<T>, T> is the @c T-component
 *     of the natural transformation @c ε: @c T @c ⇒ @c Id (head
 *     sampling @c p.at(0)).
 *   * The downstream @c IsFrobenius<path_functor<int>, int, long>
 *     static_assert witnesses the Kleisli-side bracket shape
 *     mechanically — that's the right level to certify on Path.
 */
export template <typename T>
struct path_functor {
  /** @brief F_obj: the type-constructor action of the functor.
   *         The only Functor-shaped surface @c path_functor reifies. */
  template <typename U>
  using Shape = dedekind::sequences::Path<U>;
};

// Path participates in the Kleisli witness framework as an infinite constant
// path for η and head sampling for ε.
export template <typename T>
struct unit_witness<path_functor<T>, T> final {
  constexpr auto operator()(T x) const {
    return dedekind::sequences::Path<T>{[x](std::size_t) { return x; }};
  }
};

export template <typename T>
struct counit_witness<path_functor<T>, T> final {
  constexpr T operator()(const dedekind::sequences::Path<T>& p) const {
    return p.at(0);
  }
};

}  // namespace dedekind::category

namespace dedekind::sequences {
using namespace dedekind::category;

/** @section path__Formal_Verification */
static_assert(IsSequence<Path<int>>, "Path must satisfy the sequence concept.");

// Discriminator-sharpening witness (#525).  Path<int> is itself an
// IsArrow at the type level (it carries Domain = std::size_t and a
// call operator, modelling the textbook reading "a sequence is a
// function ℕ → T").  The load-bearing distinction the #525 fix
// makes is between "Path<int> is an arrow" (true: it has Domain,
// Codomain, call operator) and "Path<int> is category-shaped"
// (false: no ::Arrow / ::Species / ::Id aliases — Path<int> is an
// object, not a category-to-category arrow).  Pre-#525 the
// IsSpokeArrow discriminator read !IsArrow<Dom<T>> — but the
// Morphism here has Dom = Path<int>, which IS an arrow, so the
// proxy mis-classified Morphism<Path,Path,...> as NOT a SpokeArrow.
// Post-#525 the discriminator reads !IsCategoryShape<Dom<T>>, which
// returns false for Path<int> (it has no category-shape aliases),
// so the Morphism IS a SpokeArrow.  This static_assert pins the fix
// mechanically; if the discriminator ever regresses, it surfaces
// here at the load-bearing case that motivated the sharpening.
namespace _path_525 {
using PathArrow = dedekind::category::CanonicalSetCCC<Path<int>>::Arrow;
// Morphism<Path<int>, Path<int>, std::function<Path<int>(Path<int>)>> —
// CanonicalSetCCC<X> is the exported alias for the in-cartesian Set<X>
// type (Set itself is not export-qualified).  Same precedent as the
// vec2_functor / matrix2x2_functor Hubs in :linear_algebra.
static_assert(dedekind::category::IsArrow<Path<int>>,
              "Path<int> is itself an IsArrow at the type level "
              "(Domain = size_t, Codomain = T, call operator).");
static_assert(!dedekind::category::IsCategoryShape<Path<int>>,
              "Path<int> is NOT a category-shaped thing — it has no "
              "::Arrow / ::Species / ::Id aliases.  This is the "
              "distinction the #525 discriminator-sharpening makes.");
static_assert(dedekind::category::IsSpokeArrow<PathArrow>,
              "Morphism<Path<int>, Path<int>, ...> is a SpokeArrow under "
              "the #525 sharpening (Dom = Path<int>, which is not a "
              "category-shaped thing). Pre-#525 this static_assert would "
              "have failed because Path<int> satisfies IsArrow at the "
              "type level, and the old !IsArrow<Dom<T>> discriminator "
              "therefore mis-classified the Morphism as non-spoke.");
}  // namespace _path_525

static_assert(IsFiniteSequence<FinitePath<int>>,
              "FinitePath must satisfy the finite sequence concept.");

// Bidirectional anchor (math ↔ stdlib) for #388: FinitePath<T>
// presents as a std::ranges::input_range out of the box
// (FinitePath::begin / end yield std::input_iterator instances),
// and the inverse direction is provided by from_range just above,
// which lifts any std::ranges::input_range into a FinitePath.
// Anchoring at the input_range concept makes FinitePath values flow
// into std::ranges algorithms such as transform and for_each, as
// well as views such as std::views::filter, and iterator-based
// reductions such as std::accumulate without a bespoke adapter,
// keeping the :sequences vocabulary connected to standard C++
// idioms while the strictly stronger std::generator coroutine is
// awaited from libc++.  Other IsFiniteSequence carriers acquire the
// same range surface by additionally exposing begin() / end(); the
// concept itself does not mandate them.
static_assert(std::ranges::input_range<FinitePath<int>>,
              "FinitePath<T> must satisfy std::ranges::input_range so "
              "library sequences flow into std::ranges algorithms.");
static_assert(
    std::input_iterator<decltype(std::declval<FinitePath<int>>().begin())>,
    "FinitePath<T>::begin() must yield a std::input_iterator.");

// Juliet-posture witness (#531): the @c std::ranges API produces bona
// fide library sequences via the @c from_range adapter — any
// @b finite @c std::ranges::input_range round-trips (when iterated to
// completion) into @c FinitePath<T>, i.e. into the
// @c IsFiniteSequence concept.  The intensional / lazy std::ranges
// surface is anchored in the library's sequence layer at the type
// level.  The static_asserts below decide the type-level commitment;
// the actual materialisation of an infinite range would not terminate
// at runtime — the witness is about the @em type carrying the
// sequence concept, not about iterating arbitrary ranges.
static_assert(
    IsFiniteSequence<decltype(from_range(std::declval<std::vector<int>>()))>,
    "from_range over a std::ranges::input_range produces a FinitePath, "
    "a bona fide IsFiniteSequence — anchors std::ranges output in the "
    "library's sequence concept hierarchy (Juliet posture).");
static_assert(
    IsFiniteSequence<
        decltype(from_range(std::declval<std::ranges::iota_view<int, int>>()))>,
    "from_range over a std::ranges::iota_view (the lazy / intensional "
    "view) produces a FinitePath, lifting the std::views::iota surface "
    "into the library's sequence layer (Juliet posture).");

static_assert(IsKleisliExtension<path_functor<int>, int, long>,
              "Path must satisfy the Kleisli extension witness.");

static_assert(IsCoKleisliExtension<path_functor<int>, int, long>,
              "Path must satisfy the co-Kleisli extension witness.");

static_assert(
    IsFrobenius<path_functor<int>, int, long>,
    "Path must satisfy the Frobenius witness (Kleisli + co-Kleisli).");

// ---------------------------------------------------------------------------
// Categorical anchor: Path<T> as a morphism out of the NNO (#602).
//
// A @c Path<T> is structurally a function @f$f : \mathbb{N} \to T@f$
// satisfying @c IsArrow (asserted above), @c IsSequence (asserted
// above), and @c IsNet (asserted at the foot of this section, below).
// @c IsSequence's definition in @c :net entails @c IsNet, so the
// new @c static_assert is technically redundant — it is kept here
// explicitly to make the directed-set / generalisation door visible
// at this site rather than reading-as-magic-via-concept-refinement.
// Categorically this is a morphism
// out of the @b Natural Numbers Object: by NNO universality (ℕ as
// initial F-algebra of @f$F(X) = 1 + X@f$), such an @c f is uniquely
// determined by an @c (a₀ ∈ T, s : T → T) pair via Peano recursion,
// with @c f(0) = a₀ and @c f(n+1) = s(f(n)).  @c Path<T>'s call
// operator IS this recursor; the type encodes "transform of ℕ" by
// construction, not by iteration.
//
// The image of such an @c f — in the categorical kernel-pair-coequalizer
// sense — is the orbit of @c a₀ under iterated @c s in @c T: the
// smallest @c s-stable subobject of @c T containing @c a₀, equivalently
// @f$\mathrm{coeq}(\mathbb{N} \times_T \mathbb{N} \rightrightarrows
// \mathbb{N})@f$.  The general @c image-as-coequalizer machinery in
// @c :category is filed for a separate slice; this section pins the
// structural reading at the @c :sequences end so CI verifies the
// connection at the type level.
//
// The @c using aliases below give the textbook names @b Sequence and
// @b Net as type-level synonyms for @c Path; the @c static_asserts
// pin @c IsNet (which @c IsSequence already entails, but spelling it
// out separately makes the directed-set / generalisation door explicit).
//
// Open architectural concerns documented for follow-up:
//
//   * The @c Path<T> implementation is currently @b extensional-first
//     (@c std::vector backing).  The paper's §3 narrative is
//     @b intensional-first (rule before realisation; see
//     §sec:intensional).  Honest direction: split @c :path's
//     extensional realisation into a sister @c :sequences:extensional
//     partition, keeping @c :path purely structural.  Tracked as
//     follow-up; not in scope for these breadcrumbs.
//   * General-domain transforms (@c Net over directed sets that aren't
//     ℕ) need the @c :order:directed-set axiomatic surface to grow.
//     For now @c Net<T> is a synonym for @c Path<T> with implicit
//     ℕ-domain, with a forward-looking parameterisation.
//
// Filed against #602 (set-transformation primitives, post-#607 shape).
// ---------------------------------------------------------------------------

/** @brief Textbook name: a Sequence over @c T is @c f : ℕ → T = @c Path<T>. */
export template <typename T>
using Sequence = Path<T>;

/** @brief Textbook name: a Net is a function from a directed set into @c T.
 *
 *  @details For now ℕ is the only directed-set domain we model
 *  concretely, so @c Net<T> = @c Path<T> in this library.  The
 *  @c requires clause pins @c D @c = @c std::size_t (i.e., ℕ) so that
 *  callers cannot quietly instantiate over a non-ℕ domain that the
 *  library does not yet support.  Once @c :order grows non-ℕ
 *  directed-set carriers, the right move is to redesign @c Net as a
 *  @c struct or @c concept (alias templates cannot be specialised),
 *  with this typedef-shape replaced by the more general definition.
 *  The constraint here is the forward-pointing marker for that future
 *  redesign, not a specialisation hook.
 */
export template <typename T, typename D = std::size_t>
  requires std::same_as<D, std::size_t>
using Net = Path<T>;

// Pin the categorical roles via existing concepts.  IsArrow and
// IsSequence are asserted above; IsNet is added here as the explicit
// witness of the directed-set generalisation hook (IsSequence
// entails IsNet by its definition in @c :net, so the assertion is
// structurally redundant — its purpose is documentation-by-CI).
static_assert(IsNet<Path<int>>,
              "Path<int> is a morphism from a directed set (ℕ): the "
              "@c IsNet structural shape that @c IsSequence refines.");

// Sanity: the textbook-name aliases resolve to Path.
static_assert(std::same_as<Sequence<int>, Path<int>>,
              "Sequence<T> is a textbook-name alias for Path<T>.");
static_assert(
    std::same_as<Net<int>, Path<int>>,
    "Net<T> with default D = ℕ is a textbook-name alias for Path<T>.");

}  // namespace dedekind::sequences
