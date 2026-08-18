/**
 * @file dedekind/sequences/finite.cppm
 * @partition :finite
 * @brief Compile-time, array-backed realizations of a sequence and a net ---
 *        the @c constexpr siblings of the heap-backed @c Path.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section finite__Overview
 * @c Path (@c :path) wraps @c std::vector: an extensional sequence that runs
 * but cannot fold to a translation-time constant.  @c FiniteSeq and @c Net
 * here are its @c std::array-backed counterparts: the same @c IsSequence /
 * @c IsNet shapes, evaluable at translation time.  The pair is the two-way
 * bridge one level down --- one realization for run time, one for compile
 * time.
 */
module;

#include <array>
#include <cstddef>

export module dedekind.sequences:finite;

import :net;  // IsNet, IsSequence, IsFiniteSequence

namespace dedekind::sequences {

/**
 * @brief A finite net: a tabulated morphism @c size_t → X (an @c IsNet, since
 *        @c std::size_t is a directed set).  Read as a function @c d(v); the
 *        @c std::array tabulation is private business, not the interface.
 */
export template <typename X, std::size_t Cap>
struct FiniteNet {
  using Domain = std::size_t;
  using Codomain = X;

  std::array<X, Cap> table{};

  constexpr Codomain operator()(Domain v) const { return table[v]; }
  constexpr Codomain& at(Domain v) { return table[v]; }  // for building
  static constexpr std::size_t cardinality() { return Cap; }
};

/**
 * @brief A finite sequence: an array-backed range of length @c n ≤ Cap ---
 *        the compile-time extensional realization of a sequence.
 */
export template <typename T, std::size_t Cap>
struct FiniteSeq {
  using Domain = std::size_t;
  using Codomain = T;

  std::array<T, Cap> data{};
  std::size_t n = 0;

  constexpr Codomain operator()(Domain i) const { return data[i]; }
  constexpr void push(T x) { data[n++] = x; }
  constexpr const T* begin() const { return data.data(); }
  constexpr const T* end() const { return data.data() + n; }
  constexpr std::size_t size() const { return n; }
};

// Consideration, pinned: these are genuine nets, not shape-alikes.
static_assert(IsNet<FiniteNet<int, 4>>);
static_assert(IsNet<FiniteNet<std::size_t, 4>>);

}  // namespace dedekind::sequences
