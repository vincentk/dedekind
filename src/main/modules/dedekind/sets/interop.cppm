/**
 * @file dedekind/sets/interop.cppm
 * @partition :interop
 * @brief Explicit bridges between Dedekind extensional sets and std set-like
 * containers.
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */
module;

#include <concepts>
#include <cstddef>
#include <functional>
#include <iterator>
#include <set>
#include <type_traits>
#include <unordered_set>

export module dedekind.sets:interop;

import dedekind.category;
import :mereology;

namespace dedekind::sets {

export template <typename T, typename L = dedekind::category::ClassicalLogic,
                 typename Hash = std::hash<T>,
                 typename Equal = std::equal_to<T>>
struct FiniteExtensionalSet {
  using Domain = T;
  using Codomain = typename L::Ω;
  using logic_species = L;
  using cardinality_type = Finite;
  using is_extensional_tag = void;

  std::unordered_set<T, Hash, Equal> elements;

  constexpr Codomain operator()(const T& value) const {
    return elements.contains(value) ? L::True : L::False;
  }

  constexpr bool contains(const T& value) const {
    return elements.contains(value);
  }

  constexpr std::size_t size() const noexcept { return elements.size(); }
  constexpr std::size_t upper_bound() const noexcept { return elements.size(); }
  constexpr cardinality_type cardinality() const noexcept {
    return cardinality_type{};
  }

  constexpr auto begin() const noexcept { return elements.begin(); }
  constexpr auto end() const noexcept { return elements.end(); }

  constexpr bool operator==(const FiniteExtensionalSet& other) const {
    return elements == other.elements;
  }
};

}  // namespace dedekind::sets

namespace dedekind::interop {

namespace detail {

template <typename...>
inline constexpr bool always_false_v = false;

template <typename C>
concept StdSetLike = requires(C c, const typename C::value_type& value) {
  typename C::value_type;
  { c.begin() } -> std::input_iterator;
  { c.end() } -> std::sentinel_for<decltype(c.begin())>;
  { c.insert(value) };
  { c.find(value) };
};

template <typename C, typename T>
concept StdSetLikeOf = StdSetLike<C> && std::same_as<typename C::value_type, T>;

}  // namespace detail

export template <typename C>
concept StdSetLike = detail::StdSetLike<C>;

export template <typename StdSetLike, typename T, typename L, typename Hash,
                 typename Equal>
  requires detail::StdSetLikeOf<StdSetLike, T>
constexpr auto to_std(
    const dedekind::sets::FiniteExtensionalSet<T, L, Hash, Equal>& source)
    -> StdSetLike {
  StdSetLike out;
  out.insert(source.begin(), source.end());
  return out;
}

export template <typename StdSetLike, typename ExtSet>
constexpr auto to_std(const ExtSet&) -> StdSetLike {
  static_assert(detail::always_false_v<StdSetLike, ExtSet>,
                "dedekind::interop::to_std requires a "
                "dedekind::sets::FiniteExtensionalSet "
                "source and a std set-like target with matching value_type.");
}

export template <typename T, typename Compare, typename Alloc>
constexpr auto from_std(const std::set<T, Compare, Alloc>& source)
    -> dedekind::sets::FiniteExtensionalSet<T> {
  dedekind::sets::FiniteExtensionalSet<T> out;
  out.elements.insert(source.begin(), source.end());
  return out;
}

export template <typename T, typename Hash, typename Equal, typename Alloc>
constexpr auto from_std(const std::unordered_set<T, Hash, Equal, Alloc>& source)
    -> dedekind::sets::FiniteExtensionalSet<
        T, dedekind::category::ClassicalLogic, Hash, Equal> {
  dedekind::sets::FiniteExtensionalSet<T, dedekind::category::ClassicalLogic,
                                       Hash, Equal>
      out;
  out.elements.insert(source.begin(), source.end());
  return out;
}

export template <typename StdSetLike>
  requires detail::StdSetLike<StdSetLike>
constexpr auto from_std(const StdSetLike& source)
    -> dedekind::sets::FiniteExtensionalSet<typename StdSetLike::value_type> {
  using T = typename StdSetLike::value_type;
  dedekind::sets::FiniteExtensionalSet<T> out;
  out.elements.insert(source.begin(), source.end());
  return out;
}

export template <typename ExtSet, typename StdSetLike>
constexpr auto from_std(const StdSetLike&) -> ExtSet {
  static_assert(detail::always_false_v<ExtSet, StdSetLike>,
                "dedekind::interop::from_std currently materializes as "
                "dedekind::sets::FiniteExtensionalSet<T>. Use auto ext = "
                "from_std(container).");
}

}  // namespace dedekind::interop
