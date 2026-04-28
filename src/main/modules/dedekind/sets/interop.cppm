/**
 * @file dedekind/sets/interop.cppm
 * @partition :interop
 * @brief Explicit bridges between Dedekind extensional sets and std set-like
 *        containers.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "At the heart of generic programming is a single, simple idea:
 *        program over the most abstract type that supports the operations
 *        the algorithm needs."
 *       — Alexander A. Stepanov & Daniel E. Rose, *From Mathematics to
 *         Generic Programming* (Addison-Wesley, 2014), §1. Stepanov
 *         designed the STL on this principle; the partition's bridges
 *         honour it by translating across the rule-vs-bucket boundary
 *         (Dedekind predicate-set ↔ std container) without forcing
 *         either side into the other's vocabulary.
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
import :cardinality;
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

template <typename T>
concept DefaultHashable = requires(const T& value) {
  { std::hash<T>{}(value) } -> std::convertible_to<std::size_t>;
  { std::equal_to<T>{}(value, value) } -> std::convertible_to<bool>;
};

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
  for (const auto& value : source) out.insert(value);
  return out;
}

export template <typename StdSetLike, typename ExtSet>
constexpr auto to_std(const ExtSet&) -> StdSetLike {
  static_assert(detail::always_false_v<StdSetLike, ExtSet>,
                "dedekind::interop::to_std requires a "
                "dedekind::sets::FiniteExtensionalSet "
                "source and a std set-like target with matching value_type.");
}

export template <typename T, typename Alloc>
  requires detail::DefaultHashable<T>
constexpr auto from_std(const std::set<T, std::less<T>, Alloc>& source)
    -> dedekind::sets::FiniteExtensionalSet<T> {
  dedekind::sets::FiniteExtensionalSet<T> out;
  for (const auto& value : source) out.elements.insert(value);
  return out;
}

export template <typename T, typename Alloc>
  requires(!detail::DefaultHashable<T>)
constexpr auto from_std(const std::set<T, std::less<T>, Alloc>&)
    -> dedekind::sets::FiniteExtensionalSet<T> {
  static_assert(detail::always_false_v<T, Alloc>,
                "dedekind::interop::from_std(std::set<T>) requires T to be "
                "hashable by std::hash<T> and comparable by std::equal_to<T> "
                "because the MVP finite extensional carrier is hash-based.");
}

export template <typename T, typename Compare, typename Alloc>
  requires(!std::same_as<Compare, std::less<T>>)
constexpr auto from_std(const std::set<T, Compare, Alloc>&)
    -> dedekind::sets::FiniteExtensionalSet<T> {
  static_assert(detail::always_false_v<T, Compare, Alloc>,
                "dedekind::interop::from_std currently supports only "
                "std::set<T> with the default std::less<T> comparator. "
                "Custom comparators may encode a different notion of "
                "membership/uniqueness than the MVP hash-based extensional "
                "carrier preserves.");
}

export template <typename T, typename Hash, typename Equal, typename Alloc>
constexpr auto from_std(const std::unordered_set<T, Hash, Equal, Alloc>& source)
    -> dedekind::sets::FiniteExtensionalSet<
        T, dedekind::category::ClassicalLogic, Hash, Equal> {
  dedekind::sets::FiniteExtensionalSet<T, dedekind::category::ClassicalLogic,
                                       Hash, Equal>
      out;
  for (const auto& value : source) out.elements.insert(value);
  return out;
}

export template <typename StdSetLike>
  requires detail::StdSetLike<StdSetLike>
constexpr auto from_std(const StdSetLike&)
    -> dedekind::sets::FiniteExtensionalSet<typename StdSetLike::value_type> {
  static_assert(detail::always_false_v<StdSetLike>,
                "dedekind::interop::from_std currently supports only "
                "std::set<T> with the default comparator and "
                "std::unordered_set<T, Hash, Equal>. Other set-like types are "
                "reserved for a follow-up interop phase.");
}

export template <typename ExtSet, typename StdSetLike>
constexpr auto from_std(const StdSetLike&) -> ExtSet {
  static_assert(detail::always_false_v<ExtSet, StdSetLike>,
                "dedekind::interop::from_std currently materializes as "
                "dedekind::sets::FiniteExtensionalSet<T>. Use auto ext = "
                "from_std(container).");
}

}  // namespace dedekind::interop
