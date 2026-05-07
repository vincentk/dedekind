/**
 * @file dedekind/sets/extensional.cppm
 * @partition :extensional
 * @brief @c ExtensionalSet — the canonical small-finite extensional carrier,
 *        with @c std::set / @c std::unordered_set round-trips.
 *
 * @details
 * In any computable representation an extensional set (one defined by
 * listing its elements rather than by predicate) is necessarily finite,
 * so the @b Finite qualifier is dropped from the carrier name; the
 * finiteness and countability propositions are kept as @c static_assert
 * breadcrumbs against @c IsFiniteSet / @c IsFinite / @c IsCountable
 * (#598 acceptance).
 *
 * The std-container bridges (@c from_std / @c to_std) live in this
 * partition rather than in a separate @c :interop sister, since they
 * are constructors / accessors of @c ExtensionalSet — there is no
 * "interop" surface independent of the carrier they materialise.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "At the heart of generic programming is a single, simple idea:
 *        program over the most abstract type that supports the operations
 *        the algorithm needs."
 *       [Trans: original is English; the gloss is that the carrier's
 *        std-container bridges should ask only for the minimal interface
 *        the algorithm uses, not commit either side to the other's
 *        representational choices.]
 *       — Alexander A. Stepanov & Daniel E. Rose, *From Mathematics to
 *         Generic Programming* (Addison-Wesley, 2014), §1. Stepanov
 *         designed the STL on this principle; the bridges honour it by
 *         translating across the rule-vs-bucket boundary (Dedekind
 *         predicate-set ↔ std container) without forcing either side
 *         into the other's vocabulary.
 */
module;

#include <concepts>
#include <cstddef>
#include <functional>
#include <iterator>
#include <set>
#include <type_traits>
#include <unordered_set>
#include <utility>

export module dedekind.sets:extensional;

import dedekind.category;
import :cardinality;
import :computability;
import :mereology;

namespace dedekind::sets {

export template <typename T, typename L = dedekind::category::ClassicalLogic,
                 typename Hash = std::hash<T>,
                 typename Equal = std::equal_to<T>>
struct ExtensionalSet {
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

  constexpr bool operator==(const ExtensionalSet& other) const {
    return elements == other.elements;
  }
};

// ---------------------------------------------------------------------------
// IsSet wiring (#598).
//
// `ExtensionalSet<T, …>` is already a callable predicate
// `T → L::Ω`, which is the only shape `dedekind::category::ambient_set<A>(…)`
// asks for.  Following the SingletonSet precedent (singleton.cppm:222–225,
// `IsSet<decltype(ambient_set<int>(SingletonSet<int>{0}))>`), we lift via
// the same `ambient_set<T>(…)` mediator rather than retrofitting
// `Ambient` / `Member` / `ι` / `χ` directly onto the struct.  Two reasons:
//
//   * Consistency with SingletonSet: both small-extensional carriers
//     reach IsSet through the same gate.  The "small finite set" rosetta
//     entry the issue calls for is the lift, not the struct shape.
//   * Avoids a copy-ctor footgun: a stored `χ` field would have to track
//     the parent's `elements` across copy/move; the lift sidesteps that
//     entirely (the lifted Subobject owns its own predicate copy).
//
// The decltype-based static_assert form is borrowed wholesale from the
// SingletonSet site — it resolves the lifted type at the type level
// without requiring `std::unordered_set` to be constexpr-constructible
// (it isn't, in C++23, with non-empty contents).
// ---------------------------------------------------------------------------
static_assert(
    dedekind::category::IsSet<
        decltype(dedekind::category::ambient_set<int>(ExtensionalSet<int>{}))>,
    "ExtensionalSet must lift to an ETCS set object via "
    "ambient_set<T>(...), the same gate SingletonSet uses (#598).");

static_assert(
    dedekind::category::IsSet<decltype(dedekind::category::ambient_set<int>(
        ExtensionalSet<int, dedekind::category::ClassicalLogic, std::hash<int>,
                       std::equal_to<int>>{}))>,
    "Hash / Equal template parameters do not perturb the IsSet lift: "
    "the predicate-shape `T → L::Ω` is what ambient_set<T>(...) gates on, "
    "and that is independent of the underlying `std::unordered_set`'s "
    "hashing policy.");

// ---------------------------------------------------------------------------
// Finiteness / countability propositions kept as type-checker breadcrumbs
// (#598: name dropped @b Finite, propositions remain).
//
// In any computable representation an extensional set must be finite —
// `std::unordered_set<T>` cannot store infinitely many elements — so the
// @c Finite qualifier was redundant in the type name and got dropped.  But
// the propositions @b are load-bearing for downstream dispatch (e.g.
// finite-image lifts, compile-time enumeration), so we pin them here.
//
// `IsFinite<C>` is the cardinality-level proposition (`!IsTransfinite<C>`,
// from `category:species`); `IsCountable<C>` is the cardinality-level
// "magnitude ≤ ℵ₀" proposition (from `:mereology`); `IsFiniteSet<S>` is
// the set-level shape predicate (from `:computability`, requires
// `S::cardinality_type` modelling `IsFinite`).
// ---------------------------------------------------------------------------
static_assert(dedekind::category::IsFinite<
                  typename ExtensionalSet<int>::cardinality_type>,
              "ExtensionalSet's cardinality_type tag (Finite) models IsFinite "
              "— the dropped name's invariant kept as a breadcrumb (#598).");

static_assert(IsCountable<typename ExtensionalSet<int>::cardinality_type>,
              "Finite ⇒ countable: ExtensionalSet's cardinality_type "
              "models IsCountable (#598).");

static_assert(IsFiniteSet<ExtensionalSet<int>>,
              "ExtensionalSet satisfies the set-level IsFiniteSet shape "
              "predicate from :computability (#598).");

// ---------------------------------------------------------------------------
// Equivalence-relation breadcrumb (#598 scout).
//
// `ExtensionalSet`'s `Equal` template parameter is the equivalence
// relation under which membership is decided.  `std::equal_to<T>` on
// `std::regular T` is the canonical witness — the same `IsEquivalence`
// that `category:equivalence` already pins for `int` is what every
// `ExtensionalSet<int>` instance silently relies on.  Pinning it here
// (instead of leaving it implicit) gives the type-checker an explicit
// arrow back to the equivalence-relation home.  The companion `Hash`
// is the partition function (each equivalence class = a hash bucket);
// `IsHashFunction` from the same partition pins that side.
// ---------------------------------------------------------------------------
static_assert(dedekind::category::IsEquivalence<int, std::equal_to<int>>,
              "ExtensionalSet's `Equal` (default `std::equal_to<int>`) "
              "must satisfy IsEquivalence — the equivalence relation "
              "under which extensional membership is decided (#598).");
static_assert(dedekind::category::IsHashFunction<std::hash<int>, int>,
              "ExtensionalSet's `Hash` (default `std::hash<int>`) must "
              "satisfy IsHashFunction — the partition function whose "
              "fibres index the underlying std::unordered_set's buckets "
              "(#598).");

// `StdSetLike`: structural shape of the std-container types `from_std`
// accepts and `to_std` produces.  Single concept (no detail wrapper —
// per PR #605 review: "not sure what is gained by declaring it twice").
export template <typename C>
concept StdSetLike = requires(C c, const typename C::value_type& value) {
  typename C::value_type;
  { c.begin() } -> std::input_iterator;
  { c.end() } -> std::sentinel_for<decltype(c.begin())>;
  { c.insert(value) };
  { c.find(value) };
};

namespace detail {

template <typename...>
inline constexpr bool always_false_v = false;

template <typename C, typename T>
concept StdSetLikeOf =
    dedekind::sets::StdSetLike<C> && std::same_as<typename C::value_type, T>;

}  // namespace detail

export template <typename StdSetLike, typename T, typename L, typename Hash,
                 typename Equal>
  requires detail::StdSetLikeOf<StdSetLike, T>
constexpr auto to_std(
    const dedekind::sets::ExtensionalSet<T, L, Hash, Equal>& source)
    -> StdSetLike {
  StdSetLike out;
  for (const auto& value : source) out.insert(value);
  return out;
}

export template <typename StdSetLike, typename ExtSet>
constexpr auto to_std(const ExtSet&) -> StdSetLike {
  static_assert(detail::always_false_v<StdSetLike, ExtSet>,
                "dedekind::sets::to_std requires a "
                "dedekind::sets::ExtensionalSet "
                "source and a std set-like target with matching value_type.");
}

// Syntactic gate only: `from_std` needs hashing (to populate the
// std::unordered_set buckets) and `==` (to satisfy `std::regular<T>`).
// The semantic claim that `std::equal_to<T>` is a textbook equivalence
// relation lives in the @c IsEquivalence breadcrumb above on the int
// witness; it is deliberately NOT required here because
// `category:equivalence` excludes floating-point from `IsEquivalence`
// (NaN breaks reflexivity), and the Python facade legitimately accepts
// `std::set<double>` despite that semantic gap.  The user-facing
// soundness contract is: the carrier behaves extensionally up to the
// equality the user supplies; if that equality isn't an equivalence
// relation in the textbook sense, that's the user's call (see #591
// Mazur-equivalence escape hatch for ε-equivalence on floats).
export template <typename T, typename Alloc>
  requires dedekind::category::IsHashFunction<std::hash<T>, T> &&
           std::regular<T>
constexpr auto from_std(const std::set<T, std::less<T>, Alloc>& source)
    -> dedekind::sets::ExtensionalSet<T> {
  dedekind::sets::ExtensionalSet<T> out;
  for (const auto& value : source) out.elements.insert(value);
  return out;
}

export template <typename T, typename Alloc>
  requires(!dedekind::category::IsHashFunction<std::hash<T>, T> ||
           !std::regular<T>)
constexpr auto from_std(const std::set<T, std::less<T>, Alloc>&)
    -> dedekind::sets::ExtensionalSet<T> {
  static_assert(detail::always_false_v<T, Alloc>,
                "dedekind::sets::from_std(std::set<T>) requires T to be "
                "hashable (IsHashFunction<std::hash<T>, T>) and "
                "std::regular (T's `==` is well-formed); the MVP finite "
                "extensional carrier is hash-based.");
}

export template <typename T, typename Compare, typename Alloc>
  requires(!std::same_as<Compare, std::less<T>>)
constexpr auto from_std(const std::set<T, Compare, Alloc>&)
    -> dedekind::sets::ExtensionalSet<T> {
  static_assert(detail::always_false_v<T, Compare, Alloc>,
                "dedekind::sets::from_std currently supports only "
                "std::set<T> with the default std::less<T> comparator. "
                "Custom comparators may encode a different notion of "
                "membership/uniqueness than the MVP hash-based extensional "
                "carrier preserves.");
}

export template <typename T, typename Hash, typename Equal, typename Alloc>
constexpr auto from_std(const std::unordered_set<T, Hash, Equal, Alloc>& source)
    -> dedekind::sets::ExtensionalSet<T, dedekind::category::ClassicalLogic,
                                      Hash, Equal> {
  dedekind::sets::ExtensionalSet<T, dedekind::category::ClassicalLogic, Hash,
                                 Equal>
      out;
  for (const auto& value : source) out.elements.insert(value);
  return out;
}

export template <typename StdSetLike>
  requires dedekind::sets::StdSetLike<StdSetLike>
constexpr auto from_std(const StdSetLike&)
    -> dedekind::sets::ExtensionalSet<typename StdSetLike::value_type> {
  static_assert(detail::always_false_v<StdSetLike>,
                "dedekind::sets::from_std currently supports only "
                "std::set<T> with the default comparator and "
                "std::unordered_set<T, Hash, Equal>. Other set-like types are "
                "reserved for a follow-up interop phase.");
}

export template <typename ExtSet, typename StdSetLike>
constexpr auto from_std(const StdSetLike&) -> ExtSet {
  static_assert(detail::always_false_v<ExtSet, StdSetLike>,
                "dedekind::sets::from_std currently materializes as "
                "dedekind::sets::ExtensionalSet<T>. Use auto ext = "
                "from_std(container).");
}

// ---------------------------------------------------------------------------
// Round-trip IsSet preservation breadcrumb (#598 acceptance criterion 4).
//
// A `std::set<int>` lifted via `from_std` lands in
// `ExtensionalSet<int>` (the default-template specialisation), which
// is then liftable to IsSet via `ambient_set<int>(…)`.  The two-step
// pipeline preserves the IsSet membership end-to-end.  We pin this at
// the type level using `std::declval` (since `std::set<int>` is not
// constexpr-default-constructible in C++23, we cannot evaluate
// `from_std(std::set<int>{...})` at constant time, but the type pipeline
// resolves under `decltype` without evaluation).
// ---------------------------------------------------------------------------
static_assert(
    dedekind::category::IsSet<decltype(dedekind::category::ambient_set<int>(
        from_std(std::declval<const std::set<int>&>())))>,
    "from_std(std::set<int>) round-trip preserves IsSet membership: "
    "the materialised ExtensionalSet<int> lifts via "
    "ambient_set<int>(...) to an ETCS set object (#598).");

static_assert(
    dedekind::category::IsSet<decltype(dedekind::category::ambient_set<int>(
        from_std(std::declval<const std::unordered_set<int>&>())))>,
    "from_std(std::unordered_set<int>) round-trip preserves IsSet "
    "membership through the same ambient_set<int>(...) gate (#598).");

// ---------------------------------------------------------------------------
// image / filter on std-container carriers — concrete realisations of
// transform-and-restrict on the canonical extensional carrier (#602).
//
// These are operationally trivial helpers: image(f, S) iterates and
// inserts; filter(p, S) iterates and conditionally inserts.  They live
// here, not in @c category:etcs, because they are imperative loops over
// std containers — the categorical / "pure" framework in @c category:
// stays free of for-loops and @c std::* iteration.  In ETCS-flavoured
// terminology these helpers are the cardinality-extensional case of the
// abstract operations (image as kernel-pair coequalizer, filter as
// pullback-of-the-subobject-classifier); the categorical primitives
// belong upstream and have their own treatment.  Not paper-relevant on
// their own; ship them here as engineering convenience for downstream
// callers that already hold std containers.
// ---------------------------------------------------------------------------

/** @brief image(f, std::unordered_set<T, ...>) — lvalue. */
export template <typename T, typename Hash, typename Equal, typename Alloc,
                 std::invocable<const T&> F>
constexpr auto image(F&& f,
                     const std::unordered_set<T, Hash, Equal, Alloc>& s) {
  using U = std::remove_cvref_t<std::invoke_result_t<F&, const T&>>;
  std::unordered_set<U> out;
  out.reserve(s.size());
  for (const auto& x : s) out.insert(f(x));
  return out;
}

/** @brief image(f, std::unordered_set<T, ...>) — rvalue. */
export template <typename T, typename Hash, typename Equal, typename Alloc,
                 std::invocable<const T&> F>
constexpr auto image(F&& f, std::unordered_set<T, Hash, Equal, Alloc>&& s) {
  using U = std::remove_cvref_t<std::invoke_result_t<F&, const T&>>;
  std::unordered_set<U> out;
  out.reserve(s.size());
  for (const auto& x : s) out.insert(f(x));
  return out;
}

/** @brief image(f, std::set<T, ...>) — lvalue. */
export template <typename T, typename Compare, typename Alloc,
                 std::invocable<const T&> F>
constexpr auto image(F&& f, const std::set<T, Compare, Alloc>& s) {
  using U = std::remove_cvref_t<std::invoke_result_t<F&, const T&>>;
  std::set<U> out;
  for (const auto& x : s) out.insert(f(x));
  return out;
}

/** @brief image(f, std::set<T, ...>) — rvalue. */
export template <typename T, typename Compare, typename Alloc,
                 std::invocable<const T&> F>
constexpr auto image(F&& f, std::set<T, Compare, Alloc>&& s) {
  using U = std::remove_cvref_t<std::invoke_result_t<F&, const T&>>;
  std::set<U> out;
  for (const auto& x : s) out.insert(f(x));
  return out;
}

/** @brief filter(p, std::unordered_set<T, ...>) — lvalue. */
export template <typename T, typename Hash, typename Equal, typename Alloc,
                 std::predicate<const T&> P>
constexpr auto filter(P&& p,
                      const std::unordered_set<T, Hash, Equal, Alloc>& s) {
  std::unordered_set<T, Hash, Equal> out;
  for (const auto& x : s) {
    if (p(x)) out.insert(x);
  }
  return out;
}

/** @brief filter(p, std::unordered_set<T, ...>) — rvalue. */
export template <typename T, typename Hash, typename Equal, typename Alloc,
                 std::predicate<const T&> P>
constexpr auto filter(P&& p, std::unordered_set<T, Hash, Equal, Alloc>&& s) {
  std::unordered_set<T, Hash, Equal> out;
  for (const auto& x : s) {
    if (p(x)) out.insert(x);
  }
  return out;
}

/** @brief filter(p, std::set<T, ...>) — lvalue. */
export template <typename T, typename Compare, typename Alloc,
                 std::predicate<const T&> P>
constexpr auto filter(P&& p, const std::set<T, Compare, Alloc>& s) {
  std::set<T, Compare> out;
  for (const auto& x : s) {
    if (p(x)) out.insert(x);
  }
  return out;
}

/** @brief filter(p, std::set<T, ...>) — rvalue. */
export template <typename T, typename Compare, typename Alloc,
                 std::predicate<const T&> P>
constexpr auto filter(P&& p, std::set<T, Compare, Alloc>&& s) {
  std::set<T, Compare> out;
  for (const auto& x : s) {
    if (p(x)) out.insert(x);
  }
  return out;
}

// Type-shape breadcrumbs (no static_assert on values, since std::* can't
// be default-constructed with non-empty contents in C++23 constexpr).
static_assert(std::same_as<decltype(image(
                               std::declval<int (&)(const int&)>(),
                               std::declval<const std::unordered_set<int>&>())),
                           std::unordered_set<int>>,
              "image(f, std::unordered_set<T>) returns std::unordered_set<U>.");

static_assert(
    std::same_as<decltype(image(std::declval<int (&)(const int&)>(),
                                std::declval<const std::set<int>&>())),
                 std::set<int>>,
    "image(f, std::set<T>) returns std::set<U>.");

static_assert(
    std::same_as<
        decltype(filter(std::declval<bool (&)(const int&)>(),
                        std::declval<const std::unordered_set<int>&>())),
        std::unordered_set<int>>,
    "filter(p, std::unordered_set<T>) returns std::unordered_set<T>.");

static_assert(
    std::same_as<decltype(filter(std::declval<bool (&)(const int&)>(),
                                 std::declval<const std::set<int>&>())),
                 std::set<int>>,
    "filter(p, std::set<T>) returns std::set<T>.");

}  // namespace dedekind::sets
