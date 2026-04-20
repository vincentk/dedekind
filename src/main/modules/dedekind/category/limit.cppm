/**
 * @file dedekind/category/limit.cppm
 * @partition :limit
 * @brief Level 0.6: The Boundary Objects (Initial and Terminal).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @quote
 * "The progress of mathematics can be viewed as progress from the
 *  infinite to the finite."
 *  — Gian-Carlo Rota, Indiscrete Thoughts
 *
 * @section Limits: The Universal Boundaries
 * In the Dedekind topos, the Initial (0) and Terminal (1) objects represent
 * the finite "anchors" of a system of otherwise infinite potential relations.
 * They are the unique sinks and sources through which the structure of
 * every other species is measured and made finite.
 *
 * @section Std_Namespace_Mappings
 * This partition asserts bidirectional mappings between categorical boundary
 * objects and `std` types:
 *
 * | Concept / Alias     | `std` representative  | Categorical role     |
 * |---------------------|-----------------------|----------------------|
 * | `One` (Terminal)    | `std::monostate`      | Unique sink (1)      |
 * | `Zero` (Initial)    | `std::nullptr_t`      | Unique source (0)    |
 *
 * @note "This conviction of the solvability of every mathematical problem is a
 * powerful incentive to the worker. We hear within us the perpetual call:
 * There is the problem. Seek its solution."
 *       -- David Hilbert, Mathematical Problems (1900)
 */
module;

#include <concepts>
#include <cstddef>
#include <exception>
#include <functional>
#include <type_traits>
#include <utility>
#include <variant>  // Required for std::monostate

export module dedekind.category:limit;

import :discrete;
import :morphism;
import :species;

namespace dedekind::category {

namespace detail {
template <typename Whole>
struct ArrowDrillDown {
  constexpr decltype(auto) operator()(const Whole& whole) const
    requires requires { whole.operator->(); }
  {
    return *whole.operator->();
  }
};
}  // namespace detail

/**
 * @brief The Terminal Object (1): the unique sink of every morphism.
 * @details Categorification of `std::monostate` as the Terminal Object.
 * `std::monostate` is a unit type (exactly one value, `{}`), making it the
 * canonical C++ representative of the categorical "1". Every species T admits
 * exactly one morphism T → One (the constant function).
 *
 * @see IsTerminalObject, unit()
 */
export using One = std::monostate;

/**
 * @brief The Initial Object (0): the unique source of every morphism.
 * @details Categorification of `std::nullptr_t` as the Initial Object.
 * `std::nullptr_t` has exactly one value (`nullptr`) that cannot be
 * constructed from anything else, making it the canonical C++ representative
 * of the categorical "0". From Zero, there exists exactly one (logically
 * unreachable) morphism to every species T, the zero morphism.
 *
 * @see IsInitialObject, zero()
 */
export using Zero = std::nullptr_t;

/**
 * @concept IsMorphicTotal
 * @brief A Morphism whose Codomain is the Terminal Object (One).
 * @details A morphism f: A → One is "morphically total" in the categorical
 * sense: it maps every element of its domain to the unique element of One.
 * This is the categorical analogue of a constant function that always returns
 * `std::monostate{}`.
 *
 * @note This is distinct from `IsTotalArrow` (in :total), which concerns
 * whether a morphism is defined on all inputs without undefined behaviour.
 */
export template <typename F>
concept IsMorphicTotal = requires {
  typename F::Domain;
  typename F::Codomain;
} && std::same_as<typename F::Codomain, One>;

/**
 * @concept IsTerminalMorphism
 * @brief A morphism that maps into the Terminal Object (One).
 * @details Alias for `IsMorphicTotal`. A terminal morphism `!: T → One`
 * collapses all information: every element of T maps to the single inhabitant
 * of `std::monostate`.
 */
export template <typename F>
concept IsTerminalMorphism = IsMorphicTotal<F>;

/**
 * @brief The unit morphism factory: produces the unique arrow !: T → One.
 * @details For every species T there is exactly one morphism to the Terminal
 * Object. This factory constructs it as a constant function that ignores its
 * input and returns `One{}` (`std::monostate{}`).
 * @tparam T The domain species.
 */
export template <typename T>
auto unit() {
  return arrow<T, One>([](const T&) { return One{}; });
}

/**
 * @concept IsTerminalObject
 * @brief A type that is (or maps canonically to) the Terminal Object One.
 * @details `std::monostate` (aliased as `One`) is the sole terminal object.
 * Any type T that produces a valid `IsTerminalMorphism` via `unit<T>()` also
 * satisfies this concept.
 */
export template <typename T>
concept IsTerminalObject = std::same_as<T, One>;

/**
 * @concept IsBoundaryProjection
 * @brief Projection witness from an owning/wrapper whole to a boundary object.
 *
 * @details
 * This concept captures the functional-part interpretation for limit objects:
 * a wrapper may expose a boundary object (`One` or `Zero`) through a
 * projection policy. It is intentionally structural and policy-driven.
 */
export template <typename Projection, typename Whole, typename Boundary>
concept IsBoundaryProjection = requires(Projection projection, Whole whole) {
  { projection(whole) } -> std::convertible_to<Boundary>;
};

/**
 * @concept IsProjectedTerminalObject
 * @brief Terminal-object witness through an optional projection policy.
 *
 * @details
 * With the default projector (`std::identity`), this reduces to
 * `IsTerminalObject<T>`. With an opt-in projector (for example an
 * `operator->` drill-down policy), wrappers can expose a terminal object while
 * keeping ownership/lifetime semantics explicit.
 */
export template <typename T, typename Project = std::identity>
concept IsProjectedTerminalObject =
    requires(Project project, const T& t) {
      { project(t) };
    } &&
    IsTerminalObject<std::remove_cvref_t<decltype(std::declval<Project>()(
        std::declval<const T&>()))>>;

/**
 * @brief The zero morphism factory: produces the unique (unreachable) arrow
 *        ?: Zero → T.
 * @details From the Initial Object `Zero` (`std::nullptr_t`) there is exactly
 * one morphism to any species T. Since `nullptr` can never actually appear as
 * input in a well-formed program, this arrow's body is logically unreachable
 * and terminates if ever called.
 * @tparam T The codomain species.
 */
export template <typename T>
auto zero() {
  return arrow<Zero, T>([](Zero) -> T {
    // Logically unreachable annihilator
    std::terminate();
  });
}

/**
 * @concept HasUniqueMorphismTo
 * @brief Asserts that there exists a unique morphism from T to U = One.
 * @details Encodes the universal property of the Terminal Object: for every
 * species T, the factory `unit<T>()` produces the unique arrow T → One.
 */
export template <typename T, typename U>
concept HasUniqueMorphismTo = std::same_as<U, One> && requires {
  { unit<T>() } -> IsTerminalMorphism;
};

/**
 * @concept HasUniqueMorphismFrom
 * @brief Asserts that there exists a unique (unreachable) morphism from Z =
 * Zero to T.
 * @details Encodes the universal property of the Initial Object: for every
 * species T, the factory `zero<T>()` produces the unique arrow Zero → T.
 */
export template <typename Z, typename T>
concept HasUniqueMorphismFrom = std::same_as<Z, Zero> && requires {
  { zero<T>() } -> IsArrow;  // zero<T> is the unique arrow 0 -> T
};

/**
 * @concept IsInitialObject
 * @brief A type that is the Initial Object Zero.
 * @details `std::nullptr_t` (aliased as `Zero`) is the sole initial object.
 * It is the unique type from which a morphism to every other species exists.
 */
export template <typename T>
concept IsInitialObject = std::same_as<T, Zero>;

/**
 * @concept IsProjectedInitialObject
 * @brief Initial-object witness through an optional projection policy.
 *
 * @details
 * With the default projector (`std::identity`), this reduces to
 * `IsInitialObject<T>`. With an opt-in projector, wrappers can expose the
 * canonical initial object (`Zero`) without changing default semantics.
 */
export template <typename T, typename Project = std::identity>
concept IsProjectedInitialObject =
    requires(Project project, const T& t) {
      { project(t) };
    } &&
    IsInitialObject<std::remove_cvref_t<decltype(std::declval<Project>()(
        std::declval<const T&>()))>>;

/** @section Realizations */
export using TerminalCategory = DiscreteCategory<One>;
export using InitialCategory = DiscreteCategory<Zero>;

namespace detail {
struct TerminalEnvelope {
  One value{};
  constexpr const One* operator->() const { return &value; }
};

struct InitialEnvelope {
  Zero value{nullptr};
  constexpr const Zero* operator->() const { return &value; }
};
}  // namespace detail

// Compiler-validated documentation witnesses for projected boundary semantics.
static_assert(IsBoundaryProjection<std::identity, One, One>);
static_assert(IsBoundaryProjection<std::identity, Zero, Zero>);
static_assert(IsProjectedTerminalObject<One>);
static_assert(IsProjectedInitialObject<Zero>);
static_assert(
    IsProjectedTerminalObject<detail::TerminalEnvelope,
                              detail::ArrowDrillDown<detail::TerminalEnvelope>>,
    "Opt-in operator-> drill-down must expose a Terminal object witness.");
static_assert(
    IsProjectedInitialObject<detail::InitialEnvelope,
                             detail::ArrowDrillDown<detail::InitialEnvelope>>,
    "Opt-in operator-> drill-down must expose an Initial object witness.");

}  // namespace dedekind::category
