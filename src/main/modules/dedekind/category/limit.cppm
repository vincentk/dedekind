/**
 * @file dedekind/category/limit.cppm
 * @partition :limit
 * @brief Universal Limits: boundary objects (Initial and Terminal) and binary
 * products.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @quote
 * "The progress of mathematics can be viewed as progress from the
 *  infinite to the finite."
 *  — Gian-Carlo Rota, Indiscrete Thoughts
 *
 * @section limit__Limits
 * In the Dedekind topos, the Initial (0) and Terminal (1) objects represent
 * the finite "anchors" of a system of otherwise infinite potential relations.
 * They are the unique sinks and sources through which the structure of
 * every other species is measured and made finite.
 *
 * @section limit__Std_Namespace_Mappings
 * This partition asserts bidirectional mappings between categorical limit
 * constructions and `std` types:
 *
 * | Concept / Alias     | `std` representative  | Categorical role          |
 * |---------------------|-----------------------|---------------------------|
 * | `One` (Terminal)    | `std::monostate`      | Unique sink (1)           |
 * | `Zero` (Initial)    | `std::nullptr_t`      | Unique source (0)         |
 * | `IsProduct`         | `std::pair<A, B>`     | Binary product (A × B)    |
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
#include <tuple>  // SpeciesTraits<std::tuple<Ts...>> in the products block (#637)
#include <type_traits>
#include <utility>
#include <variant>  // Required for std::monostate

export module dedekind.category:limit;

import :discrete;
import :mereology;
import :morphism;
import :small;  // IsSmallCategory --- the size-axis prerequisite for
                // IsCartesian
import :species;

namespace dedekind::category {

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

/** @section limit__Products_and_Projections
 *
 * @details Binary products are limits of the discrete two-object diagram
 * @c {* @c → @c A, @c * @c → @c B}; their natural home is here in @c :limit
 * alongside @c IsTerminalObject / @c IsInitialObject (themselves limits of
 * the empty / opposite-empty diagrams).  Re-homed from @c :cartesian under
 * #637 --- @c :cartesian retains the strictly CCC-completing pieces
 * (exponentials, currying, @c IsCartesianClosed).
 */

/**
 * @concept IsProduct
 * @brief Categorification of `std::pair<A, B>` as the categorical product
 * (A × B).
 * @details A product of A and B is an object P equipped with projection
 * morphisms π₁: P → A and π₂: P → B such that for any object X with morphisms
 * f: X → A and g: X → B, there exists a unique morphism u: X → P making the
 * following diagram commute:
 * @code
 *        X
 *       / \
 *      f   g
 *     /     \
 *    A       B
 *     \     /
 *      π₁  π₂
 *       \ /
 *        P
 * @endcode
 *
 * `std::pair<A, B>` satisfies this concept via its `.first` (π₁) and
 * `.second` (π₂) members.
 */
template <typename P, typename A, typename B>
concept IsPairLikeProduct = requires(P p) {
  { p.first } -> std::convertible_to<A>;
  { p.second } -> std::convertible_to<B>;
};

export template <typename P, typename A, typename B>
concept IsProduct = IsPairLikeProduct<P, A, B>;

static_assert(
    IsProduct<std::pair<int, bool>, int, bool>,
    "Verification Failed: std::pair<int, bool> must satisfy IsProduct.");

template <typename A, typename B>
struct SpeciesTraits<std::pair<A, B>> {
  using Domain = std::pair<A, B>;
  using machine_type = Domain;
};

template <typename... Ts>
  requires(sizeof...(Ts) > 0)
struct SpeciesTraits<std::tuple<Ts...>> {
  using Domain = std::tuple<Ts...>;
  using machine_type = Domain;
};

/**
 * @concept IsProductProjection
 * @brief Functional-part projection witness from a product whole to one part.
 *
 * @details
 * A projection is a morphism-like accessor from a whole `P` to one component
 * type (`A` for left, `B` for right). In categorical terms these correspond to
 * canonical product projections π₁ and π₂.
 */
export template <typename Projection, typename Whole, typename Part>
concept IsProductProjection = requires(Projection projection, Whole whole) {
  { projection(whole) } -> std::convertible_to<Part>;
};

/**
 * @concept IsProjectedProduct
 * @brief Product witness through an optional whole-projection policy.
 *
 * @details
 * By default (`WholeProject = std::identity`), this reduces to a direct
 * `IsProduct<P, A, B>` check. With an opt-in projector (for example an
 * `operator->` drill-down policy), this concept certifies that a wrapper type
 * exposes a product whole whose canonical parts are still discoverable.
 */
export template <typename P, typename A, typename B,
                 typename WholeProject = std::identity>
concept IsProjectedProduct =
    requires(WholeProject project, const P& p) {
      { project(p) };
    } &&
    IsProduct<std::remove_cvref_t<decltype(std::declval<WholeProject>()(
                  std::declval<const P&>()))>,
              A, B>;

static_assert(IsProductProjection<decltype([](const std::pair<int, bool>& p) {
                                    return p.first;
                                  }),
                                  std::pair<int, bool>, int>,
              "π1 must project Product -> LeftPart.");
static_assert(IsProductProjection<decltype([](const std::pair<int, bool>& p) {
                                    return p.second;
                                  }),
                                  std::pair<int, bool>, bool>,
              "π2 must project Product -> RightPart.");
static_assert(IsProjectedProduct<std::pair<int, bool>, int, bool>,
              "Identity projection must certify direct products.");

/**
 * @brief Mediating morphism for Products: ⟨f, g⟩: X -> (A × B)
 * @details Given f: X -> A and g: X -> B, constructs the unique morphism
 * that pairs their results.
 */
export template <IsArrow F, IsArrow G>
  requires std::same_as<Dom<F>, Dom<G>>  // Universal property: same source X
auto mediate_product(F&& f, G&& g) {
  using X = Dom<F>;
  using A = Cod<F>;
  using B = Cod<G>;

  return arrow([f = std::forward<F>(f), g = std::forward<G>(g)](const X& x) {
    return std::pair<A, B>(f(x), g(x));
  });
}

/**
 * @concept IsArrowFromProduct
 * @brief Matches an Arrow whose Domain is a categorical Product (std::pair).
 */
export template <typename F>
concept IsArrowFromProduct =
    IsArrow<F> && IsProduct<Dom<F>, typename Dom<F>::first_type,
                            typename Dom<F>::second_type>;

/**
 * @brief Left projection @c π_1 @c : @c A @c × @c B @c → @c A.
 * @details The canonical first projection out of a binary product, named per
 *          Pierce (@em Basic Category Theory for Computer Scientists §1.4).
 *          Sister of the coproduct injection @c ι_1 in @c :cartesian.
 */
export template <typename A, typename B>
constexpr auto π_1(const std::pair<A, B>& p) {
  return p.first;
}

/**
 * @brief Right projection @c π_2 @c : @c A @c × @c B @c → @c B.
 * @details The canonical second projection out of a binary product, named per
 *          Pierce.  Sister of the coproduct injection @c ι_2 in @c :cartesian.
 */
export template <typename A, typename B>
constexpr auto π_2(const std::pair<A, B>& p) {
  return p.second;
}

// Compiler-validated witnesses: π_1 / π_2 inhabit IsProductProjection.
static_assert(IsProductProjection<decltype([](const std::pair<int, bool>& p) {
                                    return π_1<int, bool>(p);
                                  }),
                                  std::pair<int, bool>, int>,
              "π_1 must inhabit IsProductProjection<Product → LeftPart>.");
static_assert(IsProductProjection<decltype([](const std::pair<int, bool>& p) {
                                    return π_2<int, bool>(p);
                                  }),
                                  std::pair<int, bool>, bool>,
              "π_2 must inhabit IsProductProjection<Product → RightPart>.");

/**
 * @concept IsCartesian
 * @brief A small category equipped with a terminal object @c 1 and binary
 *        products @c × --- a "cartesian category" in the textbook sense
 *        (Pierce, @em Basic Category Theory for Computer Scientists §1.4;
 *        Awodey).
 *
 * @details Concept-as-predicate framing: @c IsCartesian narrows the
 * type-class @c IsSmallCategory by intersecting with the finite-product
 * structure (terminal + binary products).  This is the @em finite-products
 * subset of finite limits --- finitely complete categories also have
 * equalisers and pullbacks, which are not pinned here; @c IsCartesian
 * deliberately stops at the products-and-terminal layer.
 *
 * Position in the size-and-structure lattice (read top-down as narrowing
 * intersections; cf. paper §2.3):
 *
 * @code
 *   IsSmallCategory<Cat>
 *     ∧ has terminal object @c Cat::Terminal
 *     ∧ has binary product @c Cat::template Product<A, B>
 *   = IsCartesian<Cat>
 * @endcode
 *
 * The CCC-completing exponentials live downstream in @c :cartesian; @c
 * IsCartesianClosed = @c IsCartesian + has-exponentials.  Re-using @c
 * IsCartesian as the prerequisite gates @c IsCartesianClosed cleanly
 * against the chain @c IsSmallCategory @c → @c IsCartesian @c → @c
 * IsCartesianClosed.
 */
export template <typename Cat>
concept IsCartesian = IsSmallCategory<Cat> && requires {
  // Terminal object 1 exists.
  typename Cat::Terminal;
  requires IsTerminalObject<typename Cat::Terminal>;
} && requires(typename Cat::Arrow::Domain A, typename Cat::Arrow::Codomain B) {
  // Binary product A × B exists for objects in the category.
  typename Cat::template Product<decltype(A), decltype(B)>;
  requires IsProduct<typename Cat::template Product<decltype(A), decltype(B)>,
                     decltype(A), decltype(B)>;
};

/** @section limit__Realizations */
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
    IsProjectedTerminalObject<
        detail::TerminalEnvelope,
        decltype([](const detail::TerminalEnvelope& envelope) constexpr
                     -> const One& { return arrow_drill_down(envelope); })>,
    "Opt-in operator-> drill-down must expose a Terminal object witness.");
static_assert(
    IsProjectedInitialObject<
        detail::InitialEnvelope,
        decltype([](const detail::InitialEnvelope& envelope) constexpr
                     -> const Zero& { return arrow_drill_down(envelope); })>,
    "Opt-in operator-> drill-down must expose an Initial object witness.");

}  // namespace dedekind::category
