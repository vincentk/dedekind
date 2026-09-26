/**
 * @file dedekind/python/python.cppm
 * @brief Curated binding facade for external runtimes.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section python__Description
 * This layer is intentionally positioned at the end of the build chain.
 * It provides a narrow, auditable facade that downstream wrappers
 * (e.g. Python bindings) can expose without importing the full internal
 * module graph directly.
 *
 * MVP exposure contract:
 * - Include explicit set interop boundaries (`from_std` / `to_std`).
 * - Include sequence/range adapters (`from_range` / `as_range`).
 * - Pull in downstream numeric/order/topology specializations transitively via
 *   `dedekind.numbers`, so wrappers need not manage fine-grained ownership.
 * - Keep the surface small and deterministic for notebook-facing demos.
 * - Defer broad symbolic expression builders and deep internal abstractions.
 *
 * @note "Le vrai n'est pas le tout, mais le tout dans sa structure."
 *       -- Gaston Bachelard, paraphrase
 *       [Trans: "Truth is not the whole, but the whole in its structure."]
 */

module;

#include <concepts>
#include <functional>
#include <ranges>
#include <utility>

export module dedekind.python;

import dedekind.category;
import dedekind.linear_algebra;
import dedekind.numbers;
import dedekind.sequences;
import dedekind.sets;

export namespace dedekind::python {

/** @brief Alias for the extensional carrier exposed to wrappers. */
template <typename T, typename L = dedekind::category::Boole,
          typename Hash = std::hash<T>, typename Equal = std::equal_to<T>>
using FiniteSet = dedekind::sets::ExtensionalSet<T, L, Hash, Equal>;

/** @brief Alias for finite path values intended for range-friendly adapters. */
template <typename T>
using FinitePath = dedekind::sequences::FinitePath<T>;

/** @brief Backend-kind alias surfaced for runtime wrapper routing. */
using LinearAlgebraBackendKind = dedekind::linear_algebra::BackendKind;

/** @brief GraphBLAS-stub backend alias for validation and prototyping hooks. */
using GraphBLASBackend = dedekind::linear_algebra::GraphBLASBackendStub;

/** @brief Explicit std-container materialization bridge. */
template <typename StdSetLike, typename T, typename L, typename Hash,
          typename Equal>
constexpr auto to_std(
    const dedekind::sets::ExtensionalSet<T, L, Hash, Equal>& src)
    -> StdSetLike {
  return dedekind::sets::to_std<StdSetLike>(src);
}

/** @brief Explicit bridge from supported std set-like carriers. */
template <typename StdSetLike>
constexpr auto from_std(const StdSetLike& src) {
  return dedekind::sets::from_std(src);
}

/** @brief Adapt an input range into a finite path materialization. */
template <typename R>
  requires std::ranges::input_range<R>
constexpr auto from_range(R&& range) {
  return dedekind::sequences::from_range(std::forward<R>(range));
}

/** @brief View-compatible finite-path adapter for std::ranges APIs. */
template <typename T>
constexpr const FinitePath<T>& as_range(const FinitePath<T>& path) {
  return dedekind::sequences::as_range(path);
}

/** @brief Rvalue-safe finite-path adapter for std::ranges APIs. */
template <typename T>
constexpr FinitePath<T> as_range(FinitePath<T>&& path) {
  return std::move(path);
}

/** @brief Expose GraphBLAS-stub capability for wrapper-level smoke tests. */
constexpr bool graphblas_backend_stub_available() {
  return GraphBLASBackend::supports_sparse_linear_operators;
}

// ── Jlt: a fluent DSL over the real category arrows (#961) ────────────────
//
// The Jlt exhibit exposes the ACTUAL @c :morphism arrows over TWO objects,
// @c bool and @c int, as @b duck-typed handles: compose with @c >>, apply with
// @c (), inspect @c dom / @c cod.  There is no bespoke term class; an "arrow"
// is the @b protocol (@c dom / @c cod / call / @c >>), witnessed by @c IsArrow
// on the C++ side.
//
// The generators are @b involutive @b endomorphisms: @c id and @c refl (the
// reflection @c ¬ on @c bool @c = @c logical_not; negation @c x↦-x on @c int).
// Under @c >> they generate @c ℤ/2 per object --- every element is self-inverse
// (@c IsInvolution).
//
// Python arrows are @b extensional (functions), so composition type-erases to a
// @c Morphism<T,T>.  Structural REDUCTION (@c simplify / @c cata) is
// @b intensional and stays in C++; it is @b vacuous here.  Composability
// (@c cod(f) @c = @c dom(g)) is enforced @b structurally --- a @c bool-arrow
// cannot compose with an @c int-arrow (their types do not match).

namespace jlt {

/** @brief The exhibit's type-erased arrow @c T→T: the real @c :morphism
 *  @c Morphism with a @c std::function transform.  This IS a category arrow
 *  (@c IsArrow / @c IsEndomorphism hold), not a bespoke wrapper. */
template <typename T>
using Arrow = dedekind::category::Morphism<T, T, std::function<T(T)>>;

/** @brief The identity arrow's type on @c T: the real @c :morphism
 *  @c Identity<T> (the monoid unit). */
template <typename T>
using Id = dedekind::category::Identity<T>;

/** @brief The identity arrow on @c T --- exactly @c :morphism's @c id<T>(). */
template <typename T>
inline Id<T> id() {
  return {};
}

/** @brief @c refl on @c bool: logical negation @c ¬ --- the reflection of the
 *  2-chain, the involution @c :involution witnesses
 *  (@c is_involutive<std::logical_not<bool>, @c bool>). */
inline Arrow<bool> refl_bool() {
  return Arrow<bool>{std::function<bool(bool)>{[](bool b) { return !b; }}};
}

/** @brief @c refl on @c int: negation @c x↦-x --- the reflection about @c 0,
 *  computed @b modularly (no signed-overflow UB even at @c INT_MIN).  An
 *  involution: @c refl∘refl @c = @c id. */
inline Arrow<int> refl_int() {
  return Arrow<int>{std::function<int(int)>{
      [](int x) { return static_cast<int>(0u - static_cast<unsigned>(x)); }}};
}

/** @brief Extensional composition @c f @c >> @c g (apply @c f, then @c g) of
 * two same-object endomorphisms, as a type-erased arrow --- diagrammatic order,
 *  matching @c :morphism's @c operator>>.  The @c same_as<Dom<F>,Dom<G>>
 *  constraint IS the composability law (@c cod(f) @c = @c dom(g) for
 *  endomorphisms): a cross-object compose does not type-check.  Structural
 *  reduction is NOT applied (that is intensional, C++/type-level). */
template <dedekind::category::IsEndomorphism F,
          dedekind::category::IsEndomorphism G>
  requires std::same_as<dedekind::category::Dom<F>, dedekind::category::Dom<G>>
inline Arrow<dedekind::category::Dom<F>> compose(const F& f, const G& g) {
  using T = dedekind::category::Dom<F>;
  return Arrow<T>{std::function<T(T)>{[f, g](T x) { return g(f(x)); }}};
}

}  // namespace jlt

}  // namespace dedekind::python
