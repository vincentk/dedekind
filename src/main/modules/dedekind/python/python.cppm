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

#include <functional>
#include <memory>
#include <ranges>
#include <string>
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

// ── Jlt: a runtime composition term + value-first reducer (#961) ──────────
//
// The type-level reducer (@c :f_algebra's @c cata over @c Compose<F,G> /
// @c Identity<T>, with the monoid unit law as its @c reduce_β β) dispatches
// on TYPES.  Python builds terms DYNAMICALLY (@c id @c >> @c id at runtime),
// so the term must erase those types into a runtime value.  @c ArrowTerm is
// that value-level MIRROR of the type-level term functor
// @c F(X) @c = @c Id @c + @c Atom @c + @c (X @c × @c X); @c ArrowTerm::reduce
// is the value-first @c cata.  Reduction stays on the C++ side (the package's
// handle-only contract); Python only holds @c ArrowTerm handles and calls in.
//
// Scope (#961, first slice): the monoid UNIT law only --- @c id∘f @c = @c f
// @c = @c f∘id.  Atoms are @b opaque (no inverse-cancellation / involution
// laws yet).  Reconciling this runtime mirror with the type-level @c cata ---
// one functor, two carriers --- is the embedding to be witnessed next.

/** @brief Node kind of the runtime composition term.  First iteration (#961):
 *  the objects are the booleans @c true @c | @c false and the primitive arrows
 *  are the two invertible unary boolean operations @c id and @c not (the
 *  automorphisms of @c bool, @f$\cong \mathbb{Z}/2@f$) --- a @b closed set, so
 *  the term functor is @c F(X) @c = @c Id @c + @c Not @c + @c (X @c × @c X)
 *  with no open atom alphabet. */
enum class ArrowKind { Id, Not, Compose };

/** @brief A runtime, value-level composition term over the unary boolean
 *  operations @c {id, not}: the value-level mirror of @c :morphism's
 *  @c Identity / @c Compose, reduced by the value-first @c cata (@c reduce).
 *  Composition is diagrammatic (@c f @c >> @c g means "apply @c f, then @c g",
 *  matching @c operator>>).  The arrows are C++-defined (no captured Python
 *  callable), so a handle carries no Python reference. */
class ArrowTerm {
 public:
  /** @brief The identity arrow (the monoid unit). */
  static ArrowTerm id() { return ArrowTerm{ArrowKind::Id, nullptr, nullptr}; }

  /** @brief Boolean negation @c not: @c b @c ↦ @c ¬b --- the canonical
   *  involution (the SAME one @c :involution witnesses via
   *  @c is_involutive<std::logical_not<bool>, @c bool>).  For now the reducer
   *  treats it as an opaque non-identity arrow; @c not∘not→id (the involution
   *  law) is the next slice (#961). */
  static ArrowTerm lnot() {
    return ArrowTerm{ArrowKind::Not, nullptr, nullptr};
  }

  /** @brief Diagrammatic composition @c f @c >> @c g (apply @c f, then @c g);
   *  builds an unreduced @c Compose node, exactly like @c :morphism. */
  friend ArrowTerm operator>>(const ArrowTerm& f, const ArrowTerm& g) {
    return ArrowTerm{ArrowKind::Compose, std::make_shared<ArrowTerm>(f),
                     std::make_shared<ArrowTerm>(g)};
  }

  /** @brief Apply the arrow to a boolean object: @c (g∘f)(x) for a composite.
   */
  bool operator()(bool x) const {
    switch (kind_) {
      case ArrowKind::Id:
        return x;
      case ArrowKind::Not:
        return !x;
      case ArrowKind::Compose:
        return (*right_)((*left_)(x));  // g(f(x))
    }
    return x;  // unreachable; all kinds handled
  }

  /** @brief The value-first @c cata: post-order fold applying the monoid unit
   *  β once per node.  Recurse into the legs, then drop an @c Id leg
   *  (@c id∘g @c = @c g, @c f∘id @c = @c f); a composite of two non-units is
   *  inert.  Structural, no tag --- the value-level twin of @c reduce_β. */
  ArrowTerm reduce() const {
    if (kind_ != ArrowKind::Compose) return *this;  // leaf: its own normal form
    const ArrowTerm l = left_->reduce();
    const ArrowTerm r = right_->reduce();
    if (l.kind_ == ArrowKind::Id) return r;  // id ∘ g = g
    if (r.kind_ == ArrowKind::Id) return l;  // f ∘ id = f
    return l >> r;                           // neither leg is the unit → inert
  }

  /** @brief Structural equality, so @c simplify(id @c >> @c id) @c == @c id is
   *  decidable: primitives by kind, composites leg-wise. */
  bool operator==(const ArrowTerm& other) const {
    if (kind_ != other.kind_) return false;
    switch (kind_) {
      case ArrowKind::Id:
      case ArrowKind::Not:
        return true;
      case ArrowKind::Compose:
        return *left_ == *other.left_ && *right_ == *other.right_;
    }
    return false;  // unreachable
  }

  /** @brief S-expression rendering: @c id, @c not, or @c (>> l r). */
  std::string sexpr() const {
    switch (kind_) {
      case ArrowKind::Id:
        return "id";
      case ArrowKind::Not:
        return "not";
      case ArrowKind::Compose:
        return "(>> " + left_->sexpr() + " " + right_->sexpr() + ")";
    }
    return "?";  // unreachable
  }

  ArrowKind kind() const { return kind_; }

 private:
  ArrowTerm(ArrowKind kind, std::shared_ptr<ArrowTerm> left,
            std::shared_ptr<ArrowTerm> right)
      : kind_(kind), left_(std::move(left)), right_(std::move(right)) {}

  ArrowKind kind_;
  std::shared_ptr<ArrowTerm> left_;   // Compose only
  std::shared_ptr<ArrowTerm> right_;  // Compose only
};

/** @brief The value-first reducer entry point exposed to wrappers:
 *  @c simplify @b is the runtime @c cata (the monoid unit β).  Python calls
 *  this; the reduction runs here, in C++. */
inline ArrowTerm simplify(const ArrowTerm& term) { return term.reduce(); }

}  // namespace dedekind::python
