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
#include <optional>  // the cover, the first element of a set
#include <ranges>
#include <utility>

export module dedekind.python;

import dedekind.category;
import dedekind.linear_algebra;
import dedekind.numbers;
import dedekind.order; // Lwv halfspaces: Halfspace / structured_and (#965)
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
// The generators are @b involutive @b endomorphisms: @c id and @c refl.  @c
// refl is @c :logic's @b pre-configured reflection @c logic_complement<L> (@c
// L::RFL, already witnessed an involution there): @c ¬ on @c bool (@c Boole)
// and the order-reversing @c ~ on @c int (@c Chain<int>).  Under @c >> they
// generate
// @c ℤ/2 per object --- every element is self-inverse (@c IsInvolution).
//
// Python arrows are @b extensional (functions), so a composition type-erases to
// a @c Morphism<T,T>.  But it is NOT hand-rolled: @c compose builds @c
// :morphism's
// @c Compose (@c operator>>) and runs it through @c :f_algebra's @c cata ---
// the ONE reducer engine --- then type-erases the reduced arrow.  So a Python
// composition @b dispatches to the F-algebra reducer (@c id>>refl folds to
// @c refl); the result is extensional, but structurally reduced.  Composability
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

/** @brief @c refl on @c bool: the reflection @c ¬ of the 2-chain --- reusing
 *  @c :logic's pre-configured @c logic_complement<Boole> (@c = @c Boole::RFL
 *  @c = @c !a), already witnessed as an involution there
 *  (@c is_involutive<logic_complement<Boole>, @c bool>).  No hand-rolled map.
 */
inline Arrow<bool> refl_bool() {
  return Arrow<bool>{std::function<bool(bool)>{
      dedekind::category::logic_complement<dedekind::category::Boole>{}}};
}

/** @brief The integer carrier the Python surface shares between the arrows
 *  (@c jlt), the sets (@c lwv) and the chains (@c pst): @c long @c long, what a
 *  Python @c int crosses the boundary as.  One carrier, so an arrow's image of
 *  a set is well-typed. */
using Int = long long;

/** @brief @c refl on the integer chain: @c :logic's
 *  @c logic_complement<Chain<Int>> (@c = @c Chain<Int>::RFL @c = @c ~a, the
 *  order-reversing De Morgan involution), already witnessed as an involution
 *  there. */
inline Arrow<Int> refl_int() {
  return Arrow<Int>{std::function<Int(Int)>{
      dedekind::category::logic_complement<dedekind::category::Chain<Int>>{}}};
}

/** @brief The step arrows on a carrier, the real @c :nno @c Successor /
 *  @c Predecessor, kept @b typed (not erased) so the set side can read them
 *  structurally (@c lwv::image).  @tparam T the carrier, with the NNO step. */
template <dedekind::category::HasNNOStep T>
using Succ = dedekind::category::Successor<T>;
template <dedekind::category::HasNNOStep T>
using Pred = dedekind::category::Predecessor<T>;

/** @brief Composition @c f @c >> @c g (apply @c f, then @c g) of two
 * same-object endomorphisms, as a type-erased arrow.  It REUSES the real
 * machinery --- no hand-rolled @c g(f(x)): it builds @c :morphism's @c Compose
 * via @c operator>> and runs it through @c :f_algebra's @c cata (the ONE
 * reducer engine), then type-erases the reduced arrow.  So a Python composition
 * @b dispatches to the F-algebra reducer; the result is extensional (a
 * function), but structurally reduced (e.g. @c id>>refl folds to @c refl via @c
 * cata's unit law).  The
 *  @c same_as<Dom<F>,Dom<G>> constraint IS the composability law (@c cod(f) @c
 * =
 *  @c dom(g) for endomorphisms): a cross-object compose does not type-check. */
template <dedekind::category::IsEndomorphism F,
          dedekind::category::IsEndomorphism G>
  requires std::same_as<dedekind::category::Dom<F>, dedekind::category::Dom<G>>
inline Arrow<dedekind::category::Dom<F>> compose(const F& f, const G& g) {
  using T = dedekind::category::Dom<F>;
  return Arrow<T>{std::function<T(T)>{dedekind::category::cata(f >> g)}};
}

}  // namespace jlt

// ── Lwv: the scalar set-comprehension language (#965, paper §3)
// ───────────────
//
// Lwv := Jlt ∩ Set (paper.tex:158): intensional sets over a Jlt carrier, a set
// being a membership test @c {x ∈ S | P(x)} rather than a listing.  The
// canonical README exhibit is the two overlapping halfspaces
// @c {x>3} ∩ {x<5} = {4}: a halfspace @c ℕ|(χ ⋈ fix(k)) is the principal
// filter/ideal @c ↑k / @c ↓k of the carrier order, and its meet routes through
// the @b real @b :order reducer @c structured_and (the value-first crossing law
// @c ↑a ∩ ↓b = [a,b], collapsing to a @c Singleton over an integral carrier).
// This exhibit is the SET twin of the @c jlt ARROW exhibit above: it binds the
// real reducer, NOT a Python-side reimplementation (handle-only).
//
// Carrier is @c int here (the collapse is carrier-agnostic over any integral
// carrier; the README's @c ℕ is the Cardinality variant, whose Python
// value-conversion is deferred).  The pivots are compile-time NTTPs, so this
// first iteration binds the curated README sets --- exactly as @c jlt binds the
// fixed generators @c id / @c refl over @c bool / @c int.  A general fluent
// constructor over @b runtime pivots is the next iteration, gated on the
// value-first subobject reducer's leaf-combine leg (#922 slice 2) --- until
// then a runtime pivot cannot index a compile-time halfspace type.

namespace lwv {

namespace ord = dedekind::order;

/** @brief A value-based set handle (iteration 2, #965): the pivot rides as a
 *  VALUE, so a single constructor covers all pivots (runtime), unifying the
 *  earlier one-constant-per-pivot stopgap.  Carrier @c long @c long (Python
 *  @c int).  @c ord::SetVal is a @c :order value-first set; @c meet routes
 *  through the @b same @c constexpr @c reduce_meet the compile-time
 *  @c static_assert exhibit folds, so there is no Python-side reducer and the
 *  law lives in ONE place (value-oriented relational form: a halfspace is a
 *  point plus a direction, the pivot in the value @c η(p)). */
using Set = ord::SetVal<long long>;
// Whatever crosses the Python boundary is a set OBJECT --- (reified universe,
// χ) --- and this is the compile-time MUST for it (RFC 2119): a type that does
// not conform cannot be the exported Set.
static_assert(dedekind::sets::IsSetObject<Set>,
              "the Python surface's Set must be a set object (IsSetObject).");

/** @brief @c {x | x > k} = ↑k (open).  Scalar spelling @c χ > fix(k). */
constexpr Set above(long long k) {
  return Set::half(k, ord::Direction::Upward, ord::Strictness::Strict);
}
/** @brief @c {x | x >= k} = ↑k (closed). */
constexpr Set at_least(long long k) {
  return Set::half(k, ord::Direction::Upward, ord::Strictness::NonStrict);
}
/** @brief @c {x | x < k} = ↓k (open). */
constexpr Set below(long long k) {
  return Set::half(k, ord::Direction::Downward, ord::Strictness::Strict);
}
/** @brief @c {x | x <= k} = ↓k (closed). */
constexpr Set at_most(long long k) {
  return Set::half(k, ord::Direction::Downward, ord::Strictness::NonStrict);
}
/** @brief @c {k}: the singleton / point @c η(k) --- the value-based atom. */
constexpr Set singleton(long long k) { return Set::point(k); }
/** @brief @c 𝔸: the universe (meet unit). */
constexpr Set everything() { return Set::universe(); }
/** @brief @c Ø: the empty set (meet annihilator). */
constexpr Set nothing() { return Set::empty(); }

/** @brief The meet @c a @c ∩ @c b, through the value-first @c :order
 *  @c reduce_meet (the ONE law; @c static_assert folds it at compile time, the
 *  Python surface runs it at runtime).  @c above(3) @c & @c below(5) collapses
 *  to @c singleton(4). */
constexpr Set meet(const Set& a, const Set& b) {
  return ord::reduce_meet(a, b);
}

/** @brief Translate every bound by @c k: the image of a value leaf under the
 *  order automorphism @f$x \mapsto x + k@f$ of the chain keeps its kind and
 *  moves its bounds; @c Ø and @c 𝔸 are fixed.  @f$O(1)@f$. */
constexpr Set shift(const Set& s, long long k) {
  Set r = s;
  switch (s.kind) {
    case ord::SetKind::Singleton:
    case ord::SetKind::Interval:
      r.lo += k;
      r.hi += k;
      break;
    case ord::SetKind::Halfspace:
      r.lo += k;
      break;
    default:
      break;
  }
  return r;
}
/** @brief @f$f(S)@f$ and @f$f^{-1}(S)@f$ for the structural arrows, in closed
 *  form: the successor shifts by one, the predecessor by minus one, the
 *  identity not at all (paper §4: the image of @c {n > 5} under the successor
 *  is @c {n > 6}, decided with no search of the domain).  An opaque composed
 *  arrow has no such normal form; its image is intensional (Kleene-valued)
 *  and is refused at the boundary rather than guessed. */
constexpr Set image(const jlt::Id<jlt::Int>&, const Set& s) { return s; }
constexpr Set preimage(const jlt::Id<jlt::Int>&, const Set& s) { return s; }
constexpr Set image(const jlt::Succ<jlt::Int>&, const Set& s) {
  return shift(s, 1);
}
constexpr Set preimage(const jlt::Succ<jlt::Int>&, const Set& s) {
  return shift(s, -1);
}
constexpr Set image(const jlt::Pred<jlt::Int>&, const Set& s) {
  return shift(s, -1);
}
constexpr Set preimage(const jlt::Pred<jlt::Int>&, const Set& s) {
  return shift(s, 1);
}

/** @brief Slicing by @b value: @f$S \cap [a, b)@f$, the meet with the
 *  half-open interval (Python's own convention), an absent bound meaning the
 *  ray.  @c s[:b] is the restriction to the lower cut at @c b.  One
 *  @c reduce_meet, @f$O(1)@f$.  Not positional: a set has no enumeration to
 *  index into; that reading belongs to a sequence. */
constexpr Set restrict(const Set& s, std::optional<long long> lo,
                       std::optional<long long> hi) {
  Set r = s;
  if (lo) r = meet(r, at_least(*lo));
  if (hi) r = meet(r, below(*hi));
  return r;
}

/** @brief Whether the set is bounded (on the discrete chain ℤ, equivalently
 *  finite): the point, the interval, the empty set; not a ray or @c 𝔸. */
constexpr bool is_bounded(const Set& s) {
  return s.kind == ord::SetKind::Empty || s.kind == ord::SetKind::Singleton ||
         s.kind == ord::SetKind::Interval;
}
/** @brief The least element, the unfold's seed: none for @c Ø, and none for a
 *  set unbounded below (@c ↓k, @c 𝔸) --- ℤ has no bottom. */
constexpr std::optional<long long> least(const Set& s) {
  switch (s.kind) {
    case ord::SetKind::Singleton:
      return s.lo;
    case ord::SetKind::Interval:
      return s.sl == ord::Strictness::Strict ? s.lo + 1 : s.lo;
    case ord::SetKind::Halfspace:
      if (s.dir == ord::Direction::Upward)
        return s.sl == ord::Strictness::Strict ? s.lo + 1 : s.lo;
      return std::nullopt;
    default:
      return std::nullopt;
  }
}
/** @brief One past the greatest element, where the set is bounded above;
 *  @c nullopt on a ray: the unfold does not stop. */
constexpr std::optional<long long> past_end(const Set& s) {
  switch (s.kind) {
    case ord::SetKind::Singleton:
      return s.lo + 1;
    case ord::SetKind::Interval:
      return s.su == ord::Strictness::Strict ? s.hi : s.hi + 1;
    default:
      return std::nullopt;
  }
}

}  // namespace lwv

// ── Pst: the bounded chains (#1001, paper §3) ────────────────────────────────
//
// Pst := Jlt ∩ Chain: the truth objects the sets are valued in (𝔹, K₃), and
// ℕ's proxy, a bounded chain in the same shape whose ⊤ is ℵ₀ (the memory
// boundary), not a truth object.  A chain is its endpoints and its step; the
// step is read twice --- total and saturating (the algebra side, @c Successor)
// and partial (the coalgebra side, @c cover, nothing at ⊤) --- and the chain's
// classification is whatever the C++ concepts decide.

namespace pst {

/** @brief A Pst chain by carrier: its name, endpoints and size.
 *  @tparam C the carrier (@c bool, @c Ternary, @c Cardinality). */
template <typename C>
struct Chain;
template <>
struct Chain<bool> {
  static constexpr const char* name = "𝔹";
  static constexpr bool bottom = false;
  static constexpr bool top = true;
  static constexpr dedekind::sets::Cardinality cardinality =
      dedekind::sets::finite_cardinality(2);
};
template <>
struct Chain<dedekind::category::Ternary> {
  static constexpr const char* name = "K₃";
  static constexpr dedekind::category::Ternary bottom =
      dedekind::category::Ternary::False;
  static constexpr dedekind::category::Ternary top =
      dedekind::category::Ternary::True;
  static constexpr dedekind::sets::Cardinality cardinality =
      dedekind::sets::finite_cardinality(3);
};
template <>
struct Chain<dedekind::sets::Cardinality> {
  static constexpr const char* name = "ℕ";
  static constexpr dedekind::sets::Cardinality bottom =
      dedekind::sets::finite_cardinality(0);
  static constexpr dedekind::sets::Cardinality top =
      dedekind::sets::Cardinality{dedekind::sets::ℵ_0{}};
  static constexpr dedekind::sets::Cardinality cardinality = top;
};

/** @brief The step, both readings, and the classification, on a chain's
 *  carrier.  @tparam C the carrier. */
template <dedekind::category::HasNNOStep C>
constexpr C succ(const C& x) {
  return dedekind::category::Successor<C>{}(x);
}
template <dedekind::category::HasNNOStep C>
constexpr C pred(const C& x) {
  return dedekind::category::Predecessor<C>{}(x);
}
template <dedekind::category::HasNNOStep C>
constexpr std::optional<C> cover(const C& x) {
  return dedekind::category::cover(x);
}
template <dedekind::category::HasNNOStep C>
constexpr bool saturates() {
  return succ(Chain<C>::top) == Chain<C>::top;
}
/** @brief Indexing by @b position: the element at position @c i from ⊥, the
 *  chain read as its own enumeration (the orbit of ⊥ under the step), so
 *  @c K3[1] is @c UNKNOWN and @c N[i] is @c i.  @f$O(1)@f$ on ℕ (Peano
 *  addition), a walk bounded by the chain's length on 𝔹 and K₃. */
template <dedekind::category::HasNNOStep C>
C at(std::size_t i) {
  return dedekind::sequences::SuccessorOrbit<C>{Chain<C>::bottom}.at(i);
}
template <typename C>
constexpr bool is_truth_object = dedekind::category::IsPst<C>;
// FIXME(#1004): IsDiscrete is not exported yet; discreteness is read as
// !is_dense.
template <typename C>
constexpr bool is_dense = dedekind::order::IsDense<C>;

static_assert(is_truth_object<bool> &&
                  is_truth_object<dedekind::category::Ternary> &&
                  !is_truth_object<dedekind::sets::Cardinality>,
              "𝔹 and K₃ are truth objects; ℕ is a chain of the same shape.");
static_assert(saturates<bool>() && saturates<dedekind::category::Ternary>() &&
                  saturates<dedekind::sets::Cardinality>(),
              "the three chains saturate at ⊤.");

}  // namespace pst

}  // namespace dedekind::python
