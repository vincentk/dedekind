/**
 * @file dedekind/python/python.cppm
 * @brief Curated binding facade for external runtimes.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section python__Description
 * The last layer of the build chain: what the Python bindings hold, and nothing
 * the library does not already say.  Three namespaces follow the paper's
 * fragments, @c jlt (arrows), @c lwv (sets over the integer window) and
 * @c pst (the bounded chains and the sets over them).  Each adds only what
 * crossing the boundary needs: the type erasure to one handle type per
 * fragment, and the 64-bit window's ends.  Reduction stays in C++.
 *
 * @note "Le vrai n'est pas le tout, mais le tout dans sa structure."
 *       -- Gaston Bachelard, paraphrase
 *       [Trans: "Truth is not the whole, but the whole in its structure."]
 */

module;

#include <concepts>
#include <functional>
#include <limits>     // the integer window's ends
#include <optional>   // the cover, the first element of a set
#include <stdexcept>  // std::overflow_error at the window's end
#include <string>
#include <utility>
#include <vector>  // the runs, materialised for the Python handle

export module dedekind.python;

import dedekind.category;
import dedekind.numbers; // the carriers' order registrations
import dedekind.order;
import dedekind.sequences;
import dedekind.sets;

export namespace dedekind::python {

// The library's names, said once for the partition; the fragments below add
// only the handles' erasure and the window.
using dedekind::category::Boole;
using dedekind::category::cata;
using dedekind::category::classifier_logic_t;
using dedekind::category::cover;
using dedekind::category::Dom;
using dedekind::category::HasCoveringStep;
using dedekind::category::HasNNOStep;
using dedekind::category::HaveLogicJoin;
using dedekind::category::Identity;
using dedekind::category::IsEndomorphism;
using dedekind::category::IsFiniteChain;
using dedekind::category::IsOckhamAlgebra;
using dedekind::category::IsPredicate;
using dedekind::category::IsPst;
using dedekind::category::join_logic_t;
using dedekind::category::Kleene;
using dedekind::category::lift_logic;
using dedekind::category::logic_complement;
using dedekind::category::Morphism;
using dedekind::category::Predecessor;
using dedekind::category::preimage;
using dedekind::category::Successor;
using dedekind::category::Ternary;
using dedekind::order::Direction;
using dedekind::order::reduce_meet;
using dedekind::order::SetKind;
using dedekind::order::SetVal;
using dedekind::order::Strictness;
using dedekind::sequences::Run;
using dedekind::sequences::runs;
using dedekind::sequences::SuccessorOrbit;
using dedekind::sets::Cardinality;
using dedekind::sets::chain_bottom;
using dedekind::sets::chain_top;
using dedekind::sets::Comprehension;
using dedekind::sets::finite_cardinality;
using dedekind::sets::IsSetObject;
using dedekind::sets::Ø;
using dedekind::sets::η;
using dedekind::sets::π;
using dedekind::sets::ℵ_0;
using dedekind::sets::𝔸;

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
 *  (@c IsArrow / @c IsEndomorphism hold), not a bespoke wrapper.
 *  @tparam T the carrier. */
template <typename T>
using Arrow = Morphism<T, T, std::function<T(T)>>;

/** @brief @c refl on a chain: @c :logic's pre-configured reflection
 *  @c logic_complement<L> (@c L::RFL: @c ¬ on @c Boole, the order-reversing
 *  @c ~ on @c Chain<Int>), already witnessed an involution there, erased.
 *  @tparam L the species whose @c Ω is the carrier. */
template <IsOckhamAlgebra L>
Arrow<typename L::Ω> refl() {
  using T = typename L::Ω;
  return Arrow<T>{std::function<T(T)>{logic_complement<L>{}}};
}

/** @brief The integer carrier the Python surface shares between the arrows
 *  (@c jlt), the sets (@c lwv) and the chains (@c pst): @c long @c long, what a
 *  Python @c int crosses the boundary as.  One carrier, so an arrow's image of
 *  a set is well-typed. */
using Int = long long;

/** @brief The window's end.  Python's @c int is ℤ; @c long @c long is its
 *  64-bit window, on which the step is not closed.  Where ℤ continues and the
 *  window cannot, the surface raises (@c std::overflow_error, Python's
 *  @c OverflowError) rather than wrap --- ℕ's proxy saturates to ℵ₀ instead,
 *  the total posture (@c pst).  FIXME(#1008): an erased composite
 *  (@c succ @c >> @c succ) steps unchecked; the carrier that closes this is
 *  @c SignedCardinality.
 *  @param what the operation that reached the end. */
[[noreturn]] inline void window_end(const char* what) {
  throw std::overflow_error(std::string(what) +
                            ": the 64-bit window ends here; ℤ does not");
}
/** @brief @c a @c + @c b inside the window, or @c window_end.
 *  @param a a value in the window.  @param b the offset.
 *  @param what the operation, for the message.  @return the sum. */
constexpr Int add_in_window(Int a, Int b, const char* what) {
  Int sum{};
  if (__builtin_add_overflow(a, b, &sum)) window_end(what);
  return sum;
}
/** @brief A step arrow applied inside the window: total on @c bool, and on
 *  the integer window raising at the end it would cross rather than
 *  overflowing.  @tparam T the carrier.  @tparam Step the successor or the
 *  predecessor on it.  @param f the arrow.  @param x the argument.
 *  @return @c f(x). */
template <HasNNOStep T, typename Step>
  requires std::same_as<Step, Successor<T>> ||
           std::same_as<Step, Predecessor<T>>
T step_in_window(const Step& f, T x) {
  if constexpr (std::same_as<T, Int>) {
    constexpr bool up = std::same_as<Step, Successor<T>>;
    if (x == (up ? std::numeric_limits<Int>::max()
                 : std::numeric_limits<Int>::min()))
      window_end(up ? "succ" : "pred");
  }
  return f(x);
}

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
template <IsEndomorphism F, IsEndomorphism G>
  requires std::same_as<Dom<F>, Dom<G>>
inline Arrow<Dom<F>> compose(const F& f, const G& g) {
  using T = Dom<F>;
  return Arrow<T>{std::function<T(T)>{cata(f >> g)}};
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

/** @brief A value-based set handle (#965): the pivot rides as a VALUE, so one
 *  constructor covers all pivots at runtime.  Carrier @c long @c long (Python
 *  @c int).  @c SetVal is a @c :order value-first set; its meet is the same
 *  @c constexpr @c reduce_meet the compile-time exhibits fold, so there is no
 *  Python-side reducer. */
using Set = SetVal<long long>;
// Whatever crosses the Python boundary is a set OBJECT --- (reified universe,
// χ) --- and this is the compile-time MUST for it (RFC 2119).
static_assert(IsSetObject<Set>,
              "the Python surface's Set must be a set object (IsSetObject).");

/** @brief The rays @c {x ⋈ k}: the principal filter ↑k and ideal ↓k, open or
 *  closed, as the value leaf.  @tparam D the direction.  @tparam S the
 *  strictness.  @param k the pivot. */
template <Direction D, Strictness S>
constexpr Set ray(long long k) {
  return Set::half(k, D, S);
}

/** @brief Translate every bound by @c k: the image of a value leaf under the
 *  order automorphism @f$x \mapsto x + k@f$ of the chain keeps its kind and
 *  moves its bounds; @c Ø and @c 𝔸 are fixed.  @f$O(1)@f$. */
constexpr Set shift(const Set& s, long long k) {
  Set r = s;
  switch (s.kind) {
    case SetKind::Singleton:
    case SetKind::Interval:
      r.lo = jlt::add_in_window(r.lo, k, "image");
      r.hi = jlt::add_in_window(r.hi, k, "image");
      break;
    case SetKind::Halfspace:
      r.lo = jlt::add_in_window(r.lo, k, "image");
      break;
    default:
      break;
  }
  return r;
}
/** @brief The arrows whose image of a value leaf has a closed form: the
 *  identity, the successor and the predecessor on the integer window.  An
 *  opaque composed arrow has no such normal form; its image is intensional
 *  (Kleene-valued) and is refused at the boundary rather than guessed.
 *  @tparam F the arrow. */
template <typename F>
concept IsStructuralArrow = std::same_as<F, Identity<jlt::Int>> ||
                            std::same_as<F, Successor<jlt::Int>> ||
                            std::same_as<F, Predecessor<jlt::Int>>;
/** @brief How far the arrow moves a bound: 0, +1, −1.  @tparam F the arrow. */
template <IsStructuralArrow F>
consteval long long offset() {
  if constexpr (std::same_as<F, Successor<jlt::Int>>)
    return 1;
  else if constexpr (std::same_as<F, Predecessor<jlt::Int>>)
    return -1;
  else
    return 0;
}
/** @brief @f$f(S)@f$ and @f$f^{-1}(S)@f$ for a structural arrow, in closed
 *  form (paper §4: the image of @c {n > 5} under the successor is @c {n > 6},
 *  decided with no search of the domain).  @tparam F the arrow. */
template <IsStructuralArrow F>
constexpr Set image(const F&, const Set& s) {
  return shift(s, offset<F>());
}
template <IsStructuralArrow F>
constexpr Set preimage(const F&, const Set& s) {
  return shift(s, -offset<F>());
}

/** @brief Slicing by @b value: @f$S \cap [a, b)@f$, the meet with the
 *  half-open interval (Python's own convention), an absent bound meaning the
 *  ray.  One @c reduce_meet, @f$O(1)@f$.  Not positional: a set has no
 *  enumeration to index into; that reading belongs to a sequence. */
constexpr Set restrict(const Set& s, std::optional<long long> lo,
                       std::optional<long long> hi) {
  Set r = s;
  if (lo)
    r = reduce_meet(r, ray<Direction::Upward, Strictness::NonStrict>(*lo));
  if (hi) r = reduce_meet(r, ray<Direction::Downward, Strictness::Strict>(*hi));
  return r;
}

/** @brief Whether the set is bounded (on the discrete chain ℤ, equivalently
 *  finite): the point, the interval, the empty set; not a ray or @c 𝔸. */
constexpr bool is_bounded(const Set& s) {
  return s.kind == SetKind::Empty || s.kind == SetKind::Singleton ||
         s.kind == SetKind::Interval;
}
/** @brief The lower bound as attained: a strict one at its successor.
 *  @param s a leaf bounded below. */
constexpr long long attained_lo(const Set& s) {
  return s.sl == Strictness::Strict ? jlt::add_in_window(s.lo, 1, "least")
                                    : s.lo;
}
/** @brief The least element, the unfold's seed: none for @c Ø, and none for a
 *  set unbounded below (@c ↓k, @c 𝔸) --- ℤ has no bottom. */
constexpr std::optional<long long> least(const Set& s) {
  switch (s.kind) {
    case SetKind::Singleton:
      return s.lo;
    case SetKind::Interval:
      return attained_lo(s);
    case SetKind::Halfspace:
      if (s.dir == Direction::Upward) return attained_lo(s);
      return std::nullopt;
    default:
      return std::nullopt;
  }
}
/** @brief The greatest element, where the set is bounded above (a strict upper
 *  bound is attained at its predecessor, which exists since the set is
 *  inhabited); @c nullopt on a ray: the unfold does not stop. */
constexpr std::optional<long long> greatest(const Set& s) {
  switch (s.kind) {
    case SetKind::Singleton:
      return s.lo;
    case SetKind::Interval:
      return s.su == Strictness::Strict ? s.hi - 1 : s.hi;
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

/** @brief A Pst chain by carrier: its name, endpoints and size.  The truth
 *  chains take their endpoints from their species (@c sets:boundaries); ℕ's
 *  proxy has ℵ₀ for its top.  @tparam C the carrier. */
template <typename C>
struct Chain;
template <>
struct Chain<bool> {
  static constexpr const char* name = "𝔹";
  static constexpr bool bottom = chain_bottom<bool>();
  static constexpr bool top = chain_top<bool>();
  static constexpr Cardinality cardinality = finite_cardinality(2);
};
template <>
struct Chain<Ternary> {
  static constexpr const char* name = "K₃";
  static constexpr Ternary bottom = chain_bottom<Ternary>();
  static constexpr Ternary top = chain_top<Ternary>();
  static constexpr Cardinality cardinality = finite_cardinality(3);
};
template <>
struct Chain<Cardinality> {
  static constexpr const char* name = "ℕ";
  static constexpr Cardinality bottom = finite_cardinality(0);
  static constexpr Cardinality top = Cardinality{ℵ_0{}};
  static constexpr Cardinality cardinality = top;
};

/** @brief Whether @f$S(\top) = \top@f$: the step's posture at the top.
 *  @tparam C the carrier. */
template <HasNNOStep C>
constexpr bool saturates() {
  return Successor<C>{}(Chain<C>::top) == Chain<C>::top;
}
/** @brief Indexing by @b position: the element at position @c i from ⊥, the
 *  chain read as its own enumeration (the orbit of ⊥ under the step), so
 *  @c K3[1] is @c UNKNOWN and @c N[i] is @c i.  @f$O(1)@f$ on ℕ (Peano
 *  addition), a walk bounded by the chain's length on 𝔹 and K₃.
 *  @tparam C the carrier.  @param i the position. */
template <HasNNOStep C>
C at(std::size_t i) {
  return SuccessorOrbit<C>{Chain<C>::bottom}.at(i);
}

static_assert(IsPst<bool> && IsPst<Ternary> && !IsPst<Cardinality>,
              "𝔹 and K₃ are truth objects; ℕ is a chain of the same shape.");
static_assert(saturates<bool>() && saturates<Ternary>() &&
                  saturates<Cardinality>(),
              "the three chains saturate at ⊤.");

// ── Sets over a truth chain, per the paper's Lwv grammar (#975, PR two) ─────
// The sets' quantifiers join this namespace's own overload set (the handle
// versions below forward to them).
using dedekind::sets::exists;
using dedekind::sets::forall;

//
// A Python set is a REAL library set: a Comprehension over 𝔸<C, L> whose datum
// is the type-erased classifier Chi<C, L>.  Every operator (& | ^ ~ and the
// former |) runs the library's own node and erases the result back into this
// one type, as jlt::compose does for arrows; every query (==, ⊆, ∃, ∀, runs) is
// the library's, decided by exhausting the chain (sets:boundaries,
// sets:quantifier, sequences:pst).  Nothing is reduced in Python.

/** @brief A truth chain the queries can exhaust: a Pst carrier that is finite
 *  by its representation and has the covering step --- what the exhaustion
 *  equality (@c sets:boundaries) and the runs (@c sequences:pst) require, said
 *  once here so no handle enters a body-level failure.  @tparam C the chain. */
template <typename C>
concept IsTruthChain = IsPst<C> && IsFiniteChain<C> && HasCoveringStep<C>;

/** @brief The classifier a Python handle holds: type-erased, tagged with its
 *  species, an @c IsPredicate over the chain.  @tparam C the chain.
 *  @tparam L the species the set is valued in. */
template <IsTruthChain C, IsOckhamAlgebra L>
struct Chi {
  using Domain = C;
  using Codomain = typename L::Ω;
  using logic_species = L;
  std::function<Codomain(const C&)> f;
  Codomain operator()(const C& x) const { return f(x); }
};
/** @brief A set over the chain @c C valued in @c L, as Python holds it. */
template <IsTruthChain C, IsOckhamAlgebra L>
using Set = Comprehension<𝔸<C, L>, Chi<C, L>>;
/** @brief A datum of the grammar (@c π @c > @c v, @c π @c == @c v) over @c C:
 *  Boolean, carrier-bound, waiting for a former. */
template <IsTruthChain C>
using Datum = Chi<C, Boole>;

/** @brief A predicate's answer lifted into @c L along the dominance, as a named
 *  callable (what the erased classifier stores).  @tparam L the species.
 *  @tparam P the predicate. */
template <IsOckhamAlgebra L, IsPredicate P>
struct Lifted {
  P p;
  typename L::Ω operator()(const Dom<P>& x) const {
    return lift_logic<L>(p(x));
  }
};
/** @brief Erase any predicate over @c C into the handle's type, lifting a
 *  Boolean answer where @c L is wider.  @tparam C the chain.  @tparam L the
 *  species.  @tparam P the predicate (a datum, a node, a boundary). */
template <IsTruthChain C, IsOckhamAlgebra L, IsPredicate P>
  requires std::same_as<Dom<P>, C>
Set<C, L> erase(P p) {
  return Set<C, L>{Chi<C, L>{
      std::function<typename L::Ω(const C&)>{Lifted<L, P>{std::move(p)}}}};
}
template <IsTruthChain C, IsPredicate P>
  requires std::same_as<Dom<P>, C>
Datum<C> datum(P p) {
  return erase<C, Boole>(std::move(p)).predicate;
}

/** @brief The generators of the grammar: @c 𝔸, @c Ø, @c η(v); and the atoms
 *  @c π @c ⋈ @c v as data.  @tparam C the chain.  @tparam L the species. */
template <IsTruthChain C, IsOckhamAlgebra L>
Set<C, L> universe() {
  return erase<C, L>(𝔸<C, L>{});
}
template <IsTruthChain C, IsOckhamAlgebra L>
Set<C, L> empty() {
  return erase<C, L>(Ø<C, L>{});
}
template <IsTruthChain C, IsOckhamAlgebra L>
Set<C, L> point(const C& v) {
  return erase<C, L>(η(v));
}
template <IsTruthChain C>
Datum<C> above(const C& v) {
  return datum<C>(π > v);
}
template <IsTruthChain C>
Datum<C> at_least(const C& v) {
  return datum<C>(π >= v);
}
template <IsTruthChain C>
Datum<C> below(const C& v) {
  return datum<C>(π < v);
}
template <IsTruthChain C>
Datum<C> at_most(const C& v) {
  return datum<C>(π <= v);
}
template <IsTruthChain C>
Datum<C> equal_to(const C& v) {
  return datum<C>(π == v);
}
/** @brief χ(x) = x: the identity classifier on a truth chain, valued in the
 *  chain's own species --- the simplest set with every level inhabited. */
template <IsTruthChain C>
Set<C, classifier_logic_t<C>> identity() {
  return erase<C, classifier_logic_t<C>>(Identity<C>{});
}

/** @brief The former @c S @c | @c P, and the lattice operations, each the
 *  library's node erased back.  @tparam C the chain.  @tparam L the species. */
template <IsTruthChain C, IsOckhamAlgebra L>
Set<C, L> former(const Set<C, L>& s, const Datum<C>& d) {
  return erase<C, L>(s & (𝔸<C, L>{} | d));
}
// Two species mix when comparable in the species semilattice (HaveLogicJoin,
// 𝔹 at the bottom); the result is valued in their join.
template <IsTruthChain C, IsOckhamAlgebra L1, IsOckhamAlgebra L2>
  requires HaveLogicJoin<L1, L2>
Set<C, join_logic_t<L1, L2>> meet(const Set<C, L1>& a, const Set<C, L2>& b) {
  return erase<C, join_logic_t<L1, L2>>(a & b);
}
template <IsTruthChain C, IsOckhamAlgebra L1, IsOckhamAlgebra L2>
  requires HaveLogicJoin<L1, L2>
Set<C, join_logic_t<L1, L2>> join(const Set<C, L1>& a, const Set<C, L2>& b) {
  return erase<C, join_logic_t<L1, L2>>(a | b);
}
template <IsTruthChain C, IsOckhamAlgebra L1, IsOckhamAlgebra L2>
  requires HaveLogicJoin<L1, L2>
Set<C, join_logic_t<L1, L2>> sym_diff(const Set<C, L1>& a,
                                      const Set<C, L2>& b) {
  return erase<C, join_logic_t<L1, L2>>(a ^ b);
}
template <IsTruthChain C, IsOckhamAlgebra L>
Set<C, L> complement(const Set<C, L>& a) {
  return erase<C, L>(~a);
}

/** @brief The quantifiers, the library's, in @c L: @c ∃ is @c sets:quantifier's
 *  over the universe with the restricted set as the where-clause,
 *  @f$\bigvee (\chi_S \wedge P)@f$.  (Equality and @c ⊆ need no handle
 *  version: the sets' own @c == is the exhaustion in the join species and
 *  @c ⊆ is the identity @f$(A \cap B) = A@f$.)
 *  @tparam C the chain.  @tparam L the species. */
template <IsTruthChain C, IsOckhamAlgebra L>
typename L::Ω exists(const Set<C, L>& s, const Datum<C>& d) {
  return exists(𝔸<C, L>{}, former(s, d).predicate);
}
/** @brief Bounded @f$\forall@f$ over a @b decidable set: the identity
 *  @f$S = S|P@f$, the library's own @c forall, exact for two-valued χ_S.  An
 *  L-valued @c S has no bounded ∀ until the species has a residuated
 *  implication (#980); the binding refuses it and points at the α-cut. */
template <IsTruthChain C>
bool forall(const Set<C, Boole>& s, const Datum<C>& d) {
  return s == former(s, d);
}
/** @brief The runs of a decidable set, materialised for the handle. */
template <IsTruthChain C>
std::vector<Run<C>> run_list(const Set<C, Boole>& s) {
  std::vector<Run<C>> out;
  for (const auto r : runs(s)) out.push_back(r);
  return out;
}
/** @brief An L-valued set read through its decidable sets on Ω, pulled back
 *  along χ with the generic @c preimage: the α-cut @f$\{\chi \ge \ell\}@f$ and
 *  the fibre @f$\chi^{-1}(\ell)@f$.  @tparam C the chain.  @tparam L the
 *  species. */
template <IsTruthChain C, IsOckhamAlgebra L>
Set<C, Boole> cut(const Set<C, L>& s, typename L::Ω level) {
  using Ω = typename L::Ω;
  return erase<C, Boole>(𝔸<C>{} | preimage(s, 𝔸<Ω>{} | (π >= level)));
}
template <IsTruthChain C, IsOckhamAlgebra L>
Set<C, Boole> fibre(const Set<C, L>& s, typename L::Ω level) {
  return erase<C, Boole>(𝔸<C>{} | preimage(s, η(level)));
}
/** @brief A Boolean set lifted along the dominance into Kleene's species. */
template <IsTruthChain C>
Set<C, Kleene> lift(const Set<C, Boole>& s) {
  return erase<C, Kleene>(s);
}

}  // namespace pst

}  // namespace dedekind::python
