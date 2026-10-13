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
using dedekind::order::Interval;
using dedekind::order::intervals;
using dedekind::order::Position;
using dedekind::order::Run;
using dedekind::order::Runs;
using dedekind::order::Side;
using dedekind::order::steps;
using dedekind::order::Strictness;
using dedekind::sequences::SuccessorOrbit;
using dedekind::sets::Cardinality;
using dedekind::sets::chain_bottom;
using dedekind::sets::chain_top;
using dedekind::sets::Comprehension;
using dedekind::sets::finite_cardinality;
using dedekind::sets::IsSetObject;
using dedekind::sets::lift_to;
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
// @c {x>3} ∩ {x<5} = {4}: a halfspace @c ℕ|(χ ⋈ k) is the principal
// filter/ideal @c ↑k / @c ↓k of the carrier order, and its meet routes through
// the @b real lattice reducer: the meet of two intervals is the value-first
// crossing law @c ↑a ∩ ↓b = [a,b] (collapsing to a point over an integral
// carrier), and unions and complements are run lists (@c order:runs), the
// normal form of the sets of a chain built from intervals.  This exhibit is the
// SET twin of the @c jlt ARROW exhibit above: it binds the real reducer, NOT a
// Python-side reimplementation (handle-only).

/** @brief A term's normal form, materialised for a handle: the one stored
 *  form, a run list over the term's carrier and species.  @param t the term.
 */
template <typename T>
auto materialise(const T& t) {
  using S = decltype(dedekind::order::steps(t));
  return Runs<typename S::Domain, typename S::logic_species>(
      dedekind::order::steps(t));
}

namespace lwv {

/** @brief A value-based set handle (#965): a materialised run list over
 *  @c long @c long (Python's @c int, ℤ read through the 64-bit window), its
 *  runs by start, so one constructor covers all pivots at runtime.  Its @c &,
 *  @c | and @c ~ build the library's term and materialise its normal form. */
using Set = Runs<long long>;
/** @brief One run of a set: an interval. */
using Leaf = Interval<long long>;
// Whatever crosses the Python boundary is a set OBJECT --- (reified universe,
// χ) --- and this is the compile-time MUST for it (RFC 2119).
static_assert(IsSetObject<Set>,
              "the Python surface's Set must be a set object (IsSetObject).");

/** @brief The rays @c {x ⋈ k}: the principal filter ↑k and ideal ↓k, open or
 *  closed.  @tparam D the direction.  @tparam S the strictness.  @param k the
 *  pivot. */
template <Direction D, Strictness S>
Set ray(long long k) {
  return materialise(Leaf::half(k, D, S));
}
/** @brief The point @c {k}.  @param k the point. */
inline Set point(long long k) { return materialise(Leaf::point(k)); }
/** @brief The lattice on handles: the library's term, materialised. */
inline Set meet(const Set& a, const Set& b) { return materialise(a & b); }
inline Set join(const Set& a, const Set& b) { return materialise(a | b); }
inline Set complement(const Set& a) { return materialise(~a); }

/** @brief A set translated by @c k, as a source: every step moved, the image
 *  under the order automorphism @f$x \mapsto x + k@f$ of ℤ.  Where the image
 *  leaves the window --- a step past its last value, or members moved below
 *  its first --- ℤ continues and the window cannot, so it raises rather than
 *  truncate. */
struct Shifted {
  using Domain = long long;
  using logic_species = Boole;
  const Set* s;
  long long k;
  struct Cursor {
    Set::Cursor c;
    long long k;
    std::optional<dedekind::order::Step<long long, Boole>> pull() {
      using W = std::numeric_limits<long long>;
      auto st = c.pull();
      if (!st) return st;
      st->at.at = jlt::add_in_window(st->at.at, k, "image");
      if (st->value ? st->at == Position<long long>{W::max(), Side::Above}
                    : st->at == Position<long long>{W::min(), Side::Below})
        jlt::window_end("image");
      st->at = dedekind::order::canonical(st->at);
      return st;
    }
  };
  bool first() const { return s->first(); }
  Cursor cursor() const { return {s->cursor(), k}; }
};
/** @brief The arrows whose image of a set has a closed form: the identity, the
 *  successor and the predecessor on the integer window.  An opaque composed
 *  arrow has no such normal form; its image is intensional (Kleene-valued) and
 *  is refused at the boundary rather than guessed.  @tparam F the arrow. */
template <typename F>
concept IsStructuralArrow = std::same_as<F, Identity<jlt::Int>> ||
                            std::same_as<F, Successor<jlt::Int>> ||
                            std::same_as<F, Predecessor<jlt::Int>>;
/** @brief How far the arrow moves a step: 0, +1, −1.  @tparam F the arrow. */
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
Set image(const F&, const Set& s) {
  return Set(Shifted{&s, offset<F>()});
}
template <IsStructuralArrow F>
Set preimage(const F&, const Set& s) {
  return Set(Shifted{&s, -offset<F>()});
}

/** @brief Slicing by @b value: @f$S \cap [a, b)@f$, the meet with the
 *  half-open interval (Python's own convention), an absent bound meaning the
 *  ray.  Not positional: a set has no enumeration to index into; that reading
 *  belongs to a sequence. */
inline Set restrict(const Set& s, std::optional<long long> lo,
                    std::optional<long long> hi) {
  Leaf window = Leaf::universe();
  if (lo)
    window = window & Leaf::half(*lo, Direction::Upward, Strictness::NonStrict);
  if (hi)
    window = window & Leaf::half(*hi, Direction::Downward, Strictness::Strict);
  return materialise(s & window);
}

/** @brief Whether the set is bounded (on the discrete chain ℤ, equivalently
 *  finite): no run from below the window, and the last run ends. */
inline bool is_bounded(const Set& s) {
  return !s.first() &&
         (s.runs().empty() || s.runs().rbegin()->second.end.has_value());
}
/** @brief The least element, the unfold's seed: none for @c Ø, and none for a
 *  set unbounded below (@c ↓k, @c 𝔸) --- ℤ has no bottom. */
inline std::optional<long long> least(const Set& s) {
  auto rs = dedekind::order::runs(s);
  const auto it = rs.begin();
  if (s.first() || it == rs.end()) return std::nullopt;
  return (*it).lo;
}
/** @brief The greatest element, where the set is bounded; @c nullopt on a
 *  set unbounded above: the unfold does not stop. */
inline std::optional<long long> greatest(const Set& s) {
  std::optional<long long> hi;
  if (is_bounded(s))
    for (const auto& r : dedekind::order::runs(s)) hi = r.hi;
  return hi;
}
/** @brief The least member above @c x: @c x + 1 inside a run, else the start
 *  of the next run; none past the last.  @param x a member below the window's
 *  end. */
inline std::optional<long long> next_member(const Set& s, long long x) {
  for (const auto& r : dedekind::order::runs(s)) {
    if (x < r.lo) return r.lo;
    if (x < r.hi) return x + 1;
  }
  return std::nullopt;
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

// ── Sets over a truth chain, per the paper's Lwv grammar (#975) ─────────────
//
// A Python set over a truth chain is a run list (order:runs) valued in its
// species: the normal form of every set the grammar builds.  Every operator
// (& | ^ ~ and the former |) is the library's own on run lists, and every
// query (==, ⊆, ∃, ∀, runs) reads the normal form.  Nothing is reduced in
// Python.

/** @brief A truth chain the queries can read: a Pst carrier that is finite by
 *  its representation and has the covering step.  @tparam C the chain. */
template <typename C>
concept IsTruthChain = IsPst<C> && IsFiniteChain<C> && HasCoveringStep<C>;

/** @brief A set over the chain @c C valued in @c L, as Python holds it: its
 *  materialised run list. */
template <IsTruthChain C, IsOckhamAlgebra L>
using Set = Runs<C, L>;
/** @brief A datum of the grammar (@c π @c > @c v, @c π @c == @c v) over @c C:
 *  a two-valued ray or point waiting for a former, a type of its own so that a
 *  set is never taken for a where-clause.  @tparam C the chain. */
template <IsTruthChain C>
struct Datum {
  Runs<C> set;
  constexpr bool operator()(const C& x) const { return set(x); }
};

/** @brief The generators of the grammar: @c 𝔸, @c Ø, @c η(v); and the atoms
 *  @c π @c ⋈ @c v as data.  @tparam C the chain.  @tparam L the species. */
template <IsTruthChain C, IsOckhamAlgebra L>
Set<C, L> universe() {
  return Set<C, L>::universe();
}
template <IsTruthChain C, IsOckhamAlgebra L>
Set<C, L> empty() {
  return Set<C, L>::empty();
}
template <IsTruthChain C, IsOckhamAlgebra L>
Set<C, L> point(const C& v) {
  return Set<C, L>(steps(Interval<C, L>::point(v)));
}
template <IsTruthChain C>
Datum<C> above(const C& v) {
  return {Runs<C>(steps(𝔸<C>{} | (π > v)))};
}
template <IsTruthChain C>
Datum<C> at_least(const C& v) {
  return {Runs<C>(steps(𝔸<C>{} | (π >= v)))};
}
template <IsTruthChain C>
Datum<C> below(const C& v) {
  return {Runs<C>(steps(𝔸<C>{} | (π < v)))};
}
template <IsTruthChain C>
Datum<C> at_most(const C& v) {
  return {Runs<C>(steps(𝔸<C>{} | (π <= v)))};
}
template <IsTruthChain C>
Datum<C> equal_to(const C& v) {
  return {Runs<C>(steps(Interval<C>::point(v)))};
}
/** @brief χ(x) = x: the identity classifier on a truth chain, valued in the
 *  chain's own species --- the simplest set with every level inhabited. */
template <IsTruthChain C>
Set<C, classifier_logic_t<C>> identity() {
  return Set<C, classifier_logic_t<C>>(dedekind::order::IdentitySteps<C>{});
}

/** @brief The former @c S @c | @c P: the library's meet of the set and the
 *  datum, materialised.  Lattice operations and equality across species are
 *  the library's own (@c sets: the operands are lifted into the species'
 *  join), so the handles need no more.  @tparam C the chain.  @tparam L the
 *  species. */
template <IsTruthChain C, IsOckhamAlgebra L>
Set<C, L> former(const Set<C, L>& s, const Datum<C>& d) {
  return materialise(s & d.set);
}

/** @brief The quantifiers, in @c L: @c ∃ is ⋁ (χ_S ∧ P), the existential of
 *  the former; @c ∀ over a @b decidable set is @f$S \subseteq P@f$.  An
 *  L-valued @c S has no bounded ∀ until the species has a residuated
 *  implication (#980); the binding refuses it and points at the α-cut.
 *  @tparam C the chain.  @tparam L the species. */
template <IsTruthChain C, IsOckhamAlgebra L>
typename L::Ω exists(const Set<C, L>& s, const Datum<C>& d) {
  return dedekind::order::exists(steps(s & d.set));
}
template <IsTruthChain C>
bool forall(const Set<C, Boole>& s, const Datum<C>& d) {
  return s == (s & d.set);
}
/** @brief The runs of a decidable set, read off its normal form. */
template <IsTruthChain C>
auto run_list(const Set<C, Boole>& s) {
  return dedekind::order::runs(s);
}
/** @brief An L-valued set read through its decidable sets: the α-cut
 *  @f$\{\chi \ge \ell\}@f$ and the fibre @f$\chi^{-1}(\ell)@f$.
 *  @tparam C the chain.  @tparam L the species. */
template <IsTruthChain C, IsOckhamAlgebra L>
Set<C, Boole> cut(const Set<C, L>& s, typename L::Ω level) {
  return Set<C, Boole>(dedekind::order::alpha_cut(steps(s), level));
}
template <IsTruthChain C, IsOckhamAlgebra L>
Set<C, Boole> fibre(const Set<C, L>& s, typename L::Ω level) {
  return Set<C, Boole>(dedekind::order::fibre(steps(s), level));
}
/** @brief A Boolean set lifted along the dominance into Kleene's species. */
template <IsTruthChain C>
Set<C, Kleene> lift(const Set<C, Boole>& s) {
  return materialise(lift_to<Kleene>(s));
}

}  // namespace pst

}  // namespace dedekind::python
