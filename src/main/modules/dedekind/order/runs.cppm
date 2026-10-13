/**
 * @file dedekind/order/runs.cppm
 * @partition :runs
 * @brief Run lists: the normal form of the sets of a chain built from
 *        intervals, valued in a species.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section runs__Normal_Form
 * A set of a chain valued in a species @c L, built from intervals with the
 * species' @c ∧, @c ∨ and @c ¬, is a @b step function: a value before the
 * first step, and finitely many steps, each a position on the chain where the
 * value changes.  In canonical form the positions are closed where the chain
 * has the step and no step repeats the value before it, so each set has one
 * run list, and @c &, @c |, @c ~ are total on the one type.  It is the
 * canonical form of the Boolean (De Morgan) algebra the intervals generate,
 * quotiented by the chain's order; @c Interval (@c :halfspace) is its
 * generator.  A step at the carrier's own end is kept (@c {x ≤ max} on a
 * machine word read as a window of ℤ stays bounded above); equality reads the
 * carrier's ends.
 */
module;

#include <array>
#include <concepts>
#include <cstddef>
#include <functional>  // std::less_equal: the chain's own order
#include <iterator>
#include <map>
#include <optional>
#include <ranges>
#include <type_traits>
#include <utility>

export module dedekind.order:runs;

import dedekind.category;
import dedekind.sets;
import :total;  // IsTotallyOrdered: positions live on a certified chain

namespace dedekind::order {
using namespace dedekind::sets;
using namespace dedekind::category;

/** @brief Orientation of a halfspace along the chain. */
export enum class Direction { Upward, Downward };

/** @brief Whether the boundary is strict (`>`, `<`) or inclusive (`>=`, `<=`).
 */
export enum class Strictness { Strict, NonStrict };

/** @brief Flip a @c Direction --- the direction half of a halfspace complement
 *  (@c ~{x>P} is @c {x≤P}).  Reused by the reflection-image pushforward
 *  (@c dedekind.algebra:halfspace_transport) to flip a halfspace's sense under
 *  @f$x \mapsto -x@f$. */
export constexpr Direction flip(Direction d) {
  return d == Direction::Upward ? Direction::Downward : Direction::Upward;
}
/** @brief Flip a @c Strictness --- the strictness half of the same complement
 *  (@c ~{x>P} is @c {x≤P}: @c > relaxes to @c ≤). */
export constexpr Strictness flip(Strictness s) {
  return s == Strictness::Strict ? Strictness::NonStrict : Strictness::Strict;
}

/** @brief Cardinality class of a halfspace's carrier @c T (#848/#927).
 *  @details A carrier that declares its own @c cardinality_type is the
 *  authority (e.g.\ @c numbers::Rational → @c ℵ_0, @c ExtensionalCardinal →
 *  @c Finite): this partition (@c :order) is @b upstream of @c :numbers, so a
 *  countable @b non-integral carrier such as ℚ cannot be recognised
 *  structurally here (@c IsRingIntegral is false on @c Rational), and must
 *  self-declare so the point-free @c A|pred halfspace classifies identically
 *  to the scout comprehension (which inherits the ambient @c C directly).
 *  Otherwise fall back to the @c IsRingIntegral discriminator: the structural
 *  integers @b and the @c Cardinality / @c SignedCardinality (ℕ/ℤ) proxies are
 *  @c ℵ_0, and the real proxies (@c QuadraticReal, @c double) are the continuum
 *  @c ℶ_1.  The bound only has to be tight enough for the countability
 *  verdict (@c IsCountable). */
// Module-private: the fallback is a halfspace-classification heuristic, not a
// general carrier-cardinality authority (that is @c :sets:cardinality).  It
// only feeds @c Halfspace::cardinality_type below; external carriers customise
// through their own @c T::cardinality_type, not this helper.  A finite @b enum
// carrier (e.g.\ @c Ternary) cannot self-declare a member typedef, so it is
// recognised structurally as countable and classified @c ℵ_0 --- the same
// conservative countable BOUND @c bool already gets via @c IsRingIntegral, NOT
// @c Finite.  @c Finite is the @c elevate_meet "return bare, do not @c Set
// -wrap" signal (@c :expressions); a halfspace must not trigger it, and @c ℵ_0
// keeps the pre-existing @c Set-wrapping path and stays countable, matching
// @c Ternary's @c 𝕂3 ambient.
// The @c IsRingIntegral integers / ℕ,ℤ proxies are @c ℵ_0 likewise; the real
// proxies are @c ℶ_1.
template <typename T>
struct carrier_cardinality {
  using type =
      std::conditional_t<std::is_enum_v<T> || IsRingIntegral<T>, ℵ_0, ℶ_1>;
};
template <typename T>
  requires IsCardinality<typename T::cardinality_type>
struct carrier_cardinality<T> {
  // Fires only when the self-declared alias is a genuine @c IsCardinality (a
  // carrier with an unrelated / incomplete @c cardinality_type falls to the
  // primary, not a hard error), and classifies it with the canonical
  // @c IsCountable concept.  Collapse to the
  // COUNTABILITY BOUND (never pass a bare @c Finite through): @c
  // ExtensionalCardinal declares @c Finite, and letting that reach @c
  // Halfspace::cardinality_type would trip @c elevate_meet's Finite "return
  // bare" path for an @c ExtensionalCardinal halfspace.  Countable (incl.\
  // @c Finite) ⟹ @c ℵ_0, uncountable ⟹ @c ℶ_1.
  using type =
      std::conditional_t<IsCountable<typename T::cardinality_type>, ℵ_0, ℶ_1>;
};
template <typename T>
using carrier_cardinality_t = typename carrier_cardinality<T>::type;

// A finite enum carrier classifies ℵ_0 (a conservative countable bound, like
// bool), so a Ternary halfspace stays decidable (Boole), not Kleene (#927), and
// does NOT trip the elevate_meet Finite "return bare" path.
static_assert(std::same_as<carrier_cardinality_t<Ternary>, ℵ_0>,
              "a finite enum carrier (e.g. Ternary) is countable ℵ_0, not the "
              "continuum ℶ_1 (and not Finite, the elevate_meet bare signal).");
// A self-declared @c Finite carrier is likewise collapsed to @c ℵ_0, so its
// halfspace stays Set-wrapped by @c elevate_meet (not returned bare).
static_assert(std::same_as<carrier_cardinality_t<ExtensionalCardinal<>>, ℵ_0>,
              "a self-declared Finite carrier collapses to the countable bound "
              "ℵ_0, never the bare-signal Finite.");
namespace detail_halfspace_witness {
// A carrier whose @c cardinality_type alias is NOT an @c IsCardinality: the
// self-declaring specialization must @b not fire (its IsCardinality constraint
// fails), so @c carrier_cardinality falls back to the primary rather than
// hard-erroring on the missing @c is_countable member.
struct UnrelatedCardinalityAlias {
  using cardinality_type = int;  // not a cardinality (no is_countable etc.)
};
}  // namespace detail_halfspace_witness
static_assert(
    std::same_as<carrier_cardinality_t<
                     detail_halfspace_witness::UnrelatedCardinalityAlias>,
                 ℶ_1>,
    "a carrier with an unrelated/incomplete cardinality_type falls back to the "
    "primary (ℶ_1 here), never making Halfspace ill-formed.");

/** @brief One side of an interval as a value: the bound and whether it is
 *  attained (non-strict) or not (strict).  @tparam V the carrier. */
export template <typename V>
  requires IsTotallyOrdered<V>
struct Bound {
  V at;
  Strictness strictness = Strictness::NonStrict;
  friend constexpr bool operator==(const Bound&, const Bound&) = default;
};
/** @brief The sides of an inhabited convex set; an absent side is unbounded.
 *  @tparam V the carrier. */
export template <typename V>
  requires IsTotallyOrdered<V>
struct Span {
  std::optional<Bound<V>> lower;
  std::optional<Bound<V>> upper;
  friend constexpr bool operator==(const Span&, const Span&) = default;
};

/** @section runs__Positions
 *  The places on a chain a side can sit: just below a value or just above it,
 *  a Dedekind cut named by the value beside it.  An interval's sides and a run
 *  list's steps are positions, so both compare, close and read the carrier's
 *  ends through this one layer. */

/** @brief Which side of its value a position sits on. */
export enum class Side : bool { Below, Above };

/** @brief A position on the chain: just below @c at, or just above it.
 *  @tparam V the carrier. */
export template <typename V>
  requires IsTotallyOrdered<V>
struct Position {
  V at;
  Side side;
  friend constexpr bool operator==(const Position&, const Position&) = default;
  /** @brief Along the chain: by value, and below a value before above it. */
  friend constexpr bool operator<(const Position& a, const Position& b) {
    return a.at < b.at ||
           (a.at == b.at && a.side == Side::Below && b.side == Side::Above);
  }
};

namespace detail_position {
/** @brief The position a lower side starts at: below an attained bound, above a
 *  strict one.  @tparam V the carrier. */
template <typename V>
constexpr Position<V> start_of(const Bound<V>& b) {
  return {b.at,
          b.strictness == Strictness::NonStrict ? Side::Below : Side::Above};
}
/** @brief The position an upper side ends at: above an attained bound, below a
 *  strict one.  @tparam V the carrier. */
template <typename V>
constexpr Position<V> end_of(const Bound<V>& b) {
  return {b.at,
          b.strictness == Strictness::NonStrict ? Side::Above : Side::Below};
}
/** @brief The lower side starting at @c p.  @tparam V the carrier. */
template <typename V>
constexpr Bound<V> lower_at(const Position<V>& p) {
  return {p.at,
          p.side == Side::Below ? Strictness::NonStrict : Strictness::Strict};
}
/** @brief The upper side ending at @c p.  @tparam V the carrier. */
template <typename V>
constexpr Bound<V> upper_at(const Position<V>& p) {
  return {p.at,
          p.side == Side::Above ? Strictness::NonStrict : Strictness::Strict};
}
/** @brief The carrier's least element, where it has one as a value: a
 *  registered lattice bottom (the machine words), ℕ's 0 and ℤ's −ℵ₀, a truth
 *  chain's ⊥.  @tparam V the carrier. */
template <typename V>
constexpr std::optional<V> least() {
  if constexpr (requires { LatticeBottom<V, std::less_equal<V>>::value; })
    return LatticeBottom<V, std::less_equal<V>>::value;
  else if constexpr (std::same_as<V, Cardinality>)
    return V{};
  else if constexpr (std::same_as<V, SignedCardinality>)
    return V{NegativeInfinity{}};
  else if constexpr (IsPst<V>)
    return dedekind::sets::chain_bottom<V>();
  else
    return std::nullopt;
}
/** @brief The carrier's greatest element, where it has one as a value, dual to
 *  @ref least (ℵ₀ on ℕ).  @tparam V the carrier. */
template <typename V>
constexpr std::optional<V> greatest() {
  if constexpr (requires { LatticeTop<V, std::less_equal<V>>::value; })
    return LatticeTop<V, std::less_equal<V>>::value;
  else if constexpr (std::same_as<V, Cardinality>)
    return V{ℵ_0{}};
  else if constexpr (std::same_as<V, SignedCardinality>)
    return V{PositiveInfinity{}};
  else if constexpr (IsPst<V>)
    return dedekind::sets::chain_top<V>();
  else
    return std::nullopt;
}
/** @brief A position in its canonical form: just above a value is just below
 *  the element covering it, where there is one, so on a discrete chain
 *  @c (0, 3) and @c [1, 3) have the same positions.  @tparam V the carrier. */
template <typename V>
constexpr Position<V> canonical(Position<V> p) {
  if constexpr (HasCoveringStep<V>)
    if (p.side == Side::Above)
      if (const auto n = cover(p.at)) return {*n, Side::Below};
  return p;
}
/** @brief Whether @c p lies before every element of the carrier.
 *  @tparam V the carrier. */
template <typename V>
constexpr bool before_all(const Position<V>& p) {
  const auto lo = least<V>();
  return lo && !(Position<V>{*lo, Side::Below} < p);
}
/** @brief Whether @c p lies after every element of the carrier.
 *  @tparam V the carrier. */
template <typename V>
constexpr bool after_all(const Position<V>& p) {
  const auto hi = greatest<V>();
  return hi && !(p < Position<V>{*hi, Side::Above});
}
}  // namespace detail_position
/** @brief A position in its canonical form: closed where the chain has the
 *  step.  @tparam V the carrier. */
export template <typename V>
constexpr Position<V> canonical(const Position<V>& p) {
  return detail_position::canonical(p);
}

/** @section runs__Steps
 *  A set of the chain valued in a species is a step function: a value before
 *  every step, then the steps along the chain, each a canonical position and
 *  the value from there on.  A @b step source yields them on demand through a
 *  pull cursor, so nothing is stored and every query is one traversal; the
 *  sources compose (a meet is a merge of two sources), so the normal form of a
 *  term is itself a source.  A cursor carries what it walks, so a view of it
 *  outlives a temporary source; only a stored run list is read in place.  @c
 * Runs is the one stored form, materialised on request. */

/** @brief Where the value changes, and to what.  @tparam V the carrier.
 *  @tparam L the species. */
export template <typename V, typename L>
struct Step {
  Position<V> at;
  typename L::Ω value;
  friend constexpr bool operator==(const Step&, const Step&) = default;
};

/** @brief A step source: the carrier and species, the value before every step,
 *  and a cursor whose @c pull() yields the steps in order, then @c nullopt. */
export template <typename S>
concept IsStepSource = requires(const S& s) {
  typename S::Domain;
  typename S::logic_species;
  { s.first() } -> std::same_as<typename S::logic_species::Ω>;
  {
    s.cursor().pull()
  } -> std::same_as<
      std::optional<Step<typename S::Domain, typename S::logic_species>>>;
};

/** @brief The constant source: @c Ø at ⊥, @c 𝔸 at ⊤.  @tparam V the carrier.
 *  @tparam L the species. */
export template <typename V, typename L>
struct ConstantSteps {
  using Domain = V;
  using logic_species = L;
  typename L::Ω value;
  struct Cursor {
    constexpr std::optional<Step<V, L>> pull() { return std::nullopt; }
  };
  constexpr typename L::Ω first() const { return value; }
  constexpr Cursor cursor() const { return {}; }
};

/** @brief An interval's sides as a source, valued @c value on it and ⊥
 *  elsewhere: at most two steps.  @tparam V the carrier.  @tparam L the
 *  species. */
export template <typename V, typename L>
struct SpanSteps {
  using Domain = V;
  using logic_species = L;
  std::optional<Span<V>> span;
  typename L::Ω value = L::True;
  struct Cursor {
    std::optional<Step<V, L>> steps[2];
    int next = 0;
    constexpr std::optional<Step<V, L>> pull() {
      while (next < 2)
        if (const auto s = steps[next++]) return s;
      return std::nullopt;
    }
  };
  constexpr typename L::Ω first() const {
    return span && !span->lower ? value : L::False;
  }
  constexpr Cursor cursor() const {
    Cursor c{};
    if (!span || value == L::False) return c;
    if (span->lower)
      c.steps[0] = Step<V, L>{
          detail_position::canonical(detail_position::start_of(*span->lower)),
          value};
    if (span->upper)
      c.steps[1] = Step<V, L>{
          detail_position::canonical(detail_position::end_of(*span->upper)),
          L::False};
    return c;
  }
};

/** @brief A source read through a reference: a stored run list inside a term
 *  is traversed where it is, not copied.  @tparam S the source. */
export template <typename S>
struct SourceRef {
  using Domain = typename S::Domain;
  using logic_species = typename S::logic_species;
  const S* s;
  constexpr auto first() const { return s->first(); }
  constexpr auto cursor() const { return s->cursor(); }
};
/** @brief A fixed list of steps as a source, already in order and canonical:
 *  a finite table written out (the identity on a short chain).
 *  @tparam V the carrier.  @tparam L the species.  @tparam N their number. */
export template <typename V, typename L, std::size_t N>
struct ListSteps {
  using Domain = V;
  using logic_species = L;
  typename L::Ω start;
  std::array<Step<V, L>, N> steps;
  struct Cursor {
    std::array<Step<V, L>, N> steps;
    std::size_t i = 0;
    constexpr std::optional<Step<V, L>> pull() {
      if (i == N) return std::nullopt;
      return steps[i++];
    }
  };
  constexpr typename L::Ω first() const { return start; }
  constexpr Cursor cursor() const { return {steps}; }
};

/** @brief Two sources merged along the chain under a pointwise operation: the
 *  steps of both, each read as @c f of the two values there, a step repeating
 *  the value before it dropped.  @tparam A,B the sources.  @tparam F the
 *  operation. */
export template <IsStepSource A, IsStepSource B, typename F>
struct MergeSteps {
  using Domain = typename A::Domain;
  using logic_species = typename A::logic_species;
  using Ω = typename logic_species::Ω;
  A a;
  B b;
  F f;
  struct Cursor {
    decltype(std::declval<const A&>().cursor()) ca;
    decltype(std::declval<const B&>().cursor()) cb;
    std::optional<Step<Domain, logic_species>> pa, pb;
    Ω va, vb, last;
    F f;
    constexpr std::optional<Step<Domain, logic_species>> pull() {
      while (pa || pb) {
        const auto at = !pb || (pa && pa->at < pb->at) ? pa->at : pb->at;
        if (pa && pa->at == at) va = pa->value, pa = ca.pull();
        if (pb && pb->at == at) vb = pb->value, pb = cb.pull();
        if (const Ω v = f(va, vb); v != last)
          return Step<Domain, logic_species>{at, last = v};
      }
      return std::nullopt;
    }
  };
  constexpr Ω first() const { return f(a.first(), b.first()); }
  constexpr Cursor cursor() const {
    auto ca = a.cursor();
    auto cb = b.cursor();
    auto pa = ca.pull();
    auto pb = cb.pull();
    return {ca, cb, pa, pb, a.first(), b.first(), first(), f};
  }
};

/** @brief A source with every value relabelled into the species @c L2: a
 *  complement, a lift into a wider species, an α-cut.  @tparam A the source.
 *  @tparam F the operation @c A's Ω → @c L2::Ω.  @tparam L2 the species. */
export template <IsStepSource A, typename F,
                 typename L2 = typename A::logic_species>
struct RelabelSteps {
  using Domain = typename A::Domain;
  using logic_species = L2;
  A a;
  F f;
  struct Cursor {
    decltype(std::declval<const A&>().cursor()) c;
    typename L2::Ω last;
    F f;
    constexpr std::optional<Step<Domain, L2>> pull() {
      while (const auto s = c.pull())
        if (const auto v = f(s->value); v != last)
          return Step<Domain, L2>{s->at, last = v};
      return std::nullopt;
    }
  };
  constexpr typename L2::Ω first() const { return f(a.first()); }
  constexpr Cursor cursor() const { return {a.cursor(), first(), f}; }
};

namespace detail_runs {
/** @brief The species' operations as named functions on @c L::Ω. */
template <typename L>
struct And {
  constexpr auto operator()(typename L::Ω a, typename L::Ω b) const {
    return L::AND(a, b);
  }
};
template <typename L>
struct Or {
  constexpr auto operator()(typename L::Ω a, typename L::Ω b) const {
    return L::OR(a, b);
  }
};
template <typename L>
struct Rfl {
  constexpr auto operator()(typename L::Ω a) const { return L::RFL(a); }
};
/** @brief @c a ⇔ @c b in the species: both, or neither. */
template <typename L>
struct Iff {
  constexpr auto operator()(typename L::Ω a, typename L::Ω b) const {
    return L::OR(L::AND(a, b), L::AND(L::RFL(a), L::RFL(b)));
  }
};
}  // namespace detail_runs

/** @name Queries: one traversal of a source
 *  @{ */
/** @brief χ(x): the value of the last step at or before @c x. */
export template <IsStepSource S>
constexpr auto value_at(const S& s, const typename S::Domain& x) {
  auto v = s.first();
  auto c = s.cursor();
  while (const auto st = c.pull()) {
    if (st->at.side == Side::Below ? x < st->at.at : !(st->at.at < x)) break;
    v = st->value;
  }
  return v;
}
namespace detail_runs {
/** @brief The species' fold of the values over the carrier's own pieces: a
 *  step before every element sets the first value, one after every element
 *  ends the fold.  @tparam Op the fold. */
template <typename Op, IsStepSource S>
constexpr auto fold_over_carrier(const S& s, Op op) {
  using L = typename S::logic_species;
  auto cur = s.first();
  std::optional<typename L::Ω> acc;
  auto c = s.cursor();
  while (const auto st = c.pull()) {
    if (detail_position::before_all(st->at)) {
      cur = st->value;
      continue;
    }
    if (detail_position::after_all(st->at)) break;
    acc = acc ? op(*acc, cur) : cur;
    cur = st->value;
  }
  return acc ? op(*acc, cur) : cur;
}
}  // namespace detail_runs
/** @brief @f$\bigwedge_x \chi(x)@f$ and @f$\bigvee_x \chi(x)@f$ in the species
 *  over the carrier's own elements: on @c Ø both are ⊥, on @c 𝔸 both ⊤. */
export template <IsStepSource S>
constexpr auto forall(const S& s) {
  return detail_runs::fold_over_carrier(
      s, detail_runs::And<typename S::logic_species>{});
}
export template <IsStepSource S>
constexpr auto exists(const S& s) {
  return detail_runs::fold_over_carrier(
      s, detail_runs::Or<typename S::logic_species>{});
}
/** @brief Equality of two sources as sets of the carrier, in the species:
 *  @f$\bigwedge_x (\chi_a(x) \Leftrightarrow \chi_b(x))@f$, so over K₃ two
 *  sets agree to degree U where either is U. */
export template <IsStepSource A, IsStepSource B>
  requires std::same_as<typename A::Domain, typename B::Domain> &&
           std::same_as<typename A::logic_species, typename B::logic_species>
constexpr auto equal(const A& a, const B& b) {
  using L = typename A::logic_species;
  return forall(MergeSteps<A, B, detail_runs::Iff<L>>{a, b, {}});
}
/** @} */

/** @brief A pull cursor as a @c std::ranges input view: the readings of a
 *  source are views over it, nothing stored.  @tparam Cursor the cursor. */
export template <typename Cursor>
class PullView : public std::ranges::view_interface<PullView<Cursor>> {
 public:
  using value_type =
      typename decltype(std::declval<Cursor&>().pull())::value_type;
  class iterator {
   public:
    using iterator_concept = std::input_iterator_tag;
    using value_type = PullView::value_type;
    using difference_type = std::ptrdiff_t;
    constexpr iterator() = default;
    constexpr explicit iterator(PullView* v) : v_(v), cur_(v->c_.pull()) {}
    constexpr const value_type& operator*() const { return *cur_; }
    constexpr iterator& operator++() {
      cur_ = v_->c_.pull();
      return *this;
    }
    constexpr void operator++(int) { ++*this; }
    friend constexpr bool operator==(const iterator& it,
                                     std::default_sentinel_t) {
      return !it.cur_;
    }

   private:
    PullView* v_ = nullptr;
    std::optional<value_type> cur_;
  };
  constexpr PullView() = default;
  constexpr explicit PullView(Cursor c) : c_(std::move(c)) {}
  constexpr iterator begin() { return iterator{this}; }
  constexpr std::default_sentinel_t end() const { return {}; }

 private:
  Cursor c_;
};

/** @brief The pieces on which a two-valued source holds, in order along the
 *  chain: a start (@c nullopt below every element) and an end (@c nullopt
 *  above), read over the carrier.  @tparam S the source. */
template <typename V>
struct Piece {
  std::optional<Position<V>> start;
  std::optional<Position<V>> end;
};
namespace detail_runs {
template <IsStepSource S>
struct PieceCursor {
  using V = typename S::Domain;
  decltype(std::declval<const S&>().cursor()) c;
  std::optional<Position<V>> start;
  bool inside;
  bool done = false;
  constexpr std::optional<Piece<V>> pull() {
    while (!done) {
      const auto st = c.pull();
      if (!st || detail_position::after_all(st->at)) {
        done = true;
        if (inside) return Piece<V>{start, std::nullopt};
        return std::nullopt;
      }
      if (detail_position::before_all(st->at)) {
        inside = static_cast<bool>(st->value);
        continue;
      }
      if (st->value) {
        start = st->at;
        inside = true;
      } else if (inside) {
        inside = false;
        return Piece<V>{std::exchange(start, std::nullopt), st->at};
      }
    }
    return std::nullopt;
  }
};
}  // namespace detail_runs
/** @brief The pieces of a two-valued source, as a view. */
export template <IsStepSource S>
  requires std::same_as<typename S::logic_species, Boole>
constexpr auto pieces(const S& s) {
  return PullView<detail_runs::PieceCursor<S>>{
      {s.cursor(), std::nullopt, static_cast<bool>(s.first())}};
}

/** @brief A maximal run of membership as its closed ends, @c [lo, hi].
 *  @tparam C the chain. */
export template <typename C>
struct Run {
  C lo;
  C hi;
  friend constexpr bool operator==(const Run&, const Run&) = default;
};
namespace detail_runs {
/** @brief A piece of a discrete chain with ends, read closed: a canonical
 *  start is just below its first element, an end just below the element after
 *  its last; an absent side at the carrier's end. */
template <typename C>
struct Closed {
  constexpr Run<C> operator()(const Piece<C>& p) const {
    return {p.start ? p.start->at : *detail_position::least<C>(),
            p.end ? *lower_cover(p.end->at) : *detail_position::greatest<C>()};
  }
};
}  // namespace detail_runs
/** @brief The runs of a two-valued source of a discrete chain with ends, as a
 *  view of closed pairs. */
export template <IsStepSource S>
  requires HasCoveringStep<typename S::Domain> &&
           (detail_position::least<typename S::Domain>().has_value())
constexpr auto runs(const S& s) {
  return std::views::transform(pieces(s),
                               detail_runs::Closed<typename S::Domain>{});
}

/** @brief A set of the chain @c V valued in @c L, materialised: its runs by
 *  start, each up to its end with its label, ⊥ never stored, so a gap is a
 *  missing key.  The one stored form, for a value that must outlive the term
 *  it was read from (a Python handle); a step source like any other.
 *  @tparam V the carrier.  @tparam L the species. */
export template <typename V = long long, typename L = Boole>
  requires IsTotallyOrdered<V> && IsOckhamAlgebra<L>
struct Runs : dedekind::sets::SetExpr<Runs<V, L>, V, L> {
  using Domain = V;
  using Codomain = typename L::Ω;
  using logic_species = L;
  using cardinality_type = carrier_cardinality_t<V>;
  using Ω = typename L::Ω;
  /** @brief A run up to its end (@c nullopt: to the carrier's top), and its
   *  label. */
  struct Labelled {
    std::optional<Position<V>> end;
    Ω label;
  };
  /** @brief The order of run starts: below every element first. */
  struct ByStart {
    bool operator()(const std::optional<Position<V>>& a,
                    const std::optional<Position<V>>& b) const {
      return b && (!a || *a < *b);
    }
  };
  using Map = std::map<std::optional<Position<V>>, Labelled, ByStart>;

  /** @brief The empty set. */
  Runs() = default;
  static Runs empty() { return {}; }
  static Runs universe() { return Runs(ConstantSteps<V, L>{L::True}); }
  /** @brief Materialise a source: drain it, keeping the runs not at ⊥. */
  template <IsStepSource S>
    requires std::same_as<typename S::Domain, V> &&
             std::same_as<typename S::logic_species, L>
  explicit Runs(const S& s) {
    std::optional<Position<V>> start;
    Ω label = s.first();
    auto c = s.cursor();
    while (const auto st = c.pull()) {
      if (label != L::False) runs_.emplace(start, Labelled{st->at, label});
      start = st->at;
      label = st->value;
    }
    if (label != L::False) runs_.emplace(start, Labelled{std::nullopt, label});
  }

  /** @brief The runs, by start. */
  const Map& runs() const { return runs_; }
  /** @brief χ(x): the label of the run containing @c x, ⊥ in a gap. */
  Ω operator()(const V& x) const {
    auto it = runs_.lower_bound(Position<V>{x, Side::Above});
    if (it == runs_.begin()) return L::False;
    --it;
    const auto& end = it->second.end;
    const bool before_end =
        !end || (end->side == Side::Below ? x < end->at : !(end->at < x));
    return before_end ? it->second.label : L::False;
  }
  bool contains(const V& x) const { return (*this)(x) == L::True; }

  /** @brief A run list as a source: its runs' starts and ends, read in order
   *  through the map. */
  struct Cursor {
    typename Map::const_iterator it, end;
    std::optional<Step<V, L>> pending;
    std::optional<Step<V, L>> pull() {
      if (pending) return std::exchange(pending, std::nullopt);
      while (it != end) {
        const auto& [start, run] = *it++;
        if (run.end) pending = Step<V, L>{*run.end, L::False};
        if (start) return Step<V, L>{*start, run.label};
        if (pending) return std::exchange(pending, std::nullopt);
      }
      return std::nullopt;
    }
  };
  Ω first() const {
    return !runs_.empty() && !runs_.begin()->first ? runs_.begin()->second.label
                                                   : L::False;
  }
  Cursor cursor() const { return {runs_.begin(), runs_.end(), std::nullopt}; }

 private:
  Map runs_;
};

static_assert(dedekind::sets::IsSetObject<Runs<int>> &&
                  IsStepSource<Runs<int>> &&
                  IsStepSource<Runs<Ternary, Kleene>>,
              "a run list is a set object over its universe, and a source.");

/** @section runs__Formal_Verification */
namespace detail_runs_witness {
using B = SpanSteps<int, Boole>;
using And = detail_runs::And<Boole>;
using Or = detail_runs::Or<Boole>;
constexpr auto closed = Strictness::NonStrict;
constexpr B span(int lo, int hi) {
  return {Span<int>{Bound<int>{lo, closed}, Bound<int>{hi, closed}}};
}
constexpr RelabelSteps<B, detail_runs::Rfl<Boole>> without(int p) {
  return {span(p, p), {}};
}
static_assert(
    equal(MergeSteps<B, decltype(without(5)), And>{span(0, 10), without(5), {}},
          MergeSteps<B, B, Or>{span(0, 4), span(6, 10), {}}),
    "[0,10] ∩ ¬{5} = [0,4] ∪ [6,10], by one traversal");
static_assert(std::ranges::distance(runs(without(5))) == 2 &&
                  std::ranges::distance(runs(ConstantSteps<int, Boole>{
                      false})) == 0,
              "¬{5} is two runs, Ø none");
}  // namespace detail_runs_witness

}  // namespace dedekind::order
