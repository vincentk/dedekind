/**
 * @file dedekind/sequences/pst.cppm
 * @partition :pst
 * @brief The Pst fragment decided by one fold along the chain: the normal form
 *        of a set over a finite chain is not stored, it is read off by
 *        evaluating χ from ⊥ to ⊤ by the covering step.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section pst__One_Fold_Five_Monoids
 * A set over a finite chain (@c IsFiniteLSet) is a finite table @f$\chi : C
 * \to L@f$, and normalisation by evaluation is complete for it.  Every query
 * below is the same @c fold over @c chain_view(lo, hi), differing only in the
 * monoid threaded through it:
 *
 *   - @c exists   @f$\bigvee \chi@f$ in @f$L@f$ (the Goguen @f$\exists@f$);
 *   - @c forall   @f$\bigwedge \chi@f$ in @f$L@f$ (the Goguen @f$\forall@f$);
 *   - @c equal    @f$\bigwedge (\chi_A(x) = \chi_B(x))@f$, in @c bool: the two
 *                 tables are finite and fully known, so equality is decided;
 *   - @c subset   @f$\bigwedge (\chi_A(x) \le \chi_B(x))@f$, Goguen's L-subset;
 *   - @c runs     the maximal runs of constant level @f$\ne \bot@f$, emitted as
 *                 the fold crosses a level change: the α-cuts read off (the run
 *                 of level @f$\ell@f$ lies in every cut @f$\{\chi \ge
 *                 \ell'\}@f$ with @f$\ell' \le \ell@f$).  Boole has one level,
 *                 @f$K_3@f$ two.
 *
 * O(N) steps in the length of the chain, O(1) state (the accumulator): nothing
 * is allocated, no run-list is materialised.  The chain's ends are @b values,
 * so a window @c [lo, hi] on an integral chain is a runtime bound (the budget
 * at the consumption site, #981) while the truth chains supply their ends from
 * their species.  Distributivity needs no rule here: evaluation distributes.
 *
 * @section pst__What_This_Is_Not
 * Not the unbounded chain (ℤ, @c long @c long), whose fold would have to jump
 * between the term's cut points; that is the next slice, where @c SetVal's
 * kind tag retires.  Not the dense carriers (ℚ, ℝ): no covering step, no
 * finite table --- Lwv, not Pst.
 */
module;

#include <concepts>
#include <cstddef>
#include <iterator>
#include <optional>
#include <ranges>
#include <type_traits>

export module dedekind.sequences:pst;

import dedekind.category;
import dedekind.order;
import dedekind.sets;
import :fold;
import :ranges;

namespace dedekind::sequences {
using namespace dedekind::category;
using dedekind::sets::Ø;
using dedekind::sets::𝔸;

/** @section pst__Chain_Ends
 *  A truth chain's ends are its species' ⊥ and ⊤ (@c 𝔹: @c false / @c true;
 *  @f$K_3@f$: @c False / @c True).  An integral chain has no honest ends to
 *  scan (@c int's range is a window of 2³² steps, not a model), so its folds
 *  take the window's ends as values. */
export template <IsPst C>
constexpr C chain_bottom() {
  return classifier_logic_t<C>::False;
}
export template <IsPst C>
constexpr C chain_top() {
  return classifier_logic_t<C>::True;
}

/** @brief A set object over a finite chain, as the sets layer represents it:
 *  a leaf (@c IsFiniteLSet) or a lattice node over leaves, each a finite table
 *  @f$\chi : C \to L@f$.  The lattice nodes carry @c Domain and
 *  @c logic_species but not the subobject vocabulary (@c Member, @c ι), so the
 *  gate is @c sets::IsSetObject rather than @c IsLSet.
 *  @tparam S the set. */
export template <typename S>
concept IsPstSet = dedekind::sets::IsSetObject<S> &&
                   IsFiniteChain<typename std::remove_cvref_t<S>::Domain> &&
                   requires { typename std::remove_cvref_t<S>::logic_species; };
static_assert(IsFiniteLSet<𝔸<bool>> && IsPstSet<𝔸<bool>>,
              "a leaf over a finite chain is a Pst set.");

namespace detail_pst {
/** @brief The monoid of @c exists: @f$acc \gets acc \vee \chi(x)@f$ in @c L.
 *  @tparam S the set. */
template <IsPstSet S>
struct JoinOfChi {
  const S& s;
  constexpr void operator()(typename S::logic_species::Ω& acc,
                            const typename S::Domain& x) const {
    acc = S::logic_species::OR(acc, s(x));
  }
};
/** @brief The monoid of @c forall: @f$acc \gets acc \wedge \chi(x)@f$ in @c L.
 *  @tparam S the set. */
template <IsPstSet S>
struct MeetOfChi {
  const S& s;
  constexpr void operator()(typename S::logic_species::Ω& acc,
                            const typename S::Domain& x) const {
    acc = S::logic_species::AND(acc, s(x));
  }
};
/** @brief The monoid of @c equal: the tables agree at every point seen so far.
 *  @tparam A the left set.  @tparam B the right set. */
template <IsPstSet A, IsPstSet B>
struct Agree {
  const A& a;
  const B& b;
  constexpr void operator()(bool& acc, const typename A::Domain& x) const {
    acc = acc && (a(x) == b(x));
  }
};
/** @brief The monoid of @c subset: @f$\chi_A \le \chi_B@f$ at every point seen
 *  so far, in the chain order of @c L::Ω.
 *  @tparam A the left set.  @tparam B the right set. */
template <IsPstSet A, IsPstSet B>
struct Below {
  const A& a;
  const B& b;
  constexpr void operator()(bool& acc, const typename A::Domain& x) const {
    acc = acc && (a(x) <= b(x));
  }
};
}  // namespace detail_pst

/** @brief Two sets over one finite chain, answering in one species: the operand
 *  sort of @c equal and @c subset.
 *  @tparam A the left set.  @tparam B the right set. */
export template <typename A, typename B>
concept IsPstPair =
    IsPstSet<A> && IsPstSet<B> &&
    std::same_as<typename A::Domain, typename B::Domain> &&
    std::same_as<typename A::logic_species, typename B::logic_species>;

/** @brief @f$\exists x \in [lo, hi].\ \chi(x) = \bigvee_{x} \chi(x)@f$ in
 *  @c L: the Goguen existential, the Pst filling of the L-valued quantifier
 *  (@c sets:quantifier).  @c Unknown is an honest answer on @f$K_3@f$.
 *  @tparam S the set over a finite chain.
 *  @param s the set.  @param lo the window's bottom.  @param hi its top.
 *  @return the join of χ over the window, in @c L::Ω. */
export template <IsPstSet S>
constexpr typename S::logic_species::Ω exists(const S& s,
                                              const typename S::Domain& lo,
                                              const typename S::Domain& hi) {
  using L = typename S::logic_species;
  return fold(chain_view{lo, hi}, L::False, detail_pst::JoinOfChi<S>{s});
}
/** @brief @f$\exists@f$ over the whole truth chain, ⊥ to ⊤.
 *  @tparam S the set over a truth chain. */
export template <IsPstSet S>
  requires IsPst<typename S::Domain>
constexpr typename S::logic_species::Ω exists(const S& s) {
  using C = typename S::Domain;
  return exists(s, chain_bottom<C>(), chain_top<C>());
}
/** @brief @f$\forall x \in [lo, hi].\ \chi(x) = \bigwedge_{x} \chi(x)@f$ in
 *  @c L: the Goguen universal.
 *  @tparam S the set over a finite chain.
 *  @param s the set.  @param lo the window's bottom.  @param hi its top.
 *  @return the meet of χ over the window, in @c L::Ω. */
export template <IsPstSet S>
constexpr typename S::logic_species::Ω forall(const S& s,
                                              const typename S::Domain& lo,
                                              const typename S::Domain& hi) {
  using L = typename S::logic_species;
  return fold(chain_view{lo, hi}, L::True, detail_pst::MeetOfChi<S>{s});
}
/** @brief @f$\forall@f$ over the whole truth chain, ⊥ to ⊤.
 *  @tparam S the set over a truth chain. */
export template <IsPstSet S>
  requires IsPst<typename S::Domain>
constexpr typename S::logic_species::Ω forall(const S& s) {
  using C = typename S::Domain;
  return forall(s, chain_bottom<C>(), chain_top<C>());
}

/** @brief Extensional equality on the window, decided: the two finite tables
 *  agree at every point.  @tparam A the left set.  @tparam B the right set.
 *  @param a the left set.  @param b the right set.  @param lo the window's
 *  bottom.  @param hi its top. */
export template <typename A, typename B>
  requires IsPstPair<A, B>
constexpr bool equal(const A& a, const B& b, const typename A::Domain& lo,
                     const typename A::Domain& hi) {
  return fold(chain_view{lo, hi}, true, detail_pst::Agree<A, B>{a, b});
}
export template <typename A, typename B>
  requires IsPstPair<A, B> && IsPst<typename A::Domain>
constexpr bool equal(const A& a, const B& b) {
  using C = typename A::Domain;
  return equal(a, b, chain_bottom<C>(), chain_top<C>());
}
/** @brief Goguen's L-subset on the window, decided: @f$\chi_A(x) \le
 *  \chi_B(x)@f$ at every point.  @tparam A the left set.  @tparam B the right
 *  set.  @param a the left set.  @param b the right set.  @param lo the
 * window's bottom.  @param hi its top. */
export template <typename A, typename B>
  requires IsPstPair<A, B>
constexpr bool subset(const A& a, const B& b, const typename A::Domain& lo,
                      const typename A::Domain& hi) {
  return fold(chain_view{lo, hi}, true, detail_pst::Below<A, B>{a, b});
}
export template <typename A, typename B>
  requires IsPstPair<A, B> && IsPst<typename A::Domain>
constexpr bool subset(const A& a, const B& b) {
  using C = typename A::Domain;
  return subset(a, b, chain_bottom<C>(), chain_top<C>());
}

/** @brief A maximal run of constant level @f$\ne \bot@f$: the set's χ is
 *  @c level on @c [lo, hi] and differs just outside.
 *  @tparam C the chain.  @tparam Ω the species' truth values. */
export template <typename C, typename Ω>
struct Run {
  Ω level;
  C lo;
  C hi;
  friend constexpr bool operator==(const Run&, const Run&) = default;
};

/** @brief The runs of a set over a window, as a view: the normal form read off
 *  the chain while it is walked, one run in flight.  O(1) state.
 *  @tparam S the set over a finite chain. */
export template <IsPstSet S>
class runs_view : public std::ranges::view_interface<runs_view<S>> {
 public:
  using C = typename S::Domain;
  using L = typename S::logic_species;
  using value_type = Run<C, typename L::Ω>;

  class iterator {
   public:
    using value_type = Run<C, typename L::Ω>;
    using difference_type = std::ptrdiff_t;
    using iterator_concept = std::input_iterator_tag;
    constexpr iterator() = default;
    constexpr iterator(const S* s, C lo, C hi) : s_(s), pos_(lo, hi) {
      advance();
    }
    constexpr value_type operator*() const { return *run_; }
    constexpr iterator& operator++() {
      advance();
      return *this;
    }
    constexpr void operator++(int) { ++*this; }
    friend constexpr bool operator==(const iterator& it,
                                     std::default_sentinel_t) {
      return !it.run_.has_value();
    }

   private:
    // Walk to the next level change: skip ⊥, open a run at the first x with a
    // level, extend it while the level holds, stop before the next change.
    constexpr void advance() {
      run_.reset();
      while (pos_ != std::default_sentinel) {
        const C x = *pos_;
        const typename L::Ω level = (*s_)(x);
        if (!run_) {
          if (level != L::False) run_ = value_type{level, x, x};
          ++pos_;
        } else if (level == run_->level) {
          run_->hi = x;
          ++pos_;
        } else {
          return;  // the level changed (to ⊥ or to another level): this run is
                   // complete, and pos_ stays on x for the next one
        }
      }
    }
    const S* s_ = nullptr;
    typename chain_view<C>::iterator pos_{};
    std::optional<value_type> run_{};
  };

  constexpr runs_view() = default;
  constexpr runs_view(const S& s, C lo, C hi) : s_(&s), lo_(lo), hi_(hi) {}
  constexpr iterator begin() const { return {s_, lo_, hi_}; }
  constexpr std::default_sentinel_t end() const { return {}; }

 private:
  const S* s_ = nullptr;
  C lo_{};
  C hi_{};
};

/** @brief The runs of @c s on the window @c [lo, hi].
 *  @tparam S the set over a finite chain.  @param s the set.  @param lo the
 *  window's bottom.  @param hi its top. */
export template <IsPstSet S>
constexpr runs_view<S> runs(const S& s, const typename S::Domain& lo,
                            const typename S::Domain& hi) {
  return {s, lo, hi};
}
/** @brief The runs of @c s over the whole truth chain.
 *  @tparam S the set over a truth chain. */
export template <IsPstSet S>
  requires IsPst<typename S::Domain>
constexpr runs_view<S> runs(const S& s) {
  using C = typename S::Domain;
  return {s, chain_bottom<C>(), chain_top<C>()};
}

/** @section pst__Formal_Verification */
namespace detail_pst_witness {
using dedekind::sets::π;
// 𝔹, Boole-valued: {true} and its complement.
inline constexpr auto top = 𝔸<bool>{} | (π == true);
static_assert(exists(top) && !forall(top),
              "{⊤} on 𝔹: some point is in it, not every point.");
static_assert(forall(𝔸<bool>{}) && !exists(Ø<bool>{}),
              "∀ over 𝔹 holds, ∃ over Ø fails: the units of the two monoids.");
static_assert(subset(top, 𝔸<bool>{}) && !subset(𝔸<bool>{}, top) &&
                  equal(top, top) && !equal(top, 𝔸<bool>{}),
              "⊆ and == decided on the finite table of 𝔹.");
// K₃ as the carrier, Boole-valued: {x > ⊥} = {U, ⊤}.
inline constexpr auto above_bottom = 𝔸<Ternary>{} | (π > Ternary::False);
static_assert(exists(above_bottom) && !forall(above_bottom) &&
                  forall(above_bottom, Ternary::Unknown, Ternary::True),
              "{x > ⊥} on K₃: ∃ holds; ∀ fails on the chain and holds on the "
              "window [U, ⊤].");
consteval bool one_run_u_to_top() {
  std::size_t n = 0;
  Run<Ternary, bool> last{};
  for (const auto r : runs(above_bottom)) {
    last = r;
    ++n;
  }
  return n == 1 &&
         last == Run<Ternary, bool>{true, Ternary::Unknown, Ternary::True};
}
static_assert(one_run_u_to_top(),
              "{x > ⊥} on K₃ is one run of level ⊤: [U, ⊤].");
// A union with a gap is two runs: the gap closes the first run.
inline constexpr auto above5_or_below3 =
    (𝔸<int>{} | (π > 5)) | (𝔸<int>{} | (π < 3));
consteval bool two_runs_with_a_gap() {
  std::size_t n = 0;
  Run<int, bool> first{}, second{};
  for (const auto r : runs(above5_or_below3, 0, 9)) {
    if (n == 0) first = r;
    if (n == 1) second = r;
    ++n;
  }
  return n == 2 && first == Run<int, bool>{true, 0, 2} &&
         second == Run<int, bool>{true, 6, 9};
}
static_assert(two_runs_with_a_gap(),
              "{x < 3} ∪ {x > 5} on [0, 9] is two runs: [0, 2] and [6, 9].");
// A window on the integral chain: the ends are values.
inline constexpr auto above5 = 𝔸<int>{} | (π > 5);
inline constexpr auto at_least6 = 𝔸<int>{} | (π >= 6);
static_assert(equal(above5, at_least6, 0, 20) &&
                  subset(above5, 𝔸<int>{}, 0, 20) && exists(above5, 0, 20) &&
                  !exists(above5, 0, 5) && forall(above5, 6, 20),
              "{x > 5} = {x ≥ 6} on the window [0, 20]: decided by evaluation, "
              "no closed-bound normalisation needed.");
}  // namespace detail_pst_witness

}  // namespace dedekind::sequences
