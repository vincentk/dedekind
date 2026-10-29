/**
 * @file dedekind/category/nno.cppm
 * @partition :nno
 * @brief The Natural Numbers Object — Lawvere's ETCS Axiom 9 — and the
 *        upstream home of @b Peano @b recursion / @b primitive
 *        @b recursion / the @b Peano @b successor (categorical idiom +
 *        textbook synonyms below).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section nno__Vocabulary
 *
 * The categorical and textbook idioms are interchangeable on this
 * partition — they name the same Form under different names:
 *
 *   - @b NNO @b universal @b property  ≡  @b Peano @b recursion
 *     (a.k.a.\ @b primitive @b recursion).
 *   - The arrow @c s @c : @c N @c → @c N (categorical idiom)
 *     ≡  the @b Peano @b successor.
 *   - The arrow @c z @c : @c 1 @c → @c N (categorical idiom)
 *     ≡  the @b zero @b axiom.
 *   - A morphism @c f @c : @c N @c → @c A determined by
 *     (@c a₀, @c g) (categorical idiom)
 *     ≡  a function defined by Peano recursion with base case @c a₀
 *        and recursive step @c g.
 *
 * The categorical idiom is what the @c IsNNO concept below pins
 * structurally.  The textbook idiom is recorded here so that a search
 * for @b "Peano" lands at this upstream Form, not at one of the
 * downstream @b witness @b sites where the carrier-level
 * @c successor member API is named on a specific carrier
 * (@c morphologies:archimedean for the @c IsCyclic shape;
 * @c numbers:natural for @c Cardinality's NNO witness).  See the
 * layering note in §nno__Layering_audit.
 *
 * @section nno__NNO_Universal_Property
 * In Lawvere's ETCS the natural numbers are not @b constructed --- they
 * are defined by a @b universal @b property.  An NNO consists of an
 * object @c N together with two arrows
 *
 *   @c z @c : @c 1 @c → @c N            (the zero element)
 *   @c s @c : @c N @c → @c N            (the successor map)
 *
 * such that for any object @c A together with arrows @c a₀ @c : @c 1
 * @c → @c A and @c g @c : @c A @c → @c A there exists a @b unique
 * arrow @c f @c : @c N @c → @c A making the recursion diagram commute:
 *
 *   @c f @c ∘ @c z @c = @c a₀          (initial-value clause)
 *   @c f @c ∘ @c s @c = @c g @c ∘ @c f  (step clause)
 *
 * Existence + uniqueness of @c f for any @c (A, a₀, g) is the universal
 * property; equivalently, "every Peano-recursion specification has a
 * unique total function realising it" — the engineer's honesty
 * obligation in this codebase, since C++ concepts cannot quantify
 * over the functorial content.  The structural shape ( @c z and @c s
 * as arrows of the stated signatures) @b is what the type system can
 * pin.
 *
 * @section nno__Layering_audit
 *
 * Audited 2026-05-07: the upstream home of Peano recursion / Peano
 * successor / primitive recursion @b is this partition
 * (@c :category:nno).  The downstream files that mention "Peano"
 * surface the carrier-level @b witness, not the categorical Form:
 *
 *   - @c morphologies:archimedean — has a @c T::successor(x) member
 *     API on @c IsCyclic carriers (@c Modular<N> etc.).  The
 *     numerically-coinciding-with @f$S(x) = x + 1@f$ remark there is
 *     about the @b carrier's API, not about NNO universality.
 *   - @c numbers:natural — registers @c Cardinality as the canonical
 *     @c IsNNO witness.  This is a (@c z, @c s) pair on a specific
 *     carrier; the universal property it claims-to-witness is what
 *     this partition defines.
 *
 * The layering is correct: Form upstream (@c :category:nno),
 * witnesses downstream.  The textbook-vocabulary section above is
 * the cross-reference that prevents the "Peano keeps appearing in
 * prose but is buried downstream" misreading a search-by-vocabulary
 * naturally invites.
 *
 * @section nno__This_Partition
 * Lifts ETCS Axiom 9 from the @c :etcs facade-level mention (where it
 * was previously cross-referenced as @c SpeciesTraits<unsigned>) to a
 * first-class @b concept that any carrier can witness.  Closes part of
 * #445.  Concrete carrier witnesses ( @c Cardinality from @c
 * sets:cardinality, @c Modular<N> from @c morphologies:cyclic) are
 * registered downstream where the carriers are available.
 *
 * @section nno__Form_Bias_NNO_Cardinality_ℕ
 * Per the project's Platonist stance ("Forms precede carriers"), the
 * architecture is a three-layer chain:
 *
 *   @c NNO  →  @c Cardinality  →  @c ℕ
 *
 * @li @b NNO is the @b Form — the universal property defined in this
 *     partition.  Multiple carriers may witness it.
 * @li @b Cardinality (variant @c ℕ-proxy in @c sets:cardinality) is
 *     the @b canonical @b carrier inhabiting the NNO Form, certified
 *     by the @c IsNNO witness in @c numbers:natural.  Saturating
 *     semantics ( @c ℵ_0 escalation) is the @b carrier's extra
 *     behaviour beyond the textbook NNO — the abstract NNO is
 *     purely the Peano-style universal property over a bounded /
 *     finite presentation; @c Cardinality adds @c ℵ_0 as an
 *     overflow sentinel so a machine implementation can stay
 *     honest about values that exceed its representable range.
 * @li @b ℕ (textbook symbol, in @c sets:boundaries post-#427) is an
 *     alias that @b refers to @c Cardinality — the @b symbol
 *     practitioners use.  The chain reads "NNO supplies the Form;
 *     Cardinality is the canonical witness; ℕ is the name."
 *
 * Sibling carrier witnesses ( @c Modular<2^w> as the @b bounded NNO,
 * the textbook ℤ/2^wℤ; @c unsigned @c int with the bounded-NNO
 * caveat that closure under successor only holds away from
 * @c numeric_limits<unsigned>::max() ) are downstream candidates.
 *
 * @note "Die Zahlen sind freie Schöpfungen des menschlichen Geistes;
 *        sie dienen als ein Mittel, um die Verschiedenheit der Dinge
 *        leichter und schärfer aufzufassen."
 *
 *        [Trans: "Numbers are free creations of the human mind;
 *        they serve as a means of apprehending more easily and more
 *        sharply the difference of things."]
 *       — Richard Dedekind, *Was sind und was sollen die Zahlen?*
 *         (1888), preface.
 */
module;

#include <algorithm>
#include <concepts>
#include <cstddef>
#include <cstdint>     // std::int8_t (the K₃ step)
#include <functional>  // std::invoke
#include <iterator>    // std::default_sentinel_t: where chain_view's walk ends
#include <limits>      // a machine word's ends, where its step has no value
#include <optional>    // std::optional = 1 + N, the NNO's functor
#include <ranges>      // std::ranges::view_interface: chain_view is a range
#include <type_traits>
#include <utility>  // std::pair: what a coalgebra yields

export module dedekind.category:nno;

import :logic;  // Ternary: the K₃ chain and its step
import :morphism;

namespace dedekind::category {

/**
 * @concept IsNNO
 * @brief @b Structural @b shape: @c (N, Z, S) is a Natural Numbers
 *        Object — @c Z is the zero element @c 1 @c → @c N, @c S is
 *        the successor map @c N @c → @c N.
 *
 * @details C++ concepts cannot quantify over the universal property
 * (existence + uniqueness of @c f @c : @c N @c → @c A for arbitrary
 * @c (A, a₀, g)); that is the engineer's honesty obligation.  The
 * structural shape pinned here is the operational signatures — @c Z
 * is callable with no arguments and yields an @c N (the zero
 * element); @c S is callable on an @c N and yields an @c N (the
 * successor).  Together they generate the NNO's iterated
 * presentation: @c Z(), @c S(Z()), @c S(S(Z())), ...
 *
 * The dual shape concept on the @b Functor side (NNOs as initial
 * F-algebras of @c F(X) @c = @c 1 @c + @c X) is reified in the
 * sibling partition @c :f_algebra (closes the universal-property
 * layer for #449; Pierce §5.4, Mac Lane III–VI).  The two readings
 * are operationally equivalent — the @c (z, s) pair pinned here
 * @b induces the structure map @c [z, @c s] @c : @c 1 @c + @c N @c →
 * @c N via coproduct copairing on the standard injections
 * @c inl @c : @c 1 @c → @c 1 @c + @c N and
 * @c inr @c : @c N @c → @c 1 @c + @c N.  This partition focuses on
 * the @c (z, s) shape since it is the more directly recognisable
 * one to a working mathematician.
 *
 * @tparam N The carrier object.
 * @tparam Z A nullary callable @c Z : 1 → N (the zero element).
 * @tparam S A unary callable @c S : N → N (the successor).
 *
 * @see Dedekind @cite dedekind1888zahlen, the naturals as the chain of one
 *      element under a map into itself; Peano @cite peano1889arithmetices,
 *      the same as axioms.
 */
export template <typename N, typename Z, typename S>
concept IsNNO = requires(Z z, S s, N n) {
  { z() } -> std::convertible_to<N>;
  { s(n) } -> std::convertible_to<N>;
};

/** @brief The successor arrow as a free customization point: @c successor(n)
 *  for an NNO carrier, and its partial dual @c predecessor(n).
 *
 *  @details "Is there a next element" is the NNO's successor --- an axiom of
 *  the @b category (it enters @c IsSet through @c HasETCSAxioms), not a
 *  per-carrier flag.  So the order layer's value-determined point collapse of
 *  a bounded meet (@c {x : lo < x < hi} is the single point @c succ(lo) when
 *  @c succ(lo) @c == @c pred(hi)) gates on these, @b not on @c std::integral,
 *  which is a C++ accident that would exclude the ℕ proxy @c Cardinality ---
 *  itself the canonical NNO witness.
 *
 *  Built-in integral carriers step by @c ±1.  A carrier with the
 *  @c :morphologies static-member convention @c T::successor(n) is bridged.
 *  Any other carrier provides its own overloads in its namespace, found by
 *  ADL (the ℕ proxy does so in @c :sets:cardinality; its predecessor is the
 *  monus, 0 a fixpoint dual to @c ℵ_0 under successor).  Callers spell the
 *  two-step (@c using @c dedekind::category::successor; then an unqualified
 *  call) so both the generic defaults and the ADL overloads compete. */
export template <std::integral T>
constexpr T successor(T n) noexcept {
  return static_cast<T>(n + 1);
}
export template <std::integral T>
constexpr T predecessor(T n) noexcept {
  return static_cast<T>(n - 1);
}
/** @brief Bridge to the @c :morphologies static-member convention. */
export template <typename T>
  requires requires(const T& n) {
    { T::successor(n) } -> std::convertible_to<T>;
  }
constexpr T successor(const T& n) {
  return T::successor(n);
}

/** @section nno__Pst_Chains  The step on the truth chains
 *  @f$\mathbf{Pst} = \mathbf{Jlt} \cap \mathbf{Chain}@f$: a bounded chain's
 *  cover saturates at ⊤ and its dual at ⊥, the posture @c Cardinality takes at
 *  ℵ₀.  So the shipped truth chains 𝔹 and K₃ have the NNO step (a dense Pst
 *  carrier such as the unit interval has none: density is the absence of a
 *  cover), and the order layer's point collapse reads @c {x : ⊥ < x < ⊤} on K₃
 *  as @c {Unknown}.  @c bool is
 *  integral, so its successor already saturates (@c true + 1 narrows to
 *  @c true); its predecessor must not wrap. */
export constexpr bool predecessor(bool) noexcept { return false; }
// K₃'s step (successor / predecessor) lives with Ternary, in :logic.

/** @brief A carrier with both NNO steps available: the shape a
 *  value-determined point collapse needs (@c succ(lo) @c == @c pred(hi)). */
export template <typename T>
concept HasNNOStep = requires(const T& n) {
  { successor(n) } -> std::convertible_to<T>;
  { predecessor(n) } -> std::convertible_to<T>;
};

/** @brief The successor @f$S : N \to N@f$ as an @b arrow, over any carrier with
 *  the NNO step: the one spelling of the Peano successor where an @c IsArrow is
 *  needed (@c image, @c graph, the NNO witness), in place of a struct per site.
 *  @tparam N the carrier, with @c successor / @c predecessor. */
export template <HasNNOStep N>
struct Successor {
  using Domain = N;
  using Codomain = N;
  /** @param n an element of the carrier.  @return its successor, by the
   *  carrier's own @c successor; nothrow when that is. */
  constexpr N operator()(const N& n) const noexcept(noexcept(successor(n))) {
    return successor(n);
  }
};

/** @brief The zero element @f$Z : 1 \to N@f$, the nullary @c IsNNO witness: the
 *  carrier's default value, which is the NNO's zero for the integrals and for
 *  @c Cardinality.  Nullary, so not an @c IsArrow (that concept asks for a
 *  @c Domain); a composable @f$1 \to N@f$ arrow is not needed here.
 *  @tparam N the carrier. */
export template <std::default_initializable N>
struct ZeroElement {
  using Codomain = N;
  /** @return the carrier's zero. */
  constexpr N operator()() const
      noexcept(std::is_nothrow_default_constructible_v<N>) {
    return N{};
  }
};

static_assert(IsArrow<Successor<int>>,
              "the successor is an arrow N → N (Domain, Codomain, const χ).");
static_assert(IsNNO<int, ZeroElement<int>, Successor<int>>,
              "(int, Zero, Successor) has the NNO shape.");
static_assert(Successor<int>{}(ZeroElement<int>{}()) == 1, "S(Z) = 1.");

/** @brief The predecessor @f$P : N \to N@f$ as an arrow, in the carrier's own
 *  posture at ⊥: the monus on ℕ (0 a fixpoint), @c −1 on ℤ.
 *  @tparam N the carrier, with @c successor / @c predecessor. */
export template <HasNNOStep N>
struct Predecessor {
  using Domain = N;
  using Codomain = N;
  /** @param n an element of the carrier.  @return its predecessor, by the
   *  carrier's own @c predecessor; nothrow when that is. */
  constexpr N operator()(const N& n) const noexcept(noexcept(predecessor(n))) {
    return predecessor(n);
  }
};

/** @brief The cover @f$S^{+} : N \to 1 + N@f$ (y covers x: x < y, nothing
 *  between, Davey and Priestley @cite davey2002lattices, ch.\ 1): the
 *  successor read partially,
 *  @c nullopt at a fixpoint of @c S, which on a saturating bounded chain is its
 *  top (ℵ₀ on @c Cardinality, @c True on K₃), and at a machine word's greatest
 *  value, where @c S has no value (signed) or wraps (unsigned: the ring ℤ/2^w
 *  under @c S, the chain 0 < 1 < … < 2^w − 1 under its cover); total on an
 *  unbounded chain.  Whether the step itself is monotone is the arrow's fact
 *  (@c IsMonotone<Successor<N>>, registered in @c order:completeness from the
 *  carrier's periodicity), not the carrier's.
 *  @c std::optional<N> @b is @f$1 + N@f$, the NNO's own functor
 *  @f$F(X) = 1 + X@f$.
 *  @tparam N the carrier.
 *  @param n an element.  @return the element covering @c n, if any. */
export template <HasNNOStep N>
  requires std::equality_comparable<N>
constexpr std::optional<N> cover(const N& n) {
  if constexpr (std::integral<N>)
    if (n == std::numeric_limits<N>::max()) return std::nullopt;
  const N s = successor(n);
  if (s == n) return std::nullopt;
  return s;
}
/** @section nno__Unfold  The anamorphism, and Dedekind's chain
 *  The unfold of a seed by a coalgebra @f$S \to 1 + A \times S@f$: nothing
 *  ends the walk, a pair yields an element and the next state.  This is the
 *  list anamorphism, the lens @f$[\![(g, p)]\!]@f$ of Meijer, Fokkinga and
 *  Paterson @cite meijer1991functional, with
 *  @c p the coalgebra yielding nothing: the element where it does is the
 *  delimiter and is not emitted.  Read against the Jlt paper's Figure 2
 *  (#981): the chain of counts on top, generated by the step and delimited
 *  by its fixed point.
 *
 *  | Bananas                      | here                                    |
 *  | the initial algebra of 1 + X | @c IsNNO, @c In                         |
 *  | @c out, by Lambek            | @c Prior, @c lower_cover                  |
 *  | anamorphism, a lens          | @c unfold; @c chain, Dedekind's         |
 *  | catamorphism, a banana       | @c std::ranges::fold_left, @c cata      |
 *  | hylomorphism, an envelope    | a fold over @c chain: nothing in between |
 *
 *  A forward view, O(1) state: the element, its source state, the next state,
 *  and whether the walk is live.  The coalgebra is a value (Rule 1:
 * semiregular), and two positions of one walk compare by state, so the element
 * need not be comparable.
 *  @tparam S the state.  @tparam Coalg the coalgebra, @c S to
 *  @c optional<pair<A, S>>. */
export template <std::regular S, std::semiregular Coalg>
  requires std::regular_invocable<const Coalg&, const S&> &&
           requires(std::invoke_result_t<const Coalg&, const S&> y) {
             { y.has_value() } -> std::same_as<bool>;
             { y->first };
             { y->second } -> std::convertible_to<S>;
           }
class unfold_view : public std::ranges::view_interface<unfold_view<S, Coalg>> {
  using Yield = std::invoke_result_t<const Coalg&, const S&>;

 public:
  /** @brief The elements the walk emits. */
  using element_type =
      std::remove_cvref_t<decltype(std::declval<Yield>()->first)>;
  /** @brief A position of the walk: the element, the next state, live. */
  class iterator {
   public:
    using value_type = element_type;
    using difference_type = std::ptrdiff_t;
    using iterator_concept = std::forward_iterator_tag;
    constexpr iterator() = default;
    /** @param seed the first state.  @param step the coalgebra. */
    constexpr iterator(const S& seed, Coalg step) : step_(step) {
      unfold_one(seed);
    }
    /** @return the current element, by value. */
    constexpr value_type operator*() const { return current_; }
    /** @brief Unfold the next state.  @return this iterator. */
    constexpr iterator& operator++() {
      if (live_) unfold_one(next_);
      return *this;
    }
    /** @return the position before the step. */
    constexpr iterator operator++(int) {
      iterator before = *this;
      ++*this;
      return before;
    }
    /** @return whether two positions of one walk coincide: the same source
     *  state, so, the coalgebra being a function, the same element and the
     *  same next state; the element need not be comparable.  (The next state
     *  alone would not do: a step that is not injective reaches one state
     *  from two, whose elements differ.) */
    friend constexpr bool operator==(const iterator& a, const iterator& b) {
      return a.live_ == b.live_ && (!a.live_ || a.state_ == b.state_);
    }
    /** @return whether the walk has ended. */
    friend constexpr bool operator==(const iterator& it,
                                     std::default_sentinel_t) {
      return !it.live_;
    }

   private:
    constexpr void unfold_one(const S& state) {
      if (const Yield y = std::invoke(step_, state); y.has_value()) {
        state_ = state;
        current_ = y->first;
        next_ = y->second;
        live_ = true;
      } else {
        live_ = false;
      }
    }
    value_type current_{};
    S state_{};  ///< the source of the element: positions compare by it
    S next_{};
    Coalg step_{};
    bool live_ = false;
  };
  constexpr unfold_view() = default;
  /** @param seed the first state.  @param step the coalgebra. */
  constexpr unfold_view(S seed, Coalg step) : seed_(seed), step_(step) {}
  /** @return the walk, from the seed. */
  constexpr iterator begin() const { return {seed_, step_}; }
  /** @return the sentinel the walk compares equal to once it has ended. */
  constexpr std::default_sentinel_t end() const { return {}; }
  /** @return the seed. */
  constexpr S seed() const { return seed_; }
  /** @return the coalgebra. */
  constexpr Coalg coalgebra() const { return step_; }

 private:
  S seed_{};
  Coalg step_{};
};
/** @brief The unfold of @c seed by @c step.  @tparam S the state.
 *  @tparam Coalg the coalgebra.  @param seed the first state.  @param step
 *  the coalgebra.  @return the view. */
export template <std::regular S, std::semiregular Coalg>
constexpr unfold_view<S, Coalg> unfold(S seed, Coalg step) {
  return {seed, step};
}
/** @brief The coalgebra of a partial step @f$X \to 1 + X@f$: the element,
 *  and the step as the next state, while the step has a value.  Turns
 *  @c Prior, @c lower_cover or @c cover into a lens.  @tparam X the carrier.
 *  @tparam Step the partial step. */
export template <std::regular X, std::semiregular Step>
  requires std::regular_invocable<const Step&, const X&> &&
           std::same_as<std::invoke_result_t<const Step&, const X&>,
                        std::optional<X>>
struct StepCoalgebra {
  Step step{};
  /** @param x a state.  @return @c (step(x), step(x)), the element the step
   *  lands on, as element and as next state, or nothing where the step has
   *  nothing: the chain of @c x is what the step reaches from @c x, and @c x
   *  itself is not among them (the Jlt paper's Figure 2, after Lambek). */
  constexpr std::optional<std::pair<X, X>> operator()(const X& x) const {
    if (const std::optional<X> next = std::invoke(step, x); next.has_value())
      return std::pair<X, X>{*next, *next};
    return std::nullopt;
  }
  constexpr bool operator==(const StepCoalgebra&) const = default;
};
/** @brief The chain of @c x under a partial step: what the step reaches from
 *  @c x, the step of it, then the step of that, until the step has nothing
 *  (Dedekind's chain of the step of @c x, @cite dedekind1888zahlen).  With
 *  @c Prior it is @f$n - 1, \ldots, 0@f$, the @c n elements below @c n,
 *  @c n itself excluded: the Jlt paper's Figure 2, after Lambek.
 *  @tparam X the carrier.  @tparam Step the partial step.  @param x the
 *  first element.  @param step the step.  @return the view. */
/** @brief The type of @c chain(x, step).  @tparam X the carrier.
 *  @tparam Step the partial step. */
export template <std::regular X, std::semiregular Step>
using chain_t = unfold_view<X, StepCoalgebra<X, Step>>;
export template <std::regular X, std::semiregular Step>
constexpr chain_t<X, Step> chain(X x, Step step) {
  return unfold(x, StepCoalgebra<X, Step>{step});
}

/** @section nno__Chain_As_Arrows
 *  @brief The chain in arrows (the Jlt paper, Listing 3).  Three arrows make
 *  it: an end, a predicate on the states; a step, an endo; a seed, a point.
 *  @c until(end, step) is the guard, the partial step @f$N \to 1 + N@f$ a
 *  cartesian category builds as @f$\langle end, 1\rangle ; dist ; (! +
 *  step)@f$.  @c chain(out) is the anamorphism as an arrow @f$N \to N^*@f$,
 *  so a seed composed into it, @c x >> chain(out), is its chain.
 *  @c ns >> f is the functor's action, @c f mapped over the elements, the
 *  coalgebra's emitted leg post-composed, @f$(1 + f \times 1) \circ
 *  out@f$, and @c map @c id is the chain itself, decided at the type.  A fold
 *  over the composite is a hylomorphism: nothing between the seed and the
 *  fold is built. */
export template <IsArrow End, IsArrow Step>
  requires std::same_as<Dom<End>, Dom<Step>> &&
           std::same_as<Cod<Step>, Dom<Step>> &&
           std::convertible_to<Cod<End>, bool>
struct Until {
  using Domain = Dom<Step>;
  using Codomain = std::optional<Domain>;
  End end;
  Step step;
  /** @param n a state.  @return nothing at the end, else the step of @c n. */
  constexpr Codomain operator()(const Domain& n) const {
    if (end(n)) return std::nullopt;
    return std::optional<Domain>{step(n)};
  }
  constexpr bool operator==(const Until&) const = default;
};
/** @brief The guard.  @param end where the chain stops.  @param step the
 *  total step.  @return the partial step. */
export template <IsArrow End, IsArrow Step>
constexpr Until<End, Step> until(End end, Step step) {
  return {std::move(end), std::move(step)};
}

/** @brief The anamorphism of a coalgebra as an arrow from the seeds:
 *  @c x >> unfold<S>(c) is @c unfold(x, c).  @tparam S the state.
 *  @tparam Coalg the coalgebra. */
export template <std::regular S, std::semiregular Coalg>
  requires requires(const S& s, const Coalg& c) { unfold(s, c); }
struct Unfold {
  using Domain = S;
  using Codomain = unfold_view<S, Coalg>;
  Coalg coalg;
  /** @param seed the first state.  @return the walk from it. */
  constexpr Codomain operator()(const S& seed) const {
    return unfold(seed, coalg);
  }
  constexpr bool operator==(const Unfold&) const = default;
};
/** @brief The anamorphism, as an arrow.  @tparam S the state.  @param c the
 *  coalgebra. */
export template <std::regular S, std::semiregular Coalg>
constexpr Unfold<S, Coalg> unfold(Coalg c) {
  return {std::move(c)};
}
/** @brief The chain of a partial step, as an arrow from the seeds:
 *  @c x >> chain(out) is @c chain(x, out).  @param out the partial step. */
export template <IsArrow Step>
  requires std::regular<Dom<Step>> && std::semiregular<Step>
constexpr Unfold<Dom<Step>, StepCoalgebra<Dom<Step>, Step>> chain(Step step) {
  return {StepCoalgebra<Dom<Step>, Step>{std::move(step)}};
}

/** @brief The functor's action on a coalgebra: @c f on the emitted leg.
 *  @tparam S the state.  @tparam Coalg the coalgebra.  @tparam F the arrow. */
export template <std::regular S, std::semiregular Coalg, IsArrow F>
  requires std::regular_invocable<const Coalg&, const S&> &&
           requires(std::invoke_result_t<const Coalg&, const S&> y) {
             { y->first } -> std::convertible_to<Dom<F>>;
             { y->second } -> std::convertible_to<S>;
           }
struct Mapped {
  Coalg coalg;
  F f;
  /** @param s a state.  @return @c f of the element, and the next state. */
  constexpr std::optional<std::pair<Cod<F>, S>> operator()(const S& s) const {
    if (const auto y = std::invoke(coalg, s); y.has_value())
      return std::pair<Cod<F>, S>{f(y->first), y->second};
    return std::nullopt;
  }
  constexpr bool operator==(const Mapped&) const = default;
};
/** @brief @c ns >> f: @c f mapped over the walk, as a walk.  @param v the
 *  walk.  @param f the arrow. */
export template <std::regular S, std::semiregular Coalg, IsArrow F>
  requires std::same_as<Dom<F>, typename unfold_view<S, Coalg>::element_type>
constexpr unfold_view<S, Mapped<S, Coalg, F>> operator>>(
    const unfold_view<S, Coalg>& v, F f) {
  return unfold(v.seed(), Mapped<S, Coalg, F>{v.coalgebra(), std::move(f)});
}
/** @brief The functor's unit law, decided at the type: @c map @c id is the
 *  walk itself. */
export template <std::regular S, std::semiregular Coalg, IsArrow F>
  requires std::same_as<Dom<F>, typename unfold_view<S, Coalg>::element_type> &&
           IsIdentityArrow<F>
constexpr unfold_view<S, Coalg> operator>>(const unfold_view<S, Coalg>& v, F) {
  return v;
}

namespace detail_chain_arrows {
struct IsZero {
  using Domain = unsigned;
  using Codomain = bool;
  constexpr bool operator()(unsigned n) const { return n == 0; }
  constexpr bool operator==(const IsZero&) const = default;
};
constexpr auto ns = 3u >> chain(until(IsZero{}, Predecessor<unsigned>{}));
static_assert(
    std::ranges::fold_left(ns, 0u, std::plus<unsigned>{}) == 3 &&
        std::ranges::fold_left(ns >> Successor<unsigned>{}, 0u,
                               std::plus<unsigned>{}) == 6 &&
        std::same_as<decltype(ns >> Identity<unsigned>{}),
                     decltype(auto(ns))> &&
        std::ranges::fold_left(0u >> chain(until(IsZero{},
                                                 Predecessor<unsigned>{})),
                               0u, std::plus<unsigned>{}) == 0,
    "a seed into the chain of a guard is what the guard reaches from it, 2, 1, "
    "0; f mapped over it folds "
    "to f's values; map id is the chain, by resolution; the end at the seed "
    "is the empty chain");
}  // namespace detail_chain_arrows
/** @brief The coalgebra of the bounded chain @c [lo, hi]: over the state
 *  @f$1 + C@f$, since past @c hi is no element, and the cover of a saturating
 *  top is nothing.  @tparam C the chain. */
template <HasNNOStep C>
  requires std::totally_ordered<C>
struct Below {
  C hi{};
  /** @param s nothing, or an element.  @return the element and its cover,
   *  while the element is at most @c hi. */
  constexpr std::optional<std::pair<C, std::optional<C>>> operator()(
      const std::optional<C>& s) const {
    if (s.has_value() && *s <= hi)
      return std::pair<C, std::optional<C>>{*s, cover(*s)};
    return std::nullopt;
  }
  constexpr bool operator==(const Below&) const = default;
};
/** @section nno__Chain_View
 *  The bounded chain @c [lo, hi] as a range for @b any carrier with the
 *  covering step --- @c bool, @f$K_3@f$, the integrals, ℕ's proxy: the unfold
 *  of @c lo by @c cover, ending after @c hi, or at a saturating top short of
 *  it.  @c std::views::iota cannot do this (@c Ternary has no @c ++,
 *  @c Cardinality no @c difference_type), so @c sequences::to_iota_view is
 *  this view's integral face.  Holds an @c unfold_view over the state
 *  @f$1 + C@f$ (#981): no arithmetic of its own on the bounds, and no base
 *  class, since the posture has no subtyping.
 *  @tparam C the chain, with the covering step. */
export template <HasNNOStep C>
  requires std::totally_ordered<C>
class chain_view : public std::ranges::view_interface<chain_view<C>> {
  using Walk = unfold_view<std::optional<C>, Below<C>>;

 public:
  using iterator = typename Walk::iterator;
  constexpr chain_view() = default;
  /** @param lo the window's bottom.  @param hi its top (inclusive). */
  constexpr chain_view(C lo, C hi)
      : walk_(std::optional<C>{lo}, Below<C>{hi}) {}
  /** @return the walk, starting at @c lo. */
  constexpr iterator begin() const { return walk_.begin(); }
  /** @return the sentinel the walk compares equal to once it has ended. */
  constexpr std::default_sentinel_t end() const { return {}; }

 private:
  Walk walk_{};  // held, not inherited: the posture has no subtyping
};

/** @brief The lower cover @f$P^{+} : N \to 1 + N@f$, dual to @ref cover: the
 *  predecessor read partially, @c nullopt at the least element (a fixpoint of
 *  @c P, or a machine word's least value).  Not the destructor @c Prior, which
 *  is nothing at @b zero.  @tparam N the carrier.  @param n an element.
 *  @return the element @c n covers, if any. */
export template <HasNNOStep N>
  requires std::equality_comparable<N>
constexpr std::optional<N> lower_cover(const N& n) {
  if constexpr (std::integral<N>)
    if (n == std::numeric_limits<N>::min()) return std::nullopt;
  const N p = predecessor(n);
  if (p == n) return std::nullopt;
  return p;
}

/** @brief The NNO's structure map @f$[Z, S] : 1 + N \to N@f$ as an arrow:
 *  zero, or the successor of the element given.  @c std::optional<N> @b is
 *  @f$1 + N@f$, the NNO's own functor @f$F(X) = 1 + X@f$.
 *  @tparam N the carrier. */
export template <HasNNOStep N>
  requires std::equality_comparable<N> && std::default_initializable<N>
struct In {
  using Domain = std::optional<N>;
  using Codomain = N;
  /** @param x nothing, or an element.  @return @c Z() or @c S(x). */
  constexpr N operator()(const std::optional<N>& x) const {
    return x ? Successor<N>{}(*x) : ZeroElement<N>{}();
  }
};
/** @brief The destructor @f$\langle \text{is}\;Z?,\, P\rangle : N \to 1 + N@f$:
 *  nothing at zero, else the predecessor.  @tparam N the carrier. */
export template <HasNNOStep N>
  requires std::equality_comparable<N> && std::default_initializable<N>
struct Prior {
  using Domain = N;
  using Codomain = std::optional<N>;
  /** @param n an element.  @return nothing at @c Z, else @c P(n). */
  constexpr std::optional<N> operator()(const N& n) const {
    if (n == ZeroElement<N>{}()) return std::nullopt;
    return Predecessor<N>{}(n);
  }
};
/** @brief The default chain of a carrier with the NNO's step, the one its
 *  certificate supplies: @c n >> chain<N>() is what @c Prior<N> reaches from
 *  @c n, @c n - 1, ..., 0 (the Jlt paper's Listing 3).  @tparam N the
 *  carrier. */
export template <HasNNOStep N>
  requires std::equality_comparable<N> && std::default_initializable<N> &&
           std::regular<N>
constexpr auto chain() {
  return chain(Prior<N>{});
}

/** @section nno__Lambek  Lambek's lemma as the honesty obligation
 *  The structure map @f$[Z, S] : 1 + N \to N@f$ of the @b initial algebra is an
 *  isomorphism, with inverse @f$\langle \text{is}\;Z?,\, P\rangle@f$.  Here
 * that reads @c IsIsomorphism<In<N>> (@c :morphism), which an @c inverse(In<N>)
 *  overload would declare.  No shipped carrier declares one, each for its own
 *  reason, witnessed on values below and in @c numbers: ℤ (@c int,
 *  @c SignedCardinality) is a group, @c S(−1) @c = @c Z; the bounded chains
 *  (K₃, ℕ's proxy) saturate, so @c S is not injective at ⊤ (the largest finite
 *  cardinal and ℵ₀ both step to ℵ₀); and the signed machine integers have no
 *  value at @c S(max) at all, the same refusal as their ring claim
 *  (@c sets:cardinality).  Likewise no step is declared a bijection: ℤ's proxy
 *  saturates, and @c S ⊣ P on it is registered as the adjunction it is
 *  (@c numbers:integer), not derived from an iso it is not. */

static_assert(cover(5) == 6 && !cover(std::numeric_limits<int>::max()) &&
                  lower_cover(5) == 4 &&
                  !lower_cover(std::numeric_limits<int>::min()) &&
                  !lower_cover(Ternary::False) && lower_cover(-5) == -6,
              "the covers of 5 are 6 and 4; a machine word's ends have none.");
static_assert(In<int>{}(Prior<int>{}(5)) == 5,
              "In ∘ Prior = id (every carrier with the step).");
static_assert(
    Prior<int>{}(In<int>{}(std::optional<int>{-1})) != std::optional<int>{-1},
    "Prior ∘ In ≠ id on ℤ: S(−1) = Z, so [Z, S] is not an iso --- ℤ has "
    "the NNO shape but is a group, not the NNO.");
static_assert(!IsIsomorphism<In<int>> && !IsIsomorphism<Successor<int>>,
              "on ℤ the structure map is not an iso, and the machine step is "
              "not declared one: S(max) has no value.");
static_assert(
    !cover(std::numeric_limits<unsigned>::max()) &&
        successor(std::numeric_limits<unsigned>::max()) == 0u,
    "on a word the cover stops at 2^w − 1 where the ring's step wraps "
    "to 0.");
static_assert(successor(Ternary::False) == Ternary::Unknown &&
                  successor(Ternary::Unknown) == Ternary::True &&
                  successor(Ternary::True) == Ternary::True,
              "the K₃ step: ⊥ → U → ⊤, saturating at ⊤.");
static_assert(!cover(Ternary::True) && cover(Ternary::Unknown) == Ternary::True,
              "the cover is partial at ⊤.");
static_assert(Prior<Ternary>{}(In<Ternary>{}(std::optional<Ternary>{
                  Ternary::True})) != std::optional<Ternary>{Ternary::True} &&
                  !IsIsomorphism<In<Ternary>> &&
                  !IsIsomorphism<Successor<Ternary>>,
              "Lambek fails on K₃: a bounded chain is not an NNO, and its step "
              "is no bijection.");

/**
 * @brief Recursion-via-universal-property (operational discharge).
 *
 * @details Given an NNO witness @c (N, Z, S), an initial value @c a₀
 * of type @c A, and a step function @c g : A → A, materialises the
 * unique morphism @c f : N → A in its iterated presentation:
 *
 *   @c f(0) @c = @c a₀
 *   @c f(n+1) @c = @c g(f(n))
 *
 * Operationally, this is bounded count-driven iteration: the caller
 * supplies a count @c n (machine @c std::size_t) and gets back
 * @c f(n) by composing @c g with itself @c n times starting from
 * @c a₀.  The count is the iteration driver — not the NNO element
 * itself, since materialising a transfinite NNO element through
 * machine-bounded iteration is structurally impossible.  The
 * universal-property claim that @c f exists and is unique remains
 * the honesty obligation; what this combinator does is realise @c f
 * @b at @b a @b finite @b index.
 *
 * For NNO carriers like @c Cardinality that admit a transfinite
 * @c ℵ_0 element, the recursion at the transfinite index is
 * deliberately not provided by this combinator — callers needing
 * that case state their own saturating semantics explicitly (a
 * proof obligation discharge separate from the count-bounded
 * recursion below).
 *
 * @tparam A The recursion target type.
 * @tparam G A unary callable @c G : A → A (the step function).
 *
 * @param a0  The initial value @c f(0).
 * @param g   The step function.
 * @param n   The count of iterations (the index into the NNO at
 *            which to evaluate @c f).
 *
 * @return @c f(n) — the result of applying @c g to @c a₀ exactly
 *         @c n times.
 */
export template <typename A, typename G>
  requires std::regular_invocable<const G&, const A&> &&
           std::convertible_to<std::invoke_result_t<const G&, const A&>, A> &&
           std::copyable<A>
constexpr A nno_iterate(A a0, G g, std::size_t n) {
  // The NNO's face of iterate (:morphism): n applications of g to a0.
  return iterate(g, a0, n);
}

}  // namespace dedekind::category
