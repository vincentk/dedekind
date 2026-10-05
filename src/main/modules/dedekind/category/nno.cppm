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

#include <concepts>
#include <cstddef>
#include <cstdint>     // std::int8_t (the K₃ step)
#include <functional>  // std::invoke
#include <optional>    // std::optional = 1 + N, the NNO's functor
#include <type_traits>

export module dedekind.category:nno;

import :logic;  // Ternary: the K₃ chain, whose step lives here
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
export constexpr Ternary successor(Ternary a) noexcept {
  return a == Ternary::True
             ? a
             : static_cast<Ternary>(static_cast<std::int8_t>(a) + 1);
}
export constexpr Ternary predecessor(Ternary a) noexcept {
  return a == Ternary::False
             ? a
             : static_cast<Ternary>(static_cast<std::int8_t>(a) - 1);
}

/** @brief A carrier with both NNO steps available: the shape a
 *  value-determined point collapse needs (@c succ(lo) @c == @c pred(hi)). */
export template <typename T>
concept HasNNOStep = requires(const T& n) {
  { successor(n) } -> std::convertible_to<T>;
  { predecessor(n) } -> std::convertible_to<T>;
};

/** @brief A carrier whose step is the covering map of its order: the NNO step
 *  without wrap-around.  An unsigned word is ℤ/2^w, where @c S(2^w − 1) @c = @c
 * 0 covers nothing and @c P(0) @c = @c 2^w − 1 is covered by nothing;
 * saturation at a bound (ℵ₀, ⊤) is allowed, and @c bool is the two-chain.  The
 * cover, the Lambek arrows and the order layer's monotone-step facts gate on
 * this, not on the bare step.
 *  @tparam T the carrier. */
export template <typename T>
concept HasCoveringStep =
    HasNNOStep<T> && (!std::unsigned_integral<T> || std::same_as<T, bool>);

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

/** @brief The cover @f$S^{+} : N \to 1 + N@f$: the successor read partially,
 *  @c nullopt at a fixpoint of @c S, which on a saturating bounded chain is its
 *  top (ℵ₀ on @c Cardinality, @c True on K₃); total on an unbounded chain.
 *  @c std::optional<N> @b is @f$1 + N@f$, the NNO's own functor
 *  @f$F(X) = 1 + X@f$.
 *  @tparam N the carrier.
 *  @param n an element.  @return the element covering @c n, if any. */
export template <HasCoveringStep N>
  requires std::equality_comparable<N>
constexpr std::optional<N> cover(const N& n) {
  const N s = successor(n);
  if (s == n) return std::nullopt;
  return s;
}

/** @brief The NNO's structure map @f$[Z, S] : 1 + N \to N@f$ as an arrow:
 *  zero, or the successor of the element given.  @c std::optional<N> @b is
 *  @f$1 + N@f$, the NNO's own functor @f$F(X) = 1 + X@f$.
 *  @tparam N the carrier. */
export template <HasCoveringStep N>
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
export template <HasCoveringStep N>
  requires std::equality_comparable<N> && std::default_initializable<N>
struct Out {
  using Domain = N;
  using Codomain = std::optional<N>;
  /** @param n an element.  @return nothing at @c Z, else @c P(n). */
  constexpr std::optional<N> operator()(const N& n) const {
    if (n == ZeroElement<N>{}()) return std::nullopt;
    return Predecessor<N>{}(n);
  }
};

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

static_assert(cover(5) == 6, "the cover of 5 on the unbounded chain is S(5).");
static_assert(In<int>{}(Out<int>{}(5)) == 5,
              "In ∘ Out = id (every carrier with the step).");
static_assert(
    Out<int>{}(In<int>{}(std::optional<int>{-1})) != std::optional<int>{-1},
    "Out ∘ In ≠ id on ℤ: S(−1) = Z, so [Z, S] is not an iso --- ℤ has "
    "the NNO shape but is a group, not the NNO.");
static_assert(!IsIsomorphism<In<int>> && !IsIsomorphism<Successor<int>>,
              "on ℤ the structure map is not an iso, and the machine step is "
              "not declared one: S(max) has no value.");
static_assert(
    HasCoveringStep<int> && HasCoveringStep<bool> && HasCoveringStep<Ternary> &&
        !HasCoveringStep<unsigned>,
    "the step covers on the chains; on a wrapping word S(2^w − 1) = 0 "
    "covers nothing.");
static_assert(successor(Ternary::False) == Ternary::Unknown &&
                  successor(Ternary::Unknown) == Ternary::True &&
                  successor(Ternary::True) == Ternary::True,
              "the K₃ step: ⊥ → U → ⊤, saturating at ⊤.");
static_assert(!cover(Ternary::True) && cover(Ternary::Unknown) == Ternary::True,
              "the cover is partial at ⊤.");
static_assert(Out<Ternary>{}(In<Ternary>{}(std::optional<Ternary>{
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
  requires std::invocable<G&, A const&> &&
           std::convertible_to<std::invoke_result_t<G&, A const&>, A>
constexpr A nno_iterate(A a0, G g, std::size_t n) {
  A current = a0;
  for (std::size_t i = 0; i < n; ++i) {
    current = std::invoke(g, current);
  }
  return current;
}

}  // namespace dedekind::category
