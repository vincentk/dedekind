/**
 * @file dedekind/numbers/natural.cppm
 * @brief The Dictionary of Species (The Registry).
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @partition :numbers
 * @build_order 7
 * @dependency :algebra, :topology, :cardinalities
 *
 * @section natural__Numbers
 * This partition is the final "Registry" of the ontology. It maps concrete
 * C++ types to their formal algebraic and topological identities.
 *
 * @details
 * We "Bless" the coordinate species by verifying their rungs on the ladder:
 * - IsNatural  : N (ℕ) - The Discrete Monoid.
 * - IsInteger  : Z (ℤ) - The Euclidean Group.
 * - Rational<I> : Q (ℚ) - The Countable Dense Field.
 * - Real<I> / IEEE<F> : R (ℝ) - Exact and floating-point realisations.
 *
 * @section natural__Structural_Mapping
 * This is where we perform the final 'Lifting'. We prove that
 * @c SignedExtensionalCardinal<> satisfies @c Group_ℤ (per the strict
 * @c IsCommutativeRing witness pinned in @c integer.cppm) and that
 * @c double is a hardware-constrained approximation of an exact field.
 *
 * @anchors C++ Fundamental Types: bool, char, int, long, float, double.
 *
 * Wikipedia: Number, Natural number, Integer, Rational number, Real number
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "Ubi ex mirabili magisterio in arte per novem figuras Indorum
 * introductus, scientia artis in tantum mihi pre ceteris placuit, et
 * intellexi ad illam, quod, quicquid studebatur ex ea apud Egyptum,
 * Syriam, Greciam, Siciliam et Provinciam cum suis variis modis, ad
 * que loca negotiationis tam postea peragravi per multum studium et
 * disputationis didici conflictum."
 * [Trans: "There, having been introduced to that art by a marvelous
 * method of teaching by means of the nine figures of the Indians, the
 * knowledge of the art so pleased me above all others, and I came to
 * understand it, that whatever was studied of it in Egypt, Syria,
 * Greece, Sicily, and Provence, and their various methods, to which
 * places of business I afterwards travelled — through much study and
 * the contest of disputation, I learned."]
 *       — Leonardo Pisano (Fibonacci), *Liber Abaci*, Prologus (1202;
 *         Boncompagni edition, Rome 1857).
 */
module;

#include <concepts>
#include <functional>
#include <utility>  // std::forward (used in embed_𝔹_ℕ's set-level lift)

export module dedekind.numbers:natural;

import dedekind.algebra; // HasRingOperators / HasSemiringOperators / IsArithmeticRing (canonical-spine witnesses)
import dedekind.category;
import dedekind.order; // HasLatticeOperators (canonical-spine witnesses)
import dedekind.sequences; // IsFiniteSequence (canonical-spine witnesses on FinitePath<Cardinality>)
import dedekind.sets;
import :scalars;
import :boolean;

namespace dedekind::numbers {
using namespace dedekind::category;
using namespace dedekind::sets;

/**
 * @concept IsNatural
 * @brief Structural concept for a commutative semiring with total order
 *        (the intensional ℕ).
 *
 * @details Deliberately *not* restricted to `std::unsigned_integral<N>` so
 * that user-defined certified natural-number types (e.g.
 * `ExtensionalCardinal<N>`) can satisfy the concept without being built-in C++
 * types.  The required operations are exactly those that characterise ℕ as a
 * commutative semiring with a total order:
 *
 *  - Additive monoid: `+`.
 *  - Multiplicative monoid: `*`.
 *  - Total order: `<=`.
 *  - No subtraction required — that is what distinguishes ℕ from ℤ.
 *
 * **Embedding from `std::unsigned_integral`:** machine unsigned types are the
 * extensional/IEEE-policy approximation of ℕ.  Use
 * `embed_unsigned_integral<N>(v)` to inject a machine value into a certified
 * `IsNatural` domain, and `realize_to_size_t(sentinel)` to project back.
 *
 * Wikipedia: Semiring, Peano axioms
 */
export template <typename N>
concept IsNatural = dedekind::algebra::HasSemiringOperators<N> &&
                    dedekind::category::IsCommutativeMonoid<N, std::plus<N>> &&
                    dedekind::order::IsTotallyOrdered<N>;

/**
 * @brief Canonical embedding 𝔹 ↪ ℕ: bool → unsigned.
 * @details False maps to 0, True maps to 1.
 */
export inline constexpr auto embed_𝔹_uint_ =
    arrow<bool, unsigned>([](const bool& b) noexcept { return b ? 1u : 0u; });

/**
 * @brief Canonical embedding 𝔹 ↪ ℕ: bool → Cardinality.
 * @details False maps to @c finite_cardinality(0), True to
 *          @c finite_cardinality(1).
 *
 * Sister arrow to @c embed_𝔹_uint_ above; this one lands in the
 * @c Cardinality carrier (the canonical @c IsNatural witness of
 * @c ℕ in the set-builder layer) directly, without routing through
 * the machine-width @c unsigned proxy.  Filed against #602 layer 1
 * (the set-level lift @c embed_𝔹_ℕ below uses this arrow to delegate
 * through the existing @c sets::image dispatch).
 */
export inline constexpr auto embed_𝔹_ℕ_ =
    arrow<bool, Cardinality>([](const bool& b) noexcept -> Cardinality {
      return finite_cardinality(b ? 1 : 0);
    });

/**
 * @brief Set-level lift of @c embed_𝔹_ℕ_: image of a Boolean set
 *        @c S under the canonical mono 𝔹 ↪ ℕ.
 *
 * @details Layer-1 entry per #602: names the construction at the
 * call site rather than re-spelling @c image(embed_𝔹_ℕ_, S).  The
 * accepted input @c S is anything @c dedekind::sets::image already
 * dispatches on --- @c SingletonSet (@c :sets:singleton),
 * @c std::set<bool> / @c std::unordered_set<bool> (@c :sets:extensional);
 * lazy predicate sets join the dispatch table when #602's layer 2
 * lands.  This is structurally the union of the @c IsSet
 * universe-value carriers and the std-container carriers that
 * @c image accepts today; the requires-clause checks well-formedness
 * directly rather than gating through a single concept (which would
 * exclude one or the other tier).
 *
 * Mathematically: the image of @c S under the canonical mono
 * 𝔹 ↪ ℕ is a subset of @c {0, @c 1} ⊂ @c ℕ containing whichever
 * @c bool elements are in @c S.
 */
export template <typename S>
  requires requires(S&& s) {
    dedekind::sets::image(embed_𝔹_ℕ_, std::forward<S>(s));
  }
constexpr auto embed_𝔹_ℕ(S&& s) {
  return dedekind::sets::image(embed_𝔹_ℕ_, std::forward<S>(s));
}

/**
 * @brief Canonical injection from `std::unsigned_integral` into any
 *        `IsNatural` domain `N` via its single-argument constructor.
 *
 * @details `std::unsigned_integral` types are the machine/extensional
 * approximation of ℕ.  This arrow is the Liskov injection into a certified
 * `IsNatural` domain (e.g. `ExtensionalCardinal<K>`).  The reverse direction —
 * projecting a certified natural back to a machine width — is
 * `realize_to_size_t(sentinel)`.
 *
 * @tparam N  The target `IsNatural` type.
 * @tparam U  A `std::unsigned_integral` source type (deduced).
 */
export template <IsNatural N, std::unsigned_integral U>
constexpr N embed_unsigned_integral(U v) {
  return N{v};
}

/** @section natural__Canonical_Species_Spine
 *
 * The canonical natural-numbers species ℕ is defined upstream in
 * @c dedekind.sets:boundaries (as @c NaturalNumbersOf<L, C> with
 * alias @c ℕ = @c NaturalNumbers and value-level constant
 * @c inline @c constexpr @c ℕ @c N{}).  This partition adds the
 * @c numbers-/@c order-/@c algebra-layer witnesses that pin the
 * canonical species against drift, and provides the embedding chain
 * arrows @c 𝔹 ↪ ℕ ↪ ℤ that the upstream sets layer cannot reach.
 */

using ::dedekind::sets::N;
using ::dedekind::sets::ℕ;

/** @section natural__Formal_Verification */

// (0) Universe witness: ℕ names the universe over the Cardinality
//     carrier (post-#559).  Pre-#559, ℕ was a carrier-type alias for
//     Cardinality; post-#559 it is the value Ω<Cardinality> (a constexpr
//     UniversalSet<Cardinality, ClassicalLogic, ℵ_0>{}).  Cardinality is
//     the variant ℕ-proxy carrier (= @c std::variant<ExtensionalCardinal<>,
//     ℵ_0>) — saturating to ℵ_0 on overflow; honestly models ℕ (no
//     additive inverses; rig-not-ring).  Callers wanting the bounded
//     machine carrier explicitly spell @c unsigned @c int directly.
static_assert(std::same_as<std::remove_cvref_t<decltype(dedekind::sets::ℕ)>,
                           UniversalSet<Cardinality, ClassicalLogic, ℵ_0>>,
              "ℕ is the universe Ω<Cardinality> (post-#559).");
static_assert(
    std::same_as<
        typename std::remove_cvref_t<decltype(dedekind::sets::ℕ)>::Domain,
        Cardinality>,
    "ℕ's underlying carrier IS Cardinality — the textbook "
    "universe-over-carrier reading.");

// (0a) Relationship between ℕ (the carrier) and NaturalNumbersOf<>
//      (the predicate-set / classifier).  The predicate-set's @c Domain
//      @b is the carrier — same shape as the 𝔹 ↔ UniversalSet<bool>
//      relationship from #400.  IsSet<ℕ> itself does @b not fire (carrier types
//      carry no predicate-set surface); to participate as a set, lift
//      through the predicate-set.
static_assert(std::same_as<typename NaturalNumbersOf<>::Domain, Cardinality>,
              "NaturalNumbersOf<>::Domain is the variant ℕ-proxy carrier "
              "ℕ — predicate-set's underlying element type IS the "
              "carrier.");

// (1) IsSet anchor: the predicate-set NaturalNumbersOf<> is a bona-fide
//     set.  Witnesses the set-builder DSL entry point that survives the
//     carrier migration.
static_assert(
    dedekind::category::IsSet<decltype(dedekind::category::ambient_set<
                                       Cardinality>(NaturalNumbersOf<>{}))>,
    "NaturalNumbersOf<> is the canonical IsSet anchor for ℕ.");

// (2) Syntax (the C++ operator surface that maps to ℕ's algebra).
//   - HasSemiringOperators<unsigned int>: +, * close, with T{} and T{1}.
//   - HasRingOperators<unsigned int>: +, -, unary -, * close (modular wrap
//     gives ℕ-flavoured behaviour without true negatives, but the literal
//     operators close on the carrier).
//   - HasLatticeOperators<unsigned int>: bitwise &, |, ^, ~ close.
static_assert(
    dedekind::algebra::HasSemiringOperators<unsigned int>,
    "ℕ's machine carrier (unsigned) closes the semiring operator surface "
    "(+, *, T{}, T{1}).");
static_assert(dedekind::algebra::HasRingOperators<unsigned int>,
              "ℕ's machine carrier (unsigned) also closes the literal "
              "ring operator surface (+, binary -, unary -, *) "
              "modulo wrap.");
static_assert(dedekind::order::HasLatticeOperators<unsigned int>,
              "ℕ's machine carrier (unsigned) closes the bitwise lattice "
              "operator surface (&, |, ^, ~).");

// (3) Semantics (the algebraic structures unsigned int actually carries).
//   - Self-documenting: IsNatural / IsNaturalNumber on the canonical
//     machine carrier (asserted earlier in this partition).
//   - Strict abelian-group / ring witnesses on `unsigned int` and on the
//     exact ℕ carrier (`ExtensionalCardinal<>`) live in their respective
//     trait-registration partitions; cited here as a lookup chain.
//   - The seal `IsArithmeticRing<unsigned int>` (PR #394) certifies that
//     the strict ring proof and the literal C++ operators agree on the
//     canonical machine carrier.
static_assert(IsNatural<unsigned int>,
              "unsigned int satisfies IsNatural (commutative semiring "
              "with order; +,*,<= close on the carrier).");
// `IsNaturalNumber` alias removed under ℚ-retarget chiselling; the
// `IsNatural` concept above is the canonical witness.
static_assert(
    dedekind::algebra::IsArithmeticRing<unsigned int>,
    "unsigned int is the seal where strict ℕ-flavoured ring proof and "
    "the literal C++ operators (+, binary -, unary -, *) agree --- the "
    "canonical machine arithmetic ring under modular wrap.  Under the "
    "math-wins-over-C++ stance, this is the closest strict-ring carrier "
    "ℕ has at the machine level (the unbounded ℕ proxy lives in "
    "`Cardinality` from sets:cardinality, with ℵ_0 escalation).");
// IsRig witness on the canonical machine carrier.  Pinned at @c
// unsigned @c int so the weaker @b semiring/rig claim is visible as a
// single static_assert alongside the stronger @c IsArithmeticRing
// seal above.  This records semiring closure/structure for the @c
// IsRig concept's purposes; it does @b not claim idempotent addition
// or the absence of additive inverses for the modular machine carrier
// (where @c 1+UINT_MAX==0 so additive inverses @b do exist for every
// element, and @c a+a is generally @b not equal to @c a).  The
// textbook "rig = semiring without additive inverse" reading applies
// to the abstract ℕ; the machine carrier is a stricter ring under
// modular wrap.
static_assert(dedekind::algebra::IsRig<unsigned int, std::plus<unsigned int>,
                                       std::multiplies<unsigned int>>,
              "unsigned int satisfies the IsRig witness under + and * on "
              "the canonical machine carrier; records semiring "
              "closure/structure only, not stronger textbook ℕ laws "
              "(no idempotency claim; modular-wrap inverses exist for "
              "every element).");
// Order witnesses (explicit, for documentation purposes).  ℕ is the
// canonical totally-ordered chain 0 ≤ 1 ≤ 2 ≤ ... at the literal
// level; the spaceship and the four partial-order operators all
// fire on the carrier.  Mirrors the @b shape vs.\ @b axiom split of
// HasRingOperators / IsRing from PR #394.
static_assert(dedekind::order::HasPartialOrderOperators<Cardinality>,
              "ℕ carries the partial-order operator surface "
              "(<, <=, >, >=).");
static_assert(dedekind::order::HasTotalOrderOperators<Cardinality>,
              "ℕ carries the total-order operator surface "
              "(spaceship + the four partial-order operators).");
static_assert(dedekind::order::IsTotallyOrdered<Cardinality>,
              "ℕ is axiomatically totally ordered (the chain "
              "0 ≤ 1 ≤ 2 ≤ ...).");
// Order-domain witnesses: ℕ is a directed set (every finite subset has
// an upper bound) and a directed poset (directed + antisymmetric).
// These pin ℕ as a valid @b net-domain in the Munkres / Kelley sense:
// a net is a function from a directed set, and ℕ is the prototypical
// directed set (sequences are nets indexed by ℕ).
static_assert(dedekind::order::IsDirectedSet<Cardinality>,
              "ℕ is a directed set — the prototypical net domain.");
static_assert(dedekind::order::IsDirectedPoset<Cardinality>,
              "ℕ is a directed poset (directed + antisymmetric).");
// Sequence witness: FinitePath<ℕ> is a finite sequence enumerating
// a ℕ-prefix.  Pins ℕ as a valid @b sequence codomain: any finite
// sub-sequence of natural numbers presents as IsFiniteSequence.
static_assert(dedekind::sequences::IsFiniteSequence<
                  dedekind::sequences::FinitePath<Cardinality>>,
              "FinitePath<Cardinality> is a bona-fide finite sequence; the "
              "Cardinality carrier (carrier of the ℕ universe post-#559) is a "
              "valid sequence codomain.");

// (4) Primitive-type arrows.  ℕ is the universe @c Ω<Cardinality> (post-#559;
// underlying carrier @c Cardinality from #402, replacing the earlier
// post-#401 unsigned-int reading).  Predicate-set membership reduces to
// direct calls on the classifier @c N (the namespace-level
// @c NaturalNumbersOf<> constant from sets:boundaries):
//   - Forward (unsigned → ℕ): trivially total (every unsigned is a
//     natural).
//   - Forward into a certified IsNatural domain (e.g.\ ExtensionalCardinal<>):
//     `embed_unsigned_integral<N>(v)`.
//   - Reverse (ℕ → unsigned): for the certified domain, project via
//     `realize_to_size_t(sentinel)` (lives in sets:cardinality).
static_assert(N(0u) == ClassicalLogic::True, "0 ∈ ℕ.");
static_assert(N(42u) == ClassicalLogic::True, "42 ∈ ℕ.");
static_assert(N(-7) == ClassicalLogic::False,
              "Direct N(int) call is the ℕ-as-subset-of-ℤ classifier; "
              "rejects negatives.");

// (5) Adjacent-set arrow: 𝔹 ↪ ℕ via @c embed_𝔹_uint_ above; registered
// monic at the bottom of this partition.  The machine-layer sign
// reinterpretation @c unsigned @c → @c int lives in @c :integer
// (downstream), as @c embed_uint_sint_; the canonical variant-layer
// ℕ @c ↪ @c ℤ embedding is @c lift_ℕ_ℤ_ (also in @c :integer).

// (5a) Value- and set-level witnesses for @c embed_𝔹_ℕ_ — the variant-
// layer canonical mono 𝔹 ↪ ℕ landing directly in @c Cardinality.
// Filed against #602 layer 1: the set-level lift is the entry-point
// example called out in the issue's acceptance criteria.
static_assert(embed_𝔹_ℕ_(false) == finite_cardinality(0),
              "embed_𝔹_ℕ_(false) = 0 in the variant ℕ-proxy carrier.");
static_assert(embed_𝔹_ℕ_(true) == finite_cardinality(1),
              "embed_𝔹_ℕ_(true)  = 1 in the variant ℕ-proxy carrier.");

// Set-level lift witnesses: @c embed_𝔹_ℕ on @c SingletonSet<true>
// lands at @c finite_cardinality(1), and on @c SingletonSet<false>
// at @c finite_cardinality(0).  Both pinned at the @b value level so
// the pivot equality is constant-evaluated, not just the codomain
// type (the type-only form would only check that we land in some
// @c SingletonSet<Cardinality>, not which inhabitant).  Uses the
// existing @c image(F, SingletonSet) overload from
// @c sets:singleton; the named @c embed_𝔹_ℕ surface delegates
// through it.  Sister anchor to PR #626's @c embed_𝔹_𝕂3 witness in
// @c :boolean --- same shape, different codomain.
static_assert(
    embed_𝔹_ℕ(dedekind::sets::SingletonSet<bool, ClassicalLogic>{true}).pivot ==
        finite_cardinality(1),
    "embed_𝔹_ℕ(Singleton<true>) lands at finite_cardinality(1) on the "
    "Cardinality carrier.");
static_assert(
    embed_𝔹_ℕ(dedekind::sets::SingletonSet<bool, ClassicalLogic>{false})
            .pivot == finite_cardinality(0),
    "embed_𝔹_ℕ(Singleton<false>) lands at finite_cardinality(0) on the "
    "Cardinality carrier.");

// Concept-level witness: the result of @c embed_𝔹_ℕ realises the
// categorical image of the source set under the canonical mono
// 𝔹 ↪ ℕ — i.e. it is a Subobject of @c Cod<embed_𝔹_ℕ_> = Cardinality
// (smallest-such-subobject reading per @c :category:image).
static_assert(
    dedekind::category::IsImageOf<
        decltype(embed_𝔹_ℕ(dedekind::sets::SingletonSet<bool, ClassicalLogic>{
            true})),
        decltype(embed_𝔹_ℕ_)>,
    "embed_𝔹_ℕ(S) realises IsImageOf<result, embed_𝔹_ℕ_>: result is a "
    "Subobject of Cod<embed_𝔹_ℕ_> = Cardinality, witnessing the "
    "categorical image of S under the canonical mono 𝔹 ↪ ℕ.");

// (6) The @c std::unsigned_integral family classification (textbook
//     @c ℤ/2^wℤ stance, the universal lift @c
//     embed_uint_ℕ, the @c Modular<N> / @c IsCyclic
//     correspondence, and the width-ladder ring-hom witnesses) lives
//     in the dedicated sibling partition @c :uint.  Cross-reference
//     only here — the consolidated narrative + audit trail belongs
//     in one place.

// ---------------------------------------------------------------------------
// (7) NNO witness: @c Cardinality is the canonical inhabitant of the
//     Natural Numbers Object (ETCS Axiom 9; @c
//     dedekind.category:nno).  Closes part of #445.
// ---------------------------------------------------------------------------
//
// Architecture: NNO  →  Cardinality  →  ℕ.
//   * NNO is the Form (universal property defined in @c :nno).
//   * @c Cardinality is the canonical carrier witnessing the NNO,
//     certified below by @c IsNNO<Cardinality, cardinality_zero,
//     cardinality_succ>.  Saturating semantics (@c ℵ_0 escalation)
//     honestly handles the transfinite case.
//   * @c ℕ is the name pointing to @c Cardinality (this partition's
//     @c using ℕ alias, post-#427).
//
// The zero element @c z : 1 → ℕ is @c cardinality_zero, returning
// @c finite_cardinality(0).  The successor @c s : ℕ → ℕ is @c
// cardinality_succ, returning @c n + finite_cardinality(1) on the
// finite fragment and absorbing into @c ℵ_0 on the saturation
// regime.  The latter is @b the @b carrier's @b extra @b behaviour
// beyond the textbook NNO — the abstract NNO is purely the
// Peano-style universal property; @c Cardinality adds @c ℵ_0 as
// the honest sentinel for values that exceed the machine
// implementation's representable range.

/** @brief The zero element @c 1 → @c ℕ for the NNO universal property
 *         witness on @c Cardinality.  Nullary callable, returns
 *         @c finite_cardinality(0). */
export struct cardinality_zero {
  constexpr Cardinality operator()() const noexcept {
    return finite_cardinality(0);
  }
};

/** @brief The successor map @c ℕ → @c ℕ for the NNO universal property
 *         witness on @c Cardinality.  On the finite fragment, returns
 *         @c n + 1; on @c ℵ_0, returns @c ℵ_0 (saturation). */
export struct cardinality_succ {
  constexpr Cardinality operator()(const Cardinality& n) const noexcept {
    return n + finite_cardinality(1);
  }
};

static_assert(
    dedekind::category::IsNNO<Cardinality, cardinality_zero, cardinality_succ>,
    "Cardinality is the canonical NNO witness: "
    "z = cardinality_zero, s = cardinality_succ.  ℵ_0 "
    "saturation is the carrier's extra behaviour beyond "
    "the textbook NNO — an honest sentinel for values "
    "that exceed the machine implementation's range.");

}  // namespace dedekind::numbers

namespace dedekind::category {
template <>
inline constexpr bool
    is_monic_arrow_v<std::decay_t<decltype(dedekind::numbers::embed_𝔹_uint_)>> =
        true;
static_assert(
    IsInjective<std::decay_t<decltype(dedekind::numbers::embed_𝔹_uint_)>>,
    "embed_𝔹_uint_ (𝔹 ↪ ℕ) is registered injective.");

template <>
inline constexpr bool
    is_monic_arrow_v<std::decay_t<decltype(dedekind::numbers::embed_𝔹_ℕ_)>> =
        true;
static_assert(
    IsInjective<std::decay_t<decltype(dedekind::numbers::embed_𝔹_ℕ_)>>,
    "embed_𝔹_ℕ_ (𝔹 ↪ ℕ via Cardinality) is registered injective.");

// IsEmbeddingFunctor witness (#633 refinement quartet): @c embed_𝔹_ℕ_ is
// fully faithful (source is a discrete category, so faithfulness is
// trivial) + injective on objects (the underlying function on the two
// 𝔹-objects {false, true} maps to the two distinct Cardinality witnesses
// {finite_cardinality(0), finite_cardinality(1)}).  Refines @c IsMonicArrow
// at the functor level.
template <>
inline constexpr bool is_embedding_functor_v<
    std::decay_t<decltype(dedekind::numbers::embed_𝔹_ℕ_)>> = true;
static_assert(
    IsEmbeddingFunctor<std::decay_t<decltype(dedekind::numbers::embed_𝔹_ℕ_)>>,
    "embed_𝔹_ℕ_ realises IsEmbeddingFunctor: fully faithful + injective on "
    "objects per #633's Mac Lane CWM §IV.4 reading.");
}  // namespace dedekind::category
