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
 * - RatExpr : Q (ℚ) - The Countable Dense Field.
 * - QuadraticReal<D> / 𝕃<F> : R (ℝ) - Exact and finite-float realisations.
 *
 * @section natural__Structural_Mapping
 * This is where we perform the final 'Lifting'. We prove that
 * @c IntExpr satisfies the strict
 * @c IsArithmeticAdditiveGroup / @c IsCommutativeRing witnesses pinned
 * in @c integer.cppm and that @c double is a hardware-constrained
 * approximation of an exact field.
 *
 * @anchors C++ Fundamental Types: bool, char, int, long, float, double.
 *
 * Wikipedia: Number, Natural number, IntExpr, RatExpr number, Real number
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
#include <cstddef>  // std::size_t (residue loop)
#include <functional>
#include <optional>   // std::nullopt (the cover at ℵ₀)
#include <stdexcept>  // std::domain_error (a non-positive modulus)
#include <utility>    // std::forward (used in embed_𝔹_ℕ's set-level lift)

export module dedekind.numbers:natural;

import dedekind.algebra; // HasRingOperators / HasSemiringOperators / IsArithmeticRing (canonical-spine witnesses)
import dedekind.category;
import dedekind.morphologies; // Modular<N> / Congruence<N,R> — the finite quotient the ℕ quantifier factors through
import dedekind.order;        // HasLatticeOperators (canonical-spine witnesses)
import dedekind.relational;   // graph / dagger: the step in the allegory
import dedekind.sequences; // IsFiniteSequence (canonical-spine witnesses on FinitePath<Numeral>)
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
 * `ExtensionalCardinal<>`) can satisfy the concept without being built-in C++
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
 * @brief Canonical embedding 𝔹 ↪ ℕ: bool → Numeral.
 * @details False maps to @c Numeral{0u}, True to @c Numeral{1u}.
 *
 * Sister arrow to @c embed_𝔹_uint_ above; this one lands in the
 * @c Numeral carrier (the canonical @c IsNatural witness of
 * @c ℕ in the set-builder layer) directly, without routing through
 * the machine-width @c unsigned proxy.  Filed against #602 layer 1
 * (the set-level lift @c embed_𝔹_ℕ below uses this arrow to delegate
 * through the existing @c sets::image dispatch).
 */
export inline constexpr auto embed_𝔹_ℕ_ = arrow<bool, Numeral>(
    [](const bool& b) noexcept -> Numeral { return Numeral{b ? 1u : 0u}; });

/**
 * @brief Set-level lift of @c embed_𝔹_ℕ_: image of a Boolean set
 *        @c S under the canonical mono 𝔹 ↪ ℕ.
 *
 * @details Layer-1 entry per #602: names the construction at the
 * call site rather than re-spelling @c image(embed_𝔹_ℕ_, S).  The
 * accepted input @c S is anything @c dedekind::sets::image already
 * dispatches on --- @c Singleton (@c :sets:singleton),
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
 * The canonical natural-numbers universe @c ℕ @c = @c 𝔸<Numeral>{} is
 * defined and witnessed upstream in @c dedekind.sets:boundaries.  This
 * partition adds the @c numbers-/@c order-/@c algebra-layer witnesses on top
 * and provides the embedding chain arrows @c 𝔹 ↪ ℕ ↪ ℤ that the upstream
 * sets layer cannot reach.
 */

using ::dedekind::sets::ℕ;

/** @section natural__Formal_Verification */

// (0a′) ℕ inhabits Ddk: it is an algebraic set.  The universe ℕ is an
//     IsSet over the Numeral carrier, and Numeral is closed under its rig
//     operations +, * (but @b not under unary -, since ℕ carries no
//     additive inverses).
//     So IsAlgebraOnSet fires: the same witness 𝔹 carries in :algebra:boolean,
//     now on ℕ.  This is the mechanical reading of ℕ as an object of
//     Ddk = Trsk ∩ Alg (Figure 1): a rig, closed under + and × but not −.
static_assert(dedekind::algebra::IsAlgebraOnSet<decltype(dedekind::sets::ℕ),
                                                std::plus<Numeral>,
                                                std::multiplies<Numeral>>,
              "ℕ is an algebraic set (Ddk): a set whose carrier Numeral is "
              "closed under the rig operations +, *.");

// (2) Syntax (the C++ operator surface that maps to ℕ's algebra).
//   - HasSemiringOperators<unsigned int>: +, * close, with T{} and T{1u}.
//   - HasRingOperators<unsigned int>: +, -, unary -, * close (modular wrap
//     gives ℕ-flavoured behaviour without true negatives, but the literal
//     operators close on the carrier).
//   - HasLatticeOperators<unsigned int>: bitwise &, |, ^, ~ close.
static_assert(
    dedekind::algebra::HasSemiringOperators<unsigned int>,
    "ℕ's machine carrier (unsigned) closes the semiring operator surface "
    "(+, *, T{}, T{1u}).");
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
    "ℕ has at the machine level (ℕ itself is `Numeral` from "
    "sets:cardinality, exact above the word).");
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
static_assert(dedekind::category::IsRig<unsigned int, std::plus<unsigned int>,
                                        std::multiplies<unsigned int>>,
              "unsigned int satisfies the IsRig witness under + and * on "
              "the canonical machine carrier; records semiring "
              "closure/structure only, not stronger textbook ℕ laws "
              "(no idempotency claim; modular-wrap inverses exist for "
              "every element).");

// ℕ's carrier @c Numeral is a strict @c IsSemiring (= @c IsRig): a
// commutative monoid under +, a monoid under *, distributive --- but @b NOT a
// ring, since ℕ has no additive inverse.  Totality holds exactly
// (@c is_exact_total in @c :cardinality: no overflow, memory the boundary).
// Pins the strict textbook reading of ℕ as a rig, alongside ℤ (ring) and ℚ
// (field).
static_assert(dedekind::category::IsSemiring<Numeral, std::plus<Numeral>,
                                             std::multiplies<Numeral>>,
              "ℕ = Numeral is a strict category::IsSemiring (rig).");
static_assert(!dedekind::category::IsRing<Numeral, std::plus<Numeral>,
                                          std::multiplies<Numeral>>,
              "ℕ = Numeral is NOT a ring --- no additive inverse (unlike ℤ).");
// Order witnesses (explicit, for documentation purposes).  ℕ is the
// canonical totally-ordered chain 0 ≤ 1 ≤ 2 ≤ ... at the literal
// level; the spaceship and the four partial-order operators all
// fire on the carrier.  Mirrors the @b shape vs.\ @b axiom split of
// HasRingOperators / IsRing from PR #394.
static_assert(dedekind::order::HasPartialOrderOperators<Numeral>,
              "ℕ carries the partial-order operator surface "
              "(<, <=, >, >=).");
static_assert(dedekind::order::HasTotalOrderOperators<Numeral>,
              "ℕ carries the total-order operator surface "
              "(spaceship + the four partial-order operators).");
static_assert(dedekind::order::IsTotallyOrdered<Numeral>,
              "ℕ is axiomatically totally ordered (the chain "
              "0 ≤ 1 ≤ 2 ≤ ...).");
// The exact carrier is the concept's own witness: rig, commutative under +,
// totally ordered, and its unit is spelled Numeral{1u}, the only spelling ℕ
// admits.  A signed constant is refused, so the embedding
// embed_unsigned_integral<Numeral> is the one road from a machine word.
static_assert(IsNatural<Numeral>, "ℕ = Numeral satisfies IsNatural.");
static_assert(
    !std::constructible_from<Numeral, int>,
    "ℕ's atoms are the unsigned words: a signed constant is refused.");
static_assert(embed_unsigned_integral<Numeral>(42u) == Numeral{42u},
              "a machine word embeds into ℕ at its own value.");
// Sequence witness: FinitePath<ℕ> is a finite sequence enumerating
// a ℕ-prefix.  Pins ℕ as a valid @b sequence codomain: any finite
// sub-sequence of natural numbers presents as IsFiniteSequence.
static_assert(dedekind::sequences::IsFiniteSequence<
                  dedekind::sequences::FinitePath<Numeral>>,
              "FinitePath<Numeral> is a bona-fide finite sequence; the "
              "Numeral carrier (carrier of the ℕ universe) is a valid "
              "sequence codomain.");

// (5) Adjacent-set arrow: 𝔹 ↪ ℕ via @c embed_𝔹_uint_ above; registered
// monic at the bottom of this partition.  The machine-layer sign
// reinterpretation @c unsigned @c → @c int lives in @c :integer
// (downstream), as @c embed_uint_sint_.

// (5a) Value- and set-level witnesses for @c embed_𝔹_ℕ_ — the canonical
// mono 𝔹 ↪ ℕ landing directly in @c Numeral.
// Filed against #602 layer 1: the set-level lift is the entry-point
// example called out in the issue's acceptance criteria.
static_assert(embed_𝔹_ℕ_(false) == Numeral{0u}, "embed_𝔹_ℕ_(false) = 0 in ℕ.");
static_assert(embed_𝔹_ℕ_(true) == Numeral{1u}, "embed_𝔹_ℕ_(true)  = 1 in ℕ.");

// Set-level lift witnesses: @c embed_𝔹_ℕ on @c η(true)
// lands at @c Numeral{1u}, and on @c η(false) at @c Numeral{0u}.  Both pinned
// at the @b value level so the pivot equality is constant-evaluated, not just
// the codomain type (the type-only form would only check that we land in some
// @c Singleton<Numeral>, not which inhabitant).  Uses the
// existing @c image(F, Singleton) overload from
// @c sets:singleton; the named @c embed_𝔹_ℕ surface delegates
// through it.  Sister anchor to PR #626's @c embed_𝔹_𝕂3 witness in
// @c :boolean --- same shape, different codomain.
static_assert(origin(embed_𝔹_ℕ(η(true))) == Numeral{1u},
              "embed_𝔹_ℕ(η(true)) lands at 1 on the Numeral carrier.");
static_assert(origin(embed_𝔹_ℕ(η(false))) == Numeral{0u},
              "embed_𝔹_ℕ(η(false)) lands at 0 on the Numeral carrier.");

// Concept-level witness: the result of @c embed_𝔹_ℕ realises the
// categorical image of the source set under the canonical mono
// 𝔹 ↪ ℕ — i.e. it is a Subobject of @c Cod<embed_𝔹_ℕ_> = Numeral
// (smallest-such-subobject reading per @c :category:image).
static_assert(
    IsImageOf<decltype(embed_𝔹_ℕ(η(true))), decltype(embed_𝔹_ℕ_)>,
    "embed_𝔹_ℕ(S) realises IsImageOf<result, embed_𝔹_ℕ_>: result is a "
    "Subobject of Cod<embed_𝔹_ℕ_> = Numeral, witnessing the "
    "categorical image of S under the canonical mono 𝔹 ↪ ℕ.");

// (6) The @c std::unsigned_integral family classification (textbook
//     @c ℤ/2^wℤ stance, the universal lift @c
//     embed_uint_ℕ, the @c Modular<N> / @c IsCyclic
//     correspondence, and the width-ladder ring-hom witnesses) lives
//     in the dedicated sibling partition @c :uint.  Cross-reference
//     only here — the consolidated narrative + audit trail belongs
//     in one place.

// ---------------------------------------------------------------------------
// (7) NNO witness: @c Numeral is the canonical inhabitant of the Natural
//     Numbers Object (ETCS Axiom 9; @c dedekind.category:nno).  Closes part
//     of #445.
// ---------------------------------------------------------------------------
//
// Architecture: NNO  →  Numeral  →  ℕ.
//   * NNO is the Form (universal property defined in @c :nno).
//   * @c Numeral is the canonical carrier witnessing the NNO, certified below
//     by @c IsNNO<Numeral, ZeroElement<Numeral>, Successor<Numeral>> ---
//     the :nno arrows over the carrier's own @c successor.  Lambek holds:
//     @c [Z, S] is an isomorphism (witnessed in @c :sets:cardinality).
//   * @c ℕ is the universe @c 𝔸<Numeral>.

static_assert(IsNNO<Numeral, ZeroElement<Numeral>, Successor<Numeral>>,
              "Numeral is the canonical NNO witness: Z = ZeroElement, S = "
              "Successor.");
static_assert(ZeroElement<Numeral>{}() == Numeral{},
              "the carrier's default value is the NNO's zero.");
// std::optional's own == throughout: the point datum's == (sets:singleton) is
// still found by ADL here, but its head no longer probes equality on the
// deduced T, so it is rejected as non-viable without re-entering itself.
static_assert(Prior<Numeral>{}(In<Numeral>{}(std::optional<Numeral>{
                  Numeral{3u}})) == std::optional<Numeral>{Numeral{3u}} &&
                  In<Numeral>{}(Prior<Numeral>{}(Numeral{})) == Numeral{},
              "out ∘ in = id and in ∘ out = id on ℕ.");
// P only RETRACTS S on ℕ: P ∘ S = id, but S ∘ P ≠ id at 0 (the monus), so the
// pair is not an adjunction here --- contrast ℤ (numbers:integer).
static_assert(Predecessor<Numeral>{}(Successor<Numeral>{}(Numeral{7u})) ==
                      Numeral{7u} &&
                  Successor<Numeral>{}(Predecessor<Numeral>{}(Numeral{})) ==
                      Numeral{1u},
              "P ∘ S = id; S ∘ P moves 0 to 1: a retraction, not an iso.");
using dedekind::relational::dagger;
using dedekind::relational::graph;
// In the allegory every map is adjoint to its converse, Γ_S ⊣ Γ_S°.  On ℕ the
// converse of the successor's graph is NOT the predecessor's graph: 0 has no
// S-preimage, while the monus sends 0 to 0.  P is the saturating totalisation
// of S°, which is what the monus is.
static_assert(
    !dagger(graph(Successor<Numeral>{}))(std::pair{Numeral{}, Numeral{}}) &&
        graph(Predecessor<Numeral>{})(std::pair{Numeral{}, Numeral{}}),
    "Γ_S° ≠ Γ_P on ℕ: (0, 0) is in Γ_P, not in Γ_S°.");

}  // namespace dedekind::numbers

namespace dedekind::category {

template <>
inline constexpr bool
    is_monic_arrow_v<std::decay_t<decltype(dedekind::numbers::embed_𝔹_ℕ_)>> =
        true;
static_assert(
    IsInjective<std::decay_t<decltype(dedekind::numbers::embed_𝔹_ℕ_)>>,
    "embed_𝔹_ℕ_ (𝔹 ↪ ℕ via Numeral) is registered injective.");

// IsEmbeddingFunctor witness (#633 refinement quartet): @c embed_𝔹_ℕ_ is
// fully faithful (source is a discrete category, so faithfulness is
// trivial) + injective on objects (the underlying function on the two
// 𝔹-objects {false, true} maps to the two distinct Numeral witnesses
// {0, 1}).  Refines @c IsMonicArrow
// at the functor level.
template <>
inline constexpr bool is_embedding_functor_v<
    std::decay_t<decltype(dedekind::numbers::embed_𝔹_ℕ_)>> = true;
static_assert(
    IsEmbeddingFunctor<std::decay_t<decltype(dedekind::numbers::embed_𝔹_ℕ_)>>,
    "embed_𝔹_ℕ_ realises IsEmbeddingFunctor: fully faithful + injective on "
    "objects per #633's Mac Lane CWM §IV.4 reading.");

// IsMonotone witness (#664 morphism vocabulary): @c false @c ↦ @c 0,
// @c true @c ↦ @c 1; the embedding preserves the canonical Boolean order
// under @c Numeral 's @c <=>.
template <>
inline constexpr bool is_monotone_v<
    std::decay_t<decltype(dedekind::numbers::embed_𝔹_ℕ_)>, std::less_equal<>> =
    true;
static_assert(IsMonotone<std::decay_t<decltype(dedekind::numbers::embed_𝔹_ℕ_)>>,
              "embed_𝔹_ℕ_ (𝔹 ↪ ℕ via Numeral) is monotone — preserves the "
              "Boolean / cardinality order.");
}  // namespace dedekind::category

// ───────────────────────────────────────────────────────────────────────────
// Finite-quotient quantifier bridge for ℕ (the numbers rung).
//
// A predicate that factors through the FINITE quotient Modular<N> (a
// morphologies::Congruence<N,R>) reduces ∃/∀ over the INFINITE carrier
// Numeral to exhausting the N residues.  This lives at :natural because it
// is the lowest rung where both dependency branches resolve: sets
// (𝔸<Numeral>, Ø, the exists/forall primaries) and morphologies (Modular,
// Congruence), which are parallel branches meeting first at numbers.  The
// overloads sit in namespace dedekind::sets so ADL on the Universe operand
// finds them (FIXME(#798): a generic finite-carrier materialisation).
// ───────────────────────────────────────────────────────────────────────────
namespace dedekind::sets {

// (The element<ℕ> % Modular<N> == bound<R> scout that reified Congruence<N,R>
// lived here; retired with the S | P fold --- the point-free congruence
// fragment is π % N == R, materialised by the operator| bridge
// below.)

/**
 * @struct ResidueClass
 * @brief The class @f$\{x \in \mathbb{N} \mid x \equiv r \pmod n\}@f$,
 *        read analytically: the materialisation of @c ℕ @c | @c (π @c % @c n
 *        @c == @c r).
 *
 * @details Both lattice-bound comparisons (Eqn 2) are decided without
 * enumerating anything: since @c x @c % @c n lies in @f$[0, n)@f$, the class
 * is inhabited exactly when @c r does (@c r itself is a member), and it is all
 * of ℕ exactly when it is inhabited and @f$n = 1@f$.  The modulus is positive
 * by construction: a negative constant meeting ℕ raises, and a division by
 * the constant 0 is refused when the term is built.
 * @tparam L the species.
 */
export template <typename L>
struct ResidueClass {
  std::size_t modulus;
  bool inhabited;  // the residue lies in [0, modulus)

  constexpr bool operator==(const Ø<Numeral, L>&) const { return !inhabited; }
  template <typename C>
  constexpr bool operator==(const 𝔸<Numeral, L, C>&) const {
    return inhabited && modulus == 1;
  }
  friend constexpr bool operator==(const Ø<Numeral, L>& e,
                                   const ResidueClass& s) {
    return s == e;
  }
  template <typename C>
  friend constexpr bool operator==(const 𝔸<Numeral, L, C>& u,
                                   const ResidueClass& s) {
    return s == u;
  }
};

/** @brief @c ℕ @c & @c (π @c % @c n @c == @c r): the residue class, decided
 *  analytically.  A residue outside @f$[0, n)@f$ is never met, so
 *  @c π @c % @c 3 @c == @c 3 is empty.  ADL finds it through the
 *  @c 𝔸<Numeral> operand.
 *  @tparam K1 the modulus's type.  @tparam K2 the residue's type. */
export template <typename L, typename C, std::integral K1, std::integral K2>
constexpr ResidueClass<L> operator&(
    const 𝔸<Numeral, L, C>&,
    const Cmp<Rel::Eq, BinOp<std::modulus<>, Projection<0>, Const<K1>>,
              Const<K2>>& c) {
  const std::size_t n = carrier_cast<std::size_t>(c.a.b.value);
  const K2 r = c.b.value;
  return ResidueClass<L>{n,
                         std::cmp_greater_equal(r, 0) && std::cmp_less(r, n)};
}

// ∃ / ∀ over 𝔸<Numeral> are the generic structural quantifiers of
// :quantifier (template <IsSet S, typename P> with !IsSet<P>, folded onto s |
// p; the point-free fragment is NOT an IsPredicate): the congruence fragment
// π % n == r materialises through the operator| above into a ResidueClass,
// whose == Ø / == 𝔸 decide.

}  // namespace dedekind::sets

namespace dedekind::numbers {

// The §3.1 exhibit (lst:quantifiers_def), decided by the generic
// template <IsSet S, IsPredicate P> exists / forall of :quantifier against the
// two lattice bounds (Eqn 2): ∃ vs ∅ (A), ∀ vs the input set S (B).

using dedekind::sets::π;

// 𝔹 = 𝔸<bool>, a finite carrier: the point-free membership fragment
// π == true materialises over {false, true} through s | p.
static_assert(dedekind::sets::exists(𝔹, π == true),
              "∃b∈𝔹. b — true is a member.");
static_assert(!dedekind::sets::forall(𝔹, π == true),
              "¬∀b∈𝔹. b — false is a counterexample.");

// ℕ = 𝔸<Numeral>, infinite: the point-free congruence fragment
// π % 3 == 0 (≡ 0 mod 3).  ∃/∀ are decided without enumeration: the class is
// never empty, and it is not all of ℕ since 3 ≠ 1.
static_assert(dedekind::sets::exists(
                  dedekind::sets::𝔸<dedekind::sets::Numeral>{}, π % 3 == 0),
              "∃x∈ℕ. 3∣x — residue 0 is divisible by 3.");
static_assert(!dedekind::sets::forall(
                  dedekind::sets::𝔸<dedekind::sets::Numeral>{}, π % 3 == 0),
              "¬∀x∈ℕ. 3∣x — residues 1,2 are counterexamples.");
static_assert(!dedekind::sets::exists(
                  dedekind::sets::𝔸<dedekind::sets::Numeral>{}, π % 3 == 3),
              "¬∃x∈ℕ. x mod 3 = 3 — a residue is below its modulus.");

}  // namespace dedekind::numbers
