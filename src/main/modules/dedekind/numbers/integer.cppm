/**
 * @file dedekind/numbers/integer.cppm
 * @partition :integer
 * @brief Minimal number taxonomy concepts for reintegration.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "Die ganzen Zahlen hat der liebe Gott gemacht, alles andere ist
 * Menschenwerk."
 *       ("God made the integers; all else is the work of man.")
 *       -- Leopold Kronecker, Jahresbericht der DMV 2 (1891, reported)
 */
module;

#include <concepts>
#include <numeric>
#include <type_traits>

export module dedekind.numbers:integer;

import dedekind.algebra;
import dedekind.category;
import dedekind.sets;
import :natural;
export import :cardinality;

namespace dedekind::numbers {
using namespace dedekind::category;
using namespace dedekind::sets;

export template <typename T>
concept IsReflectiveSpecies = std::regular<T> && requires(T a) {
  { -a } -> std::same_as<T>;
  { T{} } -> std::same_as<T>;
};

/**
 * @concept IsInteger
 * @brief Structural concept for an Euclidean integer domain.
 *
 * @details Deliberately *not* restricted to `std::signed_integral<T>` so that
 * user-defined multi-precision integer types (e.g. a sign-augmented
 * `ExtensionalCardinal<N>`) can satisfy this concept without being built-in
 * C++ types.  The required operations are exactly those used by
 * `Rational<Z>::simplify()` and the ring/field machinery:
 *
 *  - Additive group: `+`, `-` (binary and unary), `T{0}`.
 *  - Multiplicative monoid: `*`, `T{1}`.
 *  - Euclidean pair: `/` and `%` (needed by `std::gcd` and `simplify()`).
 *  - Total order: `<` (needed for canonical-sign normalisation).
 *
 * **Embedding from `std::signed_integral`:** every built-in signed integer
 * type satisfies this concept unchanged — the blanket `std::signed_integral`
 * constraint is now expressed as a static proof rather than a gating
 * condition.  Use `embed_signed_integral<Z>(v)` to inject a
 * `std::signed_integral` value into an arbitrary `IsInteger` type `Z`.
 */
export template <typename T>
concept IsInteger = IsReflectiveSpecies<T> && requires(T a, T b) {
  // Additive group
  { a + b } -> std::same_as<T>;
  { a - b } -> std::same_as<T>;
  // Multiplicative monoid
  { a * b } -> std::same_as<T>;
  // Euclidean domain (needed for std::gcd in Rational::simplify)
  { a / b } -> std::same_as<T>;
  { a % b } -> std::same_as<T>;
  // Total order (needed for canonical-sign normalisation)
  { a < b } -> std::convertible_to<bool>;
};

/**
 * @brief Canonical injection from `std::signed_integral` into any `IsInteger`
 *        domain `Z` via its single-argument constructor.
 *
 * @details `std::signed_integral` types (e.g. `int`) are *not* certified as
 * `IsInteger` because their addition has undefined-behaviour overflow (see the
 * `!IsMagma<int, std::plus<int>>` rejection in `dedekind.category:total`).
 * This function is the *embedding arrow* that injects a built-in signed value
 * into a well-behaved `IsInteger` domain (e.g. a future
 * `SignedExtensionalCardinal<N>`), without claiming `int` itself is such a
 * domain.
 *
 * @tparam Z  The target `IsInteger` type.
 * @tparam S  A `std::signed_integral` source type (deduced).
 */
export template <IsInteger Z, std::signed_integral S>
constexpr Z embed_signed_integral(S v) {
  return Z{v};
}

/**
 * @brief Absolute value in the integer spine.
 * @details This lives upstream of `:rational` so Euclidean normalization does
 * not need to depend on machine-only facilities.
 */
export template <IsInteger Z>
constexpr Z integer_abs(Z value) {
  return value < Z{0} ? -value : value;
}

/**
 * @brief Euclidean greatest common divisor over an `IsInteger` carrier.
 *
 * @details Uses `std::gcd` when the carrier is natively supported, otherwise
 * falls back to the Euclidean algorithm in terms of `%`, comparison, and
 * additive inversion. This is the upstream normalization primitive that
 * `Rational<Z>` should use instead of depending on `std::gcd` directly.
 */
export template <IsInteger Z>
constexpr Z euclidean_gcd(Z lhs, Z rhs) {
  // std::gcd only works for builtin integral types (it static_asserts
  // otherwise).
  if constexpr (std::is_integral_v<Z>) {
    return std::gcd(lhs, rhs);
  } else {
    lhs = integer_abs(lhs);
    rhs = integer_abs(rhs);
    while (rhs != Z{0}) {
      const Z remainder = lhs % rhs;
      lhs = rhs;
      rhs = remainder;
    }
    return lhs;
  }
}

export template <typename Z>
concept HasEuclideanGcd = IsInteger<Z> && requires(Z a, Z b) {
  { euclidean_gcd(a, b) } -> std::same_as<Z>;
};

/**
 * @brief Current extensional machine integer carrier.
 *
 * @details This names the concrete machine-level entry point explicitly so
 * embeddings into the integer spine can refer to an extensional source type
 * without hard-coding `int` everywhere downstream.
 *
 * @note `int` satisfies IsInteger syntactically (all required operations are
 * present) but is NOT a total algebra: signed overflow is UB, so it is not
 * IsMagma. For total-algebra contexts prefer `ExtensionalCardinal<>` (natural
 * numbers by fiat) or a future `SignedExtensionalCardinal<N>` (integers by
 * fiat).
 */
export using extensional_integer = int;

/**
 * @brief Default integer carrier used by downstream numeric layers.
 *
 * @details Intentionally an alias so the default can be retargeted in one
 * place.  Current choice is @c SignedExtensionalCardinal<> --- the
 * variant signed-integer-by-fiat carrier whose arithmetic is total
 * (sign-magnitude with periodic wrap, no UB).  This makes the default
 * @c Rational<default_integer> participate in the strict @c IsRing
 * chain (and so @c IsField), via the @c SignedExtensionalCardinal<>
 * @c IsInteger pinning.  The previous default was machine @c int,
 * which @b syntactically satisfies @c IsInteger but @b not
 * @c IsMagma (signed-overflow UB), blocking any axiomatic ring/field
 * proof on @c Rational<int> at the @c IsTotal certificate.  The
 * machine alias @c extensional_integer remains @c int for callsites
 * that genuinely want the IEEE-style machine carrier.
 */
export using default_integer = SignedExtensionalCardinal<>;

// FIXME(#379): the *Like cluster below is a candidate for the
// retire-Like surgery phase that follows the alignment sweep.
//   - `IsRationalLike` checks only operator closure (+, -, *, /); it is
//     structurally identical to `algebra::HasFieldOperators` (shipped in
//     #394) modulo the absence of the `T{1}` clause.  Retarget call
//     sites to `HasFieldOperators` and remove this concept.
//   - `IsFieldLike = IsRationalLike` is a tautological alias with no
//     additional content; remove and retarget to the same replacement.
//   - `IsReal = IsRealLike || IsRationalLike` is a disjunction whose
//     two arms are semantically distinct (floating-point arithmetic vs
//     exact-rational arithmetic).  The union "is approximately real"
//     reading is loose; tighten to a single explicit concept or split
//     into two.
//   - `IsContinuous` and `IsDiscrete` partition `std::regular` types by
//     `std::integral` --- a syntactic split that says nothing about
//     mathematical density / discreteness.  Reconsider as part of the
//     retire-Like sweep.
export template <typename T>
concept IsRationalLike = std::regular<T> && requires(T a, T b) {
  { a + b } -> std::same_as<T>;
  { a * b } -> std::same_as<T>;
  { a / b } -> std::same_as<T>;
  { a - b } -> std::same_as<T>;
};

export template <typename T>
concept IsFieldLike = IsRationalLike<T>;

export template <typename Q, typename Z>
concept IsRational = IsFieldLike<Q> && IsInteger<Z> && requires(Q q) {
  { q } -> std::same_as<Q>;
};

export template <typename T>
concept IsRealLike = std::floating_point<T> && IsReflectiveSpecies<T>;

export template <typename T>
concept IsReal = IsRealLike<T> || IsRationalLike<T>;

export template <typename S>
concept IsContinuous = std::regular<S> && !std::integral<S>;

export template <typename S>
concept IsDiscrete = std::regular<S> && std::integral<S>;

export template <typename C, typename R>
concept IsComplex = requires(C z) {
  { z.real() } -> std::same_as<R>;
  { z.imag() } -> std::same_as<R>;
};

/**
 * @concept Group_ℤ
 * @brief ℤ as the abelian group of integers under addition.
 *
 * @details A carrier @c T satisfies @c Group_ℤ iff
 *   - it is @c IsInteger (structural integer syntax: +, -, *, /, %, <), and
 *   - @c (T, +, 0) is certified as an @c IsAbelianGroup by the species-trait
 *     registry (associative, commutative, has an identity, has an inverse).
 *
 * Downstream code that writes `template <Group_ℤ T>` gets a contract that is
 * simultaneously *intensional* (the carrier is named by its algebraic role,
 * not by a concrete C++ type) and *safe* (carriers whose addition is UB, like
 * signed @c int, are rejected at the concept gate rather than trusted).
 * Realisation to a concrete carrier (@c ExtensionalCardinal, @c
 * SignedExtensionalCardinal, @c std::signed_integral where the user has
 * furnished the trait proof) happens at the call site, not in the concept
 * body.
 */
export template <typename T>
concept Group_ℤ =
    IsInteger<T> && dedekind::category::IsAbelianGroup<T, std::plus<T>>;

/**
 * @concept Field_ℚ
 * @brief ℚ as the field of rationals.
 *
 * @details A carrier @c Q satisfies @c Field_ℚ iff it is @c IsRational over
 * some integer domain @c Z *and* @c (Q, +, *) carries the operational
 * field-like witness. Writing `template <Field_ℚ Q>` in a generic function
 * asserts both the rational-structure shape and the field arithmetic
 * without naming a concrete @c Rational<Z>.
 *
 * @note FIXME(#379): the field-side requirement is expressed via
 * @c dedekind::algebra::HasFieldOperators (an operational shape), not
 * via the strict @c dedekind::category::IsField that shipped in #375
 * (closed 2026-04-24).  Two distinct blocks compose into the actual
 * current blocker:
 *
 * 1. @b IsTotal @b gate.  The strict ring/field ladder requires
 *    @c IsMagma, which requires @c IsTotal<T, Op> @c = @c IsPeriodic
 *    @c || @c IsIdempotent @c || @c IsSaturating (per
 *    @c category:species).  Exact carriers like @c Rational<...> /
 *    @c ExactReal<> are none of those (no wrap, no idempotence, no
 *    saturation), so the strict ladder is architecturally blocked at
 *    the totality step regardless of invertibility traits.  Lifting
 *    this would require a new @c IsTotal certification path for
 *    exact carriers (e.g.\ "infinite-domain-total" or "exact").
 * 2. @b Species-trait specialisations.  Even with @c IsTotal lifted,
 *    the @c is_invertible_v<Rational<...>, std::multiplies> /
 *    @c inverse_trait specialisations would still be missing on the
 *    exact carriers under the active numeric policy.
 *
 * Until both blocks lift, carriers that pass @c Field_ℚ are
 * guaranteed the @b arithmetic of a field via @c HasFieldOperators
 * but not every law mechanically.
 */
export template <typename Q, typename Z = int>
concept Field_ℚ = IsRational<Q, Z> && dedekind::algebra::HasFieldOperators<Q>;

/**
 * @concept Continuum_ℝ
 * @brief ℝ as a continuum-valued field carrier.
 *
 * @details Bundles the structural @c IsReal witness with @c IsContinuous and
 * the operational field-like arithmetic discipline.
 *
 * @see FIXME(#379): same retargeting story as @ref Field_ℚ ---
 * @c category::IsField exists (#375), but BOTH the @c IsTotal gate
 * (which currently admits only periodic/idempotent/saturating ops,
 * blocking exact carriers like @c ExactReal<>) AND the species-trait
 * specialisations would need lifting.
 */
export template <typename T>
concept Continuum_ℝ =
    IsReal<T> && IsContinuous<T> && dedekind::algebra::HasFieldOperators<T>;

/**
 * @concept Algebra_ℂ
 * @brief ℂ as an algebra over an underlying real-like field @c R.
 *
 * @see FIXME(#379): same retargeting story as @ref Field_ℚ ---
 * @c category::IsField exists (#375), but BOTH the @c IsTotal gate
 * (which currently admits only periodic/idempotent/saturating ops,
 * blocking exact carriers like @c Complex<ExactReal<>>) AND the
 * species-trait specialisations would need lifting.
 */
export template <typename C, typename R>
concept Algebra_ℂ = IsComplex<C, R> && dedekind::algebra::HasFieldOperators<C>;

/** @section integer__Canonical_Species_Spine (ℤ)
 *
 * The canonical species symbol @c ℤ aliases the exact ℤ @b carrier
 * @c SignedExtensionalCardinal<> per #399 (slice 3); the predicate-set
 * form @c IntegersOf<> stays partition-local (non-exported as
 * @c IntegerSet) and is reachable via the value-level constant @c Z
 * for set-builder DSL.  Both are defined further down.  The spine
 * below pins ℤ's syntax / semantics / arrow-fabric witnesses against
 * drift.  The strict (species-trait) witnesses on the exact ℤ carrier
 * land in @c :rational (where the species-trait registrations are
 * reachable); the partition-local witnesses here cover the
 * literal-shape concepts and the primitive-type arrows.
 */

// (1) IsSet anchor: deferred until after Z is defined further down.

// (2) Syntax (the C++ operator surface that maps to ℤ's algebra).
//   - HasRingOperators<int>: literal +, -, *, unary - all close on int.
//   - HasGroupOperatorsAdd<int>: literal +, binary -, unary - all close.
//   - HasSuccessorOperators<int>: pre/post ++, -- all close.
//   - HasCompoundGroupOperators*<int>: +=, -=, *=, /= all close.
static_assert(dedekind::algebra::HasRingOperators<int>,
              "ℤ's machine carrier (int) closes the literal ring operator "
              "surface.");
static_assert(dedekind::algebra::HasGroupOperatorsAdd<int>,
              "ℤ's machine carrier closes the additive-group operator surface "
              "(+, binary -, unary -).");
static_assert(dedekind::algebra::HasSuccessorOperators<int>,
              "ℤ's machine carrier supports the successor operator "
              "surface (Peano-aligned ++, --).");

// (3) Semantics (the algebraic structures int actually carries).
//   - Self-documenting: IsInteger<int> (structural Euclidean-integer-
//     domain syntax).
//   - Group_ℤ<int> deliberately does NOT fire: signed-overflow UB
//     defeats the strict abelian-group proof under the math-wins-
//     over-C++ stance.  Group_ℤ<SignedExtensionalCardinal<>> is the
//     exact-ℤ witness, asserted in `:rational`.
//   - IsArithmeticAdditiveGroup<int> likewise refused.
static_assert(IsInteger<extensional_integer>,
              "extensional_integer (= int) satisfies IsInteger "
              "(structural Euclidean-integer-domain syntax).");
static_assert(!Group_ℤ<int>,
              "int must NOT satisfy Group_ℤ: signed-overflow UB "
              "defeats the strict abelian-group proof under the "
              "math-wins-over-C++ stance.");
static_assert(!dedekind::algebra::IsArithmeticAdditiveGroup<int>,
              "int must NOT satisfy IsArithmeticAdditiveGroup: same "
              "reason as the Group_ℤ rejection.");

// (4) Primitive-type arrow:  std::signed_integral ↔ ℤ.  Forward via
// `embed_signed_integral<Z>(v)` defined earlier in this partition;
// reverse via the carrier's explicit `operator S()` (single-limb only,
// to prevent silent truncation).  Pinned below as static_asserts on
// the canonical exact ℤ carrier.

// (5) Adjacent-set arrow: ℕ ↪ ℤ via `embed_uint_sint_` defined further down
// in this partition.  Reverse direction (ℤ → ℕ via absolute value or
// signed-bit projection) is not a strict embedding and is intentionally
// not registered.

/**
 * @brief Characteristic morphism for ℤ: the integers.
 * Accepts native int and all embedded predecessors (unsigned, Ternary).
 */
export template <typename L = ClassicalLogic, typename C = ℵ_0>
struct IntegersOf {
  // Domain is the exact ℤ carrier per #399 slice 3.  Tracks the
  // top-level @c ℤ alias (@c SignedExtensionalCardinal<>) so that
  // @c var<ℤ>; @c n @c % @c Z routes the same_as check on
  // Variable's element type @c T against @c Z::Domain cleanly
  // through the carrier — the int / unsigned / Ternary / bool
  // overloads below remain reachable via implicit @c int @c → @c SEC
  // construction.
  using Domain = dedekind::sets::SignedExtensionalCardinal<>;
  using Codomain = typename L::Ω;
  using logic_species = L;
  using cardinality_type = C;

  // Native exact ℤ: always a member of ℤ.  Direct dispatch on the
  // carrier (no implicit-conversion round trip).
  constexpr typename L::Ω operator()(
      const dedekind::sets::SignedExtensionalCardinal<>&) const {
    return L::True;
  }

  // Native int: always a member of ℤ.  Retained for callsite ergonomics
  // (literals, machine-int fixtures); the implicit @c int @c → @c SEC
  // construction lifts the same predicate.
  constexpr typename L::Ω operator()(int) const { return L::True; }

  // Embedded unsigned (via embed_uint_sint_)
  constexpr typename L::Ω operator()(unsigned n) const {
    return operator()(static_cast<int>(n));
  }

  // Embedded Ternary: False ↦ -1, Unknown ↦ 0, True ↦ 1.  Delegates
  // to the int overload directly rather than routing through @c
  // embed_𝕂3_ℤ_ — post-#430 that arrow lands on @c SignedCardinality
  // (the variant ℤ-proxy), whereas this @c IntegersOf<> predicate-set
  // is parameterised on the int machine carrier.  The {-1, 0, 1}
  // mapping is the same; the implementations are deliberately
  // separate to keep this overload at the machine-int layer.
  constexpr typename L::Ω operator()(Ternary t) const {
    switch (t) {
      case Ternary::False:
        return operator()(-1);
      case Ternary::Unknown:
        return operator()(0);
      case Ternary::True:
        return operator()(1);
    }
    return L::False;
  }

  // Embedded bool (via embed_𝔹_uint_ → embed_uint_sint_)
  constexpr typename L::Ω operator()(bool b) const {
    return operator()(embed_𝔹_uint_(b));
  }
};

// Non-exported convenience alias used by the value-level @c Z constant
// below.  Per #399 (canonical species symbols as carrier types), the
// public name @c ℤ now denotes the exact ℤ @b carrier
// (@c SignedExtensionalCardinal<>) rather than the predicate-set form;
// callers who specifically need the predicate-set type should spell
// @c IntegersOf<> or @c decltype(Z) — symmetric with the
// @c BooleanSetOf<> / @c B handling on the @c 𝔹 side (#400 / #407).
using IntegerSet = IntegersOf<>;

/** @brief The canonical Boolean-tower successor: the exact ℤ @b carrier.
 *
 *  Aliased to @c dedekind::sets::SignedExtensionalCardinal<> per #399's
 *  show-to-a-wider-audience API: the species symbol names the carrier
 *  type itself, so @c static_assert(IsRing<ℤ, ...>) and @c var<ℤ>
 *  read directly against the carrier.  The value-level constant
 *  @c Z below keeps its predicate-set role for the set-builder DSL
 *  (e.g.\ @c Set{n @c % @c Z @c | @c (n @c > @c bound<-21>)}).
 *
 *  Strict semantic witnesses on the carrier (@c IsRing,
 *  @c IsArithmeticAdditiveGroup, @c Group_ℤ, etc.) live in @c :rational
 *  by module-DAG necessity (the trait registrations are reachable
 *  there); the partition-local witnesses below cover what is reachable
 *  in @c :integer's import scope.
 */
export using ℤ = dedekind::sets::SignedExtensionalCardinal<>;

static_assert(std::same_as<SignedExtensionalCardinal<>, dedekind::sets::SignedExtensionalCardinal<>>,
              "ℤ is the exact-ℤ carrier alias (#399 slice 3); the "
              "predicate-set form spells as @c IntegersOf<> or "
              "@c decltype(Z).");

export inline constexpr IntegerSet Z{};

static_assert(
    dedekind::category::IsSet<
        decltype(dedekind::category::ambient_set<
                 dedekind::sets::SignedExtensionalCardinal<>>(Z))>,
    "IntegersOf must be the canonical IsSet anchor for "
    "dedekind.numbers:integer (Domain = exact ℤ carrier per #399 slice 3).");

/**
 * @brief Canonical embedding ℕ ↪ ℤ: unsigned int → int.
 * @details The natural numbers embed into the integers via the unsigned→signed
 *          widening conversion. This is injective for values that fit in int;
 *          large unsigned values may overflow, so the domain is conventionally
 *          restricted to values ≤ INT_MAX when used with certified arithmetic.
 */
export inline constexpr auto embed_uint_sint_ = arrow<unsigned, int>(
    [](const unsigned& x) noexcept { return static_cast<int>(x); });

/**
 * @brief Canonical embedding K3 ↪ ℤ: Ternary → SignedCardinality.
 * @details Maps False → -1, Unknown → 0, True → 1.  Lands on the
 *          variant ℤ-proxy carrier @c SignedCardinality (closes #430,
 *          a #402 prerequisite); the previous @c int codomain made
 *          the K3 → variant-ℤ chain require an explicit machine-side
 *          extraction step.  The unreachable default returns @c NaZ
 *          for IEEE-NaN-style propagation if a non-canonical
 *          @c Ternary value were ever constructed (cannot happen in
 *          practice — @c Ternary is a closed enum).
 */
export inline constexpr auto embed_𝕂3_ℤ_ =
    arrow<Ternary, dedekind::sets::SignedCardinality>(
        [](const Ternary& t) noexcept -> dedekind::sets::SignedCardinality {
          switch (t) {
            case Ternary::False:
              return dedekind::sets::finite_signed_cardinality(-1);
            case Ternary::Unknown:
              return dedekind::sets::finite_signed_cardinality(0);
            case Ternary::True:
              return dedekind::sets::finite_signed_cardinality(1);
          }
          return dedekind::sets::SignedCardinality{dedekind::sets::NaZ{}};
        });

/**
 * @brief Canonical embedding of any std::unsigned_integral into formal ℕ.
 *
 * @details Every std::unsigned_integral type is a machine-width representative
 * of ℕ. ExtensionalCardinal<> is the "by fiat" unbounded ℕ. This template
 * covers any unsigned width; the concrete arrow embed_unsigned_ℕ handles the
 * canonical machine-width (unsigned) case.
 *
 * @tparam U Any std::unsigned_integral source type.
 */
export template <std::unsigned_integral U>
constexpr ExtensionalCardinal<> embed_to_ℕ(U v) {
  return ExtensionalCardinal<>{
      static_cast<ExtensionalCardinal<>::limb_type>(v)};
}

/**
 * @brief Concrete monic arrow: unsigned ↪ ℕ (ExtensionalCardinal<>).
 *
 * @details Injects the canonical machine-width unsigned into the extensional
 * natural-number carrier. unsigned is the machine representative of
 * std::unsigned_integral; for other widths use embed_to_ℕ<U>(v).
 * Declared monic: distinct machine unsigned values yield distinct
 * ExtensionalCardinal<> values within the 64-bit limb.
 */
export inline constexpr auto embed_unsigned_ℕ =
    arrow<unsigned, ExtensionalCardinal<>>(
        [](const unsigned& u) noexcept -> ExtensionalCardinal<> {
          return ExtensionalCardinal<>{
              static_cast<ExtensionalCardinal<>::limb_type>(u)};
        });

// The universal machine-to-variant lift @c embed_uint_ℕ
// and its concrete-arrow form @c embed_uint_ℕ_ live in the
// dedicated sibling partition @c numbers:uint, which consolidates the
// @c std::unsigned_integral family's textbook classification (commutative
// ring @c ℤ/2^wℤ, the @c Modular<N> / @c IsCyclic correspondence, and
// the width-ladder ring-hom witnesses).  Cross-reference only here.

/**
 * @brief Canonical variant-layer embedding @c ℕ @c ↪ @c ℤ:
 *        @c Cardinality @c → @c SignedCardinality, exposed as a
 *        first-class @c arrow object for the carrier-lattice diagram.
 *
 * @details Wraps @c dedekind::sets::lift_cardinality_to_signed (the
 *          public function definition; lives in @c sets:cardinality
 *          to remain reachable from cross-variant comparison
 *          operators without crossing the @c sets @c → @c numbers
 *          module boundary).  This @c arrow form is the named monic
 *          morphism the carrier-lattice Figure 1 labels at the
 *          variant-layer top row; structurally @b distinct from the
 *          machine-layer @c embed_uint_sint_ above (an
 *          @c arrow<unsigned, @c int> sign reinterpretation).
 *          Registered as monic below.
 */
export inline constexpr auto lift_ℕ_ℤ_ =
    arrow<dedekind::sets::Cardinality, dedekind::sets::SignedCardinality>(
        [](const dedekind::sets::Cardinality& c) noexcept {
          return dedekind::sets::lift_cardinality_to_signed(c);
        });

/**
 * @brief Canonical embedding of any std::signed_integral into ℤ.
 *
 * @details The extensional integer carrier (extensional_integer = int) is the
 * default target. For injecting into a general IsInteger domain Z, use
 * embed_signed_integral<Z>(v).
 *
 * @tparam S Any std::signed_integral source type.
 */
export template <std::signed_integral S>
constexpr extensional_integer embed_signed_to_ℤ(S v) {
  return static_cast<extensional_integer>(v);
}

}  // namespace dedekind::numbers

namespace dedekind::category {
template <>
inline constexpr bool is_monic_arrow_v<
    std::decay_t<decltype(dedekind::numbers::embed_uint_sint_)>> = true;
static_assert(
    IsInjective<std::decay_t<decltype(dedekind::numbers::embed_uint_sint_)>>,
    "embed_uint_sint_ (machine-layer ℕ → ℤ) is registered injective.");

template <>
inline constexpr bool
    is_monic_arrow_v<std::decay_t<decltype(dedekind::numbers::embed_𝕂3_ℤ_)>> =
        true;
static_assert(
    IsInjective<std::decay_t<decltype(dedekind::numbers::embed_𝕂3_ℤ_)>>,
    "embed_𝕂3_ℤ_ (𝕂3 → ℤ) is registered injective.");

template <>
inline constexpr bool is_monic_arrow_v<
    std::decay_t<decltype(dedekind::numbers::embed_unsigned_ℕ)>> = true;
static_assert(
    IsInjective<std::decay_t<decltype(dedekind::numbers::embed_unsigned_ℕ)>>,
    "embed_unsigned_ℕ (unsigned → ExtensionalCardinal) is registered "
    "injective.");
// Monicity of @c embed_uint_ℕ_ is registered in @c :uint.

template <>
inline constexpr bool
    is_monic_arrow_v<std::decay_t<decltype(dedekind::numbers::lift_ℕ_ℤ_)>> =
        true;
static_assert(IsInjective<std::decay_t<decltype(dedekind::numbers::lift_ℕ_ℤ_)>>,
              "lift_ℕ_ℤ_ (variant-layer ℕ ↪ ℤ; Grothendieck-construction unit) "
              "is registered injective.");
}  // namespace dedekind::category

// ===========================================================================
// Initial Ring + Grothendieck Group witnesses on @c SignedCardinality
// (closes part of #446).
//
// Two universal-property witnesses anchoring @c SignedCardinality
// simultaneously:
//   * @c IsInitialRing<SignedCardinality> — for every ring @c R there
//     exists a unique ring homomorphism @c SignedCardinality @c → @c R
//     (e.g.\ @c χ_{Modular<n>} as the mod-n reduction).
//   * @c IsGrothendieckGroup<SignedCardinality, Cardinality> —
//     @c SignedCardinality is the free abelian group on the
//     commutative monoid @c Cardinality; the closure-forcing operator
//     @c Cardinality @c - @c Cardinality @c → @c SignedCardinality
//     realises the construction at the operator level.
//
// Universal-property content (existence + uniqueness of the canonical
// homomorphisms) is the engineer's honesty obligation; the test
// suite exercises the operational behaviour at concrete targets.
// ===========================================================================

namespace dedekind::algebra {

template <>
inline constexpr bool is_initial_ring_v<dedekind::sets::SignedCardinality> =
    true;

template <>
inline constexpr bool is_grothendieck_group_v<dedekind::sets::SignedCardinality,
                                              dedekind::sets::Cardinality> =
    true;

}  // namespace dedekind::algebra

namespace dedekind::numbers {

static_assert(
    dedekind::algebra::IsInitialRing<dedekind::sets::SignedCardinality>,
    "SignedCardinality is the canonical Initial Ring witness: for every "
    "ring R there exists a unique ring homomorphism SignedCardinality → R "
    "(e.g. χ_{Modular<n>} = mod-n reduction).  Universal-property content "
    "is the engineer's honesty obligation.");

static_assert(
    dedekind::algebra::IsGrothendieckGroup<dedekind::sets::SignedCardinality,
                                           dedekind::sets::Cardinality>,
    "SignedCardinality is the canonical Grothendieck group of Cardinality: "
    "the free abelian group on the commutative monoid (Cardinality, +, 0).  "
    "The closure-forcing operator Cardinality - Cardinality → "
    "SignedCardinality realises the Grothendieck construction at the "
    "operator level.");

}  // namespace dedekind::numbers

// ---------------------------------------------------------------------------
// Carrier-lattice lift unification (#455): existential-proof
// specialisation of @c category::lift for the central variant-layer
// pair @c (Cardinality, SignedCardinality).  Demonstrates that the
// discoverability-alias dispatch works on a real lattice arrow;
// remaining six specialisations land as follow-up.
// ---------------------------------------------------------------------------

namespace dedekind::category {
template <>
constexpr dedekind::sets::SignedCardinality
lift<dedekind::sets::Cardinality, dedekind::sets::SignedCardinality>(
    dedekind::sets::Cardinality const& n) {
  return dedekind::numbers::lift_ℕ_ℤ_(n);
}
}  // namespace dedekind::category
