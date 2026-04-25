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
import :naturals;
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
 * place. Current choice is the machine signed integer (syntactically IsInteger,
 * operationally correct for arithmetic that stays in range).
 *
 * @note The natural-number carrier `ExtensionalCardinal<>` satisfies IsInteger
 * and forms a total ring (IsRing), but is unsigned — negative rationals need
 * a signed carrier. A future `SignedExtensionalCardinal<N>` is the intended
 * long-term retarget. See rational.cppm for `RationalPolynomial` (Q[x]).
 */
export using default_integer = extensional_integer;

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
 * @c dedekind::algebra::IsFieldLikeScalar (an operational shape), not
 * via the strict @c dedekind::category::IsField that shipped in #375
 * (closed 2026-04-24).  The actual current blocker is that
 * @c category::IsField<Rational<...>, std::plus, std::multiplies>
 * does not fire on @c Rational<...> under the active numeric policy
 * --- the species-trait registry isn't specialised
 * (@c is_invertible_v<Rational<...>, std::multiplies>, etc.), not
 * the absence of @c IsField.  Surgery candidate: specialise the
 * species traits on the @c Rational carrier so this concept can
 * retarget to @c category::IsField directly.  Until then, carriers
 * that pass @c Field_ℚ are guaranteed the @b arithmetic of a field
 * but not every law mechanically --- the species-trait registry
 * supplies the laws separately.
 */
export template <typename Q, typename Z = int>
concept Field_ℚ = IsRational<Q, Z> && dedekind::algebra::IsFieldLikeScalar<Q>;

/**
 * @concept Continuum_ℝ
 * @brief ℝ as a continuum-valued field carrier.
 *
 * @details Bundles the structural @c IsReal witness with @c IsContinuous and
 * the operational field-like arithmetic discipline.
 *
 * @see FIXME(#379): same retargeting story as @ref Field_ℚ ---
 * @c category::IsField now exists (#375), but the species-trait
 * specialisations on the carriers are the actual current block.
 */
export template <typename T>
concept Continuum_ℝ =
    IsReal<T> && IsContinuous<T> && dedekind::algebra::IsFieldLikeScalar<T>;

/**
 * @concept Algebra_ℂ
 * @brief ℂ as an algebra over an underlying real-like field @c R.
 *
 * @see FIXME(#379): same retargeting story as @ref Field_ℚ ---
 * @c category::IsField now exists (#375), but the species-trait
 * specialisations on the carriers are the actual current block.
 */
export template <typename C, typename R>
concept Algebra_ℂ = IsComplex<C, R> && dedekind::algebra::IsFieldLikeScalar<C>;

/** @section Formal_Verification (partition-local) */

// Self-documenting witness: the machine integer carrier satisfies the
// structural IsInteger concept (the strict Group_ℤ claim is asserted
// in `:rational` where the species-trait registrations land).
static_assert(IsInteger<extensional_integer>,
              "extensional_integer (= int) satisfies IsInteger "
              "(structural Euclidean-integer-domain syntax).");

// `Group_ℤ<int>` deliberately does NOT fire under the math-wins-over-
// C++ stance: signed-overflow UB defeats the strict abelian-group
// proof.  `Group_ℤ<SignedExtensionalCardinal<>>` is the canonical
// exact-ℤ witness, asserted in `:rational`.
static_assert(!Group_ℤ<int>,
              "int must NOT satisfy Group_ℤ: signed-overflow UB "
              "defeats the strict abelian-group proof under the "
              "math-wins-over-C++ stance.");

/**
 * @brief Characteristic morphism for ℤ: the integers.
 * Accepts native int and all embedded predecessors (unsigned, Ternary).
 */
export template <typename L = ClassicalLogic, typename C = ℵ_0>
struct IntegersOf {
  using Domain = int;
  using Codomain = typename L::Ω;
  using logic_species = L;
  using cardinality_type = C;

  // Native int: always a member of ℤ
  constexpr typename L::Ω operator()(int) const { return L::True; }

  // Embedded unsigned (via embed_ℕ_ℤ)
  constexpr typename L::Ω operator()(unsigned n) const {
    return operator()(static_cast<int>(n));
  }

  // Embedded Ternary (via embed_K3_ℤ)
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

  // Embedded bool (via embed_𝔹_ℕ → embed_ℕ_ℤ)
  constexpr typename L::Ω operator()(bool b) const {
    return operator()(embed_𝔹_ℕ(b));
  }
};

export using IntegerSet = IntegersOf<>;
export using ℤ = IntegerSet;

export inline constexpr ℤ Z{};

static_assert(dedekind::category::IsSet<
                  decltype(dedekind::category::ambient_set<int>(Z))>,
              "IntegersOf must be the canonical IsSet anchor for "
              "dedekind.numbers:integer.");

/**
 * @brief Canonical embedding ℕ ↪ ℤ: unsigned int → int.
 * @details The natural numbers embed into the integers via the unsigned→signed
 *          widening conversion. This is injective for values that fit in int;
 *          large unsigned values may overflow, so the domain is conventionally
 *          restricted to values ≤ INT_MAX when used with certified arithmetic.
 */
export inline constexpr auto embed_ℕ_ℤ = arrow<unsigned, int>(
    [](const unsigned& x) noexcept { return static_cast<int>(x); });

/**
 * @brief Canonical embedding K3 ↪ ℤ: Ternary → int.
 * @details Maps False -> -1, Unknown -> 0, True -> 1.
 */
export inline constexpr auto embed_K3_ℤ =
    arrow<Ternary, int>([](const Ternary& t) noexcept {
      switch (t) {
        case Ternary::False:
          return -1;
        case Ternary::Unknown:
          return 0;
        case Ternary::True:
          return 1;
      }
      return 0;
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
inline constexpr bool
    is_monic_arrow_v<std::decay_t<decltype(dedekind::numbers::embed_ℕ_ℤ)>> =
        true;

template <>
inline constexpr bool
    is_monic_arrow_v<std::decay_t<decltype(dedekind::numbers::embed_K3_ℤ)>> =
        true;

template <>
inline constexpr bool is_monic_arrow_v<
    std::decay_t<decltype(dedekind::numbers::embed_unsigned_ℕ)>> = true;
}  // namespace dedekind::category
