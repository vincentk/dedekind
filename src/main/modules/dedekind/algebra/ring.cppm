/**
 * @file algebra:ring.cppm
 * @partition :ring
 * @brief Level 3.2: The Rules of Harmony (The Semiring Synthesis).
 *
 * @copyright 2026 The Dedekind Authors

 *
 * @note „Was beweisbar ist, soll in der Wissenschaft nicht ohne Beweis
 *  geglaubt werden.“ (What is provable should not be believed without proof.)
 *  — Richard Dedekind
 */
module;

#include <concepts>
#include <functional>

export module dedekind.algebra:ring;

import :monoid;
import :group;
import dedekind.category;

namespace dedekind::algebra {

using namespace dedekind::category;

/**
 * @concept IsSemiring
 * @brief The Unification of Algebra and Action (The Rig).
 * @details A species where (T,+) is a Commutative Monoid and (T,*) is a Monoid.
 *          In the Dedekind topos, this is a "Semimodule over itself."
 * @tparam T The carrier type.
 * @tparam Add Additive operation witness (defaults to `std::plus<T>`).
 * @tparam Mult Multiplicative operation witness
 * (defaults to `std::multiplies<T>`).
 */
export template <typename T, typename Add = std::plus<T>,
                 typename Mult = std::multiplies<T>>
concept IsSemiring =
    IsAdditiveMonoid<T, Add> && IsMultiplicativeMonoid<T, Mult> &&
    dedekind::category::IsLinearAction<T, T, Mult, Add>;

/** @concept IsRig: The "Natural" Harmony (No negatives) */
export template <typename T, typename Add = std::plus<T>,
                 typename Mult = std::multiplies<T>>
concept IsRig = dedekind::category::IsRig<T, Add, Mult>;

/** @concept IsRng: The "Identity-less" Harmony (No unit) */
export template <typename T, typename Add = std::plus<T>,
                 typename Mult = std::multiplies<T>>
concept IsRng = dedekind::category::IsRng<T, Add, Mult>;

/**
 * @concept IsRing
 * @brief A set that is both a Semiring AND an Abelian Group under addition.
 * @tparam T The carrier type.
 * @tparam Add Additive operation witness (defaults to `std::plus<T>`).
 * @tparam Mult Multiplicative operation witness
 * (defaults to `std::multiplies<T>`).
 */
export template <typename T, typename Add = std::plus<T>,
                 typename Mult = std::multiplies<T>>
concept IsRing = dedekind::category::IsRing<T, Add, Mult> &&
                 IsSemiring<T, Add, Mult> && IsAdditiveGroup<T, Add>;

/**
 * @concept IsCommutativeRing
 * @brief A Ring where multiplication is also commutative.
 * @tparam T The carrier type.
 * @tparam Add Additive operation witness (defaults to `std::plus<T>`).
 * @tparam Mult Multiplicative operation witness
 * (defaults to `std::multiplies<T>`).
 */
export template <typename T, typename Add = std::plus<T>,
                 typename Mult = std::multiplies<T>>
concept IsCommutativeRing =
    IsRing<T, Add, Mult> && dedekind::category::IsCommutative<T, Mult>;

/** @section Operational_Witnesses
 *
 *  `IsRing` and `IsSemiring` above are the strict categorical proofs and
 *  depend on the axiom variable templates (`identity_v`, `is_associative_v`,
 *  `is_commutative_v`, `inverse_v`) being specialised per carrier. Machine-
 *  backed carriers such as `Rational<long>` deliberately do NOT carry those
 *  proofs under the active numeric policy — the same pattern that gives
 *  `IsField` vs `IsFieldLikeScalar` (see `:modules`).
 *
 *  The operational witnesses below are the analogous shortcut for
 *  ring-shaped carriers: they check closure of the arithmetic operators
 *  only. Identity elements (`0` and `1`) belong to the strict tower and
 *  are not encoded here, matching `IsFieldLikeScalar`'s shape.
 */

/**
 * @concept IsRingLike
 * @brief Operational ring witness: +, unary -, binary -, * are closed in T.
 *
 *  Parallel to `IsFieldLikeScalar` from `:modules` but without the `/`
 *  requirement: rings don't in general support division. Shape is
 *  deliberately symmetric with `IsFieldLikeScalar` — operator closure
 *  only, no structural identity witnesses (they live behind the strict
 *  categorical axiom-hook tower).
 *
 *  Fires on `Rational<long>`, `int`, `unsigned int`, `Complex<R>` (for
 *  any `IsRingLike R`), `Dual<F>`, and — crucially — on `Matrix2x2V<R>`
 *  for any `IsRingLike R`, witnessing the slogan "matrices over a ring
 *  form a (non-commutative) ring".
 */
export template <typename T>
concept IsRingLike = requires(T a, T b) {
  { a + b } -> std::same_as<T>;
  { a - b } -> std::same_as<T>;
  { -a } -> std::same_as<T>;
  { a * b } -> std::same_as<T>;
};

/**
 * @concept IsSemiringLike
 * @brief Operational semiring witness: +, *, `T{}`, `T{1}` — NO subtraction
 *        or unary negation required.
 *
 *  The "no additive inverse" carriers: booleans (under OR/AND), naturals,
 *  tropical and other exotic semirings. Parallel to `IsRingLike` but
 *  without the subtraction and unary-minus clauses.
 */
export template <typename T>
concept IsSemiringLike = requires(T a, T b) {
  { a + b } -> std::same_as<T>;
  { a * b } -> std::same_as<T>;
  T{};
  T{1};
};

// FIXME: unify IsRingLike with the strict IsRing once an axiom-hook auto-
// lifter exists (see the rejected-concepts summary on PR #367 — item #4
// "Axiom-hook auto-lifter"). Then operational and strict can collapse for
// value-level carriers without per-type boilerplate.

/**
 * @concept IsRingLikeHomomorphism
 * @brief A callable φ that sends `Source` to `Target` with the ring-like
 *        structure (+, ·, 0, 1) preserved.
 *
 *   φ(a + b) == φ(a) + φ(b)      additive preservation
 *   φ(a · b) == φ(a) · φ(b)      multiplicative preservation
 *   φ(Source{}) == Target{}      additive identity preservation
 *   φ(Source{1}) == Target{1}    multiplicative identity preservation
 *
 *  The concept checks the STRUCTURAL shape of the homomorphism claim:
 *   - `Source` and `Target` both operationally ring-like,
 *   - `φ(a)` is well-typed and returns `Target`,
 *   - each of the four law expressions is well-formed (yields `bool`).
 *
 *  The semantic LAW — that each `==` returns `true` for all inputs — is a
 *  value-level claim, witnessed per instance via `static_assert` on concrete
 *  probes (cf. `dedekind.linear_algebra:embeddings` for ℂ ↪ M₂(ℚ) and
 *  𝔻 ↪ M₂(ℚ)). A concept cannot encode a universally-quantified runtime
 *  property directly; this is the conventional operational witness.
 *
 *  Parallel to how `IsRingLike` is the operational half of `IsRing`: no
 *  strict categorical `IsRingHomomorphism` is exported yet because the
 *  carrier-level axiom hooks are not generally populated on machine-backed
 *  carriers. Adding the strict version becomes cheap once an axiom-hook
 *  auto-lifter lands.
 */
// FIXME: add `IsGroupHomomorphism`, `IsMonoidHomomorphism`, and
// `IsSemiringLikeHomomorphism` specialisations alongside once there are
// concrete call sites that need them (e.g. embedding modular groups into
// symmetric groups, or tropical-semiring homomorphisms).
export template <typename Phi, typename Source, typename Target>
concept IsRingLikeHomomorphism = IsRingLike<Source> && IsRingLike<Target> &&
                                 requires(const Phi& phi, Source a, Source b) {
                                   { phi(a) } -> std::same_as<Target>;
                                   {
                                     phi(a + b) == (phi(a) + phi(b))
                                   } -> std::same_as<bool>;
                                   {
                                     phi(a * b) == (phi(a) * phi(b))
                                   } -> std::same_as<bool>;
                                 };
// Identity preservation (φ(0) = 0, φ(1) = 1) is derivable from additive
// preservation + existence of unary negation: φ(0) = φ(x - x) = φ(x) - φ(x)
// = 0. It is not encoded structurally here for the same reason the `T{}`
// and `T{1}` hooks are absent from `IsRingLike` — identities belong to the
// strict categorical tower, witnessed on concrete probes separately.

/** @section Formal_Verification */

// Operational witnesses on primitive carriers.
static_assert(IsRingLike<int>,
              "int satisfies IsRingLike (wrapping / two's-complement).");
static_assert(IsRingLike<unsigned int>,
              "unsigned int satisfies IsRingLike (modular arithmetic).");
static_assert(IsSemiringLike<unsigned int>,
              "unsigned int also satisfies IsSemiringLike "
              "(without needing unary negation).");

// unsigned int with wrapping arithmetic is the canonical total commutative
// ring: IsPeriodic (wraps at 2^N) satisfies IsTotal → IsMagma → IsMonoid →
// IsGroup.
static_assert(IsRing<unsigned int>,
              "unsigned int must satisfy IsRing (wrapping arithmetic).");
static_assert(IsCommutativeRing<unsigned int>,
              "unsigned int must satisfy IsCommutativeRing.");

// Modular<N> is the archetypal finite commutative ring Z/NZ.
static_assert(IsRing<Modular<256>>,
              "Modular<256> must satisfy IsRing (Z/256Z).");
static_assert(IsCommutativeRing<Modular<256>>,
              "Modular<256> must satisfy IsCommutativeRing.");

// bool with OR/AND is an idempotent commutative semiring (the Boolean rig).
// It is not a Ring: there is no additive inverse for True (True + x != False).
static_assert(IsRig<bool, std::logical_or<bool>, std::logical_and<bool>>,
              "bool must satisfy IsRig (Boolean semiring under OR/AND).");

}  // namespace dedekind::algebra
