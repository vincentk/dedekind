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

/**
 * @concept HasRingOperators
 * @brief @b Pure @b syntactic @b shape: T supports @c +, @c -, @c *
 *        (binary and unary @c -) with closed results, and that's all.
 *
 * @details
 * Use this concept where the callsite needs @c {+, -, *} to compile and
 * close back into @c T but does @b not depend on any algebraic axiom
 * (associativity, commutativity, identity, inverse, distributivity).
 * Typical callers: container-shaped templates whose constructibility
 * just needs the operators (componentwise tuple arithmetic, value-level
 * matrix carriers under @c +/@c -/@c *, halfspace inequality evaluation
 * with @c a*x + b*y comparisons, ring-homomorphism embedding shapes).
 *
 * The structurally-identical @c IsRingLike is retained as the
 * @b operational variant: same predicates today, but reserved for
 * callsites that genuinely depend on the operator surface acting like
 * a ring under the active numeric policy (Kleene three-valued, partial
 * traits, IEEE-edge admissibility).  @c HasRingOperators is the right
 * name when the callsite's intent is "the operators must compile";
 * @c IsRingLike is the right name when the callsite's intent is "the
 * operators must compile @b and act ring-shaped under the policy in
 * force".  Audit detail: see #393.
 *
 * The strict counterpart with full categorical axioms is
 * @c dedekind::category::IsRing<T, Add, Mult> in @c category:total.
 */
export template <typename T>
concept HasRingOperators = requires(T a, T b) {
  { a + b } -> std::same_as<T>;
  { a - b } -> std::same_as<T>;
  { -a } -> std::same_as<T>;
  { a * b } -> std::same_as<T>;
};

// `HasLatticeOperators` (bitwise &, |, ^, ~ shape) lives in
// `dedekind.order:lattice` --- order is the natural home for
// lattice-flavoured concepts; this file kept ring-flavoured.
// `HasLogicalOperators` (short-circuiting &&, ||, ! shape) lives
// in `dedekind.category:logic` --- the topos-internal-logic
// partition, the deepest logic home in the project.

/**
 * @concept IsRingLikeHomomorphism
 * @brief A callable φ that sends `Source` to `Target` with additive and
 *        multiplicative preservation:
 *
 *   φ(a + b) == φ(a) + φ(b)      additive preservation
 *   φ(a · b) == φ(a) · φ(b)      multiplicative preservation
 *
 *  The concept checks the STRUCTURAL shape of the claim:
 *   - `Source` and `Target` are both operationally ring-like,
 *   - `φ(a)` is well-typed and returns `Target`,
 *   - both preservation expressions are well-formed (yield `bool`).
 *
 *  Preservation of distinguished identities (φ(0) = 0, φ(1) = 1) is NOT
 *  encoded structurally here: `IsRingLike` does not expose identity
 *  elements, so the concept cannot reach them. Unital / identity laws,
 *  when needed, must be checked by stricter concepts or on concrete
 *  probe values (cf. `dedekind.linear_algebra:embeddings` for the
 *  ℂ ↪ M₂(ℚ) and 𝔻 ↪ M₂(ℚ) cases where `zero_matrix2x2_v` and
 *  `identity_matrix2x2_v` are asserted at specific instances).
 *
 *  The semantic LAW — that each `==` returns `true` for all inputs — is
 *  a value-level claim, witnessed per instance via `static_assert` on
 *  concrete probes. A concept cannot encode a universally-quantified
 *  runtime property directly; this is the conventional operational
 *  witness.
 *
 *  Parallel to how `IsRingLike` is the operational half of `IsRing`: a
 *  strict categorical `IsRingHomomorphism` would additionally witness
 *  identity preservation and require the strict axiom-hook tower. No
 *  such strict version is exported yet because the carrier-level hooks
 *  are not generally populated on machine-backed carriers.
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
// This concept encodes additive and multiplicative preservation only.
// It does not structurally require or witness preservation of identities
// (0 or 1), because `IsRingLike` does not expose identity elements.
// Unital / identity laws, when needed, must be witnessed by stricter
// concepts or on concrete probe values.

/** @section Formal_Verification */

// Operational witnesses on primitive carriers.
static_assert(IsRingLike<int>,
              "int satisfies IsRingLike (wrapping / two's-complement).");
static_assert(IsRingLike<unsigned int>,
              "unsigned int satisfies IsRingLike (modular arithmetic).");
static_assert(IsSemiringLike<unsigned int>,
              "unsigned int also satisfies IsSemiringLike "
              "(without needing unary negation).");

// Pure-syntactic-shape witnesses for HasRingOperators.  The shape
// concept fires on every carrier IsRingLike fires on (same body) and
// is the right name to use when the callsite depends on operators-
// closing rather than on ring-shaped axioms under the policy in force.
static_assert(HasRingOperators<int>,
              "int has the syntactic ring-operator surface.");
static_assert(HasRingOperators<unsigned int>,
              "unsigned int has the syntactic ring-operator surface.");

// HasLatticeOperators / HasLogicalOperators witnesses live with their
// definitions in `order:lattice` / `category:logic` respectively.

// unsigned int with wrapping arithmetic is the canonical total commutative
// ring: IsPeriodic (wraps at 2^N) satisfies IsTotal → IsMagma → IsMonoid →
// IsGroup.
static_assert(IsRing<unsigned int>,
              "unsigned int must satisfy IsRing (wrapping arithmetic).");
static_assert(IsCommutativeRing<unsigned int>,
              "unsigned int must satisfy IsCommutativeRing.");

// `Modular<N>` (the archetypal finite commutative ring Z/NZ) lives in
// `morphologies:cyclic` and is asserted there — algebra is upstream of
// morphologies and cannot reference it from here.

// bool with OR/AND is an idempotent commutative semiring (the Boolean rig).
// It is not a Ring: there is no additive inverse for True (True + x != False).
static_assert(IsRig<bool, std::logical_or<bool>, std::logical_and<bool>>,
              "bool must satisfy IsRig (Boolean semiring under OR/AND).");

}  // namespace dedekind::algebra
