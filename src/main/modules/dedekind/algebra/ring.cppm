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
 * @concept HasRingOperators
 * @brief @b Pure @b syntactic @b shape: T closes strictly under @c +,
 *        @c -, @c * (binary and unary @c -).  Textbook closure ---
 *        results are exactly @c T, not just convertible to @c T.
 *
 * @details
 * Use this concept where the callsite needs the standard operators
 * @c {+, -, *} to compile @b and to close strictly on @c T.  This
 * is the textbook ring-closure reading: if @c T's @c + returns
 * something other than @c T (e.g.\ via integer promotion, as in
 * @c bool + bool @c -> int), then @c T is genuinely @b not closed
 * under @c +, and the concept correctly refuses.
 *
 * Carriers that fail strict closure under standard operators (@c bool,
 * @c char, narrow signed types) often still carry ring structure
 * under @b functor-wrapped operators --- @c std::bit_xor<bool>{}(a,b)
 * returns @c bool by the functor's signature regardless of the
 * internal integer-promotion path.  For those carriers the shape
 * companion is @c HasRingOperatorsFor<T, Add, Mult> below, which
 * checks the functors close on @c T rather than the literal
 * operators.  The two shape concepts are siblings; pick whichever
 * matches the callsite's actual operator surface.
 *
 * The structurally-similar @c IsRingLike is retained as the
 * @b operational variant for IEEE-edge-case carriers; see audit #393.
 * The strict categorical counterpart is
 * @c dedekind::category::IsRing<T, Add, Mult> in @c category:total.
 */
export template <typename T>
concept HasRingOperators = requires(T a, T b) {
  { a + b } -> std::same_as<T>;
  { a - b } -> std::same_as<T>;
  { -a } -> std::same_as<T>;
  { a * b } -> std::same_as<T>;
};

/**
 * @concept HasRingOperatorsFor
 * @brief @b Functor-parametric @b shape: T closes strictly under the
 *        @c Add and @c Mult @b functors (not the literal operators).
 *
 * @details
 * The escape hatch for carriers that satisfy the strict ring axioms
 * via non-standard functors but whose @b literal C++ operators don't
 * close on @c T because of integer promotion.  Canonical example:
 * @c bool under @c (std::bit_xor<bool>, std::bit_and<bool>) is a
 * Boolean ring (@f$\mathbb{F}_2@f$); both functors return @c bool by
 * signature, so this concept fires --- whereas @c HasRingOperators
 * <bool> does not, because @c bool ^ bool @c -> int.
 *
 * The strict @c HasRingOperators<T> is the parameter-free fixed
 * point @c HasRingOperatorsFor<T, std::plus<T>, std::multiplies<T>>
 * combined with the requirement that the literal @c + and @c * agree
 * with the functors.  Most ring carriers satisfy both; carriers like
 * @c bool satisfy only the functor variant, because their textbook
 * ring structure lives over different operators than the standard
 * @c + and @c *.
 */
export template <typename T, typename Add = std::plus<T>,
                 typename Mult = std::multiplies<T>>
concept HasRingOperatorsFor = requires(T a, T b, Add add, Mult mult) {
  { add(a, b) } -> std::same_as<T>;
  { mult(a, b) } -> std::same_as<T>;
};

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
// Bundle (#393): algebra::IsRing requires the strict categorical
// proof, the redundant IsSemiring + IsAdditiveGroup decomposition,
// AND the @b functor-parametric operator surface via
// HasRingOperatorsFor<T, Add, Mult>.  Math-wins-over-C++ semantics
// (per the README): if a carrier's @b literal operators don't close
// on T (e.g.\ bool's `+` returns int via promotion), then T is
// textbook-not-closed under those operators; but the same carrier
// may genuinely be a ring under @b non-standard functors (bool under
// std::bit_xor / std::bit_and is the Boolean ring 𝔽_2).  The
// functor-parametric shape captures the textbook closure regardless
// of which C++ operators happen to alias the ring's operations.  A
// callsite that wants the literal-operator surface @b in @b addition
// to the strict ring proof says so explicitly via
// `IsRing<T> && HasRingOperators<T>`.
export template <typename T, typename Add = std::plus<T>,
                 typename Mult = std::multiplies<T>>
concept IsRing =
    dedekind::category::IsRing<T, Add, Mult> && IsSemiring<T, Add, Mult> &&
    IsAdditiveGroup<T, Add> && HasRingOperatorsFor<T, Add, Mult>;

/**
 * @concept IsArithmeticRing
 * @brief @b The @b seal: a carrier that is a ring AND whose ring
 *        structure agrees with the standard C++ arithmetic operators.
 *
 * @details
 * Where the concepts meet.  @c IsArithmeticRing<T> requires both:
 *
 *   - @c IsRing<T, std::plus<T>, std::multiplies<T>> --- the strict
 *     categorical proof, with the canonical Add/Mult functors;
 *   - @c HasRingOperators<T> --- the literal C++ operators @c +,
 *     @c -, @c *, unary @c - close strictly on @c T.
 *
 * That is, @c IsArithmeticRing<T> certifies that the ring structure
 * is reachable through the natural C++ syntax: writing @c a + b in
 * source code computes the ring's addition, and the result genuinely
 * stays in @c T.  No promotion, no functor indirection, no operator
 * disagreement.
 *
 * Canonical inhabitants: @c int (modular wrap), @c unsigned int
 * (modular), @c Rational<I> (exact), @c Complex<R> for arithmetic
 * @c R, @c 𝔽64 (which deliberately defines its literal @c + to be
 * the field's XOR).
 *
 * Carriers that satisfy @c IsRing but @b not @c IsArithmeticRing
 * include @c bool under @c (std::bit_xor, std::bit_and) (the
 * Boolean ring @f$\mathbb{F}_2@f$): the strict ring claim holds
 * via the functors, but @c bool's literal @c + returns @c int via
 * integer promotion --- the ring structure does @b not align with
 * the standard operators on this carrier.  The math-wins-over-C++
 * stance the project adopts (per @c README.md) makes this
 * non-alignment a real distinction the type system surfaces.
 *
 * Use @c IsArithmeticRing where the callsite wants to write the
 * ring operations in plain C++ syntax and trust the result stays in
 * the carrier; use @c IsRing where the callsite is willing to
 * accept any ring proof, including ones expressed through
 * functor-parametric Add/Mult.
 *
 * Introduced under #393 as the user-requested "where the concepts
 * meet" point.
 */
export template <typename T>
concept IsArithmeticRing =
    IsRing<T, std::plus<T>, std::multiplies<T>> && HasRingOperators<T>;

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

// `HasRingOperators` is defined earlier in this file (right after the
// `using namespace dedekind::category;` declaration) so the bundled
// `algebra::IsRing` concept above can refer to it.
//
// `HasLatticeOperators` (bitwise &, |, ^, ~ shape) lives in
// `dedekind.order:lattice` --- order is the natural home for
// lattice-flavoured concepts; this file kept ring-flavoured.
// `HasLogicalOperators` (short-circuiting &&, ||, ! shape) lives
// in `dedekind.category:logic` --- the topos-internal-logic
// partition, the deepest logic home in the project.

// `algebra::IsRing` already exists earlier in this file as the
// bundled concept (`category::IsRing && IsSemiring && IsAdditiveGroup`);
// the `&& HasRingOperators<T>` clause was added there to encode the
// shape↔concept mapping the audit (#393) recorded.

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

// Pure-syntactic-shape witnesses for HasRingOperators.  Strict
// closure: int and unsigned int's literal operators yield int /
// unsigned int respectively, both same_as<T>.
static_assert(HasRingOperators<int>,
              "int has the syntactic ring-operator surface.");
static_assert(HasRingOperators<unsigned int>,
              "unsigned int has the syntactic ring-operator surface.");

// Sealing witness: unsigned int is the canonical arithmetic ring ---
// strict categorical ring proof under (std::plus, std::multiplies)
// (modular-wrap periodicity gives the IsPeriodic axiom that lifts to
// IsTotal → IsMagma → IsMonoid → IsGroup → IsRing) AND the literal-
// operator surface compiles and closes strictly.  This is the
// meet-point between the math-textbook ring and the C++ standard
// operators.
//
// `int` is deliberately NOT an arithmetic ring under the project's
// semantics: signed-overflow UB defeats the IsPeriodic claim, so
// the strict half fails even though the literal operators close.
// The math-wins-over-C++ stance (per README) makes that a feature,
// not a bug.
static_assert(IsArithmeticRing<unsigned int>,
              "unsigned int is the canonical arithmetic ring "
              "(modular wrap; ring proof + literal operators agree).");

// HasLatticeOperators / HasLogicalOperators witnesses live with their
// definitions in `order:lattice` / `category:logic` respectively.

// unsigned int with wrapping arithmetic is the canonical total commutative
// ring: IsPeriodic (wraps at 2^N) satisfies IsTotal → IsMagma → IsMonoid →
// IsGroup.
static_assert(IsRing<unsigned int>,
              "unsigned int must satisfy IsRing (wrapping arithmetic).");
static_assert(IsCommutativeRing<unsigned int>,
              "unsigned int must satisfy IsCommutativeRing.");

/** @subsection Narrow_Unsigned_Audit
 *
 *  Integer promotion (C++ [conv.prom]) lifts every unsigned-integral
 *  type narrower than `int` to `int` (or to `unsigned int` if `int`
 *  cannot represent the source range; on every platform we target,
 *  `int` >= `short` >= `char`, so `int` always wins).  Concretely:
 *
 *      unsigned char  + unsigned char  -> int
 *      unsigned short + unsigned short -> int
 *
 *  Under the math-wins-over-C++ stance (README), this is a textbook
 *  closure failure: `unsigned short` is genuinely *not* closed under
 *  the literal `+`.  `HasRingOperators<unsigned short>` correctly
 *  refuses; so does `IsArithmeticRing<unsigned short>`.
 *
 *  The functor-parametric companion `HasRingOperatorsFor<U,
 *  std::plus<U>, std::multiplies<U>>` *does* fire, because functors
 *  return `U` by signature regardless of the internal promotion path.
 *  Algebraically, every `std::unsigned_integral U` is the modular
 *  ring Z/2^bits(U)Z under `(std::plus<U>, std::multiplies<U>)`; the
 *  bundled `algebra::IsRing<U>` therefore fires for every
 *  `std::unsigned_integral` (narrow or wide) -- it uses the
 *  functor-parametric shape, which the math respects.
 *
 *  `IsArithmeticRing` is the strict literal-operator seal and only
 *  fires on unsigned types whose width is >= sizeof(int): namely
 *  `unsigned int`, `unsigned long`, `unsigned long long`, `std::size_t`,
 *  and any `std::uintptr_t` synonym.
 */

// Strict literal-operator closure FAILS on narrow unsigned types via
// integer promotion.  These negative witnesses pin the behaviour the
// math-wins stance requires: textbook-not-closed types are refused.
static_assert(!HasRingOperators<unsigned short>,
              "unsigned short is not closed under literal +,-,*: "
              "integer promotion lifts the result to int.");
static_assert(!HasRingOperators<unsigned char>,
              "unsigned char is not closed under literal +,-,*: "
              "integer promotion lifts the result to int.");

// Functor-parametric closure PASSES on all std::unsigned_integral
// types: std::plus<U> / std::multiplies<U> return U by signature.
static_assert(HasRingOperatorsFor<unsigned short, std::plus<unsigned short>,
                                  std::multiplies<unsigned short>>,
              "unsigned short closes under the functor surface "
              "(std::plus<U> / std::multiplies<U> return U by signature).");
static_assert(HasRingOperatorsFor<unsigned char, std::plus<unsigned char>,
                                  std::multiplies<unsigned char>>);

// Wider unsigned types: width >= sizeof(int), no promotion path.  The
// arithmetic-ring seal fires on each: strict ring proof (modular wrap)
// AND literal operators close on the carrier.  These are the
// `std::unsigned_integral` types for which the textbook ring and the
// C++ standard operators agree.
static_assert(IsArithmeticRing<unsigned long>,
              "unsigned long is an arithmetic ring (modular wrap).");
static_assert(IsArithmeticRing<unsigned long long>,
              "unsigned long long is an arithmetic ring (modular wrap).");
static_assert(IsArithmeticRing<std::size_t>,
              "std::size_t is an arithmetic ring (modular wrap).");

// All `std::unsigned_integral` carriers, narrow or wide, are rings
// under the functor-parametric bundle: the modular-wrap proof drives
// IsPeriodic -> IsTotal -> IsMagma -> IsMonoid -> IsGroup -> IsRing,
// and the std::plus / std::multiplies functors close on U by signature.
// `IsArithmeticRing` (the strict literal-operator seal) is the
// stronger claim and is only available on the wide ones.
static_assert(IsRing<unsigned char>,
              "unsigned char is a ring under (std::plus, std::multiplies) "
              "via the functor surface, even though its literal operators "
              "promote to int.");
static_assert(IsRing<unsigned short>,
              "unsigned short is likewise a functor-surface ring.");
static_assert(IsRing<unsigned long>,
              "unsigned long is a ring (also IsArithmeticRing -- both "
              "halves agree on wide unsigned carriers).");
static_assert(IsRing<unsigned long long>,
              "unsigned long long is a ring.");

// Negative witness: signed narrow types fail the strict ring proof
// (signed-overflow UB defeats IsPeriodic) AND the literal-operator
// surface (promotion to int).  No `static_assert` here -- nothing for
// the strict half to certify on signed carriers under the
// math-wins-over-C++ semantics.

// `Modular<N>` (the archetypal finite commutative ring Z/NZ) lives in
// `morphologies:cyclic` and is asserted there — algebra is upstream of
// morphologies and cannot reference it from here.

// bool with OR/AND is an idempotent commutative semiring (the Boolean rig).
// It is not a Ring: there is no additive inverse for True (True + x != False).
static_assert(IsRig<bool, std::logical_or<bool>, std::logical_and<bool>>,
              "bool must satisfy IsRig (Boolean semiring under OR/AND).");

}  // namespace dedekind::algebra
