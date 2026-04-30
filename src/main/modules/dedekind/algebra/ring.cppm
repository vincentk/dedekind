/**
 * @file dedekind/algebra/ring.cppm
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
import :universal;  // IsAlgebra (universal-algebra closure-tier predicate; PR
                    // #500 / #498)
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
 * companion is the universal-algebra anchor @c IsAlgebra<T, Add, Mult> from @c
 * :universal, which checks the functors close on @c T rather than the literal
 * operators.  The two shape concepts are siblings; pick whichever
 * matches the callsite's actual operator surface.
 *
 * Pattern-(b) carriers (e.g.\ @c Rational<long> under the active
 * numeric policy, IEEE @c double) satisfy this shape concept while
 * the strict @c category::IsRing refuses --- the literal-shape is
 * the right concept to gate templated bodies whose strict half
 * cannot fire.  The strict categorical counterpart is
 * @c dedekind::category::IsRing<T, Add, Mult> in @c category:total.
 */
export template <typename T>
concept HasRingOperators = requires(T a, T b) {
  { a + b } -> std::same_as<T>;
  { a - b } -> std::same_as<T>;
  { -a } -> std::same_as<T>;
  { a * b } -> std::same_as<T>;
};

// HasRingOperatorsFor was previously a 5-line concept here, checking
// `add(a, b) -> T` and `mult(a, b) -> T`.  The universal-algebra
// anchor IsAlgebra<T, Ops...> from :universal captures exactly that
// closure-tier check, and additionally requires std::regular<T> ---
// a strengthening relative to the retired HasRingOperatorsFor (and
// relative to the upstream IsAdditiveGroup chain, whose
// IsGroup/IsMonoid/IsSemigroup/IsMagma/IsTotal links do @b not
// require std::regular).  In practice every carrier the library
// realises as a ring is already regular (int, unsigned, double,
// Rational<I>, Complex<R>, Dual<F>, RigPolynomial<R>, ...), so the
// strengthening is principled rather than punitive: it pins the
// (A, F) bundle to value-semantics carriers, matching the textbook
// universal-algebra setting.  Downstream code that wanted the
// functor-parametric closure surface should use
// `IsAlgebra<T, Add, Mult>` directly; the literal-operator surface
// (where C++'s +, -, *, etc. close on T) is the separate sibling
// concept HasRingOperators above (#498/#500).

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
// AND the @b functor-parametric operator surface via the universal-
// algebra anchor @c IsAlgebra<T, Add, Mult> (which checks both
// @c add(a, b) -> T and @c mult(a, b) -> T).  Math-wins-over-C++
// semantics (per the README): if a carrier's @b literal operators
// don't close on T (e.g.\ bool's `+` returns int via promotion),
// then T is textbook-not-closed under those operators; but the same
// carrier may genuinely be a ring under @b non-standard functors
// (bool under std::bit_xor / std::bit_and is the Boolean ring 𝔽_2).
// The functor-parametric @c IsAlgebra shape captures the textbook
// closure regardless of which C++ operators happen to alias the
// ring's operations.  A callsite that wants the literal-operator
// surface @b in @b addition to the strict ring proof says so
// explicitly via `IsRing<T> && HasRingOperators<T>`.
export template <typename T, typename Add = std::plus<T>,
                 typename Mult = std::multiplies<T>>
concept IsRing =
    dedekind::category::IsRing<T, Add, Mult> && IsSemiring<T, Add, Mult> &&
    IsAdditiveGroup<T, Add> && IsAlgebra<T, Add, Mult>;

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
 * Canonical inhabitants: @c unsigned int (modular wrap), other wide
 * unsigned integral types whose width is at least @c sizeof(int)
 * (no integer-promotion path: @c unsigned long, @c unsigned long
 * long, @c std::size_t), @c RigPolynomial<unsigned int> (the
 * polynomial ring lifts elementwise).  @c int is @b not a
 * canonical inhabitant: signed-overflow UB defeats the @c IsPeriodic
 * axiom on which the strict @c IsRing chain depends, even though
 * the literal operators close on the carrier (see witness comment
 * below).
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

/** @section Semiring_Shape_And_Homomorphisms
 *
 *  The literal-operator shape concept @c HasRingOperators above
 *  covers the ring surface (+, -, unary -, *).  The semiring surface
 *  drops subtraction / unary negation but adds the structural
 *  identities @c T{} and @c T{1} --- the "no additive inverse"
 *  carriers (booleans under OR/AND, naturals, tropical semirings).
 *
 *  Below we add the semiring-shape sibling and the homomorphism
 *  shape concept that lifts the ring surface to morphism-preservation
 *  claims.  Both are pure-shape (no axiomatic claim); the strict
 *  categorical counterparts live in @c category:total.
 *
 *  History (#394 retire-Like sweep): @c IsRingLike (identical body
 *  to @c HasRingOperators), @c IsSemiringLike (identical body to
 *  @c HasSemiringOperators below), and @c IsRingLikeHomomorphism
 *  were collapsed into the literal-shape vocabulary --- the
 *  operational-vs-shape distinction had no structural difference, so
 *  the @c *Like names were redundant aliases.  Pattern-(b) carriers
 *  (e.g.\ @c Rational<long>, IEEE @c double) still satisfy these
 *  shape concepts via the same mechanism as before; the rationale
 *  for accepting them is now expressed by saying "the strict half
 *  (e.g.\ @c IsRing) does not fire on this carrier under the active
 *  numeric policy, but the literal-shape (@c HasRingOperators) does".
 */

/**
 * @concept HasSemiringOperators
 * @brief @b Pure @b syntactic @b shape: T closes strictly under @c +
 *        and @c *, with structural identities @c T{} and @c T{1}.
 *
 * @details
 * The semiring-shape sibling of @c HasRingOperators: drops binary /
 * unary @c - (no additive inverse required) but adds the structural
 * default-constructor and unit constructor.  Carriers that fire:
 * @c bool under OR/AND, @c unsigned int (also fires @c
 * HasRingOperators), naturals, tropical and other exotic semirings.
 *
 * Replaces the former @c IsSemiringLike (identical body) under the
 * #394 retire-Like sweep.
 */
export template <typename T>
concept HasSemiringOperators = requires(T a, T b) {
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
// `HasLogicalOperators` (the logical operator surface &&, ||, !
// shape) lives in `dedekind.category:logic` --- the topos-internal-
// logic partition, the deepest logic home in the project.  Note that
// for user-defined `T` overloaded `&&`/`||` are NOT short-circuiting
// (only the built-in operators on `bool` are); the concept makes no
// short-circuit claim.

// `algebra::IsRing` already exists earlier in this file as the
// bundled concept (`category::IsRing && IsSemiring && IsAdditiveGroup
// && IsAlgebra<T, Add, Mult>`); the functor-parametric
// shape clause was added there to encode the shape↔concept mapping
// the audit (#393) recorded under the math-wins-over-C++ stance ---
// see the doc-block on `IsRing` above for why the bundle uses the
// functor-parametric `IsAlgebra` rather than the literal
// `HasRingOperators`.

/**
 * @concept IsRingHomomorphism
 * @brief A callable φ that sends `Source` to `Target` with additive
 *        and multiplicative preservation:
 *
 *   φ(a + b) == φ(a) + φ(b)      additive preservation
 *   φ(a · b) == φ(a) · φ(b)      multiplicative preservation
 *
 *  The concept checks the STRUCTURAL shape of the claim:
 *   - `Source` and `Target` both have the literal ring operator
 *     surface (`HasRingOperators`),
 *   - `φ(a)` is well-typed and returns `Target`,
 *   - both preservation expressions are well-formed (yield `bool`).
 *
 *  Preservation of distinguished identities (φ(0) = 0, φ(1) = 1) is NOT
 *  encoded structurally here: `HasRingOperators` does not expose
 *  identity elements, so the concept cannot reach them. Unital /
 *  identity laws, when needed, must be checked by stricter concepts or
 *  on concrete probe values (cf. `dedekind.linear_algebra:embeddings`
 *  for the ℂ ↪ M₂(ℚ) and 𝔻 ↪ M₂(ℚ) cases where `zero_matrix2x2_v` and
 *  `identity_matrix2x2_v` are asserted at specific instances).
 *
 *  The semantic LAW --- that each `==` returns `true` for all inputs ---
 *  is a value-level claim, witnessed per instance via `static_assert`
 *  on concrete probes. A concept cannot encode a universally-quantified
 *  runtime property directly; this is the conventional shape witness.
 *
 *  A strict categorical `IsRingHomomorphism` would additionally witness
 *  identity preservation and require the strict axiom-hook tower. No
 *  such strict version is exported yet because the carrier-level hooks
 *  are not generally populated on machine-backed carriers.
 *
 *  Renamed from `IsRingLikeHomomorphism` under the #394 retire-Like
 *  sweep; body now uses `HasRingOperators` directly.
 */
// FIXME: add `IsGroupHomomorphism`, `IsMonoidHomomorphism`, and
// `IsSemiringHomomorphism` specialisations alongside once there are
// concrete call sites that need them (e.g. embedding modular groups
// into symmetric groups, or tropical-semiring homomorphisms).
export template <typename Phi, typename Source, typename Target>
concept IsRingHomomorphism =
    HasRingOperators<Source> && HasRingOperators<Target> &&
    requires(const Phi& phi, Source a, Source b) {
      { phi(a) } -> std::same_as<Target>;
      { phi(a + b) == (phi(a) + phi(b)) } -> std::same_as<bool>;
      { phi(a * b) == (phi(a) * phi(b)) } -> std::same_as<bool>;
    };
// This concept encodes additive and multiplicative preservation only.
// It does not structurally require or witness preservation of identities
// (0 or 1), because `HasRingOperators` does not expose identity elements.
// Unital / identity laws, when needed, must be witnessed by stricter
// concepts or on concrete probe values.

/** @section Formal_Verification */

// Shape witnesses on primitive carriers (note: HasRingOperators<int>
// and HasRingOperators<unsigned int> are also asserted further down
// in the literal-shape witness block --- the duplication is harmless
// and keeps both blocks self-contained).
static_assert(HasSemiringOperators<unsigned int>,
              "unsigned int satisfies HasSemiringOperators "
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
 *  The functor-parametric companion `IsAlgebra<U,
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
// Now expressed via the universal-algebra anchor IsAlgebra (#500/#498).
static_assert(IsAlgebra<unsigned short, std::plus<unsigned short>,
                        std::multiplies<unsigned short>>,
              "unsigned short closes under the functor surface "
              "(std::plus<U> / std::multiplies<U> return U by signature).");
static_assert(IsAlgebra<unsigned char, std::plus<unsigned char>,
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
static_assert(IsRing<unsigned long long>, "unsigned long long is a ring.");

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
