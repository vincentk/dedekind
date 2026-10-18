/**
 * @file dedekind/category/cartesian.cppm
 * @partition :cartesian
 * @brief Cartesian Closed Category (CCC) Foundations.
 * @section cartesian__Cartesian
 * This partition defines the structures required for a category to be
 * Cartesian Closed. In the Dedekind universe, this provides the "Product"
 * and "Function Space" (Exponential) mechanics necessary for ETCS.
 *
 * @section cartesian__Std_Namespace_Mappings
 * This partition asserts bidirectional mappings between the CCC-completing
 * categorical constructs and `std` types:
 *
 * | Concept          | `std` representative                          |
 * |------------------|-----------------------------------------------|
 * | `IsCoproduct`    | `std::variant<A, B>` (via ι₁ / ι₂)           |
 * | `IsExponential`  | `std::function<B(A)>`, lambdas                |
 *
 * @note The product side --- `IsProduct` for `std::pair<A, B>`, plus
 * `IsProductProjection` / `IsProjectedProduct` / `mediate_product` /
 * `IsArrowFromProduct` --- moved to `:limit` under #637, since binary
 * products are categorical limits (limit of a discrete two-object diagram).
 * @c :cartesian imports @c :limit, so consumers of the CCC umbrella
 * (`IsCartesianClosed`) keep reaching the product machinery transitively.
 *
 * Wikipedia: Cartesian closed category, Exponential object
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "Mathematical science is in my opinion an indivisible whole, an
 * organism whose vitality is conditioned upon the connection of its parts."
 *       -- David Hilbert, Mathematical Problems (1900)
 */
module;

#include <concepts>
#include <cstddef>  // std::size_t: the summand of an injection
#include <functional>
#include <memory>
#include <tuple>
#include <type_traits>
#include <utility>
#include <variant>

export module dedekind.category:cartesian;

import :logic;
import :limit;
import :mereology;
import :morphism;  // for IsArrow / Identity (used by arrow_as_relation bridge)
import :species;

namespace dedekind::category {

// NOTE (#637 re-home): the @c IsProduct concept family --- @c
// IsProduct, @c IsProductProjection, @c
// IsProjectedProduct, @c IsArrowFromProduct, plus the @c
// SpeciesTraits<std::pair<A, B>> / @c SpeciesTraits<std::tuple<Ts...>>
// specialisations and the @c mediate_product factory --- moved to @c
// :limit.  Products are categorical limits (limit of a discrete two-
// object diagram); their natural home is alongside @c IsTerminalObject /
// @c IsInitialObject in @c :limit.  This partition retains only the
// CCC-completing pieces (@c IsExponential, @c IsCartesianClosed, the @c
// curry / uncurry / eval surface, @c IsCoproduct).  Consumers reach the
// product machinery through @c :limit (which @c :cartesian imports) or
// directly via @c import dedekind.category.
/**
 * @concept IsExponential
 * @brief Categorical exponential object @c B^A — generalised structural
 *        recogniser of "eval morphism via call expression" (#698 Slice 6).
 *
 * @details
 * In a Cartesian closed category C, an exponential object @c B^A is an
 * object @c E with an evaluation morphism @c eval: @c E @c × @c A @c → @c B
 * satisfying the currying universal property.  The concrete shape of
 * @c E varies by category:
 *
 *   - @b Set / @b Cpp: @c E is a function-space type (lambda,
 *     @c std::function<B(A)>, @c std::move_only_function<B(A)>, any
 *     callable struct) — the historical reading.
 *   - @b Heyting @b algebra (thin CCC): @c E is a @em value of the
 *     carrier @c T representing @c a @c → @c b; @c operator()(x)
 *     computes the meet @c e @c ∧ x.  The universal property says
 *     @c e @c ∧ a @c ≤ @c b.
 *
 * The unifying observation: in @b every CCC, the eval morphism is
 * implemented by @b applying the exponential object to its argument.
 * In C++ that is the call expression @c e(a).  This concept therefore
 * recognises exponentials structurally — by the well-formedness and
 * return type of the call — without committing to function-space
 * representation.  Pure Juliet posture: no tag, no CPO, no wrapper-
 * for-tag.
 *
 * @section cartesian__Strict_Same_As
 * The result type is checked via @c std::same_as<B>, not @c
 * std::convertible_to<B>.  This preserves the Honest Rejection
 * negative witness below (a function returning @c int does not
 * satisfy @c IsExponential<…, int, bool>, even though @c int implicitly
 * converts to @c bool in C++).
 *
 * @section cartesian__Structural_vs_Universal_Property
 * The concept captures the eval morphism's @b operational shape
 * (eval(e, a) → B) but does @b not enforce the currying universal
 * property (∀ @c f : @c Γ @c × @c A @c → @c B, ∃! @c f̂ : @c Γ @c → @c E).
 * Universal-property enforcement is tracked separately as #707.
 *
 * @section cartesian__IsArrow_Harmonisation
 * @c IsExponential does not require @c IsArrow.  Function-space
 * inhabitants like @c std::function<B(A)> satisfy this concept but
 * @b not @c IsArrow (no @c Domain / @c Codomain typedefs).  The
 * project's canonical arrow-shaped exponentials (e.g.\ outputs of
 * @c arrow<A, B>(…), @c HeytingExponential<T, Rel>) satisfy both.
 * The refinement @c IsArrowExponential @c = @c IsExponential @c
 * && @c IsArrow is tracked as #706.
 *
 * @section cartesian__Slice6_Generalization
 * Pre-#698-Slice-6 reading was Set-specific: @c std::move_constructible
 * + @c std::invoke_result_t.  Slice 6 generalised to the structural
 * call-shape recogniser so that lattice exponentials (Heyting
 * implications, see @c :lattice::HeytingExponential) participate
 * uniformly.  Behavioural preservation: every existing Set-side witness
 * (function-spaces, lambdas, function-objects) continues to fire
 * because @b being callable with the right signature was already the
 * load-bearing fact under the old definition.
 */
export template <typename E, typename A, typename B>
concept IsExponential = requires(E e, A a) {
  { e(a) } -> std::same_as<B>;
};

/**
 * @concept IsArrowExponential
 * @brief An exponential @c E @c : @c A @c → @c B that also satisfies
 *        the project's morphism vocabulary (@c IsArrow with matching
 *        @c Domain / @c Codomain typedefs).  #706.
 *
 * @details
 * Refines @c IsExponential to additionally require @c IsArrow's
 * structural commitments — @c E::Domain typedef equal to @c A,
 * @c E::Codomain typedef equal to @c B.  Function-space inhabitants
 * (@c std::function<B(A)>, raw lambdas) satisfy @c IsExponential but
 * @b not @c IsArrowExponential because they don't expose the
 * @c Domain / @c Codomain typedefs.  Project-shipped arrow-shaped
 * exponentials — outputs of @c arrow<A, B>(…), @c HeytingExponential,
 * @c :morphism::Morphism, etc. — satisfy both.
 *
 * @section cartesian__IsArrowExponential_Consumer
 * Migrated from an ad hoc @c IsExponential<…> @c && @c IsArrow<…>
 * combination at @c :etcs::HasAxiom6Exponentiation, which named the
 * same content inline.  Refactoring that consumer to the named
 * refinement removes a chicken-and-egg seam (the supply for the
 * refinement was inlined at the one demand site) — see #706 for the
 * triage rationale.
 *
 * Consumers that want the project's morphism discipline gate on
 * @c IsArrowExponential; consumers that accept any callable
 * (function-spaces, raw lambdas, structural-only) use the bare
 * @c IsExponential.  Split mirrors the @c :morphism @c IsArrow
 * vs.\ generic-callable distinction.
 *
 * @tparam E The exponential object (must be @c IsArrow-shaped).
 * @tparam A The domain.
 * @tparam B The codomain.
 */
export template <typename E, typename A, typename B>
concept IsArrowExponential = IsExponential<E, A, B> && IsArrow<E> &&
                             std::same_as<Dom<E>, A> && std::same_as<Cod<E>, B>;

/**
 * @brief Internal Hom Factory (Exponential)
 * @details 1. Fully Automatic: Inferred from lambda signature
 */
export template <typename F>
constexpr auto exponential(F&& f) {
  return arrow(std::forward<F>(f));
}

/**
 * @details 2. Semi-Automatic: Explicit Domain A, Inferred Codomain B
 */
export template <typename A, typename F>
constexpr auto exponential(F&& f) {
  return arrow<A>(std::forward<F>(f));
}

/**
 * @details 3. Fully Explicit: Explicit A and B
 */
export template <typename A, typename B, typename F>
constexpr auto exponential(F&& f) {
  return arrow<A, B>(std::forward<F>(f));
}

/**
 * @brief The Exponential Object B^A (Type Alias)
 * @details Now defined specifically as the result of the exponential factory.
 */
export template <typename A, typename B>
using Exponential =
    decltype(exponential<A, B>(std::declval<std::function<B(A)>>()));

/**
 * @brief Currying: (X × A → B) ⟹ (X → B^A)
 */
export template <IsArrowFromProduct F>
auto curry(F&& f) {
  using X = typename Dom<F>::first_type;
  using A = typename Dom<F>::second_type;

  // Heap-allocate the morphism to ensure it can be captured by the inner lambda
  // and called multiple times without copying.
  auto shared_f = std::make_shared<std::decay_t<F>>(std::forward<F>(f));

  return arrow([shared_f](const X& x) {
    return exponential([shared_f, x](const A& a) {
      // Call the shared morphism
      return (*shared_f)({x, a});
    });
  });
}

/**
 * @brief Uncurrying: (X → B^A) ⟹ (X × A → B)
 * @details Transforms a function returning a function into a single function
 * taking a categorical product (std::pair).
 */
export template <IsArrow F>
  requires IsArrow<Cod<F>>
auto uncurry(F&& f) {
  using X = Dom<F>;
  using Exp = Cod<F>;
  using A = Dom<Exp>;
  // using B = Cod<Exp>;

  // Use explicit types in the lambda so signature_extractor works
  return arrow([f = std::forward<F>(f)](const std::pair<X, A>& p) {
    return f(p.first)(p.second);
  });
}

/**
 * @concept IsCoproduct
 * @brief Categorification of `std::variant<A, B>` as the categorical
 * coproduct (A + B).
 * @details A coproduct of A and B is an object C equipped with injection
 * morphisms ι₁: A → C and ι₂: B → C such that for any object X with morphisms
 * f: A → X and g: B → X, there exists a unique morphism v: C → X making the
 * following diagram commute:
 * @code
 *   A       B
 *    \     /
 *    ι₁   ι₂
 *      \ /
 *       C
 *      / \
 *     f   g
 *    /     \
 *   X       X
 * @endcode
 *
 * `std::variant<A, B>` satisfies this concept: any value of type A or B can be
 * injected into the variant, and `std::visit` provides the universal
 * elimination morphism.
 *
 * @note (#881) The @c Op-refinement (the injection factory @c Op::inl / @c
 * Op::inr) is landed here for symmetry with @c IsProduct's pairing factory, but
 * its consumer is @b not the set join (which is product-shaped).  Its home is
 * the set-expression @b collapse: the reduced-form outcome space
 * @c {Ø, Singleton, Interval, Set, U} is a coproduct and each collapse rule is
 * an injection into one summand (see #865, and the boundary injections
 * @c Set{AlwaysTrue/False} == U/Ø in #685).  The recursive case (fold over an
 * initial F-algebra, @c ℕ = @c μX.1+X) is #449.  Until one of those lands, the
 * @c Op parameter is exercised only by its own witnesses.
 */
export template <typename T, typename A, typename B, typename Op = AnyOperation>
concept IsCoproduct =
    // Default sentinel: the legacy structural check --- T is constructible from
    // either summand.
    (std::same_as<Op, AnyOperation> &&
     requires(A a, B b) {
       { T(a) } -> std::same_as<T>;
       { T(b) } -> std::same_as<T>;
     }) ||
    // A named Op SUPERSEDES that check (it is ambiguous for equal summands):
    // dual to IsProduct's single binary pairing factory, a coproduct has TWO
    // unary injection constructors (the Haskell Left / Right), so a named Op
    // exposes them as distinct Op::inl : A → C, Op::inr : B → C.  Two named
    // constructors (not one overloaded call, nor the constructor check) keep
    // the injections distinguishable even for equal summands A == B, so the
    // canonical std::variant<int,int> (via indexed ι_1 / ι_2) qualifies.
    requires(const A& a, const B& b) {
      { Op::inl(a) } -> std::convertible_to<T>;
      { Op::inr(b) } -> std::convertible_to<T>;
    };

/**
 * @brief Left injection ι₁: A → A + B.
 * @details Wraps a value of type A into the coproduct `std::variant<A, B>`.
 * When A and B are the same type, uses `std::in_place_index<0>` to
 * unambiguously select the first alternative.
 */
export template <typename A, typename B>
auto ι_1(A&& value) {
  using Var = std::variant<A, B>;
  if constexpr (std::same_as<A, B>) {
    return Var(std::in_place_index<0>, std::forward<A>(value));
  } else {
    return Var(std::forward<A>(value));
  }
}

/**
 * @brief Right injection ι₂: B → A + B.
 * @details Wraps a value of type B into the coproduct `std::variant<A, B>`.
 * When A and B are the same type, uses `std::in_place_index<1>` to
 * unambiguously select the second alternative.
 */
export template <typename A, typename B>
auto ι_2(B&& value) {
  using Var = std::variant<A, B>;
  if constexpr (std::same_as<A, B>) {
    return Var(std::in_place_index<1>, std::forward<B>(value));
  } else {
    return Var(std::forward<B>(value));
  }
}

/** @brief The injections of the coproduct @c std::variant<A, B> as a named
 *  operation (@c IsCoproduct's @c Op, #881): @c inl / @c inr place a value by
 *  @b index, so equal summands stay apart (@c variant<int, int>).  Static
 *  members, as the concept asks for @c Op::inl(a) / @c Op::inr(b).
 *  @tparam A, B the summands. */
export template <typename A, typename B>
struct Injections {
  static constexpr std::variant<A, B> inl(const A& a)
    requires std::constructible_from<std::variant<A, B>,
                                     std::in_place_index_t<0>, const A&>
  {
    return std::variant<A, B>(std::in_place_index<0>, a);
  }
  static constexpr std::variant<A, B> inr(const B& b)
    requires std::constructible_from<std::variant<A, B>,
                                     std::in_place_index_t<1>, const B&>
  {
    return std::variant<A, B>(std::in_place_index<1>, b);
  }
};
static_assert(
    IsCoproduct<std::variant<int, int>, int, int, Injections<int, int>> &&
        Injections<int, int>::inl(1).index() == 0 &&
        Injections<int, int>::inr(1).index() == 1,
    "the named injections keep equal summands apart");

/** @brief The injection @c ι_K : A → A + B as an arrow: monic (it is
 *  injective, and its image is disjoint from the other summand's), the ground
 *  of a sum's strength reduction (a set inside one injection's image pulls
 *  back along it without loss).  @tparam K the summand, 1 or 2. */
export template <std::size_t K, typename A, typename B>
  requires(K == 1 || K == 2)
struct Injection {
  using Domain = std::conditional_t<K == 1, A, B>;
  using Codomain = std::variant<A, B>;
  using is_monic_arrow_tag = void;
  constexpr Codomain operator()(const Domain& x) const
    requires std::constructible_from<Codomain, std::in_place_index_t<K - 1>,
                                     const Domain&>
  {
    return Codomain(std::in_place_index<K - 1>, x);
  }
};
static_assert(IsMonicArrow<Injection<1, int, int>> &&
                  IsMonicArrow<Injection<2, int, int>> &&
                  Injection<2, int, int>{}(3).index() == 1,
              "the injections of a sum are monic");
static_assert(
    !IsCoproduct<std::variant<std::unique_ptr<int>, int>, std::unique_ptr<int>,
                 int, Injections<std::unique_ptr<int>, int>> &&
        !IsArrow<Injection<1, std::unique_ptr<int>, int>>,
    "a summand the variant cannot copy in is refused, not accepted and "
    "broken at the call");

template <std::integral V>
inline constexpr bool is_symmetric_relation_v<std::equal_to<V>> = true;
template <std::integral V>
inline constexpr bool is_transitive_relation_v<std::equal_to<V>> = true;

static_assert(IsBinaryRelation<std::equal_to<int>, int, int>,
              "std::equal_to<int> is a binary relation on int × int.");
static_assert(IsEquivalenceRelation<std::equal_to<int>, int>,
              "std::equal_to<int> is the canonical equivalence relation: "
              "reflexive, symmetric, transitive.");
static_assert(IsDirectedGraph<std::equal_to<int>, int>,
              "Every binary relation on V × V is a directed graph "
              "structurally; std::equal_to<int> is the trivial loop graph.");
static_assert(IsUndirectedGraph<std::equal_to<int>, int>,
              "std::equal_to<int> is symmetric, hence an undirected graph.");

// Negative witness: std::equal_to<double> is NOT registered as
// reflexive (NaN == NaN is false under IEEE 754), so it does not
// fire IsEquivalenceRelation.  Pinned mechanically so the policy
// is type-checked, not just commentary.
static_assert(!is_reflexive_relation_v<std::equal_to<double>>,
              "std::equal_to<double> is NOT reflexive (NaN == NaN is false "
              "under IEEE 754); the integral-only constraint above prevents "
              "the floating-point specialisation from firing.");
static_assert(!IsEquivalenceRelation<std::equal_to<double>, double>,
              "std::equal_to<double> is therefore NOT registered as an "
              "equivalence relation; downstream callers needing structural "
              "equivalence on double must opt-in site-locally with a NaN-"
              "exclusion clause.");

// ---------------------------------------------------------------------------
// Congruence (universal-algebra anchor — #718 Slice 0).
//
// An equivalence relation @c R on a carrier @c V is a @b congruence
// with respect to an operation @c Op @c : @c V @c × @c V @c → @c V
// when @c R is preserved by @c Op:
//
//   @c R(x, x') @c ∧ @c R(y, y') @c ⇒ @c R(Op(x, y), @c Op(x', y'))
//
// Reference: Burris & Sankappanavar, @em A @em Course @em in
// @em Universal @em Algebra (Springer GTM 78, 1981), Definition II.5.1.
// The HSP theorem (Birkhoff 1935 / Burris-Sankappanavar §II.11) sits
// directly atop this concept: a class of algebras is closed under
// homomorphic images iff every kernel-of-homomorphism is a congruence.
//
// Single-operation case landed first; the variadic
// @c IsCongruence<R, V, Op...> for multi-op algebras (e.g.\ a ring's
// (+, ·) joint congruence) lifts when the first ring-quotient witness
// needs it.  Sollbruchstelle named at @c is_congruence_v's primary.
// ---------------------------------------------------------------------------

/**
 * @brief User-declared congruence witness: @c R respects @c Op on @c T.
 * @details A congruence is an equivalence relation preserved by @c Op:
 *          @c R(x, x') @c ∧ @c R(y, y') @c ⇒ @c R(Op(x, y), @c Op(x', y')).
 *          The full congruence definition is then
 *          @c IsEquivalenceRelation<R, T> @c ∧ @c is_congruence_v<R, T, Op>.
 *          Cannot be checked at compile time in general; opt-in.
 *
 * Single-operation form.  Multi-op congruences for ring-flavoured
 * carriers (joint @c (+, @c ·) preservation) wait on a downstream
 * witness; the variadic lift is the Sollbruchstelle here.
 */
export template <typename R, typename T, typename Op>
inline constexpr bool is_congruence_v = false;

/**
 * @concept IsCongruence
 * @brief A homogeneous binary relation @c R on a carrier @c V is a
 *        @b congruence w.r.t.\ operation @c Op when it is an
 *        equivalence relation and respects @c Op.
 *
 * @details Stratifies @c IsEquivalenceRelation by the operation that
 *          must be respected: every congruence is an equivalence
 *          relation, but not every equivalence relation is a congruence
 *          (a textbook foil: @c |x| @c = @c |y| on @c ℤ is an
 *          equivalence but @b not a congruence for @c +, since
 *          @c 3 @c ~ @c -3 and @c 5 @c ~ @c 5 yet
 *          @c 3+5 @c = @c 8 @c ≄ @c 2 @c = @c -3+5).
 *
 *          Universal-algebra reference: Burris-Sankappanavar §II.5.
 *          Form-chain row 2 in the quotient categorification (#718).
 *
 * @tparam R The candidate equivalence-relation type.
 * @tparam V The homogeneous carrier on which @c R lives.
 * @tparam Op The binary operation @c V @c × @c V @c → @c V that @c R
 *            must preserve.
 *
 * @section cartesian__IsCongruence_Op_Shape_Gate
 * The concept body requires @c Op to be structurally well-formed as
 * a binary operation @c V @c × @c V @c → @c V: @c Op(v, @c v) must be
 * callable and produce a result convertible back to @c V.  Without this
 * shape gate, a malformed @c Op could be registered via
 * @c is_congruence_v and the concept would still pass — type-checked
 * meaninglessness.  The gate is structural-shape only (mirroring the
 * upstream relation traits' shape-only semantics), not a verification
 * of the congruence law itself, which remains an opt-in honesty
 * obligation.
 */
export template <typename R, typename V, typename Op>
concept IsCongruence =
    IsEquivalenceRelation<R, V> && is_congruence_v<R, V, Op> &&
    requires(const V& v, const Op& op) {
      { op(v, v) } -> std::convertible_to<V>;
    };

// ---------------------------------------------------------------------------
// Canonical positive witnesses: std::equal_to<V> is a congruence on
// integral V w.r.t.\ the std-lib binary operations registered below
// (plus / multiplies / minus / bit_or / bit_and / bit_xor).
//
// Reasoning: equality is the smallest equivalence relation on V; if
// x == x' and y == y' then Op(x, y) is the same computation as
// Op(x', y') by referential transparency, so Op(x, y) == Op(x', y').
// This argument extends to any well-defined Op on integral V; the
// finite registered list reflects the project's std-lib opt-in
// discipline (downstream callers opt-in for further Ops or custom
// callables site-locally), not a constraint on which Ops the
// argument applies to.
//
// The integral constraint mirrors the upstream IsEquivalenceRelation
// gate (via is_reflexive_relation_v / is_symmetric_relation_v /
// is_transitive_relation_v) that excludes NaN-on-float pathology for
// std::equal_to.
// ---------------------------------------------------------------------------
template <std::integral V>
inline constexpr bool is_congruence_v<std::equal_to<V>, V, std::plus<V>> = true;
template <std::integral V>
inline constexpr bool is_congruence_v<std::equal_to<V>, V, std::multiplies<V>> =
    true;
template <std::integral V>
inline constexpr bool is_congruence_v<std::equal_to<V>, V, std::minus<V>> =
    true;
template <std::integral V>
inline constexpr bool is_congruence_v<std::equal_to<V>, V, std::bit_or<V>> =
    true;
template <std::integral V>
inline constexpr bool is_congruence_v<std::equal_to<V>, V, std::bit_and<V>> =
    true;
template <std::integral V>
inline constexpr bool is_congruence_v<std::equal_to<V>, V, std::bit_xor<V>> =
    true;

static_assert(IsCongruence<std::equal_to<int>, int, std::plus<int>>,
              "std::equal_to<int> is a congruence w.r.t. + on int: "
              "equality is preserved by + via referential transparency "
              "(and similarly for the other std-lib binary ops "
              "registered above).");
static_assert(IsCongruence<std::equal_to<int>, int, std::multiplies<int>>,
              "std::equal_to<int> is a congruence w.r.t. * on int.");
static_assert(
    IsCongruence<std::equal_to<unsigned>, unsigned, std::bit_or<unsigned>>,
    "std::equal_to<unsigned> is a congruence w.r.t. bitwise OR.");

// Negative witness: std::equal_to<double> is not an equivalence relation
// (NaN == NaN is false under IEEE 754), so it cannot be a congruence
// for any operation — IsEquivalenceRelation is the upstream gate.
static_assert(!IsCongruence<std::equal_to<double>, double, std::plus<double>>,
              "std::equal_to<double> is not an equivalence relation under "
              "IEEE 754 (NaN-on-reflexivity); therefore not a congruence "
              "for any operation.  Honest-Rejection inherited from "
              "IsEquivalenceRelation upstream.");

// ---------------------------------------------------------------------------
// operator[] — eval / CCC counit (#531, step 1).
//
// A C++ subscript expression @c s[i] on a fixed-size carrier @c Seq
// indexed by @c Idx is, in CCC vocabulary (Mac Lane @em CWM §IV.6;
// Pierce @em TAPL §29), the @b evaluation @b map @c eval: @c Idx
// @c × @c Seq @c → @c Codomain — the counit of the function-type
// adjunction @c (- @c × @c A) @c ⊣ @c (-)^A.  Equivalently after
// currying, @c s @c : @c Idx @c → @c Codomain in @b Set, with
// @c s[i] @c = @c eval(i, @c s).
//
// The codebase reifies this in two layers (mirroring the
// @c Has*Operators / strict-concept split elsewhere):
//
//   1. @c HasSubscriptOperator<Seq, @c Idx> — purely syntactic
//      operator-surface predicate.
//   2. @c IsEvalArrow<Seq, @c Idx> — opt-in, witnesses the CCC-counit
//      reading at the carrier site via @c is_eval_arrow_v.
// ---------------------------------------------------------------------------

/** @concept HasSubscriptOperator
 *  @brief Syntactic check: @c s[i] compiles for @c Seq, @c Idx.
 */
export template <typename Seq, typename Idx>
concept HasSubscriptOperator = requires(Seq const& s, Idx i) {
  { s[i] };
};

/** @brief @c is_eval_arrow_v<Seq, @c Idx>: opt-in marker that the
 *         carrier's @c operator[] realises the CCC eval counit.
 *         Default false; specialise to @c true at the carrier site
 *         (@c FinitePath<T>, @c Vec2V<T>, …) when the
 *         subscript-as-eval reading holds. */
export template <typename Seq, typename Idx>
inline constexpr bool is_eval_arrow_v = false;

/** @concept IsEvalArrow
 *  @brief @c Seq exposes @c operator[Idx] and the carrier site has
 *         declared it as the CCC eval counit (Mac Lane CWM §IV.6).
 */
export template <typename Seq, typename Idx>
concept IsEvalArrow =
    HasSubscriptOperator<Seq, Idx> && is_eval_arrow_v<Seq, Idx>;

}  // namespace dedekind::category
