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
#include <cstddef>     // std::size_t: the summand of an injection
#include <cstdlib>     // std::abort: the refusal of a composite of composites
#include <functional>  // std::invoke: a section calls what it admits
#include <memory>
#include <tuple>
#include <type_traits>
#include <utility>
#include <variant>

export module dedekind.category:cartesian;

import :logic;
import :equivalence;  // IsEquivalence, IsSymmetric, IsEquality
import :limit;
import :mereology;
import :morphism;  // for IsArrow / Identity (used by arrow_as_relation bridge)
import :posetal;   // the laws of a function (arrow_node, arrow_laws_v),
                   // ProductLeq
import :species;   // IsAssociative: what lets two sections merge

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
constexpr auto exponential(F f) {
  return arrow(std::move(f));
}

/**
 * @details 2. Semi-Automatic: Explicit Domain A, Inferred Codomain B
 */
export template <typename A, typename F>
constexpr auto exponential(F f) {
  return arrow<A>(std::move(f));
}

/**
 * @details 3. Fully Explicit: Explicit A and B
 */
export template <typename A, typename B, typename F>
constexpr auto exponential(F f) {
  return arrow<A, B>(std::move(f));
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
constexpr auto curry(F f) {
  using X = typename Dom<F>::first_type;
  using A = typename Dom<F>::second_type;

  // The morphism is called, never changed (IsArrow requires a const call
  // operator).  A copyable morphism is copied into each closure; a move-only
  // one is held as a boxed value, a shared_ptr to const, so every closure
  // shares it and none can change it.
  using G = std::decay_t<F>;
  if constexpr (std::copy_constructible<G>) {
    return arrow([g = G(std::move(f))](const X& x) {
      return exponential([g, x](const A& a) { return g({x, a}); });
    });
  } else {
    auto box = std::make_shared<const G>(std::move(f));
    return arrow([box](const X& x) {
      return exponential([box, x](const A& a) { return (*box)({x, a}); });
    });
  }
}

/**
 * @brief Uncurrying: (X → B^A) ⟹ (X × A → B)
 * @details Transforms a function returning a function into a single function
 * taking a categorical product (std::pair).
 */
export template <IsArrow F>
  requires IsArrow<Cod<F>>
constexpr auto uncurry(F f) {
  using X = Dom<F>;
  using Exp = Cod<F>;
  using A = Dom<Exp>;
  // using B = Cod<Exp>;

  // Use explicit types in the lambda so signature_extractor works
  return arrow([f = std::move(f)](const std::pair<X, A>& p) {
    return f(p.first)(p.second);
  });
}

/** @section cartesian__Curry_Of_An_Operation
 *  @brief A binary operation curried, with named arrows throughout (the Jlt
 *  paper, Listing 3): @c curry<T>(op) is @f$T \to T^T@f$, its value at @c a
 *  the operator section @f$b \mapsto op(a, b)@f$, Haskell's @c (a @c +), and
 *  @c uncurry takes it back to @f$T \times T \to U@f$.  A chain's end and
 *  step are such sections: @c curry<N>(greater_equal<N>{})(unit) stops at
 *  the unit, @c curry<N>(plus<N>{})(1) is the successor.
 *  @tparam T the carrier.  @tparam Op the operation. */
export template <typename T, typename Op>
  requires std::regular_invocable<const Op&, const T&, const T&>
struct Section {
  using Domain = T;
  using Codomain =
      std::remove_cvref_t<std::invoke_result_t<const Op&, const T&, const T&>>;
  [[no_unique_address]] Op op;
  T a;
  /** @param b the second argument.  @return @c op(a, b). */
  constexpr Codomain operator()(const T& b) const {
    return std::invoke(op, a, b);
  }
  /** @brief Two sections are equal when their elements are and, for an
   *  operation with state, their operations are; an operation with state and
   *  no @c == offers no equality.  A stateless operation is its type. */
  constexpr bool operator==(const Section& o) const
    requires std::is_empty_v<Op> || std::equality_comparable<Op>
  {
    if constexpr (std::is_empty_v<Op>)
      return a == o.a;
    else
      return op == o.op && a == o.a;
  }
};
/** @brief @f$T \to T^T@f$: the operation, one argument at a time. */
export template <typename T, typename Op>
  requires std::regular_invocable<const Op&, const T&, const T&>
struct Curried {
  using Domain = T;
  using Codomain = Section<T, Op>;
  Op op;
  /** @param a the first argument.  @return the section at @c a. */
  constexpr Codomain operator()(const T& a) const { return {op, a}; }
  constexpr bool operator==(const Curried&) const = default;
};
/** @brief The operation curried.  @tparam T the carrier.  @param op the
 *  operation, a function object and no arrow (an arrow from a product has
 *  the overload above). */
export template <typename T, typename Op>
  requires(!IsArrow<Op>) &&
          std::regular_invocable<const Op&, const T&, const T&>
constexpr Curried<T, Op> curry(Op op) {
  return {std::move(op)};
}
/** @brief @f$T \times T \to U@f$: the curried operation, taken back. */
export template <typename T, typename Op>
  requires std::regular_invocable<const Op&, const T&, const T&>
struct Uncurried {
  using Domain = std::pair<T, T>;
  using Codomain = typename Section<T, Op>::Codomain;
  Op op;
  /** @param p both arguments.  @return @c op(p.first, p.second). */
  constexpr Codomain operator()(const Domain& p) const {
    return std::invoke(op, p.first, p.second);
  }
  constexpr bool operator==(const Uncurried&) const = default;
};
/** @brief The inverse of @c curry<T>.  @param c the curried operation. */
export template <typename T, typename Op>
constexpr Uncurried<T, Op> uncurry(Curried<T, Op> c) {
  return {std::move(c.op)};
}
static_assert(
    curry<unsigned>(std::plus<unsigned>{})(1u)(2u) == 3 &&
        uncurry(curry<unsigned>(std::plus<unsigned>{}))({1u, 2u}) == 3 &&
        IsArrow<Curried<unsigned, std::plus<unsigned>>> &&
        IsArrow<Section<unsigned, std::plus<unsigned>>> &&
        curry<unsigned>(std::greater_equal<unsigned>{})(0u)(0u) &&
        !curry<unsigned>(std::greater_equal<unsigned>{})(0u)(1u),
    "curry<T>(op)(a)(b) = op(a, b), uncurry takes it back, and both are "
    "arrows with names");

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
constexpr auto ι_1(A value) {
  using Var = std::variant<A, B>;
  if constexpr (std::same_as<A, B>) {
    return Var(std::in_place_index<0>, std::move(value));
  } else {
    return Var(std::move(value));
  }
}

/**
 * @brief Right injection ι₂: B → A + B.
 * @details Wraps a value of type B into the coproduct `std::variant<A, B>`.
 * When A and B are the same type, uses `std::in_place_index<1>` to
 * unambiguously select the second alternative.
 */
export template <typename A, typename B>
constexpr auto ι_2(B value) {
  using Var = std::variant<A, B>;
  if constexpr (std::same_as<A, B>) {
    return Var(std::in_place_index<1>, std::move(value));
  } else {
    return Var(std::move(value));
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
/** @brief The injection by its textbook name, @f$\iota_K@f$ (Pierce), as the
 *  projection is @c Π.  @tparam K the summand, 1 or 2.  @tparam A, B the
 *  summands. */
export template <std::size_t K, typename A, typename B>
using ι = Injection<K, A, B>;

/** @section cartesian__Arrows_As_Coproducts
 *  @brief The endomorphisms of @c T that have a storage class, as three
 *  coproducts, each an arrow by case analysis and a coproduct by
 *  construction (the Jlt paper, Listing 2).
 *  - @c Sections<T, Op>: the identity, or a section of the operation.  Closed
 *    under composition by the rewrites @c id >> s = s and @c s >> s' = s''
 *    (associativity): a monoid.
 *  - @c Morphisms<T, Op>: a section, or a morphism over a function pointer,
 *    which every stateless arrow on @c T becomes through @c call<F{}, T>.
 *    Two pointers compose to a closure, so this is not closed: it composes
 *    into @c Arrows.
 *  - @c Arrows<T, Op>: a @c Morphisms, or the composite of two.  Composition
 *    is restricted to one level, since a composite of composites has no
 *    alternative of fixed size to land in: it is refused, at compile time by
 *    failing the constant evaluation, at run time by ending the program. Within
 * the restriction it is a partial monoid, its composition defined to one level,
 * not a monoid, so no monoid is certified on it; the identity is its unit,
 * decidably so: the unit has one representation, and equality is case by case,
 * pointers and elements, so the laws can be checked, which an erased closure
 * would not allow. Composition normalises: the identity is absorbed and two
 * sections merge, so that the laws hold on representations, which is what
 * equality reads. */
namespace detail_arrows {
/** @brief The visitor that applies the active arrow to @c x. */
template <typename T>
struct At {
  T x;
  template <IsArrow F>
  constexpr T operator()(const F& f) const {
    return f(x);
  }
};
/** @brief The refusal of a composite of composites: not a constant
 *  expression, so the compile-time evaluation that reaches it fails, and at
 *  run time the end of the program, since Rule 1 admits no throw. */
[[noreturn]] constexpr void refuse_deep_composite() { std::abort(); }
}  // namespace detail_arrows

export template <typename T, typename Op = std::plus<T>>
  requires std::same_as<typename Section<T, Op>::Codomain, T> &&
           IsAssociative<T, Op> && std::is_empty_v<Op>
struct Sections {
  using Domain = T;
  using Codomain = T;
  std::variant<Identity<T>, Section<T, Op>> v;
  constexpr Sections() = default;  // the identity
  constexpr Sections(Identity<T> i) : v(i) {}
  constexpr Sections(Section<T, Op> s) : v(s) {}
  /** @brief The injections ι₁, ι₂, named, so that @c Sections is its own
   *  coproduct operation (@c IsCoproduct's @c Op). */
  static constexpr Sections inl(const Identity<T>& i) { return {i}; }
  static constexpr Sections inr(const Section<T, Op>& s) { return {s}; }
  constexpr T operator()(const T& x) const {
    return std::visit(detail_arrows::At<T>{x}, v);
  }
  constexpr bool operator==(const Sections&) const = default;
};
/** @brief @c f >> g on sections: the identity absorbed, two sections one,
 *  @f$(b \mapsto a \cdot b) ; (c \mapsto a' \cdot c) = (b \mapsto (a' \cdot a)
 *  \cdot b)@f$ by associativity, which @c Sections asks of the operation,
 *  with no state, so that both sections are of one operation.
 *  @param f @param g the sections. */
export template <typename T, typename Op>
constexpr Sections<T, Op> operator>>(const Sections<T, Op>& f,
                                     const Sections<T, Op>& g) {
  if (f.v.index() == 0) return g;
  if (g.v.index() == 0) return f;
  const Section<T, Op>& s = std::get<1>(f.v);
  const Section<T, Op>& r = std::get<1>(g.v);
  return Section<T, Op>{s.op, s.op(r.a, s.a)};
}

export template <typename T, typename Op = std::plus<T>>
struct Morphisms {
  using Domain = T;
  using Codomain = T;
  /** @brief A morphism over a function pointer: one storage class. */
  using Pointer = Morphism<T, T, T (*)(T)>;
  std::variant<Sections<T, Op>, Pointer> v;
  constexpr Morphisms() = default;  // the identity
  constexpr Morphisms(Sections<T, Op> s) : v(s) {}
  constexpr Morphisms(Identity<T> i) : v(Sections<T, Op>{i}) {}
  constexpr Morphisms(Section<T, Op> s) : v(Sections<T, Op>{s}) {}
  constexpr Morphisms(Pointer p) : v(p) {}
  /** @brief The injections ι₁, ι₂, named. */
  static constexpr Morphisms inl(const Sections<T, Op>& s) { return {s}; }
  static constexpr Morphisms inr(const Pointer& p) { return {p}; }
  /** @brief Any stateless arrow on @c T, by its type: called through
   *  @c call<F{}, T>. */
  template <IsArrow F>
    requires std::same_as<Dom<F>, T> && std::same_as<Cod<F>, T> &&
             std::is_empty_v<F> && (!std::same_as<F, Identity<T>>)
  constexpr Morphisms(F) : v(Pointer{&call<F{}, T>}) {}
  constexpr T operator()(const T& x) const {
    return std::visit(detail_arrows::At<T>{x}, v);
  }
  constexpr bool operator==(const Morphisms&) const = default;
};

export template <typename T, typename Op = std::plus<T>>
struct Arrows {
  using Domain = T;
  using Codomain = T;
  using Composite = Compose<Morphisms<T, Op>, Morphisms<T, Op>>;
  std::variant<Morphisms<T, Op>, Composite> v;
  constexpr Arrows() = default;  // the identity
  constexpr Arrows(Morphisms<T, Op> m) : v(m) {}
  constexpr Arrows(Composite c) : v(c) {}
  /** @brief The injections ι₁, ι₂, named. */
  static constexpr Arrows inl(const Morphisms<T, Op>& m) { return {m}; }
  static constexpr Arrows inr(const Composite& k) { return {k}; }
  /** @brief Anything a @c Morphisms is made from. */
  template <typename F>
    requires std::constructible_from<Morphisms<T, Op>, F> &&
             (!std::same_as<std::remove_cvref_t<F>, Morphisms<T, Op>>) &&
             (!std::same_as<std::remove_cvref_t<F>, Composite>) &&
             (!std::same_as<std::remove_cvref_t<F>, Arrows>)
  constexpr Arrows(F f) : v(Morphisms<T, Op>(f)) {}
  constexpr T operator()(const T& x) const {
    return std::visit(detail_arrows::At<T>{x}, v);
  }
  constexpr bool operator==(const Arrows&) const = default;
};
/** @brief @c f >> g on morphisms, into @c Arrows: the identity absorbed, two
 *  sections one, two pointers a composite.  @param f @param g the morphisms. */
export template <typename T, typename Op>
constexpr Arrows<T, Op> operator>>(const Morphisms<T, Op>& f,
                                   const Morphisms<T, Op>& g) {
  const bool fs = f.v.index() == 0, gs = g.v.index() == 0;
  if (fs && std::get<0>(f.v).v.index() == 0) return g;
  if (gs && std::get<0>(g.v).v.index() == 0) return f;
  if (fs && gs) return Morphisms<T, Op>{std::get<0>(f.v) >> std::get<0>(g.v)};
  return typename Arrows<T, Op>::Composite{f, g};
}
/** @brief @c f >> g on arrows, restricted to one level: two morphisms
 *  compose as above, the identity is absorbed, and a composite of composites
 *  is refused.  @param f @param g the arrows. */
export template <typename T, typename Op>
constexpr Arrows<T, Op> operator>>(const Arrows<T, Op>& f,
                                   const Arrows<T, Op>& g) {
  const bool fm = f.v.index() == 0, gm = g.v.index() == 0;
  if (fm && gm) return std::get<0>(f.v) >> std::get<0>(g.v);
  if (fm && std::get<0>(f.v) == Morphisms<T, Op>{}) return g;
  if (gm && std::get<0>(g.v) == Morphisms<T, Op>{}) return f;
  detail_arrows::refuse_deep_composite();
}

namespace detail_arrows_witness {
struct Inc {
  using Domain = unsigned;
  using Codomain = unsigned;
  constexpr unsigned operator()(unsigned x) const { return x + 1; }
};
using A = Arrows<unsigned>;
using M = Morphisms<unsigned>;
using S = Sections<unsigned>;
constexpr auto twice = Section<unsigned, std::plus<unsigned>>{{}, 2u};
static_assert(
    IsArrow<S> && IsArrow<M> && IsArrow<A> &&
        IsCoproduct<S, Identity<unsigned>,
                    Section<unsigned, std::plus<unsigned>>> &&
        IsCoproduct<M, S, M::Pointer> && IsCoproduct<A, M, A::Composite> &&
        IsCoproduct<S, Identity<unsigned>,
                    Section<unsigned, std::plus<unsigned>>, S> &&
        IsCoproduct<M, S, M::Pointer, M> &&
        IsCoproduct<A, M, A::Composite, A> && S::inr(twice)(1u) == 3 &&
        A::inl(M{Inc{}})(1u) == 2 && A{}(5u) == 5 && A{twice}(5u) == 7 &&
        A{Inc{}}(5u) == 6 &&
        (A{twice} >> A{twice}) ==
            A{Section<unsigned, std::plus<unsigned>>{{}, 4u}} &&
        (A{} >> A{Inc{}}) == A{Inc{}} && (A{Inc{}} >> A{}) == A{Inc{}} &&
        (A{Inc{}} >> A{twice})(5u) == 8 &&
        (A{Inc{}} >> A{Inc{}}).v.index() == 1,
    "three coproducts of arrows: each an arrow; the identity absorbed, two "
    "sections one, two pointers a composite; equality by representation");
template <typename T, typename Op>
concept HasSections = requires { typename Sections<T, Op>; };
static_assert(!HasSections<int, std::minus<int>>,
              "sections merge by associativity, so a non-associative "
              "operation has none: 5 - (3 - x) is not (5 - 3) - x");
}  // namespace detail_arrows_witness
static_assert(
    !IsCoproduct<std::variant<std::unique_ptr<int>, int>, std::unique_ptr<int>,
                 int, Injections<std::unique_ptr<int>, int>> &&
        !IsArrow<Injection<1, std::unique_ptr<int>, int>>,
    "a summand the variant cannot copy in is refused, not accepted and "
    "broken at the call");

/**
 * @brief Mediating morphism for Coproducts: [f, g]: (A + B) -> X
 * @details Given f: A -> X and g: B -> X, constructs the unique morphism
 * (the "case" analysis) that handles either alternative of a variant.
 */
export template <IsArrow F, IsArrow G>
  requires std::same_as<Cod<F>, Cod<G>>  // Must map to the same Target X
constexpr auto mediate_coproduct(F f, G g) {
  using A = Dom<F>;
  using B = Dom<G>;
  using X = Cod<F>;
  using Var = std::variant<A, B>;

  return arrow<Var, X>([f = std::move(f), g = std::move(g)](const Var& v) -> X {
    if constexpr (std::same_as<A, B>) {
      return v.index() == 0 ? f(std::get<0>(v)) : g(std::get<1>(v));
    } else {
      return std::visit(
          [&](auto&& val) -> X {
            using T = std::decay_t<decltype(val)>;
            if constexpr (std::same_as<T, A>)
              return f(val);
            else
              return g(val);
          },
          v);
    }
  });
}

/**
 * @brief The canonical evaluation morphism for the CCC: (B^A × A) → B.
 * @details Applies a function (Exponential inhabitant) to its argument.
 * This is the "eval" map that witnesses the adjunction between the product
 * functor (– × A) and the exponential functor (–^A).
 *
 * @tparam F A type satisfying `IsExponential<F, A, B>`.
 * @tparam A The domain type.
 */
export template <typename F, typename A>
  requires IsExponential<F, A, std::invoke_result_t<F, A>>
constexpr auto eval(F f, A a) {
  // By value in and out: a decltype(auto) result could be a reference into
  // an argument that dies when eval returns.
  return std::invoke(std::move(f), std::move(a));
}

/** @section cartesian__Structural_Exponential_Verification */

// 1. The "Heavy" Exponential: Type-erased function space
static_assert(IsExponential<std::function<bool(int)>, int, bool>,
              "Axiom: std::function must satisfy the Internal Hom-set.");

#if __has_include(<functional>) && defined(__cpp_lib_move_only_function)
// 2. The "Move-Only" Exponential: C++23's modern function space
#include <functional>  // for std::move_only_function if available
static_assert(IsExponential<std::move_only_function<bool(int)>, int, bool>,
              "Axiom: move_only_function is a valid Exponential inhabitant.");
#endif

// 3. The "Light" Exponential: Anonymous Structural Closure
// This captures the 'essence' without the 'lineage'.  (Named @c bounded_pred,
// not @c closure, so the canonical name is free for the @c closure operator in
// @c :functor --- "closure of X under Y".)
constexpr auto bounded_pred = [limit = 42](int x) { return x < limit; };
static_assert(
    IsExponential<std::remove_cvref_t<decltype(bounded_pred)>, int, bool>,
    "Structural: A lambda is discovered as an Exponential by its scent.");

// 4. The "Honest Rejection": Mismatched Signature
// Valid function, but wrong mapping for this specific Hom-set
static_assert(!IsExponential<std::function<int(int)>, int, bool>,
              "Verification: Rejected due to Codomain mismatch.");

/**
 * @concept IsCartesianClosed
 * @brief A Category equipped with Terminal Objects, Products, and Exponentials.
 * @details This concept extends `IsSmallCategory` to satisfy the three axioms
 * of a Cartesian Closed Category (CCC):
 * 1. **Terminal Object** — a unique sink `1` (represented by `One` /
 *    `std::monostate`).
 * 2. **Products** — for any two objects A, B, a product A × B exists
 *    (represented by `std::pair<A, B>`).
 * 3. **Exponentials** — for any two objects A, B, a function space B^A
 *    exists: an *object* whose inhabitants are functions A→B (`IsExponential`).
 *    B^A is an object, not an arrow; arrow-shaped carriers that also carry
 *    `Domain`/`Codomain` are a representation refinement
 * (`IsArrowExponential`), used e.g. by `Set<T>::Exponential`'s default — not
 * required here.
 *
 * This triple of structures provides the categorical foundations required for
 * the Dedekind topos (ETCS).
 */
// Concept-as-predicate / @c &&-as-intersection composition: the cartesian
// closed concept narrows @c IsCartesian by additionally requiring
// exponentials.  Reads as the lattice meet @c IsCartesian @c ∩
// HasExponentials, mirroring the chain in paper §2.3 step 2 → step 3.
export template <typename Cat>
concept IsCartesianClosed =
    IsCartesian<Cat> &&
    requires(typename Cat::Arrow::Domain A, typename Cat::Arrow::Codomain B) {
      // Closed: exponentials exist --- an object whose inhabitants are
      // functions A -> B.  The exponential is an object, not an arrow, so the
      // general gate is IsExponential; arrow-shaped carriers are a refinement
      // (IsArrowExponential), a per-carrier choice (see Set<T>::Exponential).
      typename Cat::template Exponential<decltype(A), decltype(B)>;
      requires IsExponential<
          typename Cat::template Exponential<decltype(A), decltype(B)>,
          decltype(A), decltype(B)>;
    };

/**
 * @brief The Identity morphism for the category Set.
 */
template <typename T>
struct SetId final {
  using Domain = T;
  using Codomain = T;

  T species;

  constexpr T operator()(const T& x) const { return x; }

  /**
   * @brief The explicit bridge to the Category's Arrow type.
   * This satisfies the std::convertible_to constraint in IsSmallCategory.
   */
  constexpr operator Morphism<T, T, std::function<T(T)>>() const {
    return Morphism<T, T, std::function<T(T)>>{
        std::function<T(T)>{[](T x) { return x; }}};
  }
};

/**
 * @brief The Category of C++ Types (Set).
 * Formally reified as a Cartesian Closed Category.
 */
template <typename T>
struct Set final {
  using Species = T;
  using Arrow = Morphism<T, T, std::function<T(T)>>;
  using Id = SetId<T>;

  // 1. Terminal Object: The 'one' in Set
  using Terminal = std::monostate;

  // 2. Product: A x B
  template <typename A, typename B>
  using Product = std::pair<A, B>;

  // 3. Exponential: B^A.  The exponential is an *object*; here we *choose* an
  // arrow-shaped carrier for the canonical Set witness --- an IsArrow (A.1's
  // arrow) so ::Domain/::Codomain are carried.  This is a per-carrier
  // representation refinement (IsArrowExponential), NOT required by the general
  // IsCartesianClosed.  The carrier is a defaulted, IsArrowExponential-
  // constrained parameter defaulting to Morphism<A,B,std::function>, with
  // std::function confined to the erased callable impl.  FIXME(#764): a
  // constexpr-capable default carrier would let the exponentials fold.
  template <typename A, typename B,
            typename Exp = Morphism<A, B, std::function<B(A)>>>
    requires IsArrowExponential<Exp, A, B>
  using Exponential = Exp;

  static constexpr Id id_c(const T& x) noexcept { return Id{x}; }

  friend constexpr auto operator>>(const Arrow& f, const Arrow& g) {
    return arrow<T, T>([f, g](T x) { return g(f(std::move(x))); });
  }
};

/**
 * @brief Explicit alias for the canonical ETCS CCC witness over ambient A.
 *
 * @details
 * This alias exists to reduce naming ambiguity with `dedekind::sets::Set`
 * (the intensional set-builder DSL species).
 */
export template <typename A>
using CanonicalSetCCC = Set<A>;

// Now we can bless it right next to its definition
static_assert(IsCartesianClosed<CanonicalSetCCC<int>>,
              "Verification Failed: Set must be Cartesian Closed.");
/**
 * @concept IsProductCategory
 * @brief A Category Hub where the Species is a Categorical Product (std::pair).
 */
export template <typename Cat>
concept IsProductCategory = IsSmallCategory<Cat> && requires {
  /**
   * The species of this category must be a Product.
   * We extract A and B via the pair's internal aliases.
   */
  typename Cat::Species::first_type;
  typename Cat::Species::second_type;

  requires IsProduct<typename Cat::Species, typename Cat::Species::first_type,
                     typename Cat::Species::second_type>;
};

// A category of integer/boolean pairs is an honest Product Category
static_assert(IsProductCategory<Set<std::pair<int, bool>>>);

// ---------------------------------------------------------------------------
// Pedagogical-accessibility primitive: function as a special case of
// relation (closes #460).  Lives in @c :cartesian because relations
// are subsets of products — and products are first-class here.
//
// @section cartesian__Relation_Classification_Axes
// Binary relations admit several @b orthogonal classification axes,
// each gated by its own opt-in trait.  This partition adds one axis;
// the others live elsewhere in the project:
//
//   | property           | trait                       | located in       |
//   |--------------------|-----------------------------|------------------|
//   | reflexive          | @c is_reflexive_v<T, Rel>   | @c :mereology    |
//   | transitive         | @c is_transitive_v<T, Rel>  | @c :mereology    |
//   | antisymmetric      | @c is_antisymmetric_v<T,Rel>| @c :mereology    |
//   | connex             | @c is_connex_v<T, Rel>      | @c :mereology    |
//   | symmetric          | @c is_symmetric_v<T, Rel>   | @c :equivalence  |
//   | equality (Δ)       | @c is_equality_v<T, Rel>    | @c :equivalence  |
//   | left-total         | @c is_left_total_v<R>       | @b :cartesian    |
//   | right-unique       | @c is_right_unique_v<R>     | @b :cartesian    |
//   | left-unique        | @c is_left_unique_v<R>      | @b :cartesian    |
//   | right-total        | @c is_right_total_v<R>      | @b :cartesian    |
//
// Compositions (as a wider taxonomy across the project):
//
//   | composition                                | reading             |
//   |--------------------------------------------|---------------------|
//   | IsBinaryRelation (no extra constraint)     | generic binary rel. |
//   | IsBinaryRelation, homogeneous V × V        | directed graph      |
//   | + symmetric                                | undirected graph    |
//   | + reflexive + symmetric + transitive       | equivalence relation|
//   | + reflexive + antisymmetric + transitive   | partial order       |
//   | + left-total + right-unique                | function (here)     |
//   | + left-total + right-unique + left-unique  |                     |
//   |   + right-total                            | bijection (Rel iso) |
//
// Note the indexing difference: @c :mereology's traits are keyed on
// @c (T, Op) (homogeneous binary operation); the four traits added here
// are keyed on @c R alone (the relation type), which lets the @c A ≠
// @c B function shape work without extra parameters.
//
// Textbook layering taught in advanced high-school / undergraduate
// classes:
//
//   * A @b relation @c R @c ⊆ @c A @c × @c B is a subset of the
//     Cartesian product (a set of pairs).  At the type level, the
//     callable-indicator reading is a predicate
//     @c R(A const&, B const&) @c → @c bool.
//   * A @b function @c f @c : @c A @c → @c B is a relation that is
//     @b left-total (every @c a @c ∈ @c A has at least one image) and
//     @b right-unique (every @c a has at most one image).
//   * The categorical @c IsArrow operationalises "function" — every
//     IsArrow induces a relation @c R(a, b) @c := @c (f(a) @c == @c b)
//     that is left-total + right-unique by construction.
//
// The set-of-pairs / value-level reading of "relation" lives in
// @c dedekind.relational:dyadic as @c Relation<T1, T2, L, P> = @c Set<pair<T1,
// T2>, L, P>; the two readings are isomorphic.  This callable-
// indicator reading is the type-level sibling, parallel to @c IsArrow.
// ---------------------------------------------------------------------------

/**
 * @concept IsBinaryRelation
 * @brief A type @c R is a binary relation @c A @c × @c B @c → @c bool
 *        if it is callable on @c A and @c B and yields a boolean.
 *
 * @details Callable-indicator reading of @c R @c ⊆ @c A @c × @c B
 * (the predicate is the membership-of-pair indicator).  Sibling to
 * the value-level @c relational::Relation<T1, T2, L, P> alias, which models
 * the relation as a Set of pairs.  The two readings are isomorphic.
 */
export template <typename R, typename A, typename B, typename Ω = bool>
concept IsBinaryRelation =
    IsTruthObject<Ω> && requires(R const& r, A const& a, B const& b) {
      { r(a, b) } -> std::convertible_to<Ω>;
    };

/**
 * @brief User-declared left-totality witness for a binary relation.
 * @details Left-totality: every @c a @c ∈ @c A is related to at least
 *          one @c b @c ∈ @c B.  Cannot be checked at compile time in
 *          general; opt-in via @c is_left_total_v<R> = true.  Mirrors
 *          the @c is_monic_arrow_v / @c is_epic_arrow_v opt-in pattern
 *          in @c :morphism, and the @c is_reflexive_v / @c
 *          is_transitive_v / @c is_antisymmetric_v opt-in pattern in
 *          @c :mereology.
 */
export template <typename R>
inline constexpr bool is_left_total_v = false;

/**
 * @brief User-declared right-uniqueness witness for a binary relation.
 * @details Right-uniqueness: each @c a @c ∈ @c A is related to @b at
 *          @b most one @c b @c ∈ @c B.  Same opt-in pattern; combined
 *          with @c is_left_total_v<R>, this characterises a function
 *          as a special case of relation.
 */
export template <typename R>
inline constexpr bool is_right_unique_v = false;

/** @brief Left-uniqueness (injectivity of the relation): each @c b @c ∈ @c B
 *  is related to at most one @c a; @c R° right-unique.  Same opt-in pattern;
 *  derived for the relational nodes by the rule table (@c relational:dyadic,
 *  @c Laws). */
export template <typename R>
inline constexpr bool is_left_unique_v = false;

/** @brief Right-totality (surjectivity of the relation): every @c b @c ∈ @c B
 *  is related to some @c a; @c R° left-total.  Same opt-in pattern, derived
 *  likewise.  With the other three: an isomorphism of Rel. */
export template <typename R>
inline constexpr bool is_right_total_v = false;

/**
 * @concept IsBinaryFunction
 * @brief A binary relation @c R @c ⊆ @c A @c × @c B is a @b function
 *        when it is left-total AND right-unique.
 * @details The textbook function-as-relation characterisation:
 *          a function is a single-valued total relation.  Combines
 *          @c IsBinaryRelation (structural shape) with the two opt-in
 *          honesty traits.  Orthogonal classification axis to the
 *          posetal / equivalence axes (reflexive / symmetric /
 *          antisymmetric / transitive) in @c :mereology.
 */
export template <typename R, typename A, typename B, typename Ω = bool>
concept IsBinaryFunction =
    IsBinaryRelation<R, A, B, Ω> && is_left_total_v<std::remove_cvref_t<R>> &&
    is_right_unique_v<std::remove_cvref_t<R>>;

/**
 * @brief Adapter: realise an @c IsArrow @c F @c : @c A @c → @c B as a
 *        binary relation @c (a, @c b) @c ↦ @c f(a) @c == @c b.
 * @details Bridge between the categorical-arrow reading (in
 *          @c :morphism) and the set-theoretic function-as-relation
 *          reading (here in @c :cartesian, where products are first-
 *          class).
 *
 *          Two requirements beyond the bare @c IsArrow:
 *            (1) @c F must be callable on a @c const instance — the
 *                operator() below is @c const, so a non-const-callable
 *                arrow would not work.
 *            (2) @c F's call result must be equality-comparable with
 *                @c F::Codomain — the relation predicate is @c f(a)
 *                @c == @c b.
 *          Both are tightened in the @c requires clause below.
 *
 *          @c IsArrow itself is a structural shape (it only asserts
 *          the call expression is well-formed); it does @b not
 *          guarantee total / law-abiding behaviour over the domain.
 *          The opt-in trait specialisations below register
 *          left-totality and right-uniqueness on @c arrow_as_relation
 *          @b by @b convention — every IsArrow is intended to be a
 *          (total, deterministic) function in this codebase.  If a
 *          carrier ever ships a partial @c IsArrow (e.g. a wrapper
 *          that throws on some inputs), the engineer should
 *          un-register the trait on that specific specialisation.
 *
 *          The template parameter @c F is normalised via
 *          @c std::remove_cvref_t internally, so passing
 *          @c arrow_as_relation<const F> or @c arrow_as_relation<F&>
 *          works the same as @c arrow_as_relation<F>.
 */
export template <typename F>
  requires IsArrow<F> &&
           requires(std::remove_cvref_t<F> const& fn,
                    typename std::remove_cvref_t<F>::Domain const& a,
                    typename std::remove_cvref_t<F>::Codomain const& b) {
             { fn(a) == b } -> std::convertible_to<bool>;
           }
struct arrow_as_relation {
  using ArrowType = std::remove_cvref_t<F>;
  ArrowType f;

  constexpr bool operator()(typename ArrowType::Domain const& a,
                            typename ArrowType::Codomain const& b) const {
    return f(a) == b;
  }
};

// arrow_as_relation<F> is registered left-total + right-unique by
// convention (see docstring above).  IsArrow is a structural shape
// only; the opt-in traits anchor the engineer's claim that the arrow
// is a (total, deterministic) function in the textbook sense.
template <typename F>
  requires IsArrow<F> &&
               requires(std::remove_cvref_t<F> const& fn,
                        typename std::remove_cvref_t<F>::Domain const& a,
                        typename std::remove_cvref_t<F>::Codomain const& b) {
                 { fn(a) == b } -> std::convertible_to<bool>;
               }
inline constexpr bool is_left_total_v<arrow_as_relation<F>> = true;

template <typename F>
  requires IsArrow<F> &&
               requires(std::remove_cvref_t<F> const& fn,
                        typename std::remove_cvref_t<F>::Domain const& a,
                        typename std::remove_cvref_t<F>::Codomain const& b) {
                 { fn(a) == b } -> std::convertible_to<bool>;
               }
inline constexpr bool is_right_unique_v<arrow_as_relation<F>> = true;
// A monic arrow of Set is an injective relation, an epic one surjective: the
// two further laws read off the arrow's own certificates.
template <typename F>
  requires IsArrow<F> && IsMonicArrow<F>
inline constexpr bool is_left_unique_v<arrow_as_relation<F>> = true;
template <typename F>
  requires IsArrow<F> && IsEpicArrow<F>
inline constexpr bool is_right_total_v<arrow_as_relation<F>> = true;

// Bridge :morphism ↔ :cartesian: every IsArrow induces an
// IsBinaryFunction via arrow_as_relation.  Demonstrated on the
// canonical Identity<int>; downstream witnesses fire automatically.
static_assert(IsBinaryRelation<arrow_as_relation<Identity<int>>, int, int>,
              "Bridge :morphism ↔ :cartesian — arrow_as_relation<F> is a "
              "binary relation on F::Domain × F::Codomain (callable "
              "indicator).");
static_assert(IsBinaryFunction<arrow_as_relation<Identity<int>>, int, int>,
              "Bridge :morphism ↔ :cartesian — every IsArrow induces an "
              "IsBinaryFunction (left-total + right-unique by construction).  "
              "Identity is the canonical witness; arbitrary IsArrow F → "
              "arrow_as_relation<F> fires the same chain.");

// ---------------------------------------------------------------------------
// Carrier-lattice lift unification (closes part of #455).
//
// Discoverability alias indexed by the lattice arrow's @c (From, To)
// pair.  Calling @c lift<From, To>(x) dispatches to the canonical
// bespoke arrow registered for that pair (e.g. @c lift_ℕ_ℤ_ for the
// pair @c (Cardinality, SignedCardinality)).  Bespoke names remain
// canonical; this trait is purely additive.
//
// @b Not @b a @b categorical @b claim.  The seven existing carrier-
// lattice arrows fall into three structurally distinct families
// (set-theoretic mono inclusions, partial machine→variant lifts,
// machine sign reinterpretation); a single "monadic lift" framing
// would overstate the structure.  See @c docs/design/lift-unification.md
// for the design decision rationale.
//
// Primary template fires a useful @c static_assert if instantiated on
// an unregistered pair; specialisations live downstream where each
// canonical bespoke arrow is defined.
// ---------------------------------------------------------------------------

namespace lift_detail {
/**
 * @brief Dependent-false helper for the lift primary's static_assert.
 * @details Defers @c false until template instantiation without
 *          requiring the parameter types to be complete (unlike
 *          @c sizeof(From) @c == @c 0, which would error for
 *          incomplete @c From before emitting the intended
 *          diagnostic).
 */
template <typename...>
inline constexpr bool dependent_false_v = false;
}  // namespace lift_detail

/**
 * @brief Canonical lift @c From @c → @c To across the carrier
 *        lattice — discoverability alias dispatching to the
 *        registered bespoke arrow for @c (From, @c To).
 *
 * @details Specialise this primary for each registered lattice pair.
 * The primary fires a useful @c static_assert at instantiation if no
 * specialisation exists, naming the design doc that lists the
 * registered pairs.  The body uses a @c dependent_false_v helper
 * (avoids @c sizeof on @c From which would require completeness) and
 * ends with @c std::unreachable() — itself @c [[noreturn]] — so the
 * function does NOT need @c To to be default-constructible (no
 * @c return @c To{} required).  The function itself is NOT marked
 * @c [[noreturn]] because that attribute would apply to
 * specialisations too, and specialisations DO return their bespoke
 * result.  Full specialisations override the primary cleanly.
 */
export template <typename From, typename To>
constexpr To lift(From const&) {
  static_assert(lift_detail::dependent_false_v<From, To>,
                "No canonical lift<From, To> registered for this pair.  See "
                "docs/design/lift-unification.md for the registered carrier-"
                "lattice arrows; specialise dedekind::category::lift<From, To> "
                "in the partition that owns the canonical bespoke.");
  std::unreachable();  // Never reached: the static_assert above fires
                       // first.  Avoids requiring @c To to be default-
                       // constructible (which @c return @c To{} would).
}

// ---------------------------------------------------------------------------
// Order / equivalence / graph axes for binary relations (closes #464).
//
// Extends the relation-classification taxonomy started by #460 / PR #463
// with the symmetric-side / order-side opt-in traits, all R-keyed for
// parity with @c is_left_total_v / @c is_right_unique_v.  Sibling to
// the @c (T, Op)-keyed @c is_reflexive_v / @c is_transitive_v / @c
// is_antisymmetric_v in @c :mereology — those classify @b binary @b
// operations @c T @c × @c T @c → @c T (closure), while these classify
// @b binary @b relations @c R(A, B) @c → @c bool (membership of pair).
//
// Compositions yield textbook special cases:
//
//   | composition (homogeneous V × V)        | reading              |
//   |----------------------------------------|----------------------|
//   | IsBinaryRelation                       | directed graph       |
//   | + symmetric                            | undirected graph     |
//   | + reflexive + symmetric + transitive   | equivalence relation |
//   | + reflexive + antisymmetric+transitive | partial order        |
// ---------------------------------------------------------------------------

/**
 * @concept IsDirectedGraph
 * @brief A homogeneous binary relation @c R @c ⊆ @c V @c × @c V on a
 *        vertex set @c V is a @b directed @b graph: each ordered pair
 *        @c (u, @c v) with @c R(u, @c v) @c == @c true is an edge.
 * @details Just @c IsBinaryRelation on a homogeneous carrier — the
 *          structural shape is the relation; no extra trait registration
 *          is required to call something a "directed graph" (the
 *          generality is the point).
 */
export template <typename R, typename V>
concept IsDirectedGraph = IsBinaryRelation<R, V, V>;

/**
 * @concept IsUndirectedGraph
 * @brief A directed graph @c R is @b undirected when its relation is
 *        symmetric: @c R(u, @c v) iff @c R(v, @c u).
 */
export template <typename R, typename V>
concept IsUndirectedGraph = IsDirectedGraph<R, V> && IsSymmetric<V, R>;

// Equality on V × V is structurally an undirected graph: the loops.
static_assert(IsDirectedGraph<std::equal_to<int>, int> &&
                  IsUndirectedGraph<std::equal_to<int>, int>,
              "== on int is a symmetric relation: the undirected loop graph");
static_assert(!IsUndirectedGraph<std::equal_to<double>, double>,
              "== on double is not certified symmetric (no equality: NaN)");

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
 *          @c IsEquivalence<T, R> @c ∧ @c is_congruence_v<R, T, Op>.
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
 * @details Stratifies @c IsEquivalence by the operation that
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
concept IsCongruence = IsEquivalence<V, R> && is_congruence_v<R, V, Op> &&
                       requires(const V& v, const Op& op) {
                         { op(v, v) } -> std::convertible_to<V>;
                       };

// ---------------------------------------------------------------------------
// Equality is a congruence for every operation (Leibniz): if x = x' and
// y = y', then Op(x, y) and Op(x', y') are the same computation on the same
// values.  Derived from the equality certificate, for every Op at once.  Two
// conditions carry it: == is the diagonal (identity of values, not a coarser
// equivalence that Op could see through, as -0.0 == +0.0 is to 1/x), and Op
// is a function of values (pure, the Juliet posture of every arrow).  Any
// other equivalence (≡ mod N, tail-equivalence, cosets of a subgroup) is a
// congruence only where proved, operation by operation.
// ---------------------------------------------------------------------------
template <typename R, typename V, typename Op>
  requires is_equality_v<V, R>
inline constexpr bool is_congruence_v<R, V, Op> = true;

static_assert(IsCongruence<std::equal_to<int>, int, std::plus<int>>,
              "std::equal_to<int> is a congruence w.r.t. + on int: "
              "equality is preserved by every operation (Leibniz).");
static_assert(IsCongruence<std::equal_to<int>, int, std::multiplies<int>>,
              "std::equal_to<int> is a congruence w.r.t. * on int.");
static_assert(IsCongruence<std::equal_to<>, int, std::plus<int>>,
              "the transparent == on int is equality too: a congruence "
              "for every operation");
static_assert(
    IsCongruence<std::equal_to<unsigned>, unsigned, std::bit_or<unsigned>>,
    "std::equal_to<unsigned> is a congruence w.r.t. bitwise OR.");

// Negative witness: std::equal_to<double> is no equality and no equivalence
// (NaN == NaN is false under IEEE 754), so it is a congruence for no
// operation.
static_assert(!IsCongruence<std::equal_to<double>, double, std::plus<double>>,
              "std::equal_to<double> is not an equivalence relation under "
              "IEEE 754 (NaN-on-reflexivity); therefore not a congruence "
              "for any operation.");

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

// ---------------------------------------------------------------------------
// The formers of functions: f × g, f + g, ⟨f, g⟩, [f, g] (#1049)
// ---------------------------------------------------------------------------

/** @section cartesian__The_Formers_Of_Functions
 *  Products and sums of functions: the algebraic data types of the posture,
 *  @c std::pair the product and @c std::variant the sum.  Two formers are
 *  primitive: the pairing ⟨f, g⟩ sends one argument into a product, the
 *  copairing [f, g] is case analysis out of a sum (@c std::visit's shape: the
 *  fold of a sum).  The other two are @b defined by them, as in the textbook:
 *  f × g = ⟨f ∘ π₁, g ∘ π₂⟩ and f + g = [ι₁ ∘ f, ι₂ ∘ g].
 *
 *  The law engine (@c :posetal, @c arrow_node) keeps × and + as facts of
 *  their own because the derived form loses precision: ⟨f ∘ π₁, g ∘ π₂⟩ is
 *  jointly injective though neither leg is, so the pairing rule alone
 *  certifies nothing.  The readers below, and @c LawAlgebra's @c steer,
 *  recognise the defining form and apply the product (sum) fact to f and g;
 *  any other pairing gets the pairing rule, which is the honest answer
 *  (⟨f ∘ π₁, f ∘ π₁⟩ is not injective).  Monotonicity is read on a product
 *  under the product order @c ProductLeq; a sum has no canonical order, so
 *  its variance stays at the engine's value level. */
/** @brief ⟨f, g⟩ : A → B × C, one argument into a product. */
export template <IsArrow F, IsArrow G>
  requires std::same_as<Dom<F>, Dom<G>>
struct Pairing {
  using Domain = Dom<F>;
  using Codomain = std::pair<Cod<F>, Cod<G>>;
  F f;
  G g;
  constexpr Codomain operator()(const Domain& x) const { return {f(x), g(x)}; }
};
/** @brief [f, g] : A + B → C, case analysis out of a sum. */
export template <IsArrow F, IsArrow G>
  requires std::same_as<Cod<F>, Cod<G>>
struct Copairing {
  using Domain = std::variant<Dom<F>, Dom<G>>;
  using Codomain = Cod<F>;
  F f;
  G g;
  constexpr Codomain operator()(const Domain& x) const {
    return x.index() == 0 ? f(std::get<0>(x)) : g(std::get<1>(x));
  }
};
/** @brief f × g = ⟨f ∘ π₁, g ∘ π₂⟩ : A × C → B × D. */
export template <IsArrow F, IsArrow G>
using ProductArrow = Pairing<Compose<Π<1, std::pair<Dom<F>, Dom<G>>>, F>,
                             Compose<Π<2, std::pair<Dom<F>, Dom<G>>>, G>>;
/** @brief f + g = [ι₁ ∘ f, ι₂ ∘ g] : A + C → B + D. */
export template <IsArrow F, IsArrow G>
using SumArrow = Copairing<Compose<F, Injection<1, Cod<F>, Cod<G>>>,
                           Compose<G, Injection<2, Cod<F>, Cod<G>>>>;
/** @brief f × g, built.  @param f @param g the factors. */
export template <IsArrow F, IsArrow G>
constexpr ProductArrow<F, G> product_arrow(const F& f, const G& g) {
  return {{{}, f}, {{}, g}};
}
/** @brief f + g, built.  @param f @param g the summands. */
export template <IsArrow F, IsArrow G>
constexpr SumArrow<F, G> sum_arrow(const F& f, const G& g) {
  return {{f, {}}, {g, {}}};
}

/** @brief f * g = f × g and f + g = f + g on arrows: the product and coproduct
 *  functors' action on arrows, as category theory writes them (Mac Lane,
 *  Awodey).  A more specific overload keeps its own meaning: a set's @c * (its
 *  cartesian product, the classifier read through ∧), an additive hom-set's
 *  @c + (@c geometry's function spaces: the sum of parallel arrows, as in an
 *  additive category); the term grammar's @c * @c + act on terms, which are
 *  not arrows.  @param f @param g the arrows. */
export template <IsArrow F, IsArrow G>
constexpr ProductArrow<F, G> operator*(const F& f, const G& g) {
  return product_arrow(f, g);
}
export template <IsArrow F, IsArrow G>
constexpr SumArrow<F, G> operator+(const F& f, const G& g) {
  return sum_arrow(f, g);
}

/** @brief The legs of a former, for @c cata. */
export template <typename F, typename G>
constexpr auto legs(const Pairing<F, G>& a) {
  return std::tuple{a.f, a.g};
}
export template <typename F, typename G>
constexpr auto legs(const Copairing<F, G>& a) {
  return std::tuple{a.f, a.g};
}

// The readers: each former's laws, by the engine on the profiles of what it is
// made of.  The product's (sum's) defining form is read through its factors.
template <typename F, typename G>
inline constexpr bool is_monic_arrow_v<Pairing<F, G>> =
    arrow_node(ArrowNode::Pairing, arrow_laws_v<F>,
               arrow_laws_v<G>)[ArrowLaw::Injective] == Ternary::True;
template <typename A, typename F, typename B, typename G>
  requires(IsPairProjection<A, 1> && IsPairProjection<B, 2>)
inline constexpr bool is_monic_arrow_v<Pairing<Compose<A, F>, Compose<B, G>>> =
    arrow_node(ArrowNode::Product, arrow_laws_v<F>,
               arrow_laws_v<G>)[ArrowLaw::Injective] == Ternary::True;
template <typename A, typename F, typename B, typename G>
  requires(IsPairProjection<A, 1> && IsPairProjection<B, 2>)
inline constexpr bool is_epic_arrow_v<Pairing<Compose<A, F>, Compose<B, G>>> =
    arrow_node(ArrowNode::Product, arrow_laws_v<F>,
               arrow_laws_v<G>)[ArrowLaw::Surjective] == Ternary::True;
template <typename A, typename F, typename B, typename G, typename LeqA,
          typename LeqB>
  requires(IsPairProjection<A, 1> && IsPairProjection<B, 2>)
inline constexpr bool is_monotone_v<Pairing<Compose<A, F>, Compose<B, G>>,
                                    ProductLeq<LeqA, LeqB>> =
    arrow_node(ArrowNode::Product, arrow_laws_v<F, LeqA>,
               arrow_laws_v<G, LeqB>)[ArrowLaw::Monotone] == Ternary::True;
template <typename A, typename F, typename B, typename G, typename LeqA,
          typename LeqB>
  requires(IsPairProjection<A, 1> && IsPairProjection<B, 2>)
inline constexpr bool is_antimonotone_v<Pairing<Compose<A, F>, Compose<B, G>>,
                                        ProductLeq<LeqA, LeqB>> =
    arrow_node(ArrowNode::Product, arrow_laws_v<F, LeqA>,
               arrow_laws_v<G, LeqB>)[ArrowLaw::Antitone] == Ternary::True;
template <typename F, typename G>
inline constexpr bool is_epic_arrow_v<Copairing<F, G>> =
    arrow_node(ArrowNode::Copairing, arrow_laws_v<F>,
               arrow_laws_v<G>)[ArrowLaw::Surjective] == Ternary::True;
template <typename F, typename G, typename X, typename Y>
inline constexpr bool is_monic_arrow_v<
    Copairing<Compose<F, Injection<1, X, Y>>, Compose<G, Injection<2, X, Y>>>> =
    arrow_node(ArrowNode::Sum, arrow_laws_v<F>,
               arrow_laws_v<G>)[ArrowLaw::Injective] == Ternary::True;
template <typename F, typename G, typename X, typename Y>
inline constexpr bool is_epic_arrow_v<
    Copairing<Compose<F, Injection<1, X, Y>>, Compose<G, Injection<2, X, Y>>>> =
    arrow_node(ArrowNode::Sum, arrow_laws_v<F>,
               arrow_laws_v<G>)[ArrowLaw::Surjective] == Ternary::True;

namespace detail_formers_witness {
using I = Identity<int>;
using B = Identity<bool>;
static_assert(IsArrow<ProductArrow<I, B>> && IsArrow<SumArrow<I, B>> &&
                  IsArrow<Pairing<I, I>> && IsArrow<Copairing<I, I>>,
              "the formers are arrows");
static_assert(product_arrow(I{}, B{})({3, true}) == std::pair{3, true} &&
                  sum_arrow(I{}, B{})(std::variant<int, bool>{
                      std::in_place_index<1>, true}) ==
                      std::variant<int, bool>{std::in_place_index<1>, true},
              "f × g and f + g compute through their defining forms");
static_assert(
    IsMonicArrow<ProductArrow<I, B>> && IsEpicArrow<ProductArrow<I, B>>,
    "id × id is a bijection: the product fact, read through ⟨id ∘ π₁, "
    "id ∘ π₂⟩");
static_assert(IsMonicArrow<SumArrow<I, B>> && IsEpicArrow<SumArrow<I, B>>,
              "id + id is a bijection: the sum fact, read through [ι₁ ∘ id, ι₂ "
              "∘ id]");
static_assert(!IsMonicArrow<Pairing<Compose<Π<1, std::pair<int, int>>, I>,
                                    Compose<Π<1, std::pair<int, int>>, I>>>,
              "⟨id ∘ π₁, id ∘ π₁⟩ forgets the second component: not the "
              "product form, and not certified injective");
static_assert(IsMonicArrow<Pairing<I, I>>,
              "⟨f, g⟩ is injective as soon as one leg is (the diagonal)");
static_assert(std::same_as<decltype(I{} * B{}), ProductArrow<I, B>> &&
                  std::same_as<decltype(I{} + B{}), SumArrow<I, B>> &&
                  (I{} * B{})({3, true}) == std::pair{3, true},
              "f * g and f + g are the product and the sum of the arrows");
static_assert(IsEpicArrow<Copairing<I, I>>,
              "[f, g] is surjective as soon as one leg is (the codiagonal)");
static_assert(IsMonotone<ProductArrow<I, I>,
                         ProductLeq<std::less_equal<>, std::less_equal<>>>,
              "monotone × monotone is monotone, under the product order");
}  // namespace detail_formers_witness

}  // namespace dedekind::category
