/**
 * @file dedekind/category/cartesian.cppm
 * @partition :cartesian
 * @brief Cartesian Closed Category (CCC) Foundations.
 * @section cartesian__Cartesian: Products, Coproducts, Exponentials, and
 * Terminal Objects This partition defines the structures required for a
 * category to be Cartesian Closed. In the Dedekind universe, this provides the
 * "Product" and "Function Space" (Exponential) mechanics necessary for ETCS.
 *
 * @section cartesian__Std_Namespace_Mappings
 * This partition asserts bidirectional mappings between categorical constructs
 * and `std` types:
 *
 * | Concept          | `std` representative                          |
 * |------------------|-----------------------------------------------|
 * | `IsProduct`      | `std::pair<A, B>` (via `.first` / `.second`)  |
 * | `IsCoproduct`    | `std::variant<A, B>` (via ι₁ / ι₂)           |
 * | `IsExponential`  | `std::function<B(A)>`, lambdas                |
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

/**
 * @concept IsProduct
 * @brief Categorification of `std::pair<A, B>` as the categorical product
 * (A × B).
 * @details A product of A and B is an object P equipped with projection
 * morphisms π₁: P → A and π₂: P → B such that for any object X with morphisms
 * f: X → A and g: X → B, there exists a unique morphism u: X → P making the
 * following diagram commute:
 * @code
 *        X
 *       / \
 *      f   g
 *     /     \
 *    A       B
 *     \     /
 *      π₁  π₂
 *       \ /
 *        P
 * @endcode
 *
 * `std::pair<A, B>` satisfies this concept via its `.first` (π₁) and
 * `.second` (π₂) members.
 */
template <typename P, typename A, typename B>
concept IsPairLikeProduct = requires(P p) {
  { p.first } -> std::convertible_to<A>;
  { p.second } -> std::convertible_to<B>;
};

export template <typename P, typename A, typename B>
concept IsProduct = IsPairLikeProduct<P, A, B>;

static_assert(
    IsProduct<std::pair<int, bool>, int, bool>,
    "Verification Failed: std::pair<int, bool> must satisfy IsProduct.");

template <typename A, typename B>
struct SpeciesTraits<std::pair<A, B>> {
  using Domain = std::pair<A, B>;
  using machine_type = Domain;
};

template <typename... Ts>
  requires(sizeof...(Ts) > 0)
struct SpeciesTraits<std::tuple<Ts...>> {
  using Domain = std::tuple<Ts...>;
  using machine_type = Domain;
};

/**
 * @concept IsProductProjection
 * @brief Functional-part projection witness from a product whole to one part.
 *
 * @details
 * A projection is a morphism-like accessor from a whole `P` to one component
 * type (`A` for left, `B` for right). In categorical terms these correspond to
 * canonical product projections π₁ and π₂.
 */
export template <typename Projection, typename Whole, typename Part>
concept IsProductProjection = requires(Projection projection, Whole whole) {
  { projection(whole) } -> std::convertible_to<Part>;
};

/**
 * @concept IsProjectedProduct
 * @brief Product witness through an optional whole-projection policy.
 *
 * @details
 * By default (`WholeProject = std::identity`), this reduces to a direct
 * `IsProduct<P, A, B>` check. With an opt-in projector (for example an
 * `operator->` drill-down policy), this concept certifies that a wrapper type
 * exposes a product whole whose canonical parts are still discoverable.
 */
export template <typename P, typename A, typename B,
                 typename WholeProject = std::identity>
concept IsProjectedProduct =
    requires(WholeProject project, const P& p) {
      { project(p) };
    } &&
    IsProduct<std::remove_cvref_t<decltype(std::declval<WholeProject>()(
                  std::declval<const P&>()))>,
              A, B>;

static_assert(IsProductProjection<decltype([](const std::pair<int, bool>& p) {
                                    return p.first;
                                  }),
                                  std::pair<int, bool>, int>,
              "π1 must project Product -> LeftPart.");
static_assert(IsProductProjection<decltype([](const std::pair<int, bool>& p) {
                                    return p.second;
                                  }),
                                  std::pair<int, bool>, bool>,
              "π2 must project Product -> RightPart.");
static_assert(IsProjectedProduct<std::pair<int, bool>, int, bool>,
              "Identity projection must certify direct products.");

/**
 * @brief Mediating morphism for Products: ⟨f, g⟩: X -> (A × B)
 * @details Given f: X -> A and g: X -> B, constructs the unique morphism
 * that pairs their results.
 */
export template <IsArrow F, IsArrow G>
  requires std::same_as<Dom<F>, Dom<G>>  // Universal property: same source X
auto mediate_product(F&& f, G&& g) {
  using X = Dom<F>;
  using A = Cod<F>;
  using B = Cod<G>;

  return arrow([f = std::forward<F>(f), g = std::forward<G>(g)](const X& x) {
    return std::pair<A, B>(f(x), g(x));
  });
}

/**
 * @concept IsArrowFromProduct
 * @brief Matches an Arrow whose Domain is a categorical Product (std::pair).
 */
export template <typename F>
concept IsArrowFromProduct =
    IsArrow<F> && IsProduct<Dom<F>, typename Dom<F>::first_type,
                            typename Dom<F>::second_type>;

/**
 * @concept IsExponential
 * @brief Categorification of `std::function<B(A)>` as the Internal Hom-set
 * (B^A).
 * @details Identifies a type as an inhabitant of the function space from A to
 * B. A type F satisfies this concept when it is move-constructible and
 * callable with a single argument of type A returning B.
 *
 * Three canonical inhabitants in the `std` namespace:
 * - `std::function<B(A)>` — type-erased, copyable function wrapper.
 * - `std::move_only_function<B(A)>` (C++23) — move-only, lighter-weight
 *   alternative.
 * - Any lambda `[...](A) -> B` — anonymous structural closure.
 */
export template <typename F, typename A, typename B>
concept IsExponential =
    std::move_constructible<F> && std::same_as<std::invoke_result_t<F, A>, B>;

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
 */
export template <typename T, typename A, typename B>
concept IsCoproduct = requires(A a, B b) {
  { T(a) } -> std::same_as<T>;
  { T(b) } -> std::same_as<T>;
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

/**
 * @brief Mediating morphism for Coproducts: [f, g]: (A + B) -> X
 * @details Given f: A -> X and g: B -> X, constructs the unique morphism
 * (the "case" analysis) that handles either alternative of a variant.
 */
export template <IsArrow F, IsArrow G>
  requires std::same_as<Cod<F>, Cod<G>>  // Must map to the same Target X
auto mediate_coproduct(F&& f, G&& g) {
  using A = Dom<F>;
  using B = Dom<G>;
  using X = Cod<F>;
  using Var = std::variant<A, B>;

  return arrow<Var, X>(
      [f = std::forward<F>(f), g = std::forward<G>(g)](const Var& v) -> X {
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
auto eval(F&& f, A&& a) -> decltype(auto) {
  return std::invoke(std::forward<F>(f), std::forward<A>(a));
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
// This captures the 'essence' without the 'lineage'
auto closure = [limit = 42](int x) { return x < limit; };
static_assert(
    IsExponential<decltype(closure), int, bool>,
    "Structural: A lambda is discovered as an Exponential by its scent.");

// 4. The "Honest Rejection": Mismatched Signature
// Valid function, but wrong mapping for this specific Hom-set
static_assert(!IsExponential<std::function<int(int)>, int, bool>,
              "Verification: Rejected due to Codomain mismatch.");

/**
 * @concept IsCartesianClosed
 * @brief A Category equipped with Terminal Objects, Products, and Exponentials.
 * @details This concept extends `IsCategory` to satisfy the three axioms of a
 * Cartesian Closed Category (CCC):
 * 1. **Terminal Object** — a unique sink `1` (represented by `One` /
 *    `std::monostate`).
 * 2. **Products** — for any two objects A, B, a product A × B exists
 *    (represented by `std::pair<A, B>`).
 * 3. **Exponentials** — for any two objects A, B, a function space B^A exists
 *    (represented by `std::function<B(A)>` or any lambda).
 *
 * This triple of structures provides the categorical foundations required for
 * the Dedekind topos (ETCS).
 */
export template <typename Cat>
concept IsCartesianClosed = IsCategory<Cat> && requires {
  // 1. Terminal Object exists
  typename Cat::Terminal;
  requires IsTerminalObject<typename Cat::Terminal>;
} && requires(typename Cat::Arrow::Domain A, typename Cat::Arrow::Codomain B) {
  // 2. Cartesian: Products exist for objects in the category
  typename Cat::template Product<decltype(A), decltype(B)>;
  requires IsProduct<typename Cat::template Product<decltype(A), decltype(B)>,
                     decltype(A), decltype(B)>;

  // 3. Closed: Exponentials exist for objects in the category
  typename Cat::template Exponential<decltype(A), decltype(B)>;
  requires IsExponential<
      typename Cat::template Exponential<decltype(A), decltype(B)>, decltype(A),
      decltype(B)>;
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
   * This satisfies the std::convertible_to constraint in IsCategory.
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

  // 3. Exponential: B^A
  template <typename A, typename B>
  using Exponential = std::function<B(A)>;

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
static_assert(IsCartesianClosed<Set<int>>,
              "Verification Failed: Set must be Cartesian Closed.");

/**
 * @concept HasCanonicalSetCCC
 * @brief Mnemonic bridge: ambient species A has a canonical CCC witness.
 *
 * @details
 * This keeps category-level verification explicit without forcing object-level
 * set concepts to also be category concepts.
 */
export template <typename A>
concept HasCanonicalSetCCC = IsCartesianClosed<CanonicalSetCCC<A>>;

/**
 * @concept IsProductCategory
 * @brief A Category Hub where the Species is a Categorical Product (std::pair).
 */
export template <typename Cat>
concept IsProductCategory = IsCategory<Cat> && requires {
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
//   | reflexive          | @c is_reflexive_v<T, Op>    | @c :mereology    |
//   | transitive         | @c is_transitive_v<T, Op>   | @c :mereology    |
//   | antisymmetric      | @c is_antisymmetric_v<T, Op>| @c :mereology    |
//   | left-total         | @c is_left_total_v<R>       | @b :cartesian    |
//   | right-unique       | @c is_right_unique_v<R>     | @b :cartesian    |
//   | symmetric          | (filed for follow-up)       | @c :mereology    |
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
//   | + left-total + right-unique + monic + epic | bijection           |
//
// Note the indexing difference: @c :mereology's traits are keyed on
// @c (T, Op) (homogeneous binary operation); the two traits added here
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
// @c sets:expressions as @c Relation<T1, T2, L, P> = @c Set<pair<T1,
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
 * the value-level @c sets::Relation<T1, T2, L, P> alias, which models
 * the relation as a Set of pairs.  The two readings are isomorphic.
 */
export template <typename R, typename A, typename B>
concept IsBinaryRelation = requires(R const& r, A const& a, B const& b) {
  { r(a, b) } -> std::convertible_to<bool>;
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
export template <typename R, typename A, typename B>
concept IsBinaryFunction =
    IsBinaryRelation<R, A, B> && is_left_total_v<R> && is_right_unique_v<R>;

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
 * @brief User-declared reflexivity witness for a binary relation.
 * @details Reflexivity: @c R(a, @c a) holds for every @c a.  Cannot be
 *          checked at compile time in general; opt-in.  Sibling to
 *          @c :mereology's @c is_reflexive_v<T, Op> (which is keyed on
 *          a binary @b operation, not a binary @b relation).
 */
export template <typename R>
inline constexpr bool is_reflexive_relation_v = false;

/**
 * @brief User-declared symmetry witness for a binary relation.
 * @details Symmetry: @c R(a, @c b) implies @c R(b, @c a).  Combined
 *          with reflexivity and transitivity, characterises an
 *          @b equivalence @b relation; combined with @c IsBinaryRelation
 *          on a homogeneous @c V @c × @c V, characterises an
 *          @b undirected @b graph.
 */
export template <typename R>
inline constexpr bool is_symmetric_relation_v = false;

/**
 * @brief User-declared antisymmetry witness for a binary relation.
 * @details Antisymmetry: @c R(a, @c b) and @c R(b, @c a) imply @c a
 *          @c == @c b.  Combined with reflexivity and transitivity,
 *          characterises a @b partial @b order.  Sibling to
 *          @c :mereology's @c is_antisymmetric_v<T, Op>.
 */
export template <typename R>
inline constexpr bool is_antisymmetric_relation_v = false;

/**
 * @brief User-declared transitivity witness for a binary relation.
 * @details Transitivity: @c R(a, @c b) and @c R(b, @c c) imply
 *          @c R(a, @c c).  Sibling to @c :mereology's
 *          @c is_transitive_v<T, Op>.
 */
export template <typename R>
inline constexpr bool is_transitive_relation_v = false;

/**
 * @concept IsEquivalenceRelation
 * @brief A homogeneous binary relation @c R @c ⊆ @c V @c × @c V is an
 *        @b equivalence @b relation when reflexive, symmetric, and
 *        transitive.
 * @details Combines @c IsBinaryRelation<R, V, V> with the three opt-in
 *          honesty traits.  Canonical witness: @c std::equal_to<V>
 *          (registered below for @c V @c = @c int as the existential
 *          proof).
 */
export template <typename R, typename V>
concept IsEquivalenceRelation =
    IsBinaryRelation<R, V, V> && is_reflexive_relation_v<R> &&
    is_symmetric_relation_v<R> && is_transitive_relation_v<R>;

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
concept IsUndirectedGraph = IsDirectedGraph<R, V> && is_symmetric_relation_v<R>;

// ---------------------------------------------------------------------------
// Canonical existential-proof witness: std::equal_to<V> is the equivalence
// relation on V — but ONLY for integral V.  Floating-point @c V breaks
// reflexivity (NaN @c == @c NaN is false under IEEE 754); user-defined
// types may have an @c operator== that violates equivalence laws (the
// project trusts only what it verifies, and it has not verified
// arbitrary user-defined equality).  The constraint is therefore
// @c std::integral<V>; equality on integral types is a true equivalence
// (no NaN, no overflow-induced asymmetry).  Floating-point and user-
// defined V are NOT registered here; downstream callers that want
// structural-equivalence treatment for those carriers register their
// own opt-in traits site-locally.
// ---------------------------------------------------------------------------
template <std::integral V>
inline constexpr bool is_reflexive_relation_v<std::equal_to<V>> = true;
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

}  // namespace dedekind::category
