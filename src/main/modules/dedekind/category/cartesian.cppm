/**
 * @file cartesian.cppm
 * @brief Level 0b: Cartesian Closed Category (CCC) Foundations.
 *
 * @partition :cartesian
 * @section Cartesian: Products, Coproducts, Exponentials, and Terminal Objects
 * This partition defines the structures required for a category to be
 * Cartesian Closed. In the Dedekind universe, this provides the "Product"
 * and "Function Space" (Exponential) mechanics necessary for ETCS.
 *
 * @section Std_Namespace_Mappings
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
 */
module;

#include <concepts>
#include <functional>
#include <utility>
#include <variant>

export module dedekind.category:cartesian;

import :logic;
import :limit;

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
export template <typename P, typename A, typename B>
concept IsProduct = requires(P p) {
  { p.first } -> std::convertible_to<A>;
  { p.second } -> std::convertible_to<B>;
};

static_assert(
    IsProduct<std::pair<int, bool>, int, bool>,
    "Verification Failed: std::pair<int, bool> must satisfy IsProduct.");

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
 * @brief Currying: (X × A → B) ⟹ (X → B^A)
 */
export template <IsArrowFromProduct F>
auto curry(F&& f) {
  using X = typename Dom<F>::first_type;
  using A = typename Dom<F>::second_type;

  // Use explicit 'const X& x' instead of 'const auto& x'
  return arrow([f = std::forward<F>(f)](const X& x) {
    // Use explicit 'const A& a'
    return arrow([f, x](const A& a) { return f({x, a}); });
  });
}

/**
 * @brief Uncurrying: (X → B^A) ⟹ (X × A → B)
 * @details Transforms a function returning a function into a single function
 * taking a categorical product (std::pair).
 */
export template <IsArrow F>
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
        return std::visit(
            [&](auto&& val) -> X {
              using T = std::decay_t<decltype(val)>;
              if constexpr (std::same_as<T, A>)
                return f(val);
              else
                return g(val);
            },
            v);
      });
}

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

/** @section Structural_Exponential_Verification */

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

}  // namespace dedekind::category
