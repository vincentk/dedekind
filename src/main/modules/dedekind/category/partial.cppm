/**
 * @file ontology:category.cppm
 * @partition :partial
 * @brief Level 0.2-P: The Logic of Potentiality (Partial Algebra).
 *
 * @section The_Algebraic_Logic_of_Partiality
 * "Metoda algebraiczna w logice polega na traktowaniu każdego systemu
 *  logicznego jako pewnego określonego rodzaju algebry abstrakcyjnej." — H.
 * Rasiowa
 *
 * @details
 * This partition bridges Algebraic Species with their governing Logic Species
 * (Ω). We distinguish between 'True' (Exact), 'False' (Undefined), and
 * 'Unknown' (Truncated/Lossy) results using the Ternary Topos.
 */
module;

#include <concepts>
#include <limits>
#include <optional>
#include <utility>

export module dedekind.category:partial;

import :logic;    // Provides LogicalSpecies, TernaryLogic, Ternary
import :species;  // Provides Morphism<A, B, Func>
import :numeric;

namespace dedekind::category {

/**
 * @concept IsPotential
 * @brief Structural isomorphism for "Maybe-like" species.
 *
 * Any type that provides a value and a way to check if that
 * value is 'legitimate' (Total) satisfies this concept.
 */
export template <typename R>
concept IsPotential = requires(R r) {
  typename R::value_type;
  { *r } -> std::convertible_to<typename R::value_type&>;
  { presence_of(r) } -> LogicalValue;  // Open-ended bridge
};

// 1. Align std::optional (Classical/Binary)
template <typename T>
constexpr bool presence_of(const std::optional<T>& opt) {
  return opt.has_value();
}

// 2. Align Partial (Ternary/Ω)
template <typename T>
constexpr Ternary presence_of(const Partial<T>& p) {
  return p.status;
}

/** @brief A Logic-Aware Result container for Ternary outcomes. */
template <typename T>
struct TernaryResult {
  using value_type = T;
  Ternary status;
  T value;

  constexpr Ternary presence() const noexcept { return status; }
  constexpr const T& operator*() const noexcept { return value; }
};

/**
 * @concept IsMagmoid: (T × T) ⇸ T
 */
export template <typename T, typename Op>
concept IsMagmoid = requires(T a, T b) {
  // Resolve the logic species (Ternary for int, Classical for others)
  typename GetLogic<T>::type;
  { Op{}(std::make_pair(a, b)) } -> IsPotential<T, typename GetLogic<T>::type>;
};

/**
 * @concept IsPartialAssociative
 * @brief The Kleene Associativity Law.
 * @details "If both sides are defined, they are equal."
 */
export template <typename T, typename Op>
concept IsPartialAssociative =
    IsMagmoid<T, Op> && requires { requires is_kleene_associative_v<T, Op>; };

/**
 * @concept HasPartialIdentity
 * @brief Existence of a Neutral Element in a Partial Universe.
 */
export template <typename T, typename Op>
concept HasPartialIdentity = IsMagmoid<T, Op> && requires {
  { partial_identity_v<T, Op> } -> std::convertible_to<T>;
};

/** @section Honest_Generic_Arithmetic_Transforms */

/** @brief Addition with overflow check (Classical Logic). */
template <std::integral T>
struct SafeAddTransform {
  std::optional<T> operator()(std::pair<T, T> p) const noexcept {
    auto [a, b] = p;
    if constexpr (std::is_signed_v<T>) {
      if (b > 0 && a > (std::numeric_limits<T>::max() - b)) return std::nullopt;
      if (b < 0 && a < (std::numeric_limits<T>::min() - b)) return std::nullopt;
    }
    return a + b;
  }
  // Extension to bridge optional to IsPotential
  using logic_species = ClassicalLogic;
};

/** @brief Division with truncation-awareness (Ternary Logic). */
template <std::integral T>
struct HonestDivTransform {
  TernaryResult<T> operator()(std::pair<T, T> p) const noexcept {
    auto [a, b] = p;
    if (b == 0) return {Ternary::False, T(0)};
    if (a % b != 0) return {Ternary::Unknown, static_cast<T>(a / b)};
    return {Ternary::True, static_cast<T>(a / b)};
  }
  using logic_species = TernaryLogic;
};

/** @section Concept_Maturation */

export template <typename T, typename Op>
concept IsPartialSemigroup = IsMagmoid<T, Op> && IsPartialAssociative<T, Op>;

export template <typename T, typename Op>
concept IsPartialMonoid =
    IsPartialSemigroup<T, Op> && HasPartialIdentity<T, Op>;

/** @section Honesty_Anchors */

// 1. HonestDiv is a Magmoid.
static_assert(IsMagmoid<int, HonestDivTransform<int>>);

// 2. Addition is a Partial Monoid (0 is the identity).
static_assert(IsPartialMonoid<int, SafeAddTransform<int>>);

// 3. Division fails Semigroup maturation (Not associative).
static_assert(!IsPartialSemigroup<int, HonestDivTransform<int>>);

/**
 * @section The_Rescue_Pod
 * @brief The Kleisli Context for "Consistently Broken" Species.
 */
export template <typename T>
struct Partial {
  using value_type = T;
  T value;
  Ternary presence;  // Our Ω from :logic

  // Equality: Only true if values match AND both are present.
  constexpr bool operator==(const Partial& other) const {
    return (presence == Ternary::True && other.presence == Ternary::True)
               ? (value == other.value)
               : (presence == other.presence);
  }
};

/** @section Kleisli_Triple: η and >>= */

// η (Unit): Lifting a value into the Partial context.
export template <typename T>
struct η_impl<Partial, T> {
  constexpr Partial<T> operator()(T x) const { return {x, Ternary::True}; }
};

// >>= (Bind): The monadic gatekeeper for Kleisli composition.
export template <typename T, typename F>
constexpr auto operator>>=(const Partial<T>& m, F f) {
  using U_partial = std::invoke_result_t<F, T>;
  using U = typename U_partial::value_type;

  // The "Honest Propagation" logic:
  // If the input is already indeterminate, the output must be as well.
  if (m.presence != Ternary::True) {
    return U_partial{U{}, m.presence};
  }

  // Otherwise, execute the morphism (which may return Unknown itself).
  return f(m.value);
}

}  // namespace dedekind::category
