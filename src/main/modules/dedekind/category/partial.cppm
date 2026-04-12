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

import :logic;    // Provides IsLogicalSpecies, TernaryLogic, Ternary
import :species;  // Provides Morphism<A, B, Func>
import :topoi;    // Provides IsCharacteristic for support classifiers
import :numeric;

namespace dedekind::category {

/** @section Forward_Declarations_of_Traits */
template <typename T, typename Op>
inline constexpr bool is_kleene_associative_v = false;

template <typename T, typename Op>
inline constexpr bool is_kleene_commutative_v = false;

template <typename T, typename Op>
inline constexpr bool is_kleene_invertible_v = false;

template <typename T, typename Op>
inline constexpr T partial_identity_v = T{};

// Specialization for the Classical/Binary result
template <typename T>
struct GetLogic<std::optional<T>> {
  using type = ClassicalLogic;
};

/**
 * @section The_Rescue_Pod
 * @brief The Kleisli Context for "Consistently Broken" Species.
 */
export template <typename T>
struct Partial {
  using value_type = T;
  using logic_species = TernaryLogic;  // Add this for the bridge

  T value;
  Ternary status;  // Use 'status' consistently

  constexpr const T& operator*() const noexcept { return value; }
  constexpr T& operator*() noexcept { return value; }

  constexpr bool operator==(const Partial& other) const {
    return (status == Ternary::True && other.status == Ternary::True)
               ? (value == other.value)
               : (status == other.status);
  }
};

/** @brief A Logic-Aware Result container for Ternary outcomes. */
export template <typename T>
struct TernaryResult {
  using value_type = T;
  using logic_species = TernaryLogic;  // Required for GetLogic
  Ternary status;
  T value;

  constexpr const T& operator*() const noexcept { return value; }
  constexpr T& operator*() noexcept { return value; }
};

// 1. Classical
template <typename T>
constexpr bool presence_of(const std::optional<T>& opt) {
  return opt.has_value();
}

// 2. Ternary (Partial)
template <typename T>
constexpr Ternary presence_of(const Partial<T>& p) {
  return p.status;
}

// 3. Ternary (Result)
template <typename T>
constexpr Ternary presence_of(const TernaryResult<T>& tr) {
  return tr.status;
}

/**
 * @concept IsPotential
 * @brief Structural isomorphism for "Maybe-like" species.
 *
 * Any type that provides a value and a way to check if that
 * value is 'legitimate' (Total) satisfies this concept.
 */
export template <typename R, typename L = typename GetLogic<R>::type>
concept IsPotential = requires(R r) {
  typename R::value_type;
  { *r } -> std::convertible_to<const typename R::value_type&>;
  { presence_of(r) } -> std::convertible_to<typename L::Ω>;
};

static_assert(IsPotential<std::optional<int>>,
              "std::optional must fulfill IsPotential");
static_assert(IsPotential<Partial<int>>, "Partial<T> must fulfill IsPotential");
static_assert(IsPotential<TernaryResult<int>>,
              "TernaryResult<T> must fulfill IsPotential");

/**
 * @concept IsMagmoid: (T × T) ⇸ T
 */
export template <typename T, typename Op>
concept IsMagmoid = requires(T a, T b) {
  { Op{}(std::make_pair(a, b)) } -> IsPotential;
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

/**
 * @concept IsPartialSupportClassifier
 * @brief Characteristic morphism χ: T -> Ω_K3 for partial support.
 */
export template <typename Chi, typename T>
concept IsPartialSupportClassifier =
    IsCharacteristic<Chi> && std::same_as<Dom<Chi>, T> &&
    std::same_as<Cod<Chi>, Ternary>;

/** @section Honest_Generic_Arithmetic_Transforms */

/** @brief Addition with overflow check (Classical Logic). */
export template <std::integral T>
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

template <typename T>
inline constexpr bool is_kleene_associative_v<T, SafeAddTransform<T>> = true;

template <typename T>
inline constexpr T partial_identity_v<T, SafeAddTransform<T>> = T(0);

/**
 * @brief Addition that consults a user-supplied numeric boundary policy.
 *
 * This transform is intentionally policy-driven, so support can be supplied
 * explicitly when a canonical Lipschitz boundary is unknown.
 */
export template <std::integral T,
                 typename BoundaryPolicy = FullMachineBoundaryPolicy<T>>
  requires IsLipschitzBoundaryPolicy<BoundaryPolicy, T>
struct BoundedAddTransform {
  BoundaryPolicy boundary{};

  TernaryResult<T> operator()(std::pair<T, T> p) const noexcept {
    auto [a, b] = p;
    const auto witness = certify_add(a, b, boundary);
    return {witness.status, witness.value};
  }

  using logic_species = TernaryLogic;
};

template <std::unsigned_integral T>
inline constexpr bool is_kleene_associative_v<
    T, BoundedAddTransform<T, FullMachineBoundaryPolicy<T>>> = true;

template <std::unsigned_integral T>
inline constexpr bool is_kleene_commutative_v<
    T, BoundedAddTransform<T, FullMachineBoundaryPolicy<T>>> = true;

template <std::unsigned_integral T>
inline constexpr bool is_kleene_invertible_v<
    T, BoundedAddTransform<T, FullMachineBoundaryPolicy<T>>> = true;

template <std::unsigned_integral T>
inline constexpr T partial_identity_v<
    T, BoundedAddTransform<T, FullMachineBoundaryPolicy<T>>> = T(0);

/** @brief Multiplication with user-supplied numeric boundary policy. */
export template <std::integral T,
                 typename BoundaryPolicy = FullMachineBoundaryPolicy<T>>
  requires IsLipschitzBoundaryPolicy<BoundaryPolicy, T>
struct BoundedMulTransform {
  BoundaryPolicy boundary{};

  TernaryResult<T> operator()(std::pair<T, T> p) const noexcept {
    auto [a, b] = p;
    const auto witness = certify_mul(a, b, boundary);
    return {witness.status, witness.value};
  }

  using logic_species = TernaryLogic;
};

template <std::unsigned_integral T>
inline constexpr bool is_kleene_associative_v<
    T, BoundedMulTransform<T, FullMachineBoundaryPolicy<T>>> = true;

template <std::unsigned_integral T>
inline constexpr bool is_kleene_commutative_v<
    T, BoundedMulTransform<T, FullMachineBoundaryPolicy<T>>> = true;

template <std::unsigned_integral T>
inline constexpr T partial_identity_v<
    T, BoundedMulTransform<T, FullMachineBoundaryPolicy<T>>> = T(1);

/** @brief Division with user-supplied numeric boundary policy. */
export template <std::integral T,
                 typename BoundaryPolicy = FullMachineBoundaryPolicy<T>>
  requires IsLipschitzBoundaryPolicy<BoundaryPolicy, T>
struct BoundedDivTransform {
  BoundaryPolicy boundary{};

  TernaryResult<T> operator()(std::pair<T, T> p) const noexcept {
    auto [a, b] = p;
    const auto witness = certify_div(a, b, boundary);
    return {witness.status, witness.value};
  }

  using logic_species = TernaryLogic;
};

/** @brief Division with truncation-awareness (Ternary Logic). */
export template <std::integral T>
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

/**
 * @concept IsPartialLoop
 * @brief A Partial Monoid with local invertibility on support.
 */
export template <typename T, typename Op>
concept IsPartialLoop = IsPartialMonoid<T, Op> &&
                        requires { requires is_kleene_invertible_v<T, Op>; };

/** @concept IsPartialCommutativeSemigroup */
export template <typename T, typename Op>
concept IsPartialCommutativeSemigroup = IsPartialSemigroup<T, Op> && requires {
  requires is_kleene_commutative_v<T, Op>;
};

/** @concept IsPartialGroup */
export template <typename T, typename Op>
concept IsPartialGroup = IsPartialLoop<T, Op>;

/** @concept IsPartialAbelianGroup */
export template <typename T, typename Op>
concept IsPartialAbelianGroup = IsPartialGroup<T, Op> && requires {
  requires is_kleene_commutative_v<T, Op>;
};

/** @section Honesty_Anchors */

// 1. HonestDiv is a Magmoid.
static_assert(IsMagmoid<int, HonestDivTransform<int>>);

// 2. Addition is a Partial Monoid (0 is the identity).
static_assert(IsPartialMonoid<int, SafeAddTransform<int>>);

// 3. Division fails Semigroup maturation (Not associative).
static_assert(!IsPartialSemigroup<int, HonestDivTransform<int>>);

// 4. Unsigned bounded-add with full machine support matures to a partial
// Abelian group.
static_assert(IsPartialAbelianGroup<
              unsigned int,
              BoundedAddTransform<unsigned int,
                                  FullMachineBoundaryPolicy<unsigned int>>>);

// 5. Unsigned bounded-mul with full machine support matures to a partial
// commutative monoid.
static_assert(IsPartialCommutativeSemigroup<
              unsigned int,
              BoundedMulTransform<unsigned int,
                                  FullMachineBoundaryPolicy<unsigned int>>>);
static_assert(IsPartialMonoid<
              unsigned int,
              BoundedMulTransform<unsigned int,
                                  FullMachineBoundaryPolicy<unsigned int>>>);

}  // namespace dedekind::category
