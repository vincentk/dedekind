/**
 * @file linear_algebra:contracts.cppm
 * @partition :contracts
 * @brief Rank-nullity aware compile-time contracts for linear operators.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */
module;

#include <concepts>
#include <cstddef>

export module dedekind.linear_algebra:contracts;

export namespace dedekind::linear_algebra {

/**
 * @concept RankNullityWitness
 * @brief Static witness for the rank-nullity identity.
 */
export template <typename W>
concept RankNullityWitness = requires {
  { W::ambient_dimension } -> std::convertible_to<std::size_t>;
  { W::rank } -> std::convertible_to<std::size_t>;
  { W::nullity } -> std::convertible_to<std::size_t>;
  requires(W::rank <= W::ambient_dimension);
  requires(W::nullity <= W::ambient_dimension);
  requires(W::rank + W::nullity == W::ambient_dimension);
};

/**
 * @struct StaticRankNullity
 * @brief Convenience compile-time carrier for finite-dimensional witnesses.
 */
export template <std::size_t AmbientDimension, std::size_t Rank>
  requires(Rank <= AmbientDimension)
struct StaticRankNullity {
  static constexpr std::size_t ambient_dimension = AmbientDimension;
  static constexpr std::size_t rank = Rank;
  static constexpr std::size_t nullity = AmbientDimension - Rank;
};

/**
 * @concept LinearOperatorContract
 * @brief Minimal contract for finite-dimensional linear operator carriers.
 */
export template <typename Op>
concept LinearOperatorContract = requires {
  typename Op::scalar_type;
  typename Op::rank_nullity;
  requires RankNullityWitness<typename Op::rank_nullity>;
};

}  // namespace dedekind::linear_algebra
