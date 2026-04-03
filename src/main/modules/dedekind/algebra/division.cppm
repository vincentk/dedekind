/**
 * @file dedekind/algebra/polynomials.cppm
 * @partition :euclidean
 * @brief Level 3.3: The Euclidean Algorithm for R[x].
 */
module;

#include <functional>  // for std::plus
#include <stdexcept>   // for std::domain_error
#include <utility>     // for std::make_pair
#include <vector>

export module dedekind.algebra:division;

import :ring;
import dedekind.category;
import dedekind.order;

namespace dedekind::algebra {
using namespace dedekind::category;
using namespace dedekind::order;

/**
 * @concept IsDivisionRing
 * @brief A Ring establishing the existence of Multiplicative Inverses.
 */
export template <typename T>
concept IsDivisionRing = IsRing<T> && requires(T a, T b) {
  { a.inverse() } -> std::same_as<T>;
  { a / b } -> std::same_as<T>;
};

/**
 * @concept IsEuclidean
 * @brief A Commutative Ring endowed with a division algorithm.
 */
export template <typename T>
concept IsEuclidean =
    IsCommutativeRing<T> && IsDividableChain<T> && requires(T a, T b) {
      { a / b } -> std::same_as<T>;
      { a % b } -> std::same_as<T>;
    };

}  // namespace dedekind::algebra