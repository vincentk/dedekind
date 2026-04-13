/**
 * @file algebra:division.cppm
 * @partition :division
 * @brief Level 3.3: The Euclidean Engine (Division Rings and Domains).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section The_Euclidean_Restoration
 * « إن هذا العلم، أي الجبر، غايته استخراج المجهولات، سواء كانت
 *   مقادير عددية أو هندسية. »
 * (The aim of this science, Algebra, is to extract unknowns,
 *  whether they be numerical or geometrical magnitudes.)
 *  — Omar Khayyam (عمر الخيام), 'Treatise on Demonstration of Problems of
 * Algebra'
 *
 * @section Taxonomy_of_Extraction
 * This partition reifies the "Morphism of Remainder." By establishing
 * IsEuclidean, we enable the recursive balancing of magnitudes—the
 * foundational GCD algorithm that allows for the 'restoration' of
 * simpler algebraic forms through the Euclidean algorithm.
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
 * @tparam T The carrier type.
 * @tparam Add Additive operation witness (defaults to `std::plus<T>`).
 * @tparam Mult Multiplicative operation witness
 * (defaults to `std::multiplies<T>`).
 * @tparam Div Division operation witness (defaults to `std::divides<T>`).
 */
export template <typename T, typename Add = std::plus<T>,
                 typename Mult = std::multiplies<T>,
                 typename Div = std::divides<T>>
concept IsDivisionRing = IsRing<T, Add, Mult> && requires(T a, T b) {
  { a.inverse() } -> std::same_as<T>;
  { a / b } -> std::same_as<T>;
  { Div{}(a, b) } -> std::same_as<T>;
};

/**
 * @concept IsEuclidean
 * @brief A Commutative Ring endowed with a division algorithm.
 * @tparam T The carrier type.
 * @tparam Add Additive operation witness (defaults to `std::plus<T>`).
 * @tparam Mult Multiplicative operation witness
 * (defaults to `std::multiplies<T>`).
 * @tparam Div Division operation witness (defaults to `std::divides<T>`).
 * @tparam Rem Remainder operation witness (defaults to `std::modulus<T>`).
 */
export template <typename T, typename Add = std::plus<T>,
                 typename Mult = std::multiplies<T>,
                 typename Div = std::divides<T>, typename Rem = std::modulus<T>>
concept IsEuclidean = IsCommutativeRing<T, Add, Mult> && IsDividableChain<T> &&
                      requires(T a, T b) {
                        { a / b } -> std::same_as<T>;
                        { a % b } -> std::same_as<T>;
                        { Div{}(a, b) } -> std::same_as<T>;
                        { Rem{}(a, b) } -> std::same_as<T>;
                      };

}  // namespace dedekind::algebra