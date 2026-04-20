/**
 * @file linear_algebra:backends.cppm
 * @partition :backends
 * @brief Backend capability contracts for linear algebra realization layers.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @details
 * Defines the `LinearAlgebraBackend` concept and its companion `GenericBackend`
 * witness, providing a backend-agnostic dispatch vocabulary for downstream
 * operator layers (GraphBLAS, BLAS, etc.).
 *
 * Partitions:
 * - `BackendKind` — stable enum discriminating concrete backend families.
 * - `LinearAlgebraBackend` — concept requiring kind, name, and sparse-op flag.
 * - `GenericBackend` — portable baseline satisfying the concept for
 *   contract-only workflows that do not require a real execution engine.
 *
 * @note "L'outil ne doit pas dicter la structure."
 *       -- Paraphrase of a common sentiment in numerical software design
 *       [Trans: "The tool must not dictate the structure."]
 */
module;

#include <concepts>
#include <string_view>

export module dedekind.linear_algebra:backends;

export namespace dedekind::linear_algebra {

/** @brief Stable backend discriminator tags. */
enum class BackendKind {
  generic,
  graphblas,
};

/**
 * @concept LinearAlgebraBackend
 * @brief Backend capability witness used by contract-level dispatch.
 */
template <typename Backend>
concept LinearAlgebraBackend = requires {
  { Backend::kind } -> std::same_as<const BackendKind&>;
  { Backend::name } -> std::same_as<const std::string_view&>;
  { Backend::supports_sparse_linear_operators } -> std::same_as<const bool&>;
};

/** @brief Portable baseline backend for contract-only workflows. */
struct GenericBackend {
  static constexpr BackendKind kind = BackendKind::generic;
  static constexpr std::string_view name = "generic-contract-backend";
  static constexpr bool supports_sparse_linear_operators = false;
};

static_assert(LinearAlgebraBackend<GenericBackend>);

}  // namespace dedekind::linear_algebra
