/**
 * @file linear_algebra:graphblas.cppm
 * @partition :graphblas
 * @brief GraphBLAS-facing adapter stub for contract conformance checks.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @details
 * This partition intentionally avoids linking to a concrete GraphBLAS library.
 * It provides a compile-time adapter witness used to validate that the
 * contract-level API can host a GraphBLAS backend in a follow-up PR.
 */
module;

#include <cstddef>
#include <string_view>

export module dedekind.linear_algebra:graphblas;

import :backends;
import :contracts;

export namespace dedekind::linear_algebra {

/** @brief Contract-only GraphBLAS backend witness for Phase 1 integration. */
struct GraphBLASBackendStub {
  static constexpr BackendKind kind = BackendKind::graphblas;
  static constexpr std::string_view name = "graphblas-stub";
  static constexpr bool supports_sparse_linear_operators = true;
};

static_assert(LinearAlgebraBackend<GraphBLASBackendStub>);

/**
 * @struct SparseLinearOperatorStub
 * @brief Minimal operator carrier that binds contracts to a backend witness.
 */
template <typename Scalar, std::size_t AmbientDimension, std::size_t Rank,
          typename Backend = GraphBLASBackendStub>
  requires LinearAlgebraBackend<Backend>
struct SparseLinearOperatorStub {
  using scalar_type = Scalar;
  using rank_nullity = StaticRankNullity<AmbientDimension, Rank>;
  using backend_type = Backend;
};

/**
 * @concept BackendConformantLinearOperator
 * @brief Linear operator contract with explicit backend capability witness.
 */
template <typename Op>
concept BackendConformantLinearOperator =
    LinearOperatorContract<Op> &&
    LinearAlgebraBackend<typename Op::backend_type>;

/** @section Formal_Verification */

// SparseLinearOperatorStub satisfies the minimal linear operator contract
// (rank-nullity witness + scalar type) and the backend-aware extension.
static_assert(
    LinearOperatorContract<SparseLinearOperatorStub<double, 4, 2>>,
    "SparseLinearOperatorStub must satisfy LinearOperatorContract.");
static_assert(
    BackendConformantLinearOperator<SparseLinearOperatorStub<double, 4, 2>>,
    "SparseLinearOperatorStub must satisfy BackendConformantLinearOperator.");

}  // namespace dedekind::linear_algebra
