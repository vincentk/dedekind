#include <catch2/catch_test_macros.hpp>

import dedekind.linear_algebra;

// Acceptance criteria for #342:
// (AC1) The matrix API exposes rank at compile time.
// (AC2) The matrix API exposes number of columns (ambient_dimension) at compile
// time. (AC3) A compile-time check can express and validate: rank + nullity ==
// number_of_columns.

TEST_CASE("Linear algebra contracts: rank-nullity identity (compile-time)",
          "[linear_algebra][contracts]") {
  using namespace dedekind::linear_algebra;

  // AC1 + AC2: rank and ambient_dimension are constexpr static members.
  using Witness = StaticRankNullity<7, 4>;
  STATIC_REQUIRE(Witness::rank == 4);
  STATIC_REQUIRE(Witness::ambient_dimension == 7);
  STATIC_REQUIRE(Witness::nullity == 3);

  // AC3: compile-time identity rank + nullity == number_of_columns.
  STATIC_REQUIRE(Witness::rank + Witness::nullity ==
                 Witness::ambient_dimension);

  // Concept-level check: RankNullityWitness enforces the identity statically.
  STATIC_REQUIRE(RankNullityWitness<Witness>);
}

TEST_CASE("Linear algebra contracts: backend conformance",
          "[linear_algebra][contracts]") {
  using namespace dedekind::linear_algebra;

  using Op = SparseLinearOperatorStub<double, 5, 3>;
  STATIC_REQUIRE(LinearOperatorContract<Op>);
  STATIC_REQUIRE(BackendConformantLinearOperator<Op>);
  STATIC_REQUIRE(Op::backend_type::supports_sparse_linear_operators);

  // Backend kind discriminator is a compile-time constant.
  STATIC_REQUIRE(Op::backend_type::kind == BackendKind::graphblas);
}
