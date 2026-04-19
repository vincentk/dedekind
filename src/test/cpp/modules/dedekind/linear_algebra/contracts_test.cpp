#include <catch2/catch_test_macros.hpp>

import dedekind.linear_algebra;

TEST_CASE("Linear algebra contracts: rank-nullity and backend conformance",
          "[linear_algebra][contracts]") {
  using namespace dedekind::linear_algebra;

  using Witness = StaticRankNullity<7, 4>;
  CHECK(RankNullityWitness<Witness>);
  CHECK(Witness::rank + Witness::nullity == Witness::ambient_dimension);

  using Op = SparseLinearOperatorStub<double, 5, 3>;
  CHECK(LinearOperatorContract<Op>);
  CHECK(BackendConformantLinearOperator<Op>);
  CHECK(Op::backend_type::supports_sparse_linear_operators);
}
