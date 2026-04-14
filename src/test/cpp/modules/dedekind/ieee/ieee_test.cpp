#include <catch2/catch_test_macros.hpp>

import dedekind.ieee;

TEST_CASE("dedekind.ieee core supports explicit IEEE operations",
          "[ieee][core]") {
  const dedekind::ieee::IEEE<double> a{1.5};
  const dedekind::ieee::IEEE<double> b{2.0};

  const auto sum = dedekind::ieee::IEEEAdd<double>{}({a, b});
  const auto product = dedekind::ieee::IEEEMul<double>{}({a, b});

  CHECK(sum.resolve() == 3.5);
  CHECK(product.resolve() == 3.0);
}
