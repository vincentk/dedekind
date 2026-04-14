#include <catch2/catch_test_macros.hpp>

import dedekind.ieee;
import dedekind.ieee.approx;

TEST_CASE("dedekind.ieee.approx propagates first-order error",
          "[ieee][approx]") {
  const dedekind::ieee::Approx<double> a{dedekind::ieee::ieee_unit(1.0), 0.1,
                                         0.0};
  const dedekind::ieee::Approx<double> b{dedekind::ieee::ieee_unit(2.0), 0.2,
                                         0.0};

  const auto result = dedekind::ieee::demo_propagate_add(a, b);
  CHECK(result.value.resolve() == 3.0);
  CHECK(result.abs_error >= 0.3);
}
