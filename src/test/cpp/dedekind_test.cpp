#include <catch2/catch_test_macros.hpp>
import dedekind;

TEST_CASE("Basic math operations", "[math]") {
  SECTION("Addition works with positive numbers") {
    CHECK(dedekind::add(2, 3) == 5);
  }

  SECTION("Addition works with zero") {
    int result = dedekind::add(10, 0);
    REQUIRE(result == 10);  // Stops test if this fails
    CHECK(result > 0);      // Continues if this fails
  }
}
