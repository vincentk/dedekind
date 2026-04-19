#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <ranges>
#include <set>
#include <type_traits>
#include <vector>

import dedekind.python;

using namespace dedekind;

TEST_CASE("Python facade: explicit finite set interop", "[python][interop]") {
  std::set<int> ordered{1, 2, 3};

  auto ext = python::from_std(ordered);
  auto back = python::to_std<std::set<int>>(ext);

  REQUIRE(back == ordered);
}

TEST_CASE("Python facade: finite path range adapters", "[python][ranges]") {
  const std::vector<int> source{2, 4, 6, 8};
  const auto path = python::from_range(source);

  REQUIRE(path.size() == source.size());
  REQUIRE(std::ranges::equal(python::as_range(path), source));
}

TEST_CASE("Python facade: explicit linear algebra backend dependency",
          "[python][linear_algebra]") {
  REQUIRE(python::graphblas_backend_stub_available());
  STATIC_REQUIRE(
      std::same_as<
          python::LinearAlgebraBackendKind,
          std::remove_cvref_t<decltype(python::GraphBLASBackend::kind)>>);
}
