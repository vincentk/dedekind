#include <catch2/catch_test_macros.hpp>
#include <complex>

import dedekind.algebra;

using namespace dedekind::algebra;

TEST_CASE("Modules: Integer Polynomial Action", "[algebra][modules]") {
  SECTION("Polynomial Carrier over Z") {
    // p(x) = 3x^2 + 2x + 5
    // Coeffs in vector: {5, 2, 3} (constant first)
    Polynomial<int> p({5, 2, 3});
    CHECK(p.degree() == 2);
    CHECK_FALSE(p.is_zero());
  }

  SECTION("Reified 1D real line supports vector operations") {
    RealLine x(2.0);
    RealLine y(3.5);

    CHECK((x + y).coordinate() == 5.5);
    CHECK((2.0 * x).coordinate() == 4.0);
    CHECK((x * 2.0).coordinate() == 4.0);
    CHECK((y - x).coordinate() == 1.5);
  }

  SECTION("Other 1D spaces: complex line") {
    using C = std::complex<double>;
    using ComplexLine = OneDimensionalVector<C, struct ComplexTag>;

    ComplexLine v(C{1.0, 2.0});
    ComplexLine w(C{0.5, -1.0});

    CHECK((v + w).coordinate() == C{1.5, 1.0});
    CHECK((C{2.0, 0.0} * v).coordinate() == C{2.0, 4.0});
  }

  SECTION("Semimodule and vector-space baseline concepts are grounded") {
    CHECK(IsSemimodule<unsigned int, unsigned int>);
    CHECK(IsVectorSpaceLike<RealLine, double>);
  }
}
