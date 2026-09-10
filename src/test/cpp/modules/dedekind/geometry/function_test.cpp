#include <catch2/catch_test_macros.hpp>

import dedekind.category;
import dedekind.geometry;

using dedekind::category::arrow;
using dedekind::geometry::orbit_sum;
using dedekind::geometry::pointwise_add;
using dedekind::geometry::scale;

namespace {
// The identity and the parity involution x ↦ −x — the ℤ₂ point group on ℤ.
constexpr auto id_int = arrow<int, int>([](const int& x) { return x; });
constexpr auto neg = arrow<int, int>([](const int& x) { return -x; });
}  // namespace

// ---------------------------------------------------------------------------
// #537 slice 2: pointwise + and scalar · on the IsArrow surface, CONSTEXPR
// (unlike Path's std::function carrier — pins the function-space ops
// value-side; #764 constexpr-usable function carrier).
// ---------------------------------------------------------------------------
TEST_CASE("Function algebra: pointwise + and scalar · on arrows are constexpr",
          "[geometry][function][functionspace]") {
  constexpr auto f = arrow<int, int>([](const int& x) { return x + 1; });
  constexpr auto g = arrow<int, int>([](const int& x) { return 2 * x; });

  static_assert(pointwise_add(f, g)(3) == 10, "(3+1) + (2*3) = 10");
  static_assert(scale(5, f)(3) == 20, "5 * (3+1) = 20");

  CHECK(pointwise_add(f, g)(4) == 13);
  CHECK(scale(3, g)(4) == 24);
}

// ---------------------------------------------------------------------------
// The Reynolds machinery, field-free: the orbit sum Σ_g f∘g over the ℤ₂
// parity group is the DOUBLED even part.  (The normalised ½-projection needs
// a field for the ½; that witness is the ζ₈ → cos exhibit over ℂ/ℚ(√2) in
// analysis/function_symmetrize_test.cpp.)
// ---------------------------------------------------------------------------
TEST_CASE("Symmetrization: the ℤ₂ orbit sum is the doubled even part",
          "[geometry][function][symmetrize]") {
  // f(x) = x² + 3x + 1.  f(x) + f(−x) = 2x² + 2 (the odd 3x term cancels).
  constexpr auto f =
      arrow<int, int>([](const int& x) { return x * x + 3 * x + 1; });

  static_assert(orbit_sum(f, id_int, neg)(3) == 2 * 9 + 2,
                "orbit sum 2(x²+1) at x=3 is 20 (odd part cancels)");
  CHECK(orbit_sum(f, id_int, neg)(5) == 2 * 25 + 2);
  // Inversion-invariant (lands in the even subalgebra):
  CHECK(orbit_sum(f, id_int, neg)(4) == orbit_sum(f, id_int, neg)(-4));
}
