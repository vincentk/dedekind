"""Showcase 9 (Dual<Rational> companion): 2D LP across the bridge on the
*same* carrier the compile-time NTTP exhibit uses — exact ℚ in both the
primal and the tangent.

Carrier ladder over one kernel:
  * ``showcase_09_lp_vertex_typed_constant.cpp``
    — NTTP-driven, ℚ-exact, optimum returned as ``Vec2<Rat, 2, 2>``.
  * ``showcase_09_lp_runtime.py``
    — runtime, ``double`` carrier, primal-only.
  * ``showcase_09_lp_runtime_dual.py``
    — runtime, ``Dual<double>`` carrier, primal + ∂/∂ε (floating-point).
  * THIS file
    — runtime, ``Dual<Rational>`` carrier, primal + ∂/∂ε (ℚ-exact).
    Maximum overlap with the compile-time exhibit: the Python call
    instantiates ``detail::maximize_impl`` at the identical template
    specialisation as the C++ ``[dual][sensitivity]`` test.

Polytope (same as §5), with H1's bound perturbed by ε:

    maximize  3*x + 2*y
    subject to   x +  y <= 4 + ε   (H1')
                2*x + y <= 6        (H2)
                -x      <= 0        (H3:  x >= 0)
                     -y <= 0        (H4:  y >= 0)

Active set at the optimum: {H1', H2}.  Solving the perturbed system:
    x* = 2 - ε,    y* = 2 + 2ε
both ℚ-exact: primal 2/1 with tangent -1/1 for x; primal 2/1 with
tangent 2/1 for y.  The chain rule has already run through the Cramer
solve, all in exact rational arithmetic.
"""

import dedekind


def main() -> None:
    D = dedekind.DualRational
    R = dedekind.Rational

    objective = (D(3, 0), D(2, 0))
    halfspaces = [
        (D(1, 0), D(1, 0), D(R(4), R(1))),  # H1',  x +  y <= 4 + ε
        (D(2, 0), D(1, 0), D(6, 0)),         # H2,  2x +  y <= 6
        (D(-1, 0), D(0, 0), D(0, 0)),        # H3,  x >= 0
        (D(0, 0), D(-1, 0), D(0, 0)),        # H4,       y >= 0
    ]

    x, y, feasible = dedekind.maximize_lp_dual_rational(objective, halfspaces)

    print(f"carrier:     Dual<Rational<SignedExtensionalCardinal<>>>")
    print(f"             (same as the compile-time NTTP exhibit)")
    print(f"polytope:    {len(halfspaces)} halfspaces in 2D (H1's bound + ε)")
    print(f"feasible:    {feasible}")
    print(f"x* = {x.value().num()}/{x.value().den()}"
          f" + ({x.derivative().num()}/{x.derivative().den()})·ε")
    print(f"y* = {y.value().num()}/{y.value().den()}"
          f" + ({y.derivative().num()}/{y.derivative().den()})·ε")
    # Objective value: cx*x + cy*y; tangent via envelope theorem.
    obj_val = objective[0].value() * x.value() + objective[1].value() * y.value()
    obj_der = (objective[0].value() * x.derivative()
               + objective[1].value() * y.derivative())
    print(f"objective:   {obj_val.num()}/{obj_val.den()}"
          f" + ({obj_der.num()}/{obj_der.den()})·ε")


if __name__ == "__main__":
    main()
