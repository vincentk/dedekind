"""Showcase 9 (Dual companion): 2D LP across the bridge on a
``Dual<double>`` carrier — primal AND first-order sensitivity from
one runtime call.

Companion to:
  * ``src/test/cpp/modules/dedekind/python/showcase_09_lp_vertex_typed_constant.cpp``
    — NTTP-driven, ℚ-exact, optimum returned as a typed constant ``Vec2<Rat, 2, 2>``.
  * ``src/test/python/showcase_09_lp_runtime.py``
    — runtime, ``double`` carrier, primal-only.
  * THIS file
    — runtime, ``Dual<double>`` carrier, primal + ∂/∂ε.

The kernel ``detail::maximize_impl<T>`` is template-parametric over the
carrier ``T``.  Three call sites exercise three carriers; the kernel
itself is one ``constexpr`` function.  The two-way bridge is therefore
also a *carrier* bridge: the algebraic structure of ``T`` rides through
the active-set enumeration untouched, so swapping ``T = double`` for
``T = Dual<double>`` lifts plain LP into forward-mode automatic
differentiation with no separate derivative pass.

Polytope (same as the §5 paper exhibit), with H1's bound perturbed by ε:

    maximize  3*x + 2*y
    subject to   x +  y <= 4 + ε   (H1')
                2*x + y <= 6        (H2)
                -x      <= 0        (H3:  x >= 0)
                     -y <= 0        (H4:  y >= 0)

Active set at the optimum: {H1', H2}.  Solving the perturbed system:
    x* = 2 - ε,    y* = 2 + 2ε
so the chain rule has already run inside the Cramer solve and the
result reads out as Dual values: primal (2, 2) with tangents (-1, +2).
"""

import dedekind


def main() -> None:
    D = dedekind.Dual

    objective = (D(3.0, 0.0), D(2.0, 0.0))
    halfspaces = [
        (D(1.0, 0.0), D(1.0, 0.0), D(4.0, 1.0)),    # H1', bound + ε
        (D(2.0, 0.0), D(1.0, 0.0), D(6.0, 0.0)),    # H2
        (D(-1.0, 0.0), D(0.0, 0.0), D(0.0, 0.0)),   # H3
        (D(0.0, 0.0), D(-1.0, 0.0), D(0.0, 0.0)),   # H4
    ]

    x, y, feasible = dedekind.maximize_lp_dual(objective, halfspaces)

    print(f"carrier:     Dual<double>")
    print(f"polytope:    {len(halfspaces)} halfspaces in 2D (H1's bound + ε)")
    print(f"feasible:    {feasible}")
    print(f"x* = {x.value()} + ({x.derivative()})·ε   "
          f"(primal {x.value()}, tangent {x.derivative()})")
    print(f"y* = {y.value()} + ({y.derivative()})·ε   "
          f"(primal {y.value()}, tangent {y.derivative()})")
    obj_val = objective[0].value() * x.value() + objective[1].value() * y.value()
    obj_der = (objective[0].value() * x.derivative()
               + objective[1].value() * y.derivative())
    print(f"objective:   {obj_val} + ({obj_der})·ε")


if __name__ == "__main__":
    main()
