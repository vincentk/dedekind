"""Showcase 9 (runtime companion): 2D LP with coefficients supplied from
Python.

Paired with the compile-time showcase
``src/test/cpp/modules/dedekind/python/showcase_09_lp_vertex_typed_constant.cpp``.
Same polytope, same objective, same active-set kernel — but the
coefficients arrive as Python floats rather than C++ NTTPs.  The compile-
time showcase reduces the optimum to a typed constant ``Vec2<Rat, 2, 2>``;
this script reads out ``(2.0, 2.0)`` at run time via the nanobind facade.

    maximize 3*x + 2*y
    subject to  x +  y <= 4        (H1)   non-axis-aligned
                2*x + y <= 6        (H2)   non-axis-aligned
                -x      <= 0        (H3:  x >= 0)
                     -y <= 0        (H4:  y >= 0)
    => optimum at (2, 2), objective value 10.

Both modes route through ``detail::maximize_impl_dynamic`` on the same
active-set enumeration.  Running the two showcases on the same polytope
demonstrates that the choice of compile-time vs. runtime evaluation is a
deployment decision, not a structural change.
"""

import dedekind


# The polytope: a list of (a, b, c) halfspace triples encoding a*x + b*y <= c.
HALFSPACES = [
    (1.0, 1.0, 4.0),    # H1:  x +  y <= 4
    (2.0, 1.0, 6.0),    # H2:  2x + y <= 6
    (-1.0, 0.0, 0.0),   # H3:  x >= 0
    (0.0, -1.0, 0.0),   # H4:  y >= 0
]

# Objective: maximise 3*x + 2*y.
OBJECTIVE = (3.0, 2.0)


def main() -> None:
    x, y, feasible = dedekind.maximize_lp(OBJECTIVE, HALFSPACES)

    print(f"polytope:    {len(HALFSPACES)} halfspaces in 2D")
    print(f"objective:   max {OBJECTIVE[0]}*x + {OBJECTIVE[1]}*y")
    print(f"feasible:    {feasible}")
    print(f"optimum:     (x*, y*) = ({x}, {y})")
    print(f"obj. value:  {OBJECTIVE[0] * x + OBJECTIVE[1] * y}")


if __name__ == "__main__":
    main()
