"""Reproduce the README set-expression exhibit (showcase 4) from Python.

The C++ README shows { x∈ℕ | x>3 } ∩ { x∈ℕ | x<5 } collapsing to {4} at compile
time.  Here we evaluate the SAME expression at runtime through the Python
mirror, in blackboard-math notation, proving dedekind is not compile-time-only.
(The compile-time Singleton<4> *type* collapse stays C++-only, by the phase
wall; what crosses is the value-level math: membership + extension.)  #886.
"""

import unittest

from dedekind.lwv import χ, ext, ℕ


class ReadmeExhibitTest(unittest.TestCase):
    def test_showcase_4_membership(self) -> None:
        gt3 = ℕ | (χ > 3)  # { x ∈ ℕ | x > 3 }
        lt5 = ℕ | (χ < 5)  # { x ∈ ℕ | x < 5 }
        mid = gt3 & lt5  # (3,5) over ℕ  ≡ {4}
        self.assertIn(4, mid)
        for n in (0, 3, 5, 6, 100):
            self.assertNotIn(n, mid)

    def test_listing5_ext(self) -> None:
        gt3 = ℕ | (χ > 3)
        self.assertEqual(ext({2, 3, 4, 5, 6}, gt3), {4, 5, 6})
        mid = (ℕ | (χ > 3)) & (ℕ | (χ < 5))
        self.assertEqual(ext({2, 3, 4, 5, 6}, mid), {4})  # the exhibit, extended

    def test_complement_and_union(self) -> None:
        gt3 = ℕ | (χ > 3)
        lt5 = ℕ | (χ < 5)
        self.assertIn(2, ~gt3)  # 2 ∉ {x>3}
        self.assertNotIn(4, ~gt3)
        everywhere = gt3 | lt5  # {x>3} ∪ {x<5} = ℕ
        for n in (0, 4, 100):
            self.assertIn(n, everywhere)

    def test_membership_is_callable_chi(self) -> None:
        # x ∈ S and χ_S(x) agree (the container / predicate duality).
        s = ℕ | (χ > 3)
        for n in (2, 4, 10):
            self.assertEqual(n in s, s(n))


if __name__ == "__main__":
    unittest.main()
