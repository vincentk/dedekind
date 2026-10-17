"""Relations as terms (#1037): a composite or a projection is held as a span,
its middle a hidden column bound by ∃, and ``canonical`` takes its image."""

import unittest

from dedekind.pst import B, U64, ap, canonical, cond, π_1, π_2

succ = U64 * U64 | (π_2 == π_1 + 1)


class Composition(unittest.TestCase):
    def test_a_composite_is_a_span_until_canonical(self) -> None:
        two = succ >> succ
        self.assertEqual(repr(two), "𝔸(u64) × 𝔸(u64) | ∃π_3 ∈ 𝔸(u64). "
                                    "(π_3 == (π_1 + 1)) & (π_2 == (π_3 + 1))")
        self.assertIn((3, 5), two)
        self.assertNotIn((3, 6), two)

    def test_a_defined_middle_is_eliminated_by_substitution(self) -> None:
        two = succ >> succ
        self.assertIs(two == (U64 * U64 | (π_2 == π_1 + 2)), True)
        self.assertNotIn("∃", repr(canonical(two)))
        self.assertIs(two.is_function, True)  # the fusion law
        self.assertEqual(ap(two, 3), 5)

    def test_a_middle_on_a_small_carrier_is_expanded(self) -> None:
        # ∃b ∈ 𝔹. a > 5 ∧ c = (1 if b else 0): Shannon's expansion.
        R = U64 * B | (π_1 > 5)
        S = B * U64 | (π_2 == cond(π_1 == True, 1, 0))  # noqa: E712
        RS = R >> S
        self.assertIn((7, 1), RS)
        self.assertIn((7, 0), RS)
        self.assertNotIn((7, 2), RS)
        self.assertNotIn((3, 1), RS)
        self.assertNotIn("∃", repr(canonical(RS)))

    def test_an_undefined_middle_on_a_word_is_refused(self) -> None:
        # ∃b. b² = a ∧ b³ = c: no leg determines b, and 2⁶⁴ is too many to expand.
        sq = U64 * U64 | (π_2 * π_2 == π_1)
        cube = U64 * U64 | (π_1 * π_1 * π_1 == π_2)
        sc = sq >> cube
        self.assertIn("∃π_3 ∈ 𝔸(u64)", repr(sc))
        with self.assertRaises(TypeError):
            canonical(sc)
        with self.assertRaises(TypeError):
            (4, 8) in sc  # the bounded search is the next slice
        self.assertIs(sc.is_function, False)

    def test_the_middle_columns_agree(self) -> None:
        with self.assertRaises(TypeError):
            (U64 * B) >> (U64 * B)  # 𝔹 is not the word


class Projection(unittest.TestCase):
    def test_a_function_projects_onto_its_whole_domain(self) -> None:
        dom = succ.project(1)
        self.assertEqual(repr(dom), "𝔸(u64) | ∃π_2 ∈ 𝔸(u64). π_2 == (π_1 + 1)")
        self.assertEqual(repr(canonical(dom)), "𝔸(u64)")  # entire
        self.assertIn((5,), dom)

    def test_projection_reorders_and_keeps(self) -> None:
        flipped = succ.project(2, 1)  # the converse
        self.assertIn((4, 3), flipped)
        self.assertNotIn((3, 4), flipped)
        with self.assertRaises(TypeError):
            succ.project(1, 1)

    def test_spans_compose_with_products_and_the_lattice(self) -> None:
        dom = succ.project(1)
        self.assertIn((5, True), dom * B)
        same = dom & (U64 * U64 | (π_2 == π_1)).project(1)
        self.assertIn((5,), same)
        with self.assertRaises(TypeError):
            ~dom  # a hidden column under ~ is a ∀: the next slice


if __name__ == "__main__":
    unittest.main()
