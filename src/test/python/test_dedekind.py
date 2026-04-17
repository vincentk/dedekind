import unittest

import dedekind


class DedekindPythonSmokeTest(unittest.TestCase):
    def test_ordered_set_roundtrip(self) -> None:
        self.assertEqual(dedekind.ordered_set_roundtrip([3, 1, 2, 2]), [1, 2, 3])

    def test_unordered_set_roundtrip(self) -> None:
        self.assertEqual(
            dedekind.unordered_set_roundtrip([4, 2, 4, 1]),
            [1, 2, 4],
        )

    def test_path_from_range(self) -> None:
        self.assertEqual(dedekind.path_from_range([2, 4, 6, 8]), [2, 4, 6, 8])


if __name__ == "__main__":
    unittest.main()