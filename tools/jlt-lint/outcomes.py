"""Compare the second stage's outcome for every template of a fixture with
the outcomes the fixture expects (FILE.expected beside it, in source order).

    python3 tools/jlt-lint/outcomes.py TOOL FIXTURE [EXTRA COMPILER ARGS]
"""
import pathlib
import re
import subprocess
import sys

HERE = pathlib.Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import jlt_lint  # noqa: E402


def main():
    tool, fixture = sys.argv[1], pathlib.Path(sys.argv[2]).resolve()
    args = [tool, "--scope=jlt_listing", f"--archetypes={HERE / 'archetypes.hpp'}",
            "--check=admit,admitted", str(fixture), "--", "-std=c++23", "-xc++",
            *jlt_lint.RESOURCE_DIR, *sys.argv[3:]]
    out = subprocess.run(args, capture_output=True, text=True).stdout
    got = []
    for l in out.splitlines():
        m = re.match(r".*?:(\d+):\d+: (\S+) (admitted by type|not admitted|undecided)", l)
        if m:
            got.append((int(m[1]), m[2], m[3].replace(" by type", "")))
    got = [(n, k) for _, n, k in sorted(got)]
    want = []
    for l in fixture.with_suffix(".expected").read_text().splitlines():
        if l.strip() and not l.startswith("#"):
            name, kind = l.split(" ", 1)
            want.append((name, kind))
    if got != want:
        sys.exit(f"outcomes.py: {fixture.name}\n  expected {want}\n  got      {got}")
    print(f"outcomes.py: {fixture.name}: {len(got)} outcomes as expected")


if __name__ == "__main__":
    main()
