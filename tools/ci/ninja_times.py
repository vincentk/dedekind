"""Where the build's time goes, from ninja's own log.

    python3 tools/ci/ninja_times.py build/.ninja_log [--skip N]

Ninja writes one line per edge it ran: start and end in milliseconds since the
build began, the output, and a hash.  --skip N leaves out the first N lines, those
a restored cache brought in, so that only this run's edges count.  The summary is
Markdown, for the run's page ($GITHUB_STEP_SUMMARY): the wall time and the time
summed over the workers, the time by part (each layer's library and tests, the
Python bindings, the linter's tool, the lint), and the slowest edges.
"""
import collections
import re
import sys


def part(output):
    if m := re.search(r"CMakeFiles/dedekind_([a-z_]+)\.dir/", output):
        return f"library: {m.group(1)}"
    if m := re.search(r"CMakeFiles/test_([a-z_]+)\.dir/", output):
        return f"tests: {m.group(1)}"
    if "CMakeFiles/dedekind.dir/" in output:
        return "library: umbrella"
    if "jlt-lint/CMakeFiles/lint" in output:
        return "the lint"
    if "jlt-lint" in output:
        return "the linter's tool"
    if "python" in output or "_jlt" in output or "_pst" in output or "nanobind" in output:
        return "Python bindings"
    if "@synth" in output or "cmake_cxx_std" in output:
        return "the standard library module"
    if output.endswith(".ddi") or output.endswith(".dd") or "CXX.dd" in output:
        return "module scanning"
    return "other"


def main():
    args = sys.argv[1:]
    skip = 0
    if "--skip" in args:
        i = args.index("--skip")
        skip = int(args[i + 1])
        del args[i:i + 2]
    if len(args) != 1:
        sys.exit(__doc__)
    edges = []
    for n, line in enumerate(open(args[0], errors="replace")):
        if n < skip or line.startswith("#"):
            continue
        fields = line.rstrip("\n").split("\t")
        if len(fields) < 4:
            continue
        start, end, output = int(fields[0]), int(fields[1]), fields[3]
        edges.append((start, end, output))
    if not edges:
        print("No edges ran: the build was up to date.")
        return
    wall = (max(e for _, e, _ in edges) - min(s for s, _, _ in edges)) / 60000
    summed = sum(e - s for s, e, _ in edges) / 60000
    by = collections.Counter()
    count = collections.Counter()
    for s, e, o in edges:
        by[part(o)] += e - s
        count[part(o)] += 1
    print(f"**Build:** {len(edges)} edges, {wall:.1f} min wall, {summed:.1f} min summed over the workers.\n")
    print("| part | edges | minutes, summed |\n|---|---|---|")
    for p, ms in by.most_common():
        print(f"| {p} | {count[p]} | {ms / 60000:.1f} |")
    print("\n| slowest edges | seconds |\n|---|---|")
    for s, e, o in sorted(edges, key=lambda x: x[0] - x[1])[:15]:
        print(f"| `{o[-90:]}` | {(e - s) / 1000:.0f} |")


if __name__ == "__main__":
    main()
