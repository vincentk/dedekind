"""The choice of scan, tested: shallow and deep give the same findings.

The rules walk the declarations written in the file (--scan=shallow, the
default), and a rule whose evidence leaves the file follows it there.  This
runs every rule file's matchers and every check but the second stage on every
fixture in both modes and requires identical output.  rule3-mutual.cpp holds a
cycle through std::invoke, so the comparison covers evidence outside the
file.

    python3 tools/jlt-lint/scan.py <tool> [extra clang arguments]
"""
import json
import pathlib
import subprocess
import sys

HERE = pathlib.Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import jlt_lint  # noqa: E402


def main():
    if len(sys.argv) < 2:
        sys.exit(__doc__)
    tool = sys.argv[1]
    db = pathlib.Path(tool).resolve().parent.parent / "compile_commands.json"
    if db.exists():
        jlt_lint.set_resource_dir(json.loads(db.read_text()))
    queries, checks = [], []
    for q in sorted((HERE / "rules").glob("*.query")):
        tools = [l.split()[1] for l in q.read_text().splitlines() if l.startswith("tool ")]
        (checks.extend if tools else lambda _: queries.append(q))(tools)
    # The second stage compiles the file again per template, whatever the
    # scan; its own walk finds the same templates either way.  Left out, for
    # time.
    checks = [c for c in checks if c not in ("admit", "undecided", "admitted")]
    differ = []
    fixtures = sorted((HERE / "fixtures").glob("*.cpp"))
    for fixture in fixtures:
        out = {}
        for mode in ("shallow", "deep"):
            args = [tool, "--scope=jlt_listing", f"--scan={mode}",
                    f"--query={','.join(str(q) for q in queries)}",
                    f"--check={','.join(checks)}", str(fixture),
                    "--", "-std=c++23", "-xc++", *jlt_lint.RESOURCE_DIR, *sys.argv[2:]]
            run = subprocess.run(args, capture_output=True, text=True)
            if run.returncode != 0:
                sys.exit(f"scan.py: {fixture.name}, --scan={mode}: the tool failed, exit "
                         f"{run.returncode}\n" + run.stderr[:2000])
            out[mode] = run.stdout
        if out["shallow"] != out["deep"]:
            differ.append(fixture.name)
    if differ:
        sys.exit("scan.py: shallow and deep scans differ on " + ", ".join(differ) +
                 ": a rule depends on what lies outside the file; see README, "
                 "Choices the linter makes")
    print(f"scan.py: shallow and deep scans agree on {len(fixtures)} fixtures, "
          f"{len(queries)} rule files and {len(checks)} checks")


if __name__ == "__main__":
    main()
