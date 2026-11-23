"""The ratchet: a gate may move one way only.

Compares the scope with the scope of another commit (main, in CI) gate by
gate, for every (rule, file, namespace) both name:

- `admitted` counts certificates: it may grow, not shrink;
- every other gated rule counts findings (refusals, undecided definitions, the
  deny-rules' findings): it may shrink, not grow;
- a gate may not become `report` again.

A gate that moves the wrong way fails, unless tools/jlt-lint/ratchet.allow
names it with a reason, one line `<rule> <file> <reason>`, so that the reason
is in the diff, where a reviewer reads it.  New files, new rules, files no
longer there and the fixtures (negative controls, whose counts change on
purpose and which outcomes.py holds) are not compared.

    python3 tools/jlt-lint/ratchet.py <commit>     e.g. FETCH_HEAD after
                                                   git fetch --depth=1 origin main
"""
import pathlib
import subprocess
import sys

HERE = pathlib.Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import jlt_lint  # noqa: E402


def gates(text):
    return {(r, f, ns): e for r, f, ns, e in jlt_lint.read_scope(text, strict=False)}


def allowed():
    path = HERE / "ratchet.allow"
    if not path.exists():
        return set()
    out = set()
    for line in path.read_text().splitlines():
        line = line.split("#", 1)[0].split(None, 2)
        if len(line) >= 2:
            out.add((line[0], line[1]))
    return out


def main():
    if len(sys.argv) != 2:
        sys.exit(__doc__)
    base = subprocess.run(["git", "show", f"{sys.argv[1]}:tools/jlt-lint/scope"],
                          capture_output=True, text=True, cwd=jlt_lint.ROOT)
    if base.returncode != 0:
        sys.exit(f"ratchet.py: no scope at {sys.argv[1]}: {base.stderr.strip()}")
    before, after, allow = gates(base.stdout), gates(None), allowed()
    worse = []
    for key, now in sorted(after.items()):
        was = before.get(key)
        if was is None or was == now:
            continue
        rule, f, _ = key
        if f.startswith("tools/jlt-lint/fixtures/"):
            continue  # a negative control's count changes on purpose
        if now == "report":
            why = f"{was} -> report"
        elif was == "report":
            continue  # a report becomes a gate: tighter
        elif rule == "admitted" and int(now) < int(was):
            why = f"admitted {was} -> {now}"
        elif rule != "admitted" and int(now) > int(was):
            why = f"{was} -> {now}"
        else:
            continue
        if (rule, f) not in allow:
            worse.append(f"  {rule} {f}: {why}")
    if worse:
        sys.exit("ratchet.py: gates moved the wrong way; fix them, or name each in "
                 "tools/jlt-lint/ratchet.allow with a reason:\n" + "\n".join(worse))
    print(f"ratchet.py: {len(after)} gates, none moved the wrong way since {sys.argv[1]}")


if __name__ == "__main__":
    main()
