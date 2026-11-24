"""The ratchet: a gate may move one way only.

Compares the scope with the scope of another commit (main, in CI) gate by
gate, for every (rule, file, namespace) both name:

- `admitted` counts certificates: it may grow, not shrink;
- every other gated rule counts findings (refusals, undecided definitions, the
  deny-rules' findings): it may shrink, not grow;
- a gate may not become `report` again, nor disappear while its file stays.

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
        fields = line.split("#", 1)[0].split(None, 2)
        if not fields:
            continue
        if len(fields) < 3 or not fields[2].strip():
            sys.exit(f"ratchet.py: ratchet.allow: an entry without a reason: {line.strip()}\n"
                     "  every entry is <rule> <file> <reason>")
        out.add((fields[0], fields[1]))
    return out


def main():
    if len(sys.argv) != 2:
        sys.exit(__doc__)
    base = subprocess.run(["git", "show", f"{sys.argv[1]}:tools/jlt-lint/scope"],
                          capture_output=True, text=True, cwd=jlt_lint.ROOT)
    if base.returncode != 0:
        sys.exit(f"ratchet.py: no scope at {sys.argv[1]}: {base.stderr.strip()}")
    # The baseline's globs are expanded against this checkout, so a file the
    # baseline did not have would get a gate it never had: keep only the
    # baseline's own files (Copilot on #1213).
    tree = subprocess.run(["git", "ls-tree", "-r", "--name-only", sys.argv[1]],
                          capture_output=True, text=True, cwd=jlt_lint.ROOT)
    if tree.returncode != 0:
        sys.exit(f"ratchet.py: no tree at {sys.argv[1]}: {tree.stderr.strip()}")
    files = set(tree.stdout.splitlines())
    before = {k: v for k, v in gates(base.stdout).items() if k[1] in files}
    after, allow = gates(None), allowed()
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
    # A gate main has and this scope no longer has, for a file that is still
    # there: its rule stopped running on the file, the most quiet way to
    # loosen (a deleted line, a renamed glob or namespace).
    for key, was in sorted(before.items()):
        rule, f, _ = key
        if key in after or f.startswith("tools/jlt-lint/fixtures/"):
            continue
        if was != "report" and (rule, f) not in allow:
            worse.append(f"  {rule} {f}: the gate {was} is gone")
    if worse:
        sys.exit("ratchet.py: gates moved the wrong way; fix them, or name each in "
                 "tools/jlt-lint/ratchet.allow with a reason:\n" + "\n".join(worse))
    print(f"ratchet.py: {len(after)} gates, none moved the wrong way since {sys.argv[1]}")


if __name__ == "__main__":
    main()
