"""Stage the ranges library of libc++, as a control beside Catch2: code that
constrains its templates by concepts and was not written under the rules.

The linter judges a file; the standard library's headers are outside the
tree and under a path that depends on the installation.  This copies the
headers that hold the ranges library into a directory under the build, where
the scope names them by a glob as it names Catch2's:

    python3 tools/jlt-lint/stage_stdlib.py CXX OUT

The control is libc++ on every platform, whichever standard library the
build itself uses, so that its row is one row: libc++ ships with the
compiler, which pins its version, and keeps one header for each view, each
of which can be read on its own.  The script asks the compiler where libc++'s
<ranges> is (-stdlib=libc++ -H) and copies the headers of __ranges/.  The
copies are read with the flags of a source file beside them and
-stdlib=libc++ (jlt_lint.header_entry), and include the rest of the library
from where the compiler finds it.
"""
import pathlib
import re
import shutil
import subprocess
import sys

FLAG = "-stdlib=libc++"


def main():
    cxx, out = sys.argv[1], pathlib.Path(sys.argv[2])
    r = subprocess.run([cxx, "-std=c++23", FLAG, "-xc++", "-E", "-H", "-"], input="#include <ranges>\n",
                       capture_output=True, text=True)
    found = [l.lstrip(". ").strip() for l in r.stderr.splitlines() if l.startswith(".")]
    ranges = next((pathlib.Path(p) for p in found if pathlib.Path(p).name == "ranges"), None)
    if ranges is None:
        sys.exit(f"stage_stdlib.py: {cxx} {FLAG} does not say where <ranges> is; is libc++ installed?\n"
                 f"{r.stderr[-800:]}")
    root = ranges.resolve().parent
    if not (root / "__ranges").is_dir():
        sys.exit(f"stage_stdlib.py: {root} is not libc++ (no __ranges/)")
    if out.exists():
        shutil.rmtree(out)
    out.mkdir(parents=True)
    files = {f"__ranges/{f.name}": f for f in sorted((root / "__ranges").glob("*.h"))}
    for name, source in files.items():
        (out / name).parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(source, out / name)
    config = next((root / n for n in ("__config", "__configuration/abi.h") if (root / n).exists()), None)
    version = re.search(r"_LIBCPP_VERSION\s+(\d+)", config.read_text()) if config else None
    print(f"stage_stdlib.py: {len(files)} headers of the ranges library of libc++"
          f"{' ' + version.group(1) if version else ''} from {root} to {out}")


if __name__ == "__main__":
    main()
