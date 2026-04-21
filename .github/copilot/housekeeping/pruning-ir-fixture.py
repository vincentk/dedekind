#!/usr/bin/env python3
"""Generate/check the checked-in LLVM IR fixture for pruning demo.

The fixture is source-controlled on purpose. Any optimizer drift requires an
explicit, reviewable update to the checked-in `.ll` file.
"""

from __future__ import annotations

import argparse
import difflib
import json
import re
import shlex
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
_PYTHON_DIR = ROOT / "src" / "test" / "cpp" / "modules" / "dedekind" / "python"

# Each entry: (source file, checked-in fixture file, semantic checker)
SOURCES = [
    (
        _PYTHON_DIR / "pruning_noop_vs_runtime_fixture.cpp",
        _PYTHON_DIR / "pruning_noop_vs_runtime_fixture.ll",
    ),
    (
        _PYTHON_DIR / "showcase_01_diagonal_contradiction.cpp",
        _PYTHON_DIR / "showcase_01_diagonal_contradiction.ll",
    ),
    (
        _PYTHON_DIR / "showcase_02_lattice_singleton.cpp",
        _PYTHON_DIR / "showcase_02_lattice_singleton.ll",
    ),
]

# Keep single-source aliases for backward compatibility with any callers.
SOURCE = SOURCES[0][0]
FIXTURE = SOURCES[0][1]

BUILD_DIR = ROOT / "build"

TARGET_TRIPLE = "x86_64-unknown-linux-gnu"


def ensure_cmake_target() -> None:
    """Build the IR fixture cmake target so module BMIs are up-to-date."""
    if not BUILD_DIR.exists():
        raise RuntimeError(
            f"Build directory not found: {BUILD_DIR}\n"
            "Configure first: cmake -B build ..."
        )
    subprocess.run(
        ["cmake", "--build", str(BUILD_DIR), "--target", "set-pruning-ir-fixture"],
        check=True,
        stdout=subprocess.DEVNULL,
    )


def generate_ir(source: Path) -> str:
    """Re-compile *source* with -S -emit-llvm using the cmake-recorded flags."""
    db_path = BUILD_DIR / "compile_commands.json"
    if not db_path.exists():
        raise RuntimeError(
            f"compile_commands.json not found at {db_path}.\n"
            "Run cmake --build build/ first."
        )

    db = json.loads(db_path.read_text(encoding="utf-8"))
    entry = next(
        (e for e in db if Path(e["file"]).name == source.name),
        None,
    )
    if entry is None:
        raise RuntimeError(
            f"No compile command for {source.name} in compile_commands.json.\n"
            "Ensure set-pruning-ir-fixture has been built."
        )

    # Parse the recorded command and adapt it for IR emission.
    raw_cmd = shlex.split(entry["command"])
    adapted: list[str] = []
    skip_next = False
    for tok in raw_cmd:
        if skip_next:
            skip_next = False
            continue
        if tok == "-o":
            skip_next = True
            continue
        if tok.startswith("-o"):
            continue
        if tok == "-c":
            continue
        adapted.append(tok)
    adapted += ["-S", "-emit-llvm", "-o", "-"]

    # Compile from the recorded build directory so relative @response-file
    # paths (module maps) resolve exactly as in compile_commands.json.
    proc = subprocess.run(
        adapted,
        check=True,
        capture_output=True,
        text=True,
        cwd=entry["directory"],
    )
    return normalize_ir(proc.stdout)


def normalize_ir(ir_text: str) -> str:
    lines = ir_text.splitlines()
    normalized: list[str] = []
    for line in lines:
      if line.startswith("; ModuleID ="):
          continue
      if line.startswith("source_filename ="):
          continue
      if line.startswith("!llvm.ident ="):
          continue
      if re.match(r"^!\d+ = !\{!\".*clang version.*\"\}$", line):
          continue
      normalized.append(line)
    return "\n".join(normalized).rstrip() + "\n"


def semantic_sanity(ir_text: str, source: Path) -> None:
    name = source.name
    if "pruning_noop_vs_runtime_fixture" in name:
        if "@pruning_compile_time_noop" not in ir_text:
            raise AssertionError("IR missing pruning_compile_time_noop symbol.")
        if "@pruning_runtime_guard" not in ir_text:
            raise AssertionError("IR missing pruning_runtime_guard symbol.")
        if "ret i1 false" not in ir_text:
            raise AssertionError(
                "Expected contradictory compile-time predicates {false}∩{true}≡∅ "
                "to collapse to a constant `ret i1 false` in IR "
                "(semantic proof is also covered by static_assert in source)."
            )
        # One predicate remains unknown at compile time, so the runtime path must
        # retain a call rather than collapsing to an inlined constant function.
        if "call" not in ir_text:
            raise AssertionError(
                "Expected runtime guard to retain a runtime call instruction in IR."
            )
    elif "showcase_01_diagonal_contradiction" in name:
        if "@impress_empty_diagonal_cut" not in ir_text:
            raise AssertionError("IR missing impress_empty_diagonal_cut symbol.")
        if "ret i1 false" not in ir_text:
            raise AssertionError(
                "Expected diagonal contradiction to collapse to `ret i1 false` in IR."
            )
    elif "showcase_02_lattice_singleton" in name:
        if "@impress_lattice_square_singleton" not in ir_text:
            raise AssertionError(
                "IR missing impress_lattice_square_singleton symbol."
            )
        if "ret i1 true" not in ir_text:
            raise AssertionError(
                "Expected lattice singleton witness to collapse to `ret i1 true` in IR."
            )
    else:
        raise AssertionError(f"No semantic checks defined for {name}.")


def refresh() -> int:
    ensure_cmake_target()
    for source, fixture in SOURCES:
        ir_text = generate_ir(source)
        semantic_sanity(ir_text, source)
        fixture.write_text(ir_text, encoding="utf-8")
        print(f"Refreshed fixture: {fixture}")
    print(f"Target triple: {TARGET_TRIPLE}")
    return 0


def check() -> int:
    ensure_cmake_target()
    failed = False
    for source, fixture in SOURCES:
        actual = generate_ir(source)
        semantic_sanity(actual, source)

        if not fixture.exists():
            print(f"Missing fixture file: {fixture}", file=sys.stderr)
            print("Run: make ir-fixture-refresh", file=sys.stderr)
            failed = True
            continue

        expected = fixture.read_text(encoding="utf-8")
        if actual == expected:
            print(f"IR fixture check passed: {fixture.name}")
            continue

        print(f"IR fixture mismatch detected: {fixture.name}", file=sys.stderr)
        diff = difflib.unified_diff(
            expected.splitlines(),
            actual.splitlines(),
            fromfile=str(fixture),
            tofile="<generated>",
            n=3,
            lineterm="",
        )
        for line in diff:
            print(line, file=sys.stderr)
        failed = True

    if failed:
        print(f"Target triple: {TARGET_TRIPLE}", file=sys.stderr)
        print("\nRun: make ir-fixture-refresh", file=sys.stderr)
        return 1
    print(f"Target triple: {TARGET_TRIPLE}")
    return 0


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("mode", choices=["check", "refresh"], help="Operation mode")
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    if args.mode == "refresh":
        return refresh()
    return check()


if __name__ == "__main__":
    raise SystemExit(main())
