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
SOURCE = ROOT / "examples" / "set-pruning-ir-demo" / "pruning_noop_vs_runtime_fixture.cpp"
FIXTURE = ROOT / "examples" / "set-pruning-ir-demo" / "pruning_noop_vs_runtime_fixture.ll"
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


def generate_ir() -> str:
    """Re-compile the fixture with -S -emit-llvm using the cmake-recorded flags."""
    db_path = BUILD_DIR / "compile_commands.json"
    if not db_path.exists():
        raise RuntimeError(
            f"compile_commands.json not found at {db_path}.\n"
            "Run cmake --build build/ first."
        )

    db = json.loads(db_path.read_text(encoding="utf-8"))
    entry = next(
        (e for e in db if Path(e["file"]).name == SOURCE.name),
        None,
    )
    if entry is None:
        raise RuntimeError(
            f"No compile command for {SOURCE.name} in compile_commands.json.\n"
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

    proc = subprocess.run(adapted, check=True, capture_output=True, text=True)
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


def semantic_sanity(ir_text: str) -> None:
    if "@pruning_compile_time_noop" not in ir_text:
        raise AssertionError("IR missing pruning_compile_time_noop symbol.")
    if "@pruning_runtime_guard" not in ir_text:
        raise AssertionError("IR missing pruning_runtime_guard symbol.")
    if "ret i1 false" not in ir_text:
        raise AssertionError(
            "Expected compile-time pruned intersection {false}∩{true}≡∅ "
            "to reduce to a constant `ret i1 false` in IR."
        )
    # The runtime guard must retain an indirect call through the function pointer.
    if "call" not in ir_text:
        raise AssertionError(
            "Expected runtime guard to retain an indirect call instruction in IR."
        )


def refresh() -> int:
    ensure_cmake_target()
    ir_text = generate_ir()
    semantic_sanity(ir_text)
    FIXTURE.write_text(ir_text, encoding="utf-8")
    print(f"Refreshed fixture: {FIXTURE}")
    print(f"Target triple: {TARGET_TRIPLE}")
    return 0


def check() -> int:
    ensure_cmake_target()
    actual = generate_ir()
    semantic_sanity(actual)

    if not FIXTURE.exists():
        print(f"Missing fixture file: {FIXTURE}", file=sys.stderr)
        print("Run: make ir-fixture-refresh", file=sys.stderr)
        return 2

    expected = FIXTURE.read_text(encoding="utf-8")
    if actual == expected:
        print("IR fixture check passed.")
        print(f"Target triple: {TARGET_TRIPLE}")
        return 0

    print("IR fixture mismatch detected.", file=sys.stderr)
    print(f"Target triple: {TARGET_TRIPLE}", file=sys.stderr)
    diff = difflib.unified_diff(
        expected.splitlines(),
        actual.splitlines(),
        fromfile=str(FIXTURE),
        tofile="<generated>",
        n=3,
        lineterm="",
    )
    for line in diff:
        print(line, file=sys.stderr)
    print("\nRun: make ir-fixture-refresh", file=sys.stderr)
    return 1


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
