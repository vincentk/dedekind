#!/usr/bin/env python3
"""Generate/check the checked-in LLVM IR fixture for pruning demo.

The fixture is source-controlled on purpose. Any optimizer drift requires an
explicit, reviewable update to the checked-in `.ll` file.
"""

from __future__ import annotations

import argparse
import difflib
import os
import re
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
SOURCE = ROOT / "examples" / "set-pruning-ir-demo" / "pruning_noop_vs_runtime_fixture.cpp"
FIXTURE = ROOT / "examples" / "set-pruning-ir-demo" / "pruning_noop_vs_runtime_fixture.ll"

TARGET_TRIPLE = "x86_64-unknown-linux-gnu"


def find_compiler() -> str:
    candidates = [
        os.environ.get("CXX"),
        os.environ.get("CLANGXX"),
        "clang++-22",
        "clang++",
    ]
    for c in candidates:
        if not c:
            continue
        try:
            subprocess.run([c, "--version"], check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
            return c
        except (OSError, subprocess.CalledProcessError):
            continue
    raise RuntimeError("No usable clang++ compiler found (tried CXX/CLANGXX/clang++-22/clang++).")


def generate_ir(compiler: str) -> str:
    cmd = [
        compiler,
        "-std=c++23",
        "-O2",
        "-S",
        "-emit-llvm",
        "-target",
        TARGET_TRIPLE,
        str(SOURCE),
        "-o",
        "-",
    ]
    proc = subprocess.run(cmd, check=True, capture_output=True, text=True)
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
        raise AssertionError("Expected compile-time no-op path to return constant false in IR.")
    if "@pruning_runtime_guard" in ir_text and "and i1" not in ir_text:
        raise AssertionError("Expected runtime guard path to retain runtime conjunction in IR.")


def refresh() -> int:
    compiler = find_compiler()
    ir_text = generate_ir(compiler)
    semantic_sanity(ir_text)
    FIXTURE.write_text(ir_text, encoding="utf-8")
    print(f"Refreshed fixture: {FIXTURE}")
    print(f"Compiler: {compiler}")
    print(f"Target triple: {TARGET_TRIPLE}")
    return 0


def check() -> int:
    compiler = find_compiler()
    actual = generate_ir(compiler)
    semantic_sanity(actual)

    if not FIXTURE.exists():
        print(f"Missing fixture file: {FIXTURE}", file=sys.stderr)
        print("Run: make ir-fixture-refresh", file=sys.stderr)
        return 2

    expected = FIXTURE.read_text(encoding="utf-8")
    if actual == expected:
        print("IR fixture check passed.")
        print(f"Compiler: {compiler}")
        print(f"Target triple: {TARGET_TRIPLE}")
        return 0

    print("IR fixture mismatch detected.", file=sys.stderr)
    print(f"Compiler: {compiler}", file=sys.stderr)
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
