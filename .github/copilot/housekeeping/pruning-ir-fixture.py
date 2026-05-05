#!/usr/bin/env python3
"""Generate/check the checked-in LLVM IR fixture for pruning demo.

The fixture is source-controlled on purpose. Any optimizer drift requires an
explicit, reviewable update to the checked-in `.ll` file.
"""

from __future__ import annotations

import argparse
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
    (
        _PYTHON_DIR / "showcase_03_halfspace_contradiction.cpp",
        _PYTHON_DIR / "showcase_03_halfspace_contradiction.ll",
    ),
    (
        _PYTHON_DIR / "showcase_05_halfspace_real_ambient.cpp",
        _PYTHON_DIR / "showcase_05_halfspace_real_ambient.ll",
    ),
    (
        _PYTHON_DIR / "showcase_06_halfspace_interval_42.cpp",
        _PYTHON_DIR / "showcase_06_halfspace_interval_42.ll",
    ),
    (
        _PYTHON_DIR / "showcase_07_lattice_real_interval.cpp",
        _PYTHON_DIR / "showcase_07_lattice_real_interval.ll",
    ),
    (
        _PYTHON_DIR / "showcase_08_halfspace_2d_product.cpp",
        _PYTHON_DIR / "showcase_08_halfspace_2d_product.ll",
    ),
    (
        _PYTHON_DIR / "showcase_09_lp_vertex_typed_constant.cpp",
        _PYTHON_DIR / "showcase_09_lp_vertex_typed_constant.ll",
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
    # Coverage instrumentation flags prevent constant-folding; strip them so
    # the optimizer can collapse compile-time predicates as expected.
    _COVERAGE_FLAGS = {"-fprofile-instr-generate", "-fcoverage-mapping",
                       "-fprofile-generate", "--coverage", "-fprofile-arcs",
                       "-ftest-coverage"}
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
        if tok in _COVERAGE_FLAGS or tok.startswith("-fprofile-instr-generate="):
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
        if line.startswith("target datalayout ="):
            continue
        if line.startswith("target triple ="):
            continue
        if line.startswith("!llvm.ident ="):
            continue
        if re.match(r"^!\d+ = !\{!\".*clang version.*\"\}$", line):
            continue
        normalized.append(line)
    return "\n".join(normalized).rstrip() + "\n"


def extract_function_block(ir_text: str, symbol: str) -> str | None:
    pattern = re.compile(
        rf"(?ms)^define\b.*@{re.escape(symbol)}\b.*?\n\}}"
    )
    match = pattern.search(ir_text)
    return match.group(0) if match else None


def has_indirect_call(function_ir: str) -> bool:
    return re.search(r"(?m)\bcall\b[^\n@]*%[-a-zA-Z$._0-9]+", function_ir) is not None


def semantic_sanity(ir_text: str, source: Path) -> None:
    name = source.name
    if "pruning_noop_vs_runtime_fixture" in name:
        noop_block = extract_function_block(ir_text, "pruning_compile_time_noop")
        if noop_block is None:
            raise AssertionError("IR missing pruning_compile_time_noop symbol.")
        runtime_block = extract_function_block(ir_text, "pruning_runtime_guard")
        if runtime_block is None:
            raise AssertionError("IR missing pruning_runtime_guard symbol.")
        if "ret i1 false" not in noop_block:
            raise AssertionError(
                "Expected contradictory compile-time predicates {false}∩{true}≡∅ "
                "to collapse to a constant `ret i1 false` in IR "
                "(semantic proof is also covered by static_assert in source)."
            )
        # One predicate remains unknown at compile time, so the runtime path must
        # retain an indirect call through the function pointer.
        if not has_indirect_call(runtime_block):
            raise AssertionError(
                "Expected runtime guard to retain an indirect call instruction in IR."
            )
    elif "showcase_01_diagonal_contradiction" in name:
        block = extract_function_block(ir_text, "witness_empty_diagonal_cut")
        if block is None:
            raise AssertionError("IR missing witness_empty_diagonal_cut symbol.")
        if "ret i1 false" not in block:
            raise AssertionError(
                "Expected diagonal contradiction to collapse to `ret i1 false` in IR."
            )
    elif "showcase_02_lattice_singleton" in name:
        block = extract_function_block(ir_text, "witness_lattice_square_singleton")
        if block is None:
            raise AssertionError(
                "IR missing witness_lattice_square_singleton symbol."
            )
        if "ret i1 true" not in block:
            raise AssertionError(
                "Expected lattice singleton witness to collapse to `ret i1 true` in IR."
            )
    elif "showcase_03_halfspace_contradiction" in name:
        block = extract_function_block(ir_text, "witness_empty_halfspace_meet")
        if block is None:
            raise AssertionError("IR missing witness_empty_halfspace_meet symbol.")
        if "ret i1 false" not in block:
            raise AssertionError(
                "Expected halfspace contradiction (x > 5) ∧ (x < 3) on ℕ to "
                "collapse to `ret i1 false` in IR."
            )
    elif "showcase_05_halfspace_real_ambient" in name:
        block = extract_function_block(ir_text, "witness_real_halfspace_empty")
        if block is None:
            raise AssertionError("IR missing witness_real_halfspace_empty symbol.")
        if "ret i1 false" not in block:
            raise AssertionError(
                "Expected halfspace contradiction on ℝ to collapse to "
                "`ret i1 false` in IR."
            )
    elif "showcase_06_halfspace_interval_42" in name:
        block = extract_function_block(ir_text, "witness_interval_42_member")
        if block is None:
            raise AssertionError("IR missing witness_interval_42_member symbol.")
        if "ret i1 true" not in block:
            raise AssertionError(
                "Expected membership query at 0 in (-21, 21] on ℤ to collapse "
                "to `ret i1 true` in IR."
            )
    elif "showcase_07_lattice_real_interval" in name:
        block = extract_function_block(ir_text, "witness_lattice_real_interval")
        if block is None:
            raise AssertionError("IR missing witness_lattice_real_interval symbol.")
        if "ret i1 true" not in block:
            raise AssertionError(
                "Expected ℤ lattice ∩ real interval (-21.0, 21.0] at 0 to "
                "collapse to `ret i1 true` in IR."
            )
    elif "showcase_08_halfspace_2d_product" in name:
        block = extract_function_block(ir_text, "witness_2d_box_member")
        if block is None:
            raise AssertionError("IR missing witness_2d_box_member symbol.")
        if "ret i1 true" not in block:
            raise AssertionError(
                "Expected 2D box membership at (0, 5) to collapse to "
                "`ret i1 true` in IR."
            )
    elif "showcase_09_lp_vertex_typed_constant" in name:
        # LP `maximize(3x + 2y, {x+y≤4, 2x+y≤6, x,y≥0})` reduces to the
        # vertex (2, 2) at compile time. The two witness_ symbols should
        # fold to `ret i64 2` — the numerator of each coordinate as a
        # literal in the emitted IR, with no LP solver / no active-set
        # iteration surviving the optimizer.
        for symbol in ("witness_lp_optimum_x", "witness_lp_optimum_y"):
            block = extract_function_block(ir_text, symbol)
            if block is None:
                raise AssertionError(f"IR missing {symbol} symbol.")
            if "ret i64 2" not in block:
                raise AssertionError(
                    f"Expected {symbol} to collapse to `ret i64 2` in IR "
                    "(the optimum is the typed constant Vec2<Rat, 2, 2>)."
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
    """Verify semantic invariants in freshly generated IR.

    Exact-text comparison is intentionally omitted: attribute groups and
    section annotations are platform-specific (macOS vs. Linux ELF) and would
    cause false failures in cross-platform CI.  The semantic checks
    (ret i1 false / ret i1 true / indirect-call presence) are the load-bearing
    correctness gate; use `make ir-fixture-refresh` to update the stored
    snapshots for human review.
    """
    ensure_cmake_target()
    failed = False
    for source, fixture in SOURCES:
        actual = generate_ir(source)
        try:
            semantic_sanity(actual, source)
            print(f"IR semantic check passed: {source.name}")
        except AssertionError as exc:
            print(f"IR semantic check FAILED: {source.name}: {exc}", file=sys.stderr)
            failed = True

    if failed:
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
