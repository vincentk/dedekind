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
    # showcase_06_halfspace_interval_42.cpp and
    # showcase_08_halfspace_2d_product.cpp dropped from the IR-fixture
    # rotation under PR #673.  The paper's claim is the realistic one:
    # sometimes the membership evaluates all the way to a constant;
    # when an argument is not yet known at compile time the collapse
    # is partial (cf. Turchin supercompilation).  Whether a *given*
    # toolchain + ambient BMI carries the fold to completion on a
    # variant-carrier ℤ comparison is an inliner-cost-model question
    # downstream of the structural claim --- which is witnessed in
    # source via static_assert(iv.size() == 42u),
    # static_assert(IsExtensional<…>), HasDecidableMembership, etc.
    (
        _PYTHON_DIR / "showcase_07_lattice_real_interval.cpp",
        _PYTHON_DIR / "showcase_07_lattice_real_interval.ll",
    ),
    (
        _PYTHON_DIR / "showcase_09_lp_vertex_typed_constant.cpp",
        _PYTHON_DIR / "showcase_09_lp_vertex_typed_constant.ll",
    ),
    # showcase_13 is the diamond-necklace critical path: the SAME intensional
    # semiring closure folds to `ret i64 1` (reachability, Boolean semiring),
    # `ret i64 8` (critical path, MaxPlus), and `ret i64 1`/`ret i64 0`
    # (envelope-theorem criticality of an on-path vs floated branch, via
    # MaxPlus<Dual<F>>).  No solver survives the optimizer.
    (
        _PYTHON_DIR / "showcase_13_necklace_critical_path.cpp",
        _PYTHON_DIR / "showcase_13_necklace_critical_path.ll",
    ),
    # showcase_09b is the runtime counterpart of showcase_09: the same
    # active-set kernel called through `maximize_with_values<double>(span, …)`
    # with coefficients as function arguments.  Unlike showcase_09's
    # `ret i64 2`, the body cannot fold — its semantic_sanity check below
    # asserts that the Cramer 2×2 solve (`fdiv double`) and the loop
    # accumulators (`phi`) survive into IR.  This guards against optimizer
    # regressions that would silently fold what the paper claims is residual.
    (
        _PYTHON_DIR / "showcase_09b_lp_runtime_residual.cpp",
        _PYTHON_DIR / "showcase_09b_lp_runtime_residual.ll",
    ),
    # showcase_12 is the compile-time NTTP form of the bit-ops fast path
    # (unit-square pack — signed-unimodular + axis-aligned).  The fast
    # path folds at translation time and the witness IR is `ret i64 1`.
    (
        _PYTHON_DIR / "showcase_12_lp_unimodular_fast_path.cpp",
        _PYTHON_DIR / "showcase_12_lp_unimodular_fast_path.ll",
    ),
    # showcase_12b is the runtime counterpart of showcase_12: the same
    # fast-path kernel called through
    # `maximize_axis_aligned_with_values<int>(span, cx, cy)`.  The
    # semantic check asserts the bit-ops claim mechanically — the loop
    # `phi` accumulators survive (no fold) while `mul`/`sdiv`/`udiv` on
    # the int carrier are absent (the fast path does no arithmetic
    # beyond compares, negations, and selects).
    (
        _PYTHON_DIR / "showcase_12b_lp_unimodular_runtime.cpp",
        _PYTHON_DIR / "showcase_12b_lp_unimodular_runtime.ll",
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
    elif "showcase_07_lattice_real_interval" in name:
        block = extract_function_block(ir_text, "witness_lattice_real_interval")
        if block is None:
            raise AssertionError("IR missing witness_lattice_real_interval symbol.")
        if "ret i1 true" not in block:
            raise AssertionError(
                "Expected ℤ lattice ∩ real interval (-21.0, 21.0] at 0 to "
                "collapse to `ret i1 true` in IR."
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
            # Whole-instruction match: `ret i64 2` alone, not as a
            # substring of `ret i64 20`, `ret i64 25`, etc.
            if not re.search(r"^\s*ret i64 2\s*$", block, re.MULTILINE):
                raise AssertionError(
                    f"Expected {symbol} to collapse to `ret i64 2` in IR "
                    "(the optimum is the typed constant Vec2<Rat, 2, 2>)."
                )
    elif "showcase_13_necklace_critical_path" in name:
        # The diamond-necklace critical path: the SAME intensional semiring
        # closure folds to a literal per semiring — reachability (Boolean,
        # `ret i64 1`), the critical value (MaxPlus, `ret i64 8`), and
        # envelope-theorem criticality of an on-path vs floated branch
        # (MaxPlus<Dual<F>>, `ret i64 1` / `ret i64 0`).  No solver / no loop
        # survives in the compile-time witnesses.
        expected = {
            "witness_necklace_reachable": r"^\s*ret i64 1\s*$",
            "witness_necklace_critical": r"^\s*ret i64 8\s*$",
            "witness_necklace_sensitivity_critical": r"^\s*ret i64 1\s*$",
            "witness_necklace_sensitivity_floated": r"^\s*ret i64 0\s*$",
            # annotate -> pred net -> critical_path -> fold: the same 8.
            "witness_necklace_path_value": r"^\s*ret i64 8\s*$",
        }
        for symbol, want in expected.items():
            block = extract_function_block(ir_text, symbol)
            if block is None:
                raise AssertionError(f"IR missing {symbol} symbol.")
            if not re.search(want, block, re.MULTILINE):
                raise AssertionError(
                    f"Expected {symbol} to fold to `{want.strip()}` in IR "
                    "(necklace semiring closure / envelope-theorem sensitivity)."
                )
        # Partial evaluation: with the endpoints (or width) as runtime
        # arguments the SAME closure must NOT fold to a constant — a residual
        # relaxation loop (a `phi`) must survive, with the semiring and the
        # topology rule specialised into it.  Its absence would mean the
        # runtime witnesses collapsed, contradicting the optionality claim.
        block = extract_function_block(ir_text, "witness_necklace_critical_between")
        if block is None:
            raise AssertionError("IR missing witness_necklace_critical_between.")
        if not re.search(r"=\s*phi\s+\S", ir_text):
            raise AssertionError(
                "Expected a residual `phi` (the runtime-endpoint relaxation "
                "loop) to survive in IR — its absence would mean partial "
                "evaluation collapsed the runtime witnesses."
            )
    elif "showcase_12_lp_unimodular_fast_path" in name:
        # The bit-ops fast path on the unit-square pack (signed-unimodular
        # + axis-aligned) folds at compile time to Vec2<Rat, 1, 1>.  The
        # two witness symbols should collapse to `ret i64 1` — the
        # numerator of each coordinate as a literal in the emitted IR,
        # the fast-path dispatch having selected `detail::maximize_impl_axis_aligned`
        # at the NTTP entry @ref maximize.
        for symbol in ("witness_lp_fast_optimum_x", "witness_lp_fast_optimum_y"):
            block = extract_function_block(ir_text, symbol)
            if block is None:
                raise AssertionError(f"IR missing {symbol} symbol.")
            # Whole-instruction match: `ret i64 1` alone, not as a
            # substring of `ret i64 10`, `ret i64 15`, etc.
            if not re.search(r"^\s*ret i64 1\s*$", block, re.MULTILINE):
                raise AssertionError(
                    f"Expected {symbol} to collapse to `ret i64 1` in IR "
                    "(fast-path optimum is the typed constant Vec2<Rat, 1, 1>)."
                )
    elif "showcase_12b_lp_unimodular_runtime" in name:
        # The runtime counterpart of showcase_12: the same fast-path
        # kernel called through `maximize_axis_aligned_with_values<int>(
        # span, cx, cy)` with coefficients as function arguments.  The
        # body cannot fold — coefficients are runtime data — so the
        # algorithm's residual structure must survive into IR.  The
        # bit-ops claim is mechanically witnessed: the loop's `phi`
        # accumulators survive somewhere in the fixture IR while
        # `mul`/`sdiv`/`udiv` on the int carrier are absent everywhere.
        #
        # LLVM may inline the kernel into the witness blocks or outline
        # it as a separate definition depending on body size; the
        # whole-fixture check below is agnostic to which choice the
        # optimiser makes.
        for symbol in (
            "witness_lp_axis_aligned_x",
            "witness_lp_axis_aligned_y",
            "witness_lp_axis_aligned_feasible",
        ):
            block = extract_function_block(ir_text, symbol)
            if block is None:
                raise AssertionError(f"IR missing {symbol} symbol.")

        # The fast-path loop's `phi` accumulators must survive in the
        # emitted IR.  Match the LLVM SSA `phi` opcode with a leading
        # `=` and a trailing type (`phi i32`, `phi i8`, `phi ptr`, ...);
        # plain substring "phi" would also match `tail call ...phi...`
        # or symbol names.
        if not re.search(r"=\s*phi\s+\S", ir_text):
            raise AssertionError(
                "Expected residual `phi` SSA nodes (per-axis bound "
                "accumulators) to survive in IR — their absence would "
                "mean the fast-path loop was eliminated."
            )

        # Bit-ops claim, applied to the whole emitted IR: no integer
        # multiplication or division on the int carrier (i32) anywhere.
        # Allow LLVM's optional `nuw`/`nsw`/`exact`/`disjoint` flags;
        # restrict to `i32` so std::span's i64 pointer arithmetic does
        # not false-positive.
        if re.search(
            r"\b(mul|sdiv|udiv)\b(?:\s+(?:nuw|nsw|exact|disjoint))*\s+i32\b",
            ir_text,
        ):
            raise AssertionError(
                "The bit-ops fast path must emit no `mul`/`sdiv`/`udiv` "
                "on the int carrier (i32) anywhere in the fixture IR — "
                "its presence would mean the kernel fell back to "
                "Cramer-style arithmetic."
            )
    elif "showcase_09b_lp_runtime_residual" in name:
        # The runtime counterpart of showcase_09: the same active-set
        # kernel called through the power-user runtime entry
        # `maximize_cramer_with_values<double>(span, cx, cy)` with
        # coefficients as function arguments — the IR microscope on the
        # Cramer kernel in isolation (the default `maximize_with_values`
        # would route via mechanical scan + branch, contaminating the
        # residual signature this fixture exists to exhibit).  Unlike
        # showcase_09's
        # `ret i64 2`, the body cannot fold — every coefficient is runtime
        # data — so the algorithm's residual structure must survive into IR.
        # The paired contrast (folded vs. residual on the same kernel) is
        # the bridge claim made mechanically; these assertions guard it
        # against optimizer drift that would silently invert the contrast.
        for symbol in (
            "witness_lp_runtime_x",
            "witness_lp_runtime_y",
            "witness_lp_runtime_feasible",
        ):
            block = extract_function_block(ir_text, symbol)
            if block is None:
                raise AssertionError(f"IR missing {symbol} symbol.")
            # Residual Cramer 2x2 solve: two `fdiv double` (x* and y*
            # numerators divided by the active-set determinant).
            if "fdiv double" not in block:
                raise AssertionError(
                    f"{symbol}: expected residual `fdiv double` from the "
                    "Cramer 2x2 solve to survive in IR — its absence would "
                    "mean the runtime kernel was accidentally constant-folded."
                )
            # Residual loop / argmax structure: the outer C(n,2) enumeration
            # and the argmax accumulator survive as `phi` nodes.
            if "phi" not in block:
                raise AssertionError(
                    f"{symbol}: expected residual `phi` nodes (loop "
                    "counters / argmax accumulators) to survive in IR — "
                    "their absence would mean the loops were eliminated."
                )
            # Residual FMA arithmetic: the 2x2 determinant and the
            # objective `cx*x + cy*y` go through `llvm.fmuladd.f64`.
            if "fmuladd" not in block:
                raise AssertionError(
                    f"{symbol}: expected `llvm.fmuladd.f64` (Cramer "
                    "determinant + objective FMA) to survive in IR."
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
