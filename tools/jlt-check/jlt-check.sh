#!/usr/bin/env bash
# Run every <rule> <file> [expected] line of tools/jlt-check/scope through
# clang-query; fail when a file's findings differ from its expected count
# (default 0), and on any file that does not parse.
# Usage: jlt-check.sh [CLANG_QUERY] [BUILD_DIR]
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
query="${1:-clang-query}"
build="${2:-$root/build}"

[ -f "$build/compile_commands.json" ] || {
  echo "jlt-check: no $build/compile_commands.json; build first" >&2
  exit 2
}

findings=0
failures=0
while read -r rule file expected; do
  case "$rule" in '' | '#'*) continue ;; esac
  expected="${expected:-0}"
  # A fixture under tools/jlt-check/fixtures is standalone: no imports, no
  # entry in the compilation database, so it is parsed with explicit flags.
  case "$file" in
  tools/jlt-check/fixtures/*) args=("$root/$file" -- -std=c++23 -xc++) ;;
  *) args=(-p "$build" "$root/$file") ;;
  esac
  out="$("$query" -f "$here/rules/$rule.query" "${args[@]}" 2>&1)" || {
    echo "jlt-check: $rule $file: clang-query failed" >&2
    echo "$out" >&2
    failures=$((failures + 1))
    continue
  }
  if grep -qE "error:" <<<"$out"; then
    echo "jlt-check: $rule $file: does not parse" >&2
    grep -E "error:" <<<"$out" | head -20 >&2
    failures=$((failures + 1))
    continue
  fi
  n="$(grep -oE '^[0-9]+ match' <<<"$out" | awk '{s += $1} END {print s + 0}')"
  echo "jlt-check: $rule $file: $n finding(s), expected $expected"
  if [ "$n" -ne "$expected" ]; then
    grep -E "note: \"root\" binds here" <<<"$out" || true
    if [ "$expected" -eq 0 ]; then
      findings=$((findings + n))
    else
      echo "jlt-check: $rule $file: the control expected $expected, so the rule is mis-scoped" >&2
      failures=$((failures + 1))
    fi
  fi
done <"$here/scope"

echo "jlt-check: $findings finding(s), $failures failure(s)"
[ "$findings" -eq 0 ] && [ "$failures" -eq 0 ]
