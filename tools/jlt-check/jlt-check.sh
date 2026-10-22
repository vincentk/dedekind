#!/usr/bin/env bash
# Run every <rule> <file> pair of tools/jlt-check/scope through clang-query;
# fail on any finding, and on any file that does not parse.
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
while read -r rule file; do
  case "$rule" in '' | '#'*) continue ;; esac
  out="$("$query" -p "$build" -f "$here/rules/$rule.query" "$root/$file" 2>&1)" || {
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
  echo "jlt-check: $rule $file: $n finding(s)"
  if [ "$n" -gt 0 ]; then
    grep -E "note: \"root\" binds here" -B0 -A0 <<<"$out" || true
    findings=$((findings + n))
  fi
done <"$here/scope"

echo "jlt-check: $findings finding(s), $failures failure(s)"
[ "$findings" -eq 0 ] && [ "$failures" -eq 0 ]
