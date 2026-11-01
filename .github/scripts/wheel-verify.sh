#!/usr/bin/env bash
# Installs pynzli at a version into a fresh environment and uses it: from
# dist/, or from an index (testpypi, pypi), retried while the index catches
# up.  The installed package must carry the version asked for.
#
# Usage: wheel-verify.sh <version> [testpypi|pypi]
set -euo pipefail

version=$1
index=${2:-}
venv=build/venv-wheel-verify
rm -rf "$venv"
python -m venv "$venv"
# The wheel only: an unusable wheel must fail here, not fall back to building
# the sdist from source.
pip=("$venv/bin/python" -m pip install --quiet --no-cache-dir --only-binary=:all:)

case "$index" in
  "") "${pip[@]}" dist/pynzli-"$version"-*.whl ;;
  testpypi|pypi)
    url=https://pypi.org/simple/
    [ "$index" = testpypi ] && url=https://test.pypi.org/simple/
    for i in $(seq 1 10); do
      "${pip[@]}" --index-url "$url" "pynzli==$version" && break
      [ "$i" = 10 ] && { echo "pynzli==$version is not on $index" >&2; exit 1; }
      echo "not on $index yet ($i)"; sleep 30
    done ;;
  *) echo "unknown index: $index" >&2; exit 2 ;;
esac

mkdir -p build/python-docs
"$venv/bin/python" -m pip show pynzli > build/python-docs/install-report.txt
"$venv/bin/python" -m pydoc pynzli > build/python-docs/pydoc-pynzli.txt
PYNZLI_VERSION=$version "$venv/bin/python" - <<'PY'
import os
import pynzli
from pynzli.jlt import succ
from pynzli.pst import A, image, u64, π

assert pynzli.__version__ == os.environ["PYNZLI_VERSION"], pynzli.__version__
U = A(u64)
assert (U & (π > 3)) & (U & (π < 5)) == U & (π == 4)
assert image(succ(int), U & (π > 5)) == U & ((π > 6) | (π == 0))
print("pynzli", pynzli.__version__, "ok")
PY
