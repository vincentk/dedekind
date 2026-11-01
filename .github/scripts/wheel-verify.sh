#!/usr/bin/env bash
# Installs pynzli's wheel at a version into a fresh environment and runs the
# kick-the-tyres suite on it: from dist/, or from an index by its host
# (test.pypi.org, pypi.org),
# retried while the index catches up.  The installed package must carry the
# version asked for, and its doctests and the Python tests pass on it.
#
# Usage: wheel-verify.sh <version> [index host]
set -euo pipefail

version=$1
host=${2:-}
# Isolated: no path, home or startup file from the build, a fresh venv without
# the system's packages, Python in isolated mode (-I: no PYTHON* variables,
# no user site, not the current directory) and pip ignoring PIP_* and user
# configuration (--isolated), so what runs is the wheel and nothing else.
unset PYTHONPATH PYTHONHOME PYTHONSTARTUP
venv=$(pwd)/build/venv-wheel-verify
rm -rf "$venv"
python -I -m venv "$venv"
py=("$venv/bin/python" -I)
# The wheel only: an unusable wheel must fail here, not fall back to building
# the sdist from source.
pip=("${py[@]}" -m pip --isolated install --quiet --no-cache-dir --only-binary=:all:)

if [ -z "$host" ]; then
  "${pip[@]}" dist/pynzli-"$version"-*.whl
else
  for i in $(seq 1 10); do
    "${pip[@]}" --index-url "https://$host/simple/" "pynzli==$version" && break
    [ "$i" = 10 ] && { echo "pynzli==$version is not on $host" >&2; exit 1; }
    echo "not on $host yet ($i)"; sleep 30
  done
fi

mkdir -p build/python-docs
"${py[@]}" -m pip --isolated show pynzli > build/python-docs/install-report.txt
"${py[@]}" -m pydoc pynzli > build/python-docs/pydoc-pynzli.txt
"${py[@]}" - "$version" <<'PY'
import sys
import pynzli
from pynzli.jlt import succ
from pynzli.pst import A, image, u64, π

assert pynzli.__version__ == sys.argv[1], pynzli.__version__
U = A(u64)
assert (U & (π > 3)) & (U & (π < 5)) == U & (π == 4)
assert image(succ(int), U & (π > 5)) == U & ((π > 6) | (π == 0))
print("pynzli", pynzli.__version__, "ok")
PY

# The kick-the-tyres suite on the installed binary: the package's doctests
# (collected from the installed package) and the Python tests, run from
# outside the tree and without its pytest configuration, so that every
# import is the wheel's.
"${pip[@]}" pytest  # a wheel on PyPI, the index pip asks by default
tests=$(pwd)/src/test/python
(cd "$venv" && "${py[@]}" -m pytest -q -p no:cacheprovider -c /dev/null \
  --doctest-modules --pyargs pynzli "$tests")
