#!/usr/bin/env bash
# Builds pynzli's wheel at a version into dist/: the version written into
# python/pynzli/_version.py, the wheel repaired, and checked (twine).  On Linux
# the wheel is retagged manylinux (auditwheel; CMake's linux_x86_64 tag PyPI
# refuses) and the sdist kept; on macOS the libraries it links are copied in
# (delocate; Homebrew's libc++ is not on a user's machine) and the sdist
# dropped, as a release publishes one, Linux's.
#
# Usage: wheel.sh <version>   (CC/CXX in the environment pick the compiler)
set -euo pipefail

version=$1
sed -i.bak "s/^__version__ = .*/__version__ = \"$version\"/" python/pynzli/_version.py
rm -f python/pynzli/_version.py.bak
export CMAKE_ARGS="-DCMAKE_C_COMPILER=${CC:-cc} -DCMAKE_CXX_COMPILER=${CXX:-c++} -DCMAKE_CXX_SCAN_FOR_MODULES=ON ${CMAKE_ARGS:-}"

python -m pip install --quiet --upgrade pip build twine
rm -rf dist
python -m build
if [ "$(uname -s)" = Darwin ]; then
  python -m pip install --quiet --upgrade delocate
  delocate-wheel -v dist/*.whl
  rm dist/*.tar.gz
else
  python -m pip install --quiet --upgrade auditwheel patchelf
  python -m auditwheel repair dist/*-linux_*.whl -w dist/
  rm dist/*-linux_*.whl
fi
python -m twine check dist/*

mkdir -p build/python-docs
ls -lh dist | tee build/python-docs/dist-manifest.txt
shasum -a 256 dist/* | tee build/python-docs/dist-sha256.txt
