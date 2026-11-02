#!/usr/bin/env bash
# Builds pynzli's wheel and sdist at a version into dist/: the version written
# into python/pynzli/_version.py, the wheel retagged manylinux (auditwheel;
# CMake's linux_x86_64 tag PyPI refuses), both checked (twine).
#
# Usage: wheel.sh <version>   (CC/CXX in the environment pick the compiler)
set -euo pipefail

version=$1
sed -i.bak "s/^__version__ = .*/__version__ = \"$version\"/" python/pynzli/_version.py
rm -f python/pynzli/_version.py.bak
export CMAKE_ARGS="-DCMAKE_C_COMPILER=${CC:-cc} -DCMAKE_CXX_COMPILER=${CXX:-c++} -DCMAKE_CXX_SCAN_FOR_MODULES=ON ${CMAKE_ARGS:-}"

python -m pip install --quiet --upgrade pip build twine auditwheel patchelf
rm -rf dist
python -m build
python -m auditwheel repair dist/*-linux_*.whl -w dist/
rm dist/*-linux_*.whl
python -m twine check dist/*

mkdir -p build/python-docs
ls -lh dist | tee build/python-docs/dist-manifest.txt
sha256sum dist/* | tee build/python-docs/dist-sha256.txt
