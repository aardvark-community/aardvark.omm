#!/bin/bash
set -e

git submodule update --init --recursive external/omm-sdk
cd external

mkdir -p support/scripts
cp omm-sdk/support/scripts/postinstall.cmake support/scripts/postinstall.cmake
rm -rf build

cmake -DCMAKE_BUILD_TYPE=Release -DOMM_ENABLE_TESTS=off -DOMM_BUILD_VIEWER=off -DOMM_INTEGRATION_LAYER_NVRHI=off -DOMM_ENABLE_PRECOMPILED_SHADERS_DXIL=off -S . -B build
cmake --build build --config Release
cmake --install build --config Release

rm -rf support