@echo off


git submodule update --init --recursive external\omm-sdk
cd external

IF NOT EXIST support\scripts MKDIR support\scripts
COPY omm-sdk\support\scripts\postinstall.cmake support\scripts\postinstall.cmake

IF EXIST build RMDIR /S /Q build
cmake -DCMAKE_BUILD_TYPE=Release -DOMM_ENABLE_TESTS=off -DOMM_BUILD_VIEWER=off -DOMM_INTEGRATION_LAYER_NVRHI=off -DOMM_ENABLE_PRECOMPILED_SHADERS_DXIL=off -S . -B build || exit 1
cmake --build build --config Release || exit 1
cmake --install build --config Release || exit 1

RMDIR /S /Q support