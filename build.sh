#!/bin/bash
# Full build script: configures, builds, and tests slang
# 
# First-time setup or clean build:
#   ./build.sh
#
# Quick rebuild of specific target (after initial cmake -B):
#   cmake --build build-darwin-arm64 --target slang_hier -j
#   cmake --build build-darwin-arm64 --target slang -j

set -e

echo "=== Building Slang with Core Tests ==="

# Detect platform for build directory
PLATFORM=$(uname -s | tr '[:upper:]' '[:lower:]')
ARCH=$(uname -m)
case "$ARCH" in
    x86_64) ARCH="x64" ;;
    aarch64) ARCH="arm64" ;;
esac

BUILD_DIR="build-${PLATFORM}-${ARCH}"
echo "Build directory: $BUILD_DIR"

# Clean previous build
if [ -d "$BUILD_DIR" ]; then
    echo "Cleaning previous build..."
    rm -rf "$BUILD_DIR"
fi

# Configure build with default options
echo "Configuring build..."
cmake -B "$BUILD_DIR"

echo "Building..."
cmake --build "$BUILD_DIR" -j

echo "Running tests..."
cd "$BUILD_DIR"
ctest --output-on-failure

echo "=== Build Complete ==="
echo "Slang Binary: $BUILD_DIR/bin/slang"
echo "Hier Binary: $BUILD_DIR/bin/slang-hier"