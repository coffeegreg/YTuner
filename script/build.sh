#!/bin/bash
set -eu

ROOT=$(cd -- "$(dirname -- "$0")/.." && pwd)
cd "$ROOT"

OUT_CPU=$(go env GOARCH)
OUT_OS=$(go env GOOS)

# Cross compilation overrides if we are in github actions matrix and we're missing something
if [ -n "${GOOS:-}" ]; then
  OUT_OS="$GOOS"
fi
if [ -n "${GOARCH:-}" ]; then
  OUT_CPU="$GOARCH"
fi

GO_OS=$OUT_OS
GO_ARCH=$OUT_CPU

if [ "$OUT_OS" = "darwin" ]; then
    OUT_OS="darwin"
elif [ "$OUT_OS" = "linux" ]; then
    OUT_OS="linux"
fi

if [ "$OUT_CPU" = "amd64" ]; then
    OUT_CPU="x86_64"
elif [ "$OUT_CPU" = "arm64" ]; then
    OUT_CPU="aarch64"
elif [ "$OUT_CPU" = "386" ]; then
    OUT_CPU="i386"
fi

TARGET_DIR=$ROOT/bin/$OUT_CPU-$OUT_OS
mkdir -p "$TARGET_DIR"

EXE_SUFFIX=""
if [ "$OUT_OS" = "windows" ]; then
  EXE_SUFFIX=".exe"
fi

env GOOS=$GO_OS GOARCH=$GO_ARCH go build -o "$TARGET_DIR/viritin$EXE_SUFFIX" main.go
echo "Built $TARGET_DIR/viritin$EXE_SUFFIX"
