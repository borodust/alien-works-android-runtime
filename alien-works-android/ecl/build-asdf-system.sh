#!/bin/bash

WORK_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
. $WORK_DIR/utils/build-common.sh

OPTIONAL_PARAMS=

if [[ -z "$NDK" ]]; then
    echo "Path to Android NDK must be provided via --ndk"
    exit 1
fi

if [[ -z "$TARGET_ARCH" ]]; then
    echo "Target arch not specified via --target option, using aarch64"
    TARGET_ARCH=aarch64
fi

if [[ -z "$REST_ARGS" ]]; then
    echo "ASDF system name not provided"
    exit 1
fi

if [[ ! -z "$BUILD_DIR" ]]; then
    OPTIONAL_PARAMS+="--build-dir $BUILD_DIR"
fi

$HOST_ECL --norc  \
          --shell "$WORK_DIR/utils/build-system.lisp" -- \
          --ndk "$NDK" \
          --ecl "$WORK_DIR/ecl-$TARGET_ARCH" \
          --work-dir "$WORK_DIR" \
          --lib-dir "$WORK_DIR/../lib/" \
          --system "$REST_ARGS" $OPTIONAL_PARAMS
