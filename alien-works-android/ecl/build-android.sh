#!/bin/bash

WORK_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
. $WORK_DIR/utils/build-common.sh

if [[ -z "$NDK" ]]; then
    echo "Path to Android NDK must be provided via --ndk"
    exit 1
fi

REQUESTED_TARGET=
COMPATIBLE_TARGET=
CROSS_CONFIG=

case "$TARGET_ARCH" in
    aarch64)
        REQUESTED_TARGET=aarch64-linux-android
        COMPATIBLE_TARGET=$REQUESTED_TARGET
        CROSS_CONFIG=android-arm64.cross_config
    ;;
    armv7a)
        REQUESTED_TARGET=armv7a-linux-androideabi
        COMPATIBLE_TARGET=arm-linux-androideabi
        CROSS_CONFIG=android-arm.cross_config
    ;;
    *)
        echo "Unrecognized TARGET_ARCH provided: $TARGET_ARCH, need aarch64 or armv7a"
        exit 1
    ;;
esac

## Adapted from https://developer.android.com/ndk/guides/other_build_systems#autoconf

export TOOLCHAIN=$NDK/toolchains/llvm/prebuilt/linux-x86_64
export TARGET=$REQUESTED_TARGET
export API=$ANDROID_API
# Configure and build.
export AR=$TOOLCHAIN/bin/$COMPATIBLE_TARGET-ar
export AS=$TOOLCHAIN/bin/$COMPATIBLE_TARGET-as
export CC=$TOOLCHAIN/bin/$TARGET$API-clang
export CXX=$TOOLCHAIN/bin/$TARGET$API-clang++
export LD=$TOOLCHAIN/bin/$COMPATIBLE_TARGET-ld
export RANLIB=$TOOLCHAIN/bin/$COMPATIBLE_TARGET-ranlib
export STRIP=$TOOLCHAIN/bin/$COMPATIBLE_TARGET-strip

## Adapted from https://gitlab.com/eql/EQL5-Android/-/blob/master/scripts/2-make-ecl-android.sh

export ECL_TO_RUN=$HOST_ECL

clean_ecl
./configure CFLAGS="-g -O2" LDFLAGS="-g -O2" $CONFIGURE_OPTS --host=$TARGET \
            --prefix=$WORK_DIR/ecl-$TARGET_ARCH \
            --disable-c99complex \
            --disable-manual \
            --without-cmp \
            --without-dffi \
            --without-fpe \
            --with-bytecmp \
            --with-asdf \
            --with-cross-config=$ECL_DIR/src/util/$CROSS_CONFIG
make
make install
