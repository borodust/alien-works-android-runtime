#!/bin/bash

ECL_DIR=$WORK_DIR/ecl/
HOST_ECL_DIR=$WORK_DIR/ecl-host/
HOST_ECL=$HOST_ECL_DIR/bin/ecl
CONFIGURE_OPTS=
ASDF_SYSTEM=

clean_ecl() {
    cd $ECL_DIR && git reset --hard && git clean -ffdx
}

TARGET_ARCH=aarch64
NDK=
ANDROID_API=23
BUILD_DIR=

REST_ARGS=
while [[ $# -gt 0 ]]
do
key="$1"

case $key in
    -t|--target)
        TARGET_ARCH="$2"
        shift
        shift
        ;;
    --ndk)
        NDK="$2"
        shift
        shift
        ;;
    -d)
        BUILD_DIR="$2"
        shift
        shift
        ;;
    --api-version)
        ANDROID_API="$2"
        shift
        shift
        ;;
    --debug)
        CONFIGURE_OPTS+= --enable-debug
        shift
        ;;
    *)
        REST_ARGS+="$1"
        shift
        ;;
esac
done
