#!/bin/bash

WORK_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
. $WORK_DIR/utils/build-common.sh

clean_ecl

## Adapted from https://gitlab.com/eql/EQL5-Android/-/blob/master/scripts/1-make-ecl-host.sh
./configure CFLAGS="-g -O2" LDFLAGS="-g -O2" CC=clang CXX=clang++ $CONFIGURE_OPTS \
            --prefix=$HOST_ECL_DIR \
            --disable-c99complex \
            --disable-manual \
            --without-fpe \
            --with-cmp \
            --with-bytecmp \
            --with-asdf \
            --with-sse \
            --enable-manual=no
make
make install
