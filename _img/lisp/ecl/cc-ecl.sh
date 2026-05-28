export ECL_SRC_DIR=ecl

[ -d $ECL_SRC_DIR ] || git clone https://gitlab.com/embeddable-common-lisp/ecl.git $ECL_SRC_DIR

cd $ECL_SRC_DIR

export IOS_VERSION_MIN="8.0"
export IOS_SDK_DIR="`xcode-select --print-path`/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk/"

export CC="clang"
export CXX="clang++"

export CFLAGS="-arch arm64 -miphoneos-version-min=${IOS_VERSION_MIN} -isysroot ${IOS_SDK_DIR}"
export CFLAGS="$CFLAGS -pipe -Wno-trigraphs -Wreturn-type -Wunused-variable"
export CFLAGS="$CFLAGS -fpascal-strings -fasm-blocks -fmessage-length=0 -fvisibility=hidden"
export CFLAGS="$CFLAGS -O2 -DNO_ASM"

export LD="ld"
export LDFLAGS="-arch arm64 -pipe -std=c99 -gdwarf-2 -isysroot ${IOS_SDK_DIR}"
export LIBS="-framework Foundation"

export CFLAGS="$CFLAGS -DGC_DISABLE_INCREMENTAL -DECL_RWLOCK"
export CXXFLAGS="$CFLAGS"

export ECL_TO_RUN=ecl

./configure --host=aarch64-apple-darwin \
            --prefix=`pwd`/ecl-iOS \
            --disable-c99complex \
            --disable-shared \
            --with-cross-config=`pwd`/src/util/iOS-arm64.cross_config

make -j$`nproc`
make install

export IOS_APP_SRC=/path/to/iMaxima/iMaxima/
cp -r $ECL_SRC_DIR/ecl-iOS/include $ECL_SRC_DIR/ecl-iOS/lib $IOS_APP_SRC
