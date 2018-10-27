#/usr/bin/env bash
#-------------------------------------------
# 引导程序
# @auther raydrawc@gmail.com
#-------------------------------------------

## notice: luajit incompatible moon_nif
## in sometime will core dump

ROOT=$(cd "$(dirname "$0")"; pwd)

# 检测运行环境
get_env_os(){
    local unameout="$(uname -s)"
    # 主要用于配置lua编译
    case "${unameout}" in
        Linux*)     PLATFORM=linux;;
        Darwin*)    PLATFORM=macosx;;
        CYGWIN*)    PLATFORM=mingw;;
        MINGW*)     PLATFORM=mingw;;
        *)          PLATFORM=generic;;
    esac
}

## for lua make platform
get_env_os

mkdir -p ${ROOT}/deps
cd deps

wget -c -t 0 -w 15 "https://dl.bintray.com/boostorg/release/1.68.0/source/boost_1_68_0.tar.bz2"
tar -jxv -f boost_1_68_0.tar.bz2
mv boost_1_68_0 boost
cd boost && ./bootstrap.sh
./b2 stage --with-thread link=static threading=multi runtime-link=static cxxflags="-shared -fPIC"

cd ${ROOT}/deps
wget -c -t 0 -w 15 "https://www.lua.org/ftp/lua-5.2.4.tar.gz"
tar -zxv -f lua-5.2.4.tar.gz
mv lua-5.2.4 lua

cd ${ROOT}/deps/lua/src
make ${PLATFORM} CFLAGS+="-shared -fPIC -g"

if [ $PLATFORM == "mingw" ]; then
    cd ..
    ./bootstrap.bat
    exit 0
fi