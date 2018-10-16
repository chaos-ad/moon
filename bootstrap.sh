#/usr/bin/env bash
#-------------------------------------------
# 引导程序
# @auther raydrawc@gmail.com
#-------------------------------------------
PWD=`pwd`

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



mkdir -p deps
cd deps

## install boost
wget https://dl.bintray.com/boostorg/release/1.68.0/source/boost_1_68_0.tar.bz2
tar -jxv -f boost_1_68_0.tar.bz2
mv boost_1_68_0 boost
cd boost && ./bootstrap.sh
./b2 stage --with-thread link=static threading=multi runtime-link=static cxxflags="-shared -fPIC"

cd ..
wget https://luajit.org/download/LuaJIT-2.1.0-beta3.tar.gz
tar -zxv -f LuaJIT-2.1.0-beta3.tar.gz
mv LuaJIT-2.1.0-beta3 luajit
cd luajit/src
make install CXXFLAGS="-shared -fPIC"
echo /usr/local/lib > /etc/ld.so.conf.d/usrlocal.conf


## install luajit
# git clone http://luajit.org/git/luajit-2.0.git luajit
# cd luajit
# git checkout v2.1
# sudo make install CFLAGS+="-shared -fPIC"
# sudo ldconfig