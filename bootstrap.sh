#/usr/bin/env bash
#-------------------------------------------
# 引导程序
# @auther raydrawc@gmail.com
#-------------------------------------------

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

# 配置运行环境
get_env_os

# copy dep
git clone https://github.com/raydrawc/lua_nif_dep.git deps
cd deps && ./decode.sh

# if you use luajit and you can use this
cd lua/src/ && make XCFLAGS+="-shared -fPIC"

# if you use lua.org and you can use this
# cd lua/src/ %% make ${PLATFORM} -cxxflag=-fpic