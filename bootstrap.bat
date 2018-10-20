@echo off

:: get visual studio tools path
:check2017
if exist %VS150COMNTOOLS% (
    set VS_TOOL_VER=vs130
    set VCVARS=%VS150COMNTOOLS%\..\..\VC\Auxiliary\Build\
    goto build
)
:check2016
if exist %VS140COMNTOOLS% (
    set VS_TOOL_VER=vs130
    set VCVARS=%VS140COMNTOOLS%..\..\VC\bin\
    goto build
)
:check2015
if exist %VS130COMNTOOLS% (
    set VS_TOOL_VER=vs130
    set VCVARS=%VS130COMNTOOLS%..\..\VC\bin\
    goto build
)
:check2013
if exist %VS120COMNTOOLS% (
    set VS_TOOL_VER=vs120
    set VCVARS=%VS120COMNTOOLS%..\..\VC\bin\
    goto build
)
:check2012
if exist %VS110COMNTOOLS% (
    set VS_TOOL_VER=vs110
    set VCVARS=%VS110COMNTOOLS%..\..\VC\bin\
    goto build
)
:check2010
if exist %VS100COMNTOOLS% (
    set VS_TOOL_VER=vs100
    set VCVARS=%VS100COMNTOOLS%..\..\VC\bin\
    goto build
)
:check2008
if exist %VS90COMNTOOLS% (
    set VS_TOOL_VER=vs90
    set VCVARS=%VS90COMNTOOLS%..\..\VC\bin\
    goto build
)
else (
    goto missing
)

:build
set ENVPATH=%VCVARS%vcvars32.bat
if exist %windir%\SysWOW64 set ENVPATH=%VCVARS%vcvars64.bat

call "%ENVPATH%"

cd deps/luajit/src

rem if use my dep luajit,use this. else rem this
REM call "msvcbuild.bat" static

cd ../../..
call "rebar.cmd" co

pause
goto :eof

:missing
echo Can't find Visual Studio, compilation fails!

goto :eof