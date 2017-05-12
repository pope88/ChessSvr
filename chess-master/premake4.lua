solution "zo"
    location "build"
    language "C++"
    objdir "obj"

    configurations { "Debug", "Release" }

    configuration "windows"
        defines { "WIN32", "_WIN32" }

    configuration "bsd"
        defines { "LUA_USE_POSIX" }
        includedirs { "/usr/local/include", "/usr/local/include/mysql" }
        libdirs { "/usr/local/lib", "/usr/local/lib/mysql", "/usr/local/lib/event2" }

    configuration "macosx"
        defines { "LUA_USE_MACOSX" }
        includedirs { "/opt/local/include", "/opt/local/include/mysql55/mysql" }
        libdirs { "/opt/local/lib", "/opt/local/lib/mysql55/mysql" }

    configuration "linux"
        defines { "LUA_USE_LINUX" }
        includedirs { "/usr/include/mysql", "/usr/local/include/mysql" }
        libdirs { "/usr/local/lib/mysql" , "/usr/lib/x86_64-linux-gnu" }

    configuration "vs*"
        flags "EnableSSE2"
        buildoptions { "-wd4996" }
        defines { "_CRT_SECURE_NO_WARNINGS" }

    configuration "not vs*"
        buildoptions { "-Wall", "-fexceptions", "-fno-strict-aliasing", "-Wno-multichar" }
    configuration {"not vs*", "windows"}
        buildoptions { "-static-libgcc", "-static-libstdc++" }

    configuration "Debug"
        flags "Symbols"
        defines "_DEBUG"
        targetdir "bin/debug"

    configuration { "Release", "not vs*" }
        flags "Symbols"

    configuration { "Release", "linux or freebsd" }
        buildoptions "-march=native"

    configuration "Release"
        flags { "OptimizeSpeed" }
        targetdir "bin/release"

    include "System"
    include "Network"
    include "zo"
    include "lua"
	if os.is("windows") then
	include "event"
	end
