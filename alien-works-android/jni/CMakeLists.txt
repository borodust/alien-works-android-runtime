cmake_minimum_required(VERSION 3.6)

project(ALIEN_WORKS_ANDROID)

# armeabi-v7a requires cpufeatures library
include(AndroidNdkModules)
android_ndk_import_module_cpufeatures()

#
# main
#
set(TARGET main)
set(MAIN main.c)

find_library(log-lib log)

add_library(${TARGET} SHARED ${MAIN})

#target_include_directories(${TARGET} PUBLIC ../ecl/ecl-host/include)

target_link_libraries(${TARGET} ${log-lib} ${CMAKE_DL_LIBS})
