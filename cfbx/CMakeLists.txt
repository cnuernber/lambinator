cmake_minimum_required(VERSION 2.6)
project(CBFXProj)
add_library(libfbxsdk STATIC IMPORTED)

set(FBX_INS_DIR $ENV{FBX_INSTALL_DIR})

set_property(TARGET libfbxsdk PROPERTY
                IMPORTED_LOCATION $FBX_INS_DIR/lib/libfbxsdk_gcc4_ub.a)
set(CMAKE_VERBOSE_MAKEFILE ON)
include_directories(FBX_INS/include $FBX_INS_DIR/examples/ViewScene)
find_library(OPENGL_LIBRARY OpenGL)
find_library(GLUT_LIBRARY Glut)
find_library(CARBON_LIBRARY Carbon)
find_library(ICONV iconv)
find_library(CFNETWORK Cocoa)
find_library(sysconfig SystemConfiguration)

add_library(cfbx SHARED cfbx.cxx)

target_link_libraries(cfbx libfbxsdk
	${OPENGL_LIBRARY} ${GLUT_LIBRARY}
	${CARBON_LIBRARY} ${ICONV}
	${CFNETWORK} ${SYSCONFIG})
