/*FBX wrapper that exposes a 'c' interface, meaning simply datatypes, structs, and no classes
There is nothing about this file that is threadsafe*/
#ifndef CFBXH
#define CFBXH

#define CFBXExport extern "C"

typedef unsigned int THandle;

CFBXExport THandle CreateFBXManager();

#endif
