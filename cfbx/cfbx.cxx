#include "cfbx.h"
#include <map>
#include <fbxsdk.h>
#include <fbxfilesdk/fbxfilesdk_nsuse.h>

using namespace std;

//Follow opengl standards to disallow 0 based handles
static THandle g_HandleCounter = 1;
/*KFbxSdkManager* gSdkManager;
100 KFbxImporter* gImporter;
101 KFbxScene* gScene;*/

typedef map<THandle, KFbxSdkManager*> TFBXSdkManagerMap;
static TFBXSdkManagerMap g_FBXSdkManagers;

THandle CreateFBXManager()
{
  THandle hdl_val = g_HandleCounter;
  ++g_HandleCounter;
  g_FBXSdkManagers.insert( make_pair( hdl_val, KFbxSdkManager::Create() ) );
  return hdl_val;
}
