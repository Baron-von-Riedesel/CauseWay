//
//
// A short and hopefully easy to understand demonstration of CauseWay DLL
// usage with Watcom.
//
//
#include <stdio.h>

// Need function definitions
#include "dllfunc.h"

#define CALLCONV 1 /* 0=stack based, 1=register based */

// Name the module. DLLS for stack version and DLLR for register
#if CALLCONV
const char ModuleName[]={"DLLR"};
const char ProcName[]={"SayHello_"};
#else
const char ModuleName[]={"DLLS"};
const char ProcName[]={"SayHello"};
#endif


//
//The actual do something code.
//
int main()
{
    void *DLL;
    void (*DLLFunction)(char *);

    // Try and load the module.
    DLL=LoadModule(ModuleName);
    if (DLL) {

        printf("Module %s loaded sucessfully\n", ModuleName);

        // Fetch the test function address
        DLLFunction=GetProcAddress(DLL,ProcName);
        if (DLLFunction) {

            // Give the test function a shout
            DLLFunction("Hello World!\n");

        } else {

            printf("GetProcAddress('%s') failed\n", ProcName);

        }

        // Lose the module again
        FreeModule(DLL);

        printf("Module %s discarded\n", ModuleName);

    } else {
        printf("Failed to load %s module...\n", ModuleName);

    }

    return(0);
}
