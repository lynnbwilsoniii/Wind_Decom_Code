/* wind_os_def.h - include file for preprocessor definitions dependant
   on operating system

   Note:  CDF_LIB_USEABLE is defined in the makefile and specified on the
   cc command line.
*/

#ifdef USE_MACOSX

#define VMS 0
#define SUNOS 0
#define MACOSX 1
#define SYSTEM MACOSX

#else

#define VMS 1
#define SUNOS 0
#define MACOSX 0
#define SYSTEM SUNOS

#endif
