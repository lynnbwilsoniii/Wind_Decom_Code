/** \file libsmsInit.c
    \brief Initializes global variables
*/
/*
   Author: Jim Raines, 16Feb00

   Description: Performs one-time initialization of some libsms stuff.  Also,
   provides repository for general comments about code flow and coding 
   philosophy.

   General libsms information:

   There are two flavors of the library: libsms.so and libsmsc.so The
   former is accessed via libsms.pm and contains wrapper code to be
   used from Perl.  The latter is intended to be accessed from C.
   Both are shared libraries, which means they are actually linked at
   run time.  (Thus, a change in the library will change all existing
   executables even if they aren't recompiled.)

   The library is coded with a version number which is
   major.minor.build_number, e.g. 1.0.5 is version 1.0 build 5.  The
   major number is incremented when a large amount of changes have
   taken place.  The minor version is incremented for smaller changes
   like added small features or non-bug related improvements to
   existing features.  The build number is incremented mainly as bug
   fixes are done.

   See libsms.html for further documentation.

   This file is controlled by the Concurent Version System (CVS):
   $Id: libsmsInit.c,v 1.7 2005/11/29 19:22:11 jfeeman Exp $

   Modification History
     Added build date/time and version.  J. Raines, 29Feb00
*/

#include "libsms.h"

int libsmsInit(){
  int i,j; /* disposable loop counters */

  char thisprog[] = "libsmsInit";

  /************************************/
  /* initialize some global variables */
  /************************************/
  gDbgLvl = INITGDBGLVL;
  gTrace = INITGTRACE;
  gfIncCycOver = FALSE;
  gfQualOver = FALSE;
  /*++ Initialize variable that keeps track of whether EDB is in high or low bit rate mode ++*/
  isHigh = -1;
  sprintf(gVersion, "%s",LIBSMSVERSION); /* returned by smsgver() */
  
  /***********************/
  /* print 'splash' line */
  /***********************/
  /* Note: LIBSMSVERSION is passed in by preprocessor from makefile.
     __DATE__ and __TIME__ are defined by the preprocessor. */
  printf("libsms -- C/Perl data access library for WIND/SMS data.\n");
  printf("          Version %s built %s %s\n",gVersion,__DATE__,__TIME__);

  return(SMSSUCCESS);
}
