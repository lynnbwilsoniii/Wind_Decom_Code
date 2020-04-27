/** \file getFLR.c
    \brief Get info from File Label Record of SMS data. See Ref. 1 
   (DFCD) in smsRead.c
*/
/*
   Author: Jim Raines, 15Nov99

   See libsmsInit for further documentation.

   This file is controlled by the Concurent Version System (CVS):
   $Id: getFLR.c,v 1.5 2005/11/29 19:22:11 jfeeman Exp $

   Modifcation History:
   Changed return value to SMSFATAL when record length is not standard.
   J. Raines, 26Jan00
*/

#include "libsms.h"

int getFLR(unsigned char *abFLR) {

  int i, j;  /* loop counters */
  char thisprog[] = "getFLR"; /* name of this module for labeling output */
  int errors=0; /* accumulated non-fatal errors */
  int iMajFrameExpect; /* major frames expected in file */

  /**********************/
  /* fill FLR structure */
  /**********************/

  flr.iScId = lBuildWord(abFLR, 0, 3);
  flr.iMajFrameInFile = lBuildWord(abFLR, 84, 87);
  flr.iBytesPerRec = lBuildWord(abFLR, 176, 179);

  /****************************************************/
  /* test to see if it looks like there was a problem */
  /****************************************************/

  if (flr.iBytesPerRec != 10800) {
  /* If iBytesPerRec is not 10800, then the file pointer 
     will be in the wrong place -- BUT, smsOpenFile will re-position it.
  */
    if (DEBUG > 1) printf("%s -D- %d (not 10800) bytes per record.\n",
		      flr.iBytesPerRec,thisprog);
    errors = SMSFATAL;
    
  }

  iMajFrameExpect = lBuildWord(abFLR, 80, 83);
  if (flr.iMajFrameInFile != iMajFrameExpect) {
    if (DEBUG > 1) printf("%s -D- %d major frames present but %d expected\n",
	   thisprog, flr.iMajFrameInFile, iMajFrameExpect);
    errors++;
  }

  /*******************/
  /* print some info */
  /*******************/

  if (DEBUG > 2) {
    printf("%s -D- Spacecraft ID: %d (WIND=25)\n",thisprog,
	   flr.iScId);

    printf("%s -D- %d records in file\n",thisprog,
	   lBuildWord(abFLR, 20, 23));

    printf("%s -D- major frame count -- begining of first: %d\n",thisprog,
	   lBuildWord(abFLR, 24, 27));

    printf("%s -D- major frame count -- beginning of last: %d\n",thisprog,
	   lBuildWord(abFLR, 28, 31));

    printf("%s -D- %d Major Frames in file (expected %d)\n",thisprog,
	   flr.iMajFrameInFile,iMajFrameExpect );

    printf("%s -D- %d bytes per record\n",thisprog,flr.iBytesPerRec);
  }

  return(errors);
}
