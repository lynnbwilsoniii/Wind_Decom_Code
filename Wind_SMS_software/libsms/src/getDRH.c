/** \file getDRH.c
   \brief Get info from Data Record Header of SMS data. See Ref. 1 
   (DFCD) in smsReadCycle.c
*/
/*
   Author: Jim Raines, 30Nov99

   See libsmsInit for further documentation.

   This file is controlled by the Concurent Version System (CVS):
   $Id: getDRH.c,v 1.4 2005/11/29 19:22:11 jfeeman Exp $
*/

#include <stdio.h>
#include "libsms.h"

int getDRH(unsigned char *abDRH) {

  int i, j;  /* loop counters */
  char thisprog[] = "getDRH"; /* name of this module for labeling output */
  int errors=0; /* accumulated non-fatal errors */
  char *szTmMode[]=  /* names of modes for printing */
  {"<filler>", /* b/c Science-92s must be element 1 */
    "Science-92s","<not used>","Maneuver-92s","Contingency-92s",
   "Science-46s","<not used2>","Maneuver-46s","Contingency-46s",
   "Transitional", "Unknown"};
  enum TmMode        /* names of modes for testing */
  {Science92s = 1,NotUsed,Maneuver92s,Contingency_92s,
   Science46s,NotUsed2,Maneuver46s,Contingency46s,
   Transitional = 128, Unknown = 256};

  float SecRemain; /* seconds remaining for msec -> hh:mm:ss conversion */

  /*****************************/
  /* decode FLR into structure */
  /*****************************/ 

  drh.iInstNum = lBuildWord(abDRH, 0, 3);
  drh.iTmMode = lBuildWord(abDRH, 44, 47);
  strcpy(drh.abTmMode,szTmMode[drh.iTmMode]);

  /* universal time for record */

  drh.year = lBuildWord(abDRH, 20, 23);
  drh.day = lBuildWord(abDRH, 24, 27);
  drh.msec = lBuildWord(abDRH, 28, 31);
  drh.usec = lBuildWord(abDRH, 32, 35);

  SecRemain = drh.msec/1000; /* seconds of day */
  drh.hour  = (int)(SecRemain/3600);
  SecRemain = SecRemain - drh.hour*3600; /* seconds beyond drh.hour */
  drh.min = (int)(SecRemain/60);
  SecRemain = SecRemain - drh.min*60; /* seconds beyond drh.hour:drh.min */
  drh.sec = (int)SecRemain;

  if (DEBUG > 1) {
    printf("%s -D- record UT -- %4.4d%3.3d %2.2d:%2.2d:%2.2d (%d msec %d usec)\n",
	   thisprog, drh.year, drh.day,
	   drh.hour, drh.min, drh.sec, drh.msec, drh.usec);
  }

  /****************************************************/
  /* test to see if it looks like there is a problem */
  /****************************************************/

  /* check TM mode */
  if (drh.iTmMode == NotUsed ) errors++;
  else if (drh.iTmMode == NotUsed2 ) errors++;
  else if (drh.iTmMode == Transitional ) errors++;
  else if (drh.iTmMode == Unknown ) errors++;

  if (errors) {
    printf("%s -W- warning -- Unusual TM mode: %s\n",thisprog,
	   szTmMode[drh.iTmMode]);
  }
  else {
    if (DEBUG > 2) 
      printf("%s -D- TM mode is %s\n",thisprog,szTmMode[drh.iTmMode]);
  }

  /* check to be sure instrument number is right, SMS=5 */
  if (drh.iInstNum != 5){
    printf("%s -E- error -- wrong instrument number, %d; SMS=5\n",
	   thisprog, drh.iInstNum);
    errors++;
  }
  else if (DEBUG > 2) {
    printf("%s -D- instrument number is %d (SMS=5)\n",thisprog,drh.iInstNum);
  }

  return(errors);
}
