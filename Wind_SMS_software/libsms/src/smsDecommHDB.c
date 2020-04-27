/** \file smsDecommHDB.c
    \brief Get values from HDB which are commuted over a number of HDBs
*/
/*
   Author: Jim Raines, 10Dec99

   Method: Search through file to get a full set of 64 HDB's (from the current
   file position) to get a complete set of values which are commuted over this
   number of HDB's

   Requires: (called functions and globals read/written)

   See libsmsInit for further documentation.

   This file is controlled by the Concurent Version System (CVS):
   $Id: smsDecommHDB.c,v 1.5 2005/11/29 19:22:11 jfeeman Exp $

   Modification History:
     changed exit plan for EOF (from getXDB) and incomplete set of HDBs to 
     get file pointer set back to where it was.  Did this by 
     setting RetVal to something other than SMSSUCCES.  Protected rest of stuff
     with test for SMSSUCCESS.  Alternative: goto error-exit.
     J. Raines, 14Jan00
*/

#include "libsms.h"

int smsDecommHDB(){
  int i,j;  /* short range loop counters */
  char thisprog[] = "decommHDB"; /* name of this program */

  long OldFilePos; /* file position coming into routine */
  unsigned char MinorFrame[250][42]; /* minor frames; dim. from DFCD p3-39 */
  int mf = 249; /* current minor frame in record; range 0-249 */
  int fDone; /* T/F flag; T when routine is finished */
  int RetVal = SMSSUCCESS; /* return value, success unless set otherwise */

  HDB hdb; /* local hdb structure */
  int nhdb = 0; /* number of HDB's found */
  DECOMM_HDB dchdb; /* local decommuted hdb structure, copied out at end */

  int countdb = 0; /* ocunt of no. of H/EDBs found so far */
  int result; /* returned value from getXDB */

  BYTE abXDB[800]; /* byte array of H/EDB; passed to decodeHDB */
  int msn = 0; /* measurement spin number -- not needed in this rountine but
		  address is required in call to getXDB */

  /* Fred String -- phrase for verifying complete set of 64 HDBs */
  int SciRecCount;
  char fredstring[80];
  int fGotHDB[64]; /* array of flags; fGotHDB[x] is true if HDB x was found */

  /* save current file pointer location to get back */
  OldFilePos = ftell(pInfile);
  if (DEBUG > 2) printf("%s -D- IncomingFilePos=%d\n",thisprog,OldFilePos);

  /* initialize arrays */
  for (i = 0; i < 64; i++) {
    fGotHDB[i] = FALSE;
    dchdb.tMeasPosStepTab_Raw[i] = -999; /* obvious fictitious value */
    dchdb.tMeasNegStepTab_Raw[i] = -999; /* obvious fictitious value */
  }

  while (!fDone) {
    /* get an HDB  */

    /* search through file until an HDB is found (getXDB returns 2) */
    countdb = 0;
    while ((result = getXDB(abXDB,&msn)) != 2) {
      /* return on end of file */
      if (result == SMSEOF) {
	RetVal = SMSEOF;
	break;
      }

      countdb++;

      if (countdb > 1000) { /* Fix: this is not necessarily fatal */
	printf("%s -I- got more than 1000 XDB's without 64 HDB's.  Near EOF? ",
	       thisprog);
	printf("  Returning...\n");
	RetVal = SMSERROR;
	break;
      }
    }

    /* for HDBs do some work, else print some messages */
    if (result == 2) {

      /* --- decode HDB and keep track of those found --- */
      hdb =  decodeHDB(abXDB);
      ++nhdb;
      if (DEBUG > 2) printf("%s -D- found HDB #%2.2d\n",thisprog,hdb.NumHDB);
      fGotHDB[hdb.NumHDB] = TRUE;

      /* --- Accumulate items over set of 64 HDBs --- */
      /* STICS measurec stepping voltages */
      /* NOTE: conversions to volts are from UMD source code */
      dchdb.tMeasPosStepTab_Raw[hdb.NumHDB] = hdb.tmpDPPShv ;
      dchdb.tMeasPosStepTab[hdb.NumHDB] = 
	2.9562 * dchdb.tMeasPosStepTab_Raw[hdb.NumHDB] - 24.53;

      dchdb.tMeasNegStepTab_Raw[hdb.NumHDB] = hdb.tmpDPPShv ;
      dchdb.tMeasNegStepTab[hdb.NumHDB] = 
	-2.9791 * dchdb.tMeasNegStepTab_Raw[hdb.NumHDB] - 24.22;

      /* MASS measured stepping voltages */
      dchdb.mMeasStepTab_Raw[hdb.NumHDB] = hdb.r[96];
      dchdb.mMeasStepTab[hdb.NumHDB] = 
	1.0 * dchdb.mMeasStepTab_Raw[hdb.NumHDB] + 0.0;
      /*  6.66 is MASS analyzer constant from T. Zurbuchen, 11Oct2000 */

      /* Fred string */
      fredstring[hdb.NumHDB] = hdb.r[231];
      /* Two methods of keeping the end clean */
      if (1) {
	/* Simon's */
	if( hdb.NumHDB == 63 ) fredstring[64] = 0;
      }
      else if (0) {
	/* Mine */
	if ( hdb.NumHDB == 0 ) strcpy(fredstring, "\0");
      }

      //fredstring[hdb.NumHDB+1] = '\0';
      if (0) printf("%s -I- %c FS: %s\n",thisprog,hdb.r[231],fredstring);

      //smssetdbg(0); /* set debug level back to 0 */
    }
    else {
      if (DEBUG > 2) printf("%s -D- DBG -- getXDB returned %d\n",thisprog,result);
    }
  
    /* finished when we get each of the 64 HDBs */
    i = 0;
    if (RetVal != SMSSUCCESS){
      fDone = FALSE;
    }
    else {
      do {
	fDone = fGotHDB[i++];
      } while (fDone && (i < 64));
    }
  }

  /* print some stuff */
  if (TRACE) printf("%s -I- FS: %s\n",thisprog,fredstring);

  if (0) {
    printf("%s -I- FS: dchdb.tMeasPosStepTab=",thisprog);
    for (i = 0; i < 64; i++)
      printf(" %6.2f",dchdb.tMeasPosStepTab[i]);
    printf("\n");
  }

  /* copy out results */
  if (RetVal == SMSSUCCESS) cycle.decomm_hdb = dchdb;

  /* return file pointer to original positon */
  fseek(pInfile, OldFilePos, SEEK_SET);
  if (DEBUG > 2) printf("%s -D- OutgoingFilePos=%d\n",thisprog,ftell(pInfile));

  return (RetVal);
}
