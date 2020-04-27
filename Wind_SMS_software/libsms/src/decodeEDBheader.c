/** \file  decodeEDBheader.c
    \brief Decode EDB header into detailed structure
*/
/**
   Description: Uses bit manipulation and some knowledge of values
   (from SMS DPU manual) to decode header into more useful values.
*/
/*
   Author: Jim Raines, 29Nov99

   Notes:
   See definition of h_dc structure for explanation of everything.
   Only items used later are currently decoded!

   See libsmsInit for further documentation.

   This file is controlled by the Concurent Version System (CVS):
   $Id: decodeEDBheader.c,v 1.5 2005/11/29 19:22:11 jfeeman Exp $

   Modification history:

   In decoding of mnpha, multiplied byte 8 * 2 because it is supposed to be 
   bits 8-1 of this number.
   J. Raines, 25Jan00

*/

#include "libsms.h"

EDB_H_DC decodeEDBheader(BYTE h[]) {
  /* NOTE:  Only items used later are currently decoded! */
  int i,j; /* loop counters */

  char thisprog[] = "decodeEDBheader";

  EDB_H_DC h_dc; /* decoded header structure returned by this routine */

  int fSfId; /* flag determining number of bytes per subframe */

  /*********************************************/
  /* Put some info about the structure into it */
  /*********************************************/

  /* update this list when structure changes */
  char *members[] = {
    "numsf", "fIsEDB", "sfbytes", "fHighBitrate", "fMPresent",
    "fRamCheckStat", "msn", "revcount", "xnpha", "tnpha", "fValidCom",
    "fComSemErr", "fComSynErr", "fMCovPowOn", "fXHeatPowOn", "fXCovPowOn",
    "fTHeatPowOn", "fTCovPowOn", "mnpha", "fMDischargeB", "fRuntimeErr",
    "fXDAPSdischarge", "RemProcTime", "fMHPSstpflg", "fMSPMCPstpflg",
    "fMSTMCPstpflg", "fXPAPSstpflg", "fXSPMCPstpflg", "fXSTMCPstpflg",
    "fTMCP3456stpflg", "fTMCP012stpflg"};

  /* update this when structure and members change */
  h_dc.nmembers = 31;

  for (i = 0; i < h_dc.nmembers; i++) {
    h_dc.members[i] = members[i];
  }

  /**************************/
  /* Fill Decoded Structure */
  /**************************/
  h_dc.numsf = getBits(h[2],0,4);
  if(DEBUG > 2) printf("%s -I- h_dc.numsf: %d", thisprog, h_dc.numsf);
  
  h_dc.fIsEDB = getBits(h[2],5,5);
  if(DEBUG > 2) printf("  -  h_dc.fIsEDB: %d", h_dc.fIsEDB);

  /* number of bytes per subframe */
  fSfId = getBits(h[2],6,6);
  if (fSfId) h_dc.sfbytes = 40;
  else h_dc.sfbytes = 37;

  /* bit rate */
  h_dc.fHighBitrate = getBits(h[2],7,7);
  //if(isHigh != h_dc.fHighBitrate)
  //printf("%s -W- 'getXDB' says bit rate mode is %d but '%s' says bit rate mode is %d.  Memory may be corrupted!!\n", thisprog, isHigh, thisprog, h_dc.fHighBitrate);
  if(h_dc.fHighBitrate == 1) {
    isHigh = 1;
    if(DEBUG > 2) printf(" - In high bit rate mode (%d)!!\n", h_dc.fHighBitrate);
  }
  else {
    isHigh = 0;
    if(DEBUG > 2) printf(" - In low bit rate mode (%d)\n", h_dc.fHighBitrate);
  }

  /* mass sensor present */
  h_dc.fMPresent = getBits(h[3],0,0);

  /* RAM check status */
  h_dc.fRamCheckStat = getBits(h[3],1,1);

  /* measurement spin number */
  h_dc.msn = getBits(h[3],2,7);

  /* revolution spin counter */
  h_dc.revcount = h[4];

  /* number x pha words */
  h_dc.xnpha = h[5];

  /* number t pha words */
  h_dc.tnpha = h[6];

  /* HK stuff -- mostly cover/heater power */
  h_dc.fValidCom = getBits(h[7],7,7);
  h_dc.fComSemErr = getBits(h[7],6,6);
  h_dc.fComSynErr = getBits(h[7],5,5);
  h_dc.fMCovPowOn = getBits(h[7],4,4);
  h_dc.fXHeatPowOn = getBits(h[7],3,3);
  h_dc.fXCovPowOn = getBits(h[7],2,2);
  h_dc.fTHeatPowOn = getBits(h[7],1,1);
  h_dc.fTCovPowOn = getBits(h[7],0,0);

  /* HK stuff */
  h_dc.fTCovPowOn = getBits(h[9],7,7);
  h_dc.fMDischargeB = getBits(h[9],6,6);
  h_dc.fRuntimeErr = getBits(h[9],5,5);
  h_dc.fXDAPSdischarge = getBits(h[9],4,4);
  h_dc.RemProcTime = getBits(h[9],0,3);

  /* number m pha words */
  h_dc.mnpha = h[8]*2 + getBits(h[9],7,7);

  /* HK stuff -- high voltage stepping flags */
  h_dc.fMHPSstpflg = getBits(h[10],7,7);
  h_dc.fMSPMCPstpflg = getBits(h[10],6,6);
  h_dc.fMSTMCPstpflg = getBits(h[10],5,5);
  h_dc.fXPAPSstpflg = getBits(h[10],4,4);
  h_dc.fXSPMCPstpflg = getBits(h[10],3,3);
  h_dc.fXSTMCPstpflg = getBits(h[10],2,2);
  h_dc.fTMCP3456stpflg = getBits(h[10],1,1);
  h_dc.fTMCP012stpflg = getBits(h[10],0,0);

  if (DEBUG > 2) {
    printf("%s: ",thisprog);
      printf("%s=%3.3d ",h_dc.members[8],
	     h_dc.xnpha);
      printf("%s=%3.3d ",h_dc.members[9],
	     h_dc.tnpha);
      printf("%s=%3.3d ",h_dc.members[18],
	     h_dc.mnpha);
      printf("totpha=%3.3d",h_dc.xnpha + h_dc.tnpha +
	     h_dc.mnpha);
      printf("\n");
  }
    
  return(h_dc);
}

