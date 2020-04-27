/** \file getXDB.c
    \brief Search through minor frames to find  a WIND/SMS EDB or HDB
   then copy into global structure
*/
/**
   Method:
   Search through minor frames 0-249, reading records when necessary, looking
   for H/EDB id bytes.  When found, read copy into an array based on type 
   (H/EDB), number of subframes and number of bytes per subframe.

   returns abXDB[] byte array containing the DB found
           pMeasSpinNum the spin number pulled out of header and required for
	     an EDB to be put in its place in the cycle
	   (return value) codes what getXDB found; see end of routine for 
	     explanation

   Note:  This version has multiple returns -- every time it calls readRecord,
   it returns with SMSEOF if readRecord returns with SMSEOF.  This could be 
   changed by making readRecord zero the MinorFrame array which would cause
   getXDB to exit after not finding an H/EDB after a few major frames.  
*/
/*
   Author: Jim Raines, 22Oct99

   See libsmsInit for further documentation.

   This file is controlled by the Concurent Version System (CVS):
   $Id: getXDB.c,v 1.6 2005/11/29 19:22:11 jfeeman Exp $

   Major Modification history:

   EDBs starting at mf=249 were being missed because a new record would be read
   even after and XDB had been copied (mean that mf=249 had not been checked 
   for idents.)  Changed logic at end of routine to only read record if an XDB
   *had not* been copied (meaning this mf had been checked).
   J. Raines, 25Jan00

*/
#include "libsms.h"

int getXDB (BYTE abXDB[], int *pMeasSpinNum) {
  int i, j;
  char thisprog[] = "getXDB"; /* name of this routine for print statements */

  int fDone = FALSE; /* T/F flag for when to stop looping over minor frames */

  /* decoded header stuff --FIX: should be done with call to decodeEDBheader */
  int is_edb = FALSE; /* TRUE if current minor from is an EDB */
  int found_id = FALSE; /* TRUE if 14h and 6Fh identifiers were found */

  unsigned short  msn, /* measurement spin number (local) */
    numsf, /* number of subframes in EDB */
    noidmf = 0, /* number of minor frames without XDB id's (bytes 0 and 1) */
    sfbytes; /* number of bytes per subframe, 37 or 40 */

  /* minor/major frame variables */
  static BYTE MinorFrame[250][42]; /* minor frames; dim. from DFCD p3-39 */
  static int mf = -1; /* current minor frame in record; range 0-249
			 init. to 249 forces read of record on first entry */
  int nMFs = 0; /* number of Major Frames searched in this call
		   -- for data debugging (TRACE)*/

  EDB_H_DC h_dc; /* decoded EDB header */

  int highbitrate; /* high bit rate mode (1/0 i.e. T/F) */

  /* read record upon first call (in cycle?) */
  if (mf == -1) { 
    /* fills MinorFrame array and sets mf = 0 */
    mf = 249; /* prevent readRecord from generating error of lost data */
    if (readRecord(MinorFrame, &mf) == SMSEOF) 
      return (SMSEOF);
  }

  while (!fDone) {
    if (DEBUG > 2) printf("%s -D- ",thisprog);
    if (DEBUG > 2) printf("MF[%3.3d]",mf);

    /*********************************************************************/
    /*** Extract EDB/HDB header info if we are at beginning of EDB/HDB ***/
    /*********************************************************************/
    /* FIX? This is redundant with decodeEDBheader. */
    if ( ((MinorFrame[mf][0] == 0x14) && (MinorFrame[mf][1] == 0x6f))){
      /* EDB/HDB start markers */

      found_id = TRUE;

      if (DEBUG > 2) printf(" found idents");

      /* moving toward getting rid of this redundancy with decodeEDBheader */
      //h_dc = decodeEDBheader(MinorFrame[mf]);

      /* get number of subframes */
      numsf = (MinorFrame[mf][2] & 31); /* byte 2 bits 0-4 */
      if (DEBUG > 2) printf(" - numsf=%d",numsf);

      /* HDB or EDB ? */
      if (((MinorFrame[mf][2]/32) & 1)) { /* byte 2 bit 5 */
	if (DEBUG > 2) printf(" - HDB");
	is_edb = FALSE;
      }
      else if (! ((MinorFrame[mf][2]/32) & 1)){
	if (DEBUG > 2) printf(" - EDB");
	is_edb = TRUE;
      }
      else { /* just for debugging bit shifting; should never get here */
	if (DEBUG > 2) printf(" ERROR-byte 2 bit 5 ambiguous");
	is_edb = FALSE;
      }

      /* number of bytes per subframe, byte 2 bit 6 */
      if (! is_edb ){
	/* HDB */
	if (MinorFrame[mf][2]/64 & 1) sfbytes = 40;
	else if (!(MinorFrame[mf][2]/64 & 1)) sfbytes = 33;
      }
      else {
	/* EDB */
	if (MinorFrame[mf][2]/64 & 1) sfbytes = 40;
	else if (!(MinorFrame[mf][2]/64 & 1)) sfbytes = 37;
      }

      if (DEBUG > 2) printf(" - sfbytes=%d",sfbytes);  


      /* bit rate --  0=low, 1=high, (byte 2 bit 7) */
      //highbitrate = ((unsigned short)(MinorFrame[mf][2]/128) & 1);
      highbitrate = getBits(MinorFrame[mf][2],7,7);

      if (DEBUG > 2) printf(" - bitrate=%d",highbitrate);
      if(highbitrate == 1) 
     	isHigh = 1;
      else
	isHigh = 0;
      

      /* extract measurement spin number for voltage step */
      if (is_edb) {
	msn = getBits(MinorFrame[mf][3],2,7);
	//msn = ((MinorFrame[mf][3] & 252)/4) & 63 ; /* masks (keeps) bits 2-7 */
	if (DEBUG > 2) printf(" - msn=%d",msn);
      }
      else {
	if (DEBUG > 2) printf(" - HDB#%d",getBits(MinorFrame[mf][5],0,5));
      }
    }
    else{ /* idents not found -- maybe record-spanning H/EDB ? */
      if (DEBUG > 2) {
	printf(" idents not found");
	printf(" - MinorFrame[%3.3d] - count=%d",mf,++noidmf);
	printf("\n%s: bytes 0-11: ",thisprog);
	for (i = 0; i < 12; i++)
	  printf("%d ",MinorFrame[mf][i]);
      }

      found_id = FALSE;
    }
    if (DEBUG > 2) printf("\n");

    /***********************************/
    /*** assemble H/EDB from subframes ***/
    /***********************************/

    if (found_id && is_edb) {    /* EDB */

      /* test that is in range; out of range could cause seg. violation */
      if (numsf < 6 || numsf > 20){
	
	if (TRACE) {
	  printf("%s -W- warning -- numsf=%d is out of range;",thisprog, numsf);
	  printf(" EDB not assembled\n");
	}

	mf++; /* to keep from getting stuck on this mf */

	/* mark EDB as all bad*/
	cycle.EDBQuality[msn] = cycle.EDBQuality[msn] | EQALLBAD; 
	break;
      }

      for( i = 0; i < numsf; i++) {
	for( j = 0; j < sfbytes; j++) {
	  abXDB[i * 40 + j] = MinorFrame[mf][j];
	}

	/* increment mf for each subframe copied out (above) */
	if (mf == 249){
	  if (readRecord(MinorFrame, &mf) == SMSEOF) return(SMSEOF);
	  nMFs++; /* add one to number of Major Frames searched */

	  if ((DEBUG > 2) && (i < (numsf - 1))) 
	    printf("%s -D- EDB spans two records\n",thisprog);
	  /* numsf - 1 b/c EDB doesn't span if last subframe is mf=249 */
	}
	else {
	  mf++;
	}
      }

      fDone = TRUE;
    }
    else if(!is_edb && found_id) {     /* HDB */

      /* test that is in range; out of range could cause seg. violation */
      if (numsf != 7){

	if (TRACE) {
	  printf("%s -W- warning -- numsf=%d is out of range; HDB not assembled\n"
		 ,thisprog,numsf);
	}

	mf++; /* to keep from getting stuck on this mf */
	break; /* is this what is wanted here? */
      }

      for( i = 0; i <  numsf; i++) {
	for( j = 0; j < sfbytes; j++) {
	  abXDB[i * 40 + j] = MinorFrame[mf][j];
	}
	/* increment mf for each subframe copied out (above) */
	if (mf == 249){
	  /* read new record */
	  if (readRecord(MinorFrame, &mf) == SMSEOF) return(SMSEOF);
	  nMFs++;

	  if (DEBUG > 2) 
	    printf("%s -D- HDB spans two records\n",thisprog);
	}
	else {
	  /* just increment */
	  mf++;
	}
      }

      fDone = TRUE;
    }
    
    /* print warnings when no H/EDBs are found in a whole Major Frame  */
    /* FIX: should only print this warning once per record not each mf */
    if ((nMFs > 1) && TRACE) {
      printf("%s -W- warning -- %d major frames searched",thisprog,nMFs);
      printf(" without finding H/EDB\n");
    }

    /* read a record if at end of Major Frame (minor frame 249) *and*
     if an XDB was not found (i.e. this mf was already checked ) */
    if (!fDone) {
      if (mf == 249) {
	/* this record is done; read another */
	if (readRecord(MinorFrame, &mf) == SMSEOF) return(SMSEOF);
	nMFs++;
      }
      else {
	mf++; 
      }
    }

  } /* loop over minor frames until an H/EDB is found*/

  /* copy msn into for return */
  *pMeasSpinNum = msn;

  /* encoded return value */
  /* 3 ==> EDB, 2 ==> HDB, 1 ==> undefined, 0 ==> no H/EDB found */
  return((short)pow(2,is_edb) + found_id); 
}
