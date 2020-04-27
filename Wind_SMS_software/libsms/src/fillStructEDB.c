/** \file fillStructEDB.c
    \brief Decode EDB array into detailed structure (for low bit rate mode)
*/
/**
   Description: Intelligently copies EDB array into EDB structure and returns
   the EDB structure.
   FOR LOW BIT RATE MODE!!
*/
/*
   Author: Jim Raines, 17Nov99

   Method:
   This uses an overly complicated method of laying an array over an EDB
   structure with a union.  Pointers are then set to the beginning of this
   array and the abEDB array passed in.  Elements are copied by advancing
   the pointers.  (This routine was written early in libsms dev.  It wasn't
   realized at the time that the union with the array was un-needed.)

   Arguments:
   --> abEDB[] is byte array passed in.  It is the array of EDB bytes 
   constructed by getXDB.
   <-- h_dc is a decoded edb header structure.  This is passed out.
   (It has to be decoded here so its values can be used.)
   <-- quality is EDB quality passed out.

   See libsmsInit for further documentation.

   This file is controlled by the Concurent Version System (CVS):
   $Id: fillStructEDB.c,v 1.7 2005/11/29 19:22:11 jfeeman Exp $

   Modification history:
     Caused decoded header to be returned instead of decoding into place 
     in global cycle so that this would work with fixReadoutDelay stuff.
     J. Raines, 11Feb00

     Added testing of h_dc.fHighBitrate (0 or 1) just to be sure it
     hadn't gotten corrupted.  J. Raines, 8Mar00

     Added comments about zero-filling second 1/2 of tcore, changed
     note about methods, and removed debuging if block around
     xpha->end.  J. Raines, 9Mar00

     Added sanity tests and marking EDB quality.  J. Raines, 9Mar00.

     Revised EDB quality to only set bits not already set.  J. Raines, 15Mar00
 */
#include "libsms.h"

EDB fillStructEDB(BYTE abEDB[],EDB_H_DC *ph_dc, short quality){
  int i,j;
  int iinit; /* (temporary) initial value for loop counter */
  int imax; /* (temporary) maximum for loop counter */
  
  char thisprog[] = "fillStructEDB"; /* name of this program */

  BYTE *pabEDB; /* pointer to abEDB */  

  EDB edb_to_ret;
  
  EDB_H_DC h_dc;

  union U {
    EDB_R_L s; /* EDB raw low structure */
    BYTE a[sizeof(EDB_R_L)]; /* array to copy into */
  } edb;
  
  BYTE *pa; /* pointer to edb.a */
  BYTE *pxer; /* pointer to xer */
  
  char *xerNames[] = {"fsr","dcr","tcr","ssd1","ssd2","ssd3"};

  pa = &edb.a[0]; /* pointer to array part of union */
  pabEDB = &abEDB[0]; /* pointer to EDB array passed in */

  /*************************************************/
  /* copy into union via array -- see Method above */
  /*************************************************/

  /* header */
  for (i=0; i < 11; i++){ /* header */
    *pa++ = *(pabEDB++);
    //printf("*(pabEDB)=%3.3d (*pxa)=%3.3d\n", *(pabEDB++),*pxa++);
  }

  /* Decode EDB header into  structure */
  h_dc = decodeEDBheader(edb.a);

  /* do some sanity tests to avoid core dumps */
  /*------------------------------------------*/

  /* test bit rate flag -- As a little insurance before doing the
     pointer copying, test to be sure this one bit number is either 0
     or 1.  (This warns if it got corrupted somehow.) */
  if ((h_dc.fHighBitrate != 0) && (h_dc.fHighBitrate != 1)) {
    printf("%s -W- warning -- bit rate flag out of range (%d).",thisprog,
	   h_dc.fHighBitrate);
    printf("  Memory may be corrupted\n");
    quality = quality | EQALLSUS;
  }

  /* test npha's and set to max if above */
  if (h_dc.xnpha > 81){
    h_dc.xnpha = 81;
    /* mark EDB as all suspect if it hasn't been already */
    /* Note: This statement actually does not execute if EQALLBAD was
       set but, hey, the EDB is bad anyway, why does it matter that it
       isn't marked as suspect */
    quality = quality | EQALLSUS;
  }

  if (h_dc.tnpha > 74){
    h_dc.tnpha = 74;
    /* mark EDB as all suspect if it hasn't been already -- see Note above */
    quality = quality | EQALLSUS;
  }

  if (h_dc.mnpha > 181){
    h_dc.mnpha = 181;
    /* mark EDB as all suspect if it hasn't been already -- see Note above */
    quality = quality | EQALLSUS;
  }


  /*------------------------*/
  /* back to actual copying */

  /* xcore */
  if (h_dc.fHighBitrate) 
    imax = 116;
  else
    imax = 62;

  for (i=0 ; i < imax; i++){
    *pa++ = *(pabEDB++);
  }

  /* xpha */
  for (i=0 ; i < 4*h_dc.xnpha; i++){ /* 4 bytes per PHA word */
    *pa++ = *(pabEDB++);
  }

  /* proceed to tcore in structure */
  for (i=0 ; i < (324 - 4*h_dc.xnpha); i++){
    *pa++ = 0;       /* ^-- 4 bytes per PHA word */
  }


  /* tcore */
  if (h_dc.fHighBitrate)
    imax = 180;
  else
    imax = 90;

  for (i=0 ; i < imax; i++){ /* copy the tcore */
    *pa++ = *(pabEDB++);
  }

  if (!h_dc.fHighBitrate){      /* fill bytes 90-179 of tcore in structure  */
    for (i=0 ; i < imax; i++){  /* with 0s in lowbit rate mode because tcore */
      *pa++ = 0;                /* is only 90 bytes in low bitrate mode */
    }
  }


  /* tpha */
  for (i=0 ; i < 4*h_dc.tnpha; i++){ /* 4 bytes per PHA word */
    *pa++ = *(pabEDB++);       
  }

  /* proceed to mcore in structure */
  for (i=0 ; i < (296 - 4*h_dc.tnpha); i++){ 
    *pa++ = 0;       /* ^-- 4 bytes per PHA word */
  }
  
  /* mcore */
  for (i=0 ; i < 27; i++){
    *pa++ = *(pabEDB++);
  }

  /* mpha */
  for (i=0 ; i < 2*h_dc.mnpha; i++){ /* 2 bytes per PHA word */
    *pa++ = *(pabEDB++);
  }


  /* test bit rate flag -- As a little insurance *after* doing the
     pointer copying, test to be sure this one bit number is still
     either 0 or 1.  (This warns if it got corrupted somehow.) */
  if ((h_dc.fHighBitrate != 0) && (h_dc.fHighBitrate != 1)) {
    printf("%s -W- warning -- bit rate flag out of range (%d).",thisprog,
	   h_dc.fHighBitrate);
    printf("  Memory may be corrupted\n");
    quality = quality | EQALLSUS;
  }



  /***************************************************/
  /* copy out, print some debugging info and get out */
  /***************************************************/

  /* copy local edb.s to returned edb and local h_dc to *ph_dc */
  edb_to_ret = edb.s;
  *ph_dc = h_dc;
  
  if (DEBUG > 2) {
    pxer = &edb.s.xfsr;
    printf("%s -D- xer: ",thisprog);
    for (i=0; i < 6; i++)
      printf("%s=%3.3d ",xerNames[i],smsDecompress(*pxer++,1));
    printf("\n");
    j = *pxer++;
    printf("%s -I- xhk=%3.3d DPPSstep=%3.3d PHArange=%1.1d\n",thisprog,
	   j, getBits(j,0,5),getBits(j,6,7) );
  }

  return(edb_to_ret);
}
