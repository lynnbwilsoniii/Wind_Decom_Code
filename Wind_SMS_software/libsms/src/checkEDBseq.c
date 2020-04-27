/** \file checkEDBseq.c
    \brief Loops through EDBs and checks for "bad" EDBs
*/
/**
   Description: Loops through the 61 EDBs of the current cycle looking
   for those that are out of sequence or haven't been read in.  It
   then marks the appropriate EDBs with EDBQuality (EQ) flags and
   returns true if at least one EDB was bad.
*/

/*
   Author: Jim Raines, 27Mar00

   Requires:(--> in; <-- out) 
     Globals: 
     -->cycle.h_dc[].msn
     <--cycle.EDBQuality[] 

   See libsmsInit for further documentation.

   This file is controlled by the Concurent Version System (CVS):
   $Id: checkEDBseq.c,v 1.2 2005/11/29 19:22:11 jfeeman Exp $

   Modification History: (keep this last section)

*/

#include "libsms.h"

int checkEDBseq(){
  int i,j;  /* short range loop counters */
  char thisprog[] = "checkEDBseq"; /* name of this program */
  
  int offset; /* handles msn = 0 at EDB[60] condition */

  /* flags */
  int fHasBadEDB; /* T ==> bad edb present in cycle */
  int fBP1; /* T ==> mark EDB with EQBADPLUS1 */
  int fBP2; /* T ==> mark EDB with EQBADPLUS2 */

  fHasBadEDB = FALSE; /* init to be sure */
  fBP1 = FALSE;
  fBP2 = FALSE;

  for (i = 0; i < 60; i++){
    /* check sequence - look ahead two for each */
    if (cycle.h_dc[i].msn != -1){

      /**** check current EDB + 1 ****/

      /* out of sequence */
      if (i >= 59) /* handle h_dc[60].msn == 0 and h_dc[61].msn == 1 */
	offset = 60;
      else
	offset = 0;

      if (cycle.h_dc[i + 1].msn != (i + 1 - offset))
	fBP1 = TRUE;

      /* section we need is bad */
      if (getBits(cycle.EDBQuality[i + 1],EQBADPLUS1bit,EQBADPLUS1bit))
	fBP1 = TRUE;

      /* whole thing is suspect */
      if (getBits(cycle.EDBQuality[i + 1],EQALLSUSbit,EQALLSUSbit))
	fBP1 = TRUE;

      /* whole thing is bad */
      if (getBits(cycle.EDBQuality[i + 1],EQALLBADbit,EQALLBADbit))
	fBP1 = TRUE;

      /* finally do the actual bad marking */
      if (fBP1)
	cycle.EDBQuality[i] = 
	  cycle.EDBQuality[i] | EQBADPLUS1;

      /**** check current EDB + 2 ****/

      /* out of sequence */
      if (i >= 58) /* handle h_dc[60].msn == 0 and h_dc[61].msn == 1 */
	offset = 60;
      else
	offset = 0;

      if (cycle.h_dc[i + 2].msn != (i + 2 - offset))
	fBP2 = TRUE;

      /* section we need is bad */
      if (getBits(cycle.EDBQuality[i + 2],EQBADPLUS2bit,EQBADPLUS2bit))
	fBP2 = TRUE;
      /* whole thing is suspect */
      if (getBits(cycle.EDBQuality[i + 2],EQALLSUSbit,EQALLSUSbit))
	fBP2 = TRUE;
      /* whole thing is bad */
      if (getBits(cycle.EDBQuality[i + 2],EQALLBADbit,EQALLBADbit))
	fBP2 = TRUE;

      /* finally do the actual bad marking */
      if (fBP2) cycle.EDBQuality[i] = cycle.EDBQuality[i] | EQBADPLUS2;
	
    }
    else { /* this EDB wasn't really read */
      cycle.EDBQuality[i] = cycle.EDBQuality[i] | EQALLBAD;
    }

    if (cycle.EDBQuality[i]){
      fHasBadEDB = TRUE;
    }
  }

  return(fHasBadEDB);
}
