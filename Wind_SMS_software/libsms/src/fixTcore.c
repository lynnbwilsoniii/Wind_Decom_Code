/** \file fixTcore.c
    \brief Fix the STICS core area by copying the two commuted halfs
   into one tcore area.
*/
/*
   Author: Jim Raines, 30Nov99

   See libsmsInit for further documentation.

   This file is controlled by the Concurent Version System (CVS):
   $Id: fixTcore.c,v 1.10 2005/12/05 18:00:32 jfeeman Exp $

   Modification History:
     Changed high bit rate message.  J. Raines, 7Mar00.

     Added marking of tcore as bad if bit rate is high.  J. Raines, 8Mar00.

     Corrected setting of EDBQuality bit.  J. Raines, 15Mar00
*/

#include "libsms.h"

/** facility for debugging even/odd edb thing and copying procedure */
#define DUMPTCORE FALSE
/** Local debug switch to show or not show special debugging messages */
#define LOCALDEBUG FALSE

/** Prototype for function below */
void dumptcore(int);
/**  Returns "(!getBits(cycle.edb[nedb].h[3],2,2))" */
int fEven(int);

/**
   Method: Loop over EDB's in cycle.  Copy bytes 0-89 of tcore
   from subsequent odd edb into bytes 90-179 of even edb, creating a
   complete tcore in even edb.  Then, to create a complete edb in the
   odd edb, copy this newly created tcore area in the even edb into
   the subsequent odd edb.
*/
int fixTcore(int maxedb){
  int i,j; /* short range loop counters */
  char thisprog[] = "fixTcore"; /* name of this routine */
  
  int nedb = 0; /* number of edb being copied */
  int fEvenLast = FALSE; /* T/F flag indicating last EDB was even */
  

  BYTE *pEven, *pOdd; /* pointer to needed parts of even and odd edb */
  BYTE *pPart1, *pPart2; /* pointers to parts 1 (even) and 2 (odd) of edb
			    used when odd is encountered first */
  if (TRACE) {
    printf("%s -I- de-commuting STICS core area -- ",thisprog);
    printf("subsequent EDB's contain same one\n");
  }


  /* loop over all EDB's in cycle */
  while (nedb < maxedb){
    
    /* double check bit rate */
    if (cycle.h_dc[nedb].fHighBitrate){
      if (TRACE) {
	printf("%s -W- warning -- called for high bit rate EDB[%2.2d]",thisprog,
	       nedb);
	printf("-- STICS core marked bad\n");
      }
      /* set EQBADTCORE bit */
      cycle.EDBQuality[nedb] = cycle.EDBQuality[nedb] | EQBADTCORE;
    }

    /* check to see if this edb is even or odd measurement spin 
       (DPU manual p281) */
    if (fEven(nedb)){ /* 1 ==> Even */

      if (LOCALDEBUG) printf("%s -D- edb[%2.2d] is even\n",thisprog,nedb);

      /* edb's should alternate even - odd ; warn otherwise */
      if (fEven(nedb+1)){
	if (TRACE) {
	  printf("%s -W- warning -- edb[%2.2d] is even ",thisprog,nedb);
	  printf("but edb[%2.2d] is even also\n",nedb + 1);
	}
      }

      if (DUMPTCORE) {
	printf("%s -I- calling dumptcore before copy\n",thisprog);
	dumptcore(nedb);
	dumptcore(nedb + 1);
      }

      /*** step 1 ***/
      /* copy tcore byte 0-89 of nedb + 1 into bytes 90-179 of this one */
      pEven = &cycle.edb[nedb].thmr[90];
      pOdd = &cycle.edb[nedb + 1].thmr[0];
      
      for (i = 0; i < 90; i++)
	*pEven++ = *pOdd++;

      /*** step 2 ***/
      /* copy tcore of this edb into next one */
      pEven = &cycle.edb[nedb].thmr[0];
      pOdd = &cycle.edb[nedb + 1].thmr[0];
      
      for (i = 0; i < 180; i++)
	*pOdd++ = *pEven++;

      if (DUMPTCORE) {
	printf("%s -I- calling dumptcore after copy\n",thisprog);
	dumptcore(nedb);
	dumptcore(nedb + 1);
      }

      /* increment edb by two because we just fixed this one and next */
      nedb += 2;

      /* set flag */
      fEvenLast = TRUE;
    }
    else { 
      if (LOCALDEBUG) printf("%s -D- edb[%2.2d] is odd\n",thisprog,nedb);

      /* got odd first so can't get a whole tcore -- instead copy bytes 0-89
	 of this (odd) tcore into end (90-179) of this (odd) tcore .
	 FIX: mark this EDB bad?
      */

      if (TRACE){
	printf("%s -W- warning -- odd edb encountered before even",thisprog);
	printf(" -- thmr[0-89] are junk\n");
      }

      if (DUMPTCORE) {
	printf("%s -I- calling dumptcore before copy\n",thisprog);
	dumptcore(nedb);
      }

      pPart1 = &cycle.edb[nedb].thmr[0]; /* beginning of odd tcore  */
      pPart2 = &cycle.edb[nedb].thmr[90]; /* byte 90 of odd tcore */
      
      for (i = 0; i < 90; i++)
	*pPart2++ = *pPart1++;

      if (DUMPTCORE) {
	printf("%s -I- calling dumptcore after copy\n",thisprog);
	dumptcore(nedb);
      }

      nedb++;
    }
      
  } /* loop over edb's in cycle */

  return (SMSSUCCESS);
}

void dumptcore(int nedb){
  int i;
  char thisprog[] = "dumptcore";

  /* print entire tcore -- for debugging this routine only */
  printf("%s -D- EDB[%2.2d] tcore bytes 0-179:\n",thisprog,nedb);

  for (i = 0; i < 180; i++) {
    printf("%3.3d ",cycle.edb[nedb].thmr[i]);
    if (i == 89 || i == 179 ) {
      printf("byte%3.3d bits0-5=%2.2d\n",i,
	     getBits(cycle.edb[nedb].thmr[i],0,5));
    }
  }

  printf("\n");
}
int fEven(int nedb){
  return (!getBits(cycle.edb[nedb].h[3],2,2));
}
