/** \file fixReadoutDelay.c
    \brief Copy data items from future EDBs (1 or 2 later) in cycle 
   to put items in the EDB in which they were accumulated; they are originally
   in the EDB in which they are read out.
*/
/*
   WARNING: only works for low bit rate mode!
   10Jun2001 --> Roughly implemented for high bit rate mode as well!

   Input/ouput
   --> nEdbInCycle is number of EDB in cycle.  This is built in for eventual
   support of incomplete cycles.
   --> cycle.edb[2-61] and cycle.h_dc[2-61] these are global arrays of 
   structures that are the sources
   <-- cycle.edb[0-59] and cycle.h_dc[0-59] same as above but these are 
   the destinations
*/
/*
   Author: Jim Raines, 14Feb00

   See libsmsInit for further documentation.

   This file is controlled by the Concurent Version System (CVS):
   $Id: fixReadoutDelay.c,v 1.6 2005/11/29 19:22:11 jfeeman Exp $

   Modification History:

*/

#include "libsms.h"

/** local debugging flag only for debugging this routine */
#define LDEBUG FALSE

/**
   Method: Point pFrom at source region, i.e. tpha[0], in proper EDB 
   and point pTo at same region in later EDB (1 or 2 spins, depending on
   region).  Copy source to dest. by advancing pointers.  Note: sizes of each
   region hard-coded in from EDB structure in libsms.h.  Comments in code
   describe number of EDBs ahead that each region is read out ahead
   compared to when it was accumulated.

   Here are the elements that are affected:
   <table>
   <tr><td> <b> item </b> </td><td> <b> N spins later </b> </td></tr>
   <tr><td colspan='2'> <b> STICS: </b> </td></tr>
   <tr><td> PHA </td><td> 1 </td></tr>
   <tr><td> corHR, SMR, BR </td><td> 1 </td></tr>
   <tr><td> OMR </td><td> 2 </td></tr>
   <tr><td> ER </td><td> 2 </td></tr>

   <tr><td colspan='2'> <b> SWICS: </b> </td></tr>
   <tr><td> PHA </td><td> 1 </td></tr>
   <tr><td> ER </td><td> 2 </td></tr>
   <tr><td colspan='2'> (other elements do not work) </td></tr>

   <tr><td colspan='2'> <b> MASS: </b> </td></tr>
   <tr><td> PHA </td><td> 1 </td></tr>
   <tr><td> corBR, cor-secER </td><td> 1 </td></tr>
   <tr><td> MR </td><td> 2 </td></tr>
   <tr><td> spinER </td><td> 2 </td></tr>

   </table>

   'cor' stands for corrected, as opposed to erroneous (which are?) <br>
   'sec' sector oriented <br>
   'spin' spin oriented

   SWICS matrix elements are actually transmitted two cycles later but
   since they don't work it doesn't matter.
*/
/* 
   Here are the elements that are affected: (without HTML table tags)

       item			N spins later
   STICS:
       PHA			1	
       corHR,SMR,BR		1
       OMR			2
       ER			2
   SWICS:
       PHA			1
       ER			2
	(other elements do not work)
   MASS:
       PHA			1
       corBR,cor-secER		1
       MR			2
       spinER			2
*/
int fixReadoutDelay(int nEdbInCycle){
  int i,j; /* short range loop counters */
  char thisprog[] = "fixReadoutDelay"; /* name of this routine */

  int nedb; /* number in cycle of EDB being processed */
  BYTE *pFrom; /* pointers for copying, pFrom points to source */
  BYTE *pTo; /* pointers for copying, pTo points to destination */

  if (TRACE)
    printf("%s -I- moving data items back to EDB in which they were collected.\n",
	   thisprog);

  for (nedb = 0; nedb < nEdbInCycle; nedb++){
    /*********/
    /* SWICS */
    /*********/
    /* ER --  read out 2 EDBs later than accumulated */
    /*-----------------------------------------------*/
    /*++ EDB structure to use depends on whether its in high or low bit rate mode ++*/
    if(isHigh == 0) {
      pFrom = &cycle.edb[nedb + 2].xfsr;
      pTo = &cycle.edb[nedb].xfsr;
    }
    else if(isHigh == 1) {
      pFrom = &cycle.edb_r_h[nedb + 2].xfsr;
      pTo = &cycle.edb_r_h[nedb].xfsr;
    }
    for (i = 0; i < 6; i++)
      *pTo++ = *pFrom++;

    /* PHA -- read out 1 EDB later than accumulated */
    /*----------------------------------------------*/
    /*++ Low bit rate mode ++*/
    if(isHigh == 0) {
      pFrom = &cycle.edb[nedb + 1].xpha[0];
      pTo = &cycle.edb[nedb].xpha[0];

      for (i = 0; i < 324; i++)
	*pTo++ = *pFrom++;
    }

    /*++ High bit rate mode ++*/
    else if(isHigh == 1) {
      pFrom = &cycle.edb_r_h[nedb + 1].xpha[0];
      pTo = &cycle.edb_r_h[nedb].xpha[0];

      for (i = 0; i < 672; i++)
	*pTo++ = *pFrom++;
    }

    /* change number of PHAs */
    cycle.h_dc[nedb].xnpha = cycle.h_dc[nedb + 1].xnpha; 


    /*********/
    /* STICS */
    /*********/ 
    /* corrected HR, SMR and BR -- read out 1 EDB later than accumulated */
    /*-------------------------------------------------------------------*/
    /*++ Depends on whether its in high or low bit rate mode ++*/
    if(isHigh == 0) {
      pFrom = &cycle.edb[nedb + 1].thmr[0];
      pTo = &cycle.edb[nedb].thmr[0];
    }
    else if(isHigh == 1) {
      pFrom = &cycle.edb_r_h[nedb + 1].thmr[0];
      pTo = &cycle.edb_r_h[nedb].thmr[0];
    }
    for (i = 0; i < 144; i++)
      *pTo++ = *pFrom++;

    /* OMR -- read out 2 EDBs later than accumulated */
    /*-----------------------------------------------*/
    /*++ Depends on whether its in high or low bit rate mode ++*/
    if(isHigh == 0) {
      pFrom = &cycle.edb[nedb + 2].tomr[0];
      pTo = &cycle.edb[nedb].tomr[0];
    }
    else if(isHigh == 1) {
      pFrom = &cycle.edb_r_h[nedb + 2].tomr[0];
      pTo = &cycle.edb_r_h[nedb].tomr[0];
    }
    for (i = 0; i < 20; i++)
      *pTo++ = *pFrom++;

    /* ER --  read out 2 EDBs later than accumulated */
    /*-----------------------------------------------*/
    /*++ Depends on whether its in high or low bit rate mode ++*/
    if(isHigh == 0) {
      pFrom = &cycle.edb[nedb + 2].tfsr[0];
      pTo = &cycle.edb[nedb].tfsr[0];
    }
    else if(isHigh == 1) {
      pFrom = &cycle.edb_r_h[nedb + 2].tfsr[0];
      pTo = &cycle.edb_r_h[nedb].tfsr[0];
    }
    for (i = 0; i < 15; i++)
      *pTo++ = *pFrom++;

    /* PHA -- read out 1 EDB later than accumulated */
    /*----------------------------------------------*/
    /*++ Depends on whether its in high or low bit rate mode ++*/
    if(isHigh == 0) {
      pFrom = &cycle.edb[nedb + 1].tpha[0];
      pTo = &cycle.edb[nedb].tpha[0];

      for (i = 0; i < 296; i++)
	*pTo++ = *pFrom++;
    }

    else if(isHigh == 1) {
      pFrom = &cycle.edb_r_h[nedb + 1].tpha[0];
      pTo = &cycle.edb_r_h[nedb].tpha[0];
      
      for (i = 0; i < 608; i++)
	*pTo++ = *pFrom++;
    }

    /* change number of PHAs */
    cycle.h_dc[nedb].tnpha = cycle.h_dc[nedb + 1].tnpha; 





    /**********************************/
    /* MASS */
    /* See page 288 of the WIND SMS manual */
    /**********************************/

    /* PHA -- read out 1 EDB later than accumulated */
    /*----------------------------------------------*/
    /*++ Low bit rate mode ++*/
    if(isHigh == 0) {
      pFrom = &cycle.edb[nedb + 1].mpha[0];
      pTo = &cycle.edb[nedb].mpha[0];

      for (i = 0; i < 362; i++)
	*pTo++ = *pFrom++;
    }

    /*++ High bit rate mode ++*/
    else if(isHigh == 1) {
      pFrom = &cycle.edb_r_h[nedb + 1].mpha[0];
      pTo = &cycle.edb_r_h[nedb].mpha[0];

      for (i = 0; i < 762; i++)
	*pTo++ = *pFrom++;
    }
    /* change number of PHAs */
    cycle.h_dc[nedb].mnpha = cycle.h_dc[nedb + 1].mnpha; 

    /* Corrected basic rates and sectored engineering rates - read out
       1 EDB later than accumulated */
    /***********************************************************/
    if(isHigh == 0) {
      pFrom = &cycle.edb[nedb + 1].mufsr_s;
      pTo = &cycle.edb[nedb].mufsr_s;
    }
    else if(isHigh == 1) {
      pFrom = &cycle.edb_r_h[nedb + 1].mufsr_s;
      pTo = &cycle.edb_r_h[nedb].mufsr_s;
    }
    for (i = 0; i < 11; i++)
      *pTo++ = *pFrom++;
    
    /* matrix rates - read out 2 EDBs later than accumulated */
    /***************************************************/
    if(isHigh == 0) {
      pFrom = &cycle.edb[nedb + 2].mmr[0];
      pTo = &cycle.edb[nedb].mmr[0];
    }
    else if(isHigh == 1) {
      pFrom = &cycle.edb_r_h[nedb + 2].mmr[0];
      pTo = &cycle.edb_r_h[nedb].mmr[0];
    }
    for (i = 0; i < 10; i++)
      *pTo++ = *pFrom++;
    
    /* spinER - read out 2 EDBs later than accumulated */
    /***************************************************/
    /* Unsectored engineering rates: FSR 1, FSR 2, FSR B, MDCR */
    if(isHigh == 0) {
      pFrom = &cycle.edb[nedb + 2].mfsr[0];
      pTo = &cycle.edb[nedb].mfsr[0];
    }
    else if(isHigh == 1) {
      pFrom = &cycle.edb_r_h[nedb + 2].mfsr[0];
      pTo = &cycle.edb_r_h[nedb].mfsr[0];
    }
    for (i = 0; i < 4; i++)
      *pTo++ = *pFrom++;    


  } /* loop over number of EDBs in cycle */  
  return (SMSSUCCESS);
}
