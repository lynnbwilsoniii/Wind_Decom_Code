/** \file smsReadCycle.c
    \brief Read one cycle of 60 EDB and 1 HDB from WIND/SMS data file
*/
/*
   Author: Jim Raines (jraines)
   Created: 22Oct99

   Requires:  

   References:
   1.  Data Format Control Document (DFCD) between ISTPa and MOSDD ...
       Rev. 2, May 1996 -- hereafter referred to as DFCD or ref. 1
   2.  Commands and Data Formats for the SMS Instrument
       Rev. 3, 05/07/94 (essentially the DPU Users Guide)
                        -- hereafter referred to as DPUUG or ref. 2

   See libsmsInit for further documentation.

   This file is controlled by CVS:
   $Id: smsReadCycle.c,v 1.9 2005/11/29 19:22:11 jfeeman Exp $

   Modification history:
     Re-worked to handle reading next two EDBs of next cycle for 
     fixReadoutDelay.  J. Raines, 14Feb00
 
     Added EDB quality checking.  J. Raines, 3Mar00.

     Revised EDB quality checking to only set bits not already set.  
     J. Raines, 15Mar00

     Extensive modifications to read loop, logic and resetting of
     items, so incomplete and/or partially bad cycles could be kept as an
     option, but are discarded by default.  J. Raines, 16Mar00

 */

#include "libsms.h"

int smsReadCycle(){
  int i,j,k,l; /* short-range loop counters */
  int imax; /* short-range loop max. */

  /* initialize local variables, (mostly) in order used */
  char thisprog[] = "smsReadCycle"; /* name of this program */

  int dbtype; /* type of data block found (see end of getXDB for poss. val.*/

  BYTE abXDB[800];  /* H/EDB byte array filled by getXDB */
  BYTE abTmpXDB[800];  /* Temporary H/EDB byte array for printing */
  int msn; /* measurement spin number -- returned by getXDB */
  int spin; /* msn + offset to allow for read ahead to EDBs 0 and 1 next cyc.*/
  int lastspin; /* last measurement spin number (previous msn) */
  EDB_H_DC h_dc; /* decoded EDB header structure (comes out of fillStructEDB
		    and goes right into cycle.h_dc[msn] */
  short quality; /* local EDB quality returned by fillStructEDB and
                    assigned unaltered to (global) EDBQuality */

  int tnphatot = 0; /* trace: total STICS pha words found this cycle */
  int xnphatot = 0; /* trace: total SWICS pha words found this cycle */
  int mnphatot = 0; /* trace: total MASS pha words found this cycle */


  int offset; /* offset for reading two EDBs beyond HDB */
  static DRH ra_drh; /* read ahead DRH */
  int nedb_beyond;  /* number of EDBs found beyond HDB */ 

  /* FIX?, put these in block with related code */
  int dumpEDBcpl; /* characters per line for debugging EDB dump */
  int dumpEDBnlines; /* number of lines for debugging EDB dump */

  /* (local) flags -- all but fDone initialized in while loop below */
  int fDone = FALSE; /* T ==> an acceptable cycle read; routine is done */
  int fGotHDB; /* T/F; T when a whole cycle has been read */
  static int fGotTwoMore = FALSE; /* T when two EDBs beyond HDB are found */
  int fCompCyc; /* complete cycle flag; T ==> complete */
  int fHasBadEDB; /* T ==> bad edb present in cycle */

  int previous = -1;  /* to keep track of bit rate of previous EDB while testing */
  int mixedBitrate = -1;  /* 0 if all EDBs in cycle are one bit rate - 1 if there are both high and low bit rates in cycle.  If this is the case, mark all EDBs in that cycle with the EQMIXEDBRC flag. */

  if (TRACE || (DEBUG > 2)) printf("\n%s -I- Reading cycle...\n",thisprog);
  
  /*************************************************************************
   loop until acceptable cycle is found (handling missing or bad EDBs
   as per set options
  **************************************************************************/
  while (!fDone){

    /********************/
    /* reset everything */
    /********************/
    /* move EDBs which were read-ahead to their proper place in the 
     now-current cycle. */
    if (fGotTwoMore){ /* two more EDBs were found (hopefully) */

      /* copy items that were read-ahead into positions */
      /*++ Does there need to be a condition here to decide whether cycle.edb or cycle.edb_r_h is filled?? Bit rate mode has not been determined yet. ++*/
      if(isHigh == 0)  //for low bit rate mode
	cycle.edb[0] = cycle.edb[60];
      else if(isHigh == 1) //for high bit rate mode
	cycle.edb_r_h[0] = cycle.edb_r_h[60];
      cycle.h_dc[0] = cycle.h_dc[60];
      cycle.EDBQuality[0] = cycle.EDBQuality[60];

      if(isHigh == 0)
	cycle.edb[1] = cycle.edb[61];
      else if(isHigh == 1)
	cycle.edb_r_h[1] = cycle.edb_r_h[61];
      cycle.h_dc[1] = cycle.h_dc[61];
      cycle.EDBQuality[1] = cycle.EDBQuality[61];

      cycle.drh = ra_drh;

      /* reset rest of EDB quality flags to zero and mark msn with init value*/
      for (i = 2; i < 62; i++){
	cycle.EDBQuality[i] = 0;
	cycle.h_dc[i].msn = -1; /* -1 marks as init. value (not a real one) */
      }

      /* now have first two EDBs already */
      cycle.nedb = 2;
      
      /* set this or quality checks (below) will think this is out of seq. */
      lastspin = 2;
    }
    else{ /* new cycle (I hope) */
      
      /* set EDB quality flags to zero and mark msn with init value */
      for (i = 0; i < 62; i++){
	cycle.EDBQuality[i] = 0;
	cycle.h_dc[i].msn = -1; /* -1 marks as init. value (not a real one) */
      }

      /* no edbs have been collected */
      cycle.nedb = 0; 

      /* so we know this is first time through */
      lastspin = -1;
    }    

    /* initialize flags and variables */

    offset = 0;           /* start at EDB 0 again for msn = 0 */
    nedb_beyond = 0;      /* reset */

    /* reset pha totals */
    tnphatot = 0;
    xnphatot = 0;
    mnphatot = 0;

    /* reset flags */
    fGotHDB = FALSE;
    fGotTwoMore = FALSE;
    fCompCyc = FALSE;
    fHasBadEDB = FALSE;

    previous = -1;
    mixedBitrate = -1;


    /*************************************************************************
     get H/EDBs until a full cycle is found, storing the EDBs in the
     global cycle structure along the way.
    **************************************************************************/
    while (!(fGotHDB && fGotTwoMore) && (cycle.nedb <= 62)) {
      /* Note: The first two flags *are* necessary.  The RHS
	 condition will not handle it all.  For cycles with gaps, it could
	 cause subsequent cycle(s) to get out of phase -- reading 62
	 from the middle of one cycle to the middle of another. */

      /* get an H/EDB */
      if (dbtype == 3) /* if last was EDB; msn only read then anyway */ 
	lastspin = msn + offset;

      dbtype = getXDB(abXDB, &msn);
      spin = msn + offset;

      /* test to see what was found and operate accordingly */
      if (dbtype == 3) { /* got EDB */
	cycle.nedb++;

	/* handle read-ahead */
	if (fGotHDB && !fGotTwoMore){
	  if (DEBUG > 2)
	    printf("%s -D- reading ahead, cycle.nedb=%d, msn=%d\n",
		   thisprog,cycle.nedb,spin);
	}

	/* toggle flag when 62 EDBs found (60 for cycle + 2 read-ahead)*/
	if (fGotHDB) {
	  nedb_beyond++;
	  if (nedb_beyond == 2)
	    fGotTwoMore = TRUE;
	}

	/* assign first data record header (drh) as the one to represent this
	   cycle  */
	if ((spin) == 0){
	  cycle.drh = drh;
	}
	else if ((spin) == 60){
	  /* get drh for EDB[60] (EDB[0] of next cycle) just in case it changes
	     by the time get it after we read EDB[2] of the next cycle */
	  ra_drh = drh;
	}	

	/* DEBUGGING -- save XDB -- for dumping at very high debug levels */
	if (DEBUG > 3) {
	  for (i = 0; i< 800; i++)
	    abTmpXDB[i] = abXDB[i];
	}

	/* Fill structure  */
	if (spin > 61){ /* check if in bounds */
	  if (TRACE) {
	    printf("%s -W- warning -- EDB[%2.2d] found after EDB[%2.2d]. ",
		   thisprog,spin, lastspin);
	  }

	  msn = 0;
	  spin = 0;
	  if (TRACE) printf("%s -I- Setting spin to %2.2d.\n",thisprog,spin);
	  quality = quality | EQALLBAD;  /* set EQALLBAD bit */
	}
	else
	  quality = 0; /* init. so prev. values are not passed in to fSE */

	/*++ Here, if its high bit rate mode, call a different
	  'fillStructEDB' to handle high bit rate mode ++*/
	if(isHigh == 0) {
	  //printf("%s -I- Calling 'fillStructEDB'\n", thisprog);
	  cycle.edb[spin] = fillStructEDB(abXDB, &h_dc, quality);
	}
	else if(isHigh == 1) {
	  //printf("%s -I- Calling 'fillStructEDB_R_H'\n", thisprog);
	  cycle.edb_r_h[spin] = fillStructEDB_R_H(abXDB, &h_dc, quality);
	}
	cycle.h_dc[spin] = h_dc;
	/* Check if there are both high and low bit rate mode EDBs in
           this cycle. If there is, set 'mixedBitrate' variable, and
           then after the cycle is complete, mark all EDBs with the
           EQMIXEDBRC flag. */
	if(previous == -1) {
	  previous = cycle.h_dc[spin].fHighBitrate;
	}
	else if( (mixedBitrate != 1) && (cycle.h_dc[spin].fHighBitrate == previous) ) {
	  previous = cycle.h_dc[spin].fHighBitrate;
	  mixedBitrate = 0;
	}
	else if( (mixedBitrate != 1) && (cycle.h_dc[spin].fHighBitrate != previous) ) {
	  previous = cycle.h_dc[spin].fHighBitrate;
	  mixedBitrate = 1;
	}
	
	  

	cycle.EDBQuality[spin] = 
	  cycle.EDBQuality[spin] | quality ;

	/* Check for unsupported STICS diagnostic mode */
	if (smsgtmode(spin)){
	  printf("%s -W- warning -- STICS in Diagnostic mode",
		 thisprog,msn);
	  printf(" -- STICS EDB[%2.2d] OMRs and FSRs are re-defined.",msn);
	  printf("  See smsgtmode docs for details.\n");
	}


	/*++ This part would be changed once it IS supported in high bit rate mode. ++*/
	if (cycle.h_dc[spin].fHighBitrate){
	  if (TRACE){
	    printf("%s -W- warning -- ",thisprog);
	    //printf("WIND/SMS in unsupported high bit rate mode.\n");
	    printf("WIND/SMS in SHAKY, but supported, high bit rate mode.\n");
	}

	  /*++ It shouldn't be marked all bad once high bit rate mode is supported ++*/
	  //cycle.EDBQuality[spin] = 
	  //cycle.EDBQuality[spin] | EQALLBAD;
	}


	/* DEBUGGING -- Dump EDB */
	if (DEBUG > 3) {
	  printf("%s -D- dumping EDB[%3.3d]...",thisprog, spin);
	  printf("xnpha=%2.2d tnpha=%2.2d mnpha=%2.2d\n", 
		 smsgxnpha(spin),smsgtnpha(spin),
		 smsgmnpha(spin));

	  dumpEDBcpl = 10; /* set number of characters per line in EDB dump */

	  dumpEDBnlines = 800/dumpEDBcpl;/*calc. number of lines for 800b EDB*/
	  if ((800 % dumpEDBcpl) != 0) dumpEDBnlines++;/*round up if not int */

	  /* dump EDB as dumpEDBcpl characters by dumpEDBnlines lines */
	  for ( i = 0; i < dumpEDBnlines; i++){
	    printf("%3.3d-%3.3d:",i*dumpEDBcpl, i*dumpEDBcpl+(dumpEDBcpl-1));
	    for (j = 0; j < dumpEDBcpl; j++){
	      printf(" %3.3d",abTmpXDB[i*dumpEDBcpl + j]);
	    }
	    printf("\n");
	  }
	}

      
	/* collect some information about npha for each */
	if (smsgtnpha(spin) > 0) tnphatot++;
	else if (smsgxnpha(spin) > 0) xnphatot++;
	else if (smsgmnpha(spin) > 0) mnphatot++;
      

	if (DEBUG > 1) printf("%s -I- got %d EDB(s) so far\n\n",thisprog,
			      cycle.nedb);
      }
      else if (dbtype == 2) {      /* got HDB */

	/* decode HDB into (global) cycle.hdb */
	cycle.hdb = decodeHDB(abXDB);

	fGotHDB = TRUE;
	offset = 60;

	if (DEBUG > 0) {
	  printf("%s -I- got HDB --  ",thisprog);
	  printf("SciRecCount=%5.5d HDB#=%2.2d\n",cycle.hdb.SciRecCount,
		 cycle.hdb.NumHDB);
	}

	if (cycle.nedb == 60){ /* 60 EDB + 1 HDB = 1 cycle */
	  fCompCyc = TRUE;
	}
	else {
	  fCompCyc = FALSE;
	}
      }
      else if (dbtype == SMSEOF) {
	/* end of file */
	return (SMSEOF);
      }
      else {
	/* unclear return condition */
	if (DEBUG > 0) 
	  printf("%s -W- warning -- getXDB returned without finding an H/EDB\n",
		 thisprog);
      }
    } /* loop to get full cycle */



    /********************/
    /* Apply some fixes */
    /********************/
    /** Note: These are done *before* checking for badEDBs because
        they may mark some EDBs bad.  **/

    /* fix STICS core commutation when in low bit rate mode */
    if (cycle.h_dc[0].fHighBitrate == 0) {
      fixTcore(cycle.nedb);
    }

    /* move items (like PHA & rates ) from the read out spin 
     to the spin in which they were accumulated. Only currently works for
     low bit-rate mode */
    /*++ This should work now in high bit rate mode too. ++*/
    fixReadoutDelay(cycle.nedb - 2); /* FIX: cycle.nedb passes in 62 which is bad */


    /*******************************************************/
    /* Mark all EDBs in cycle if there are high AND low bit rate modes */
    /* This is not working correctly!! It thinks that some cycles have mixed bit rate modes when they actually don't!!! */
    /*********************************************************/
    //    for(i=0; i<cycle.nedb; i++) {
    //    if(previous == -1) 
    //previous = cycle.h_dc[i].fHighBitrate;
      
    //else if( (mixedBitrate != 1) && (cycle.h_dc[i].fHighBitrate == previous) ) {
    //previous = cycle.h_dc[i].fHighBitrate;
    //mixedBitrate = 0;
    //}

    //else if( (mixedBitrate != 1) && (cycle.h_dc[i].fHighBitrate != previous) ) {
	//	printf("HERE! fHighBitrate[%d]: %d previous: %d\n", i, cycle.h_dc[i].fHighBitrate, previous);
    //previous = cycle.h_dc[i].fHighBitrate;
    //mixedBitrate = 1;
    // }
    //}

    if(mixedBitrate == 1) {
      if(DEBUG > 2) 
	printf("%s -D- Warning: This cycle contained EDBs in both high and low bit rate mode.  Marking all EDBs with EQMIXEDBRC flag!\n", thisprog); 
      for(i=0; i<cycle.nedb; i++) 
	cycle.EDBQuality[i] = cycle.EDBQuality[i] | EQMIXEDBRC;
    }


    /***********************************************************************/
    /* Decide if this cycle should be returned or a new one should be read */
    /***********************************************************************/

    /* test to see if there was a bad EDB */
    if (cycle.nedb == 62) { /* this is true for complete cycles */
      fHasBadEDB = checkEDBseq();
    }
    else { /* if cycle isn't complete it *must* have at least one bad EDB */
      fHasBadEDB = TRUE; 
    }

    /* If there WAS a bad EDB... */
    if (fHasBadEDB){
      if (TRACE) printf("%s -W- cycle has bad EDB(s) ... ",thisprog);

      if (gfQualOver){
	fDone = TRUE;
	if (TRACE) printf("keeping.\n");
      }
      else {
	fDone = FALSE;
	if (TRACE) printf("discarding.\n");
      }
    }
    /* If there WASN'T a bad EDB... */
    else {
      fDone = TRUE;
    }

    /* test for completeness */
    if (fDone){ /* don't bother if it failed quality check (w/o override) */
      if (fCompCyc){ /* complete cycle */
	if (TRACE) 
	  printf("%s -I- complete cycle found -- %d EDB 1 HDB\n", 
		 thisprog,
		 cycle.nedb);

	fDone = TRUE;
      }
      else{
	if (TRACE)
	  printf("%s -I- INCOMPLETE cycle found  -- %d EDB 1 HDB ... ",
		 thisprog,
		 cycle.nedb);

	if (gfIncCycOver){
	  fDone = TRUE;
	  if (TRACE) printf("keeping.\n");
	}
	else{
	  fDone = FALSE;
	  if (TRACE) printf("discarding.\n");
	}
      }
    }
      
    if (fDone) { /* print some info on the way out */
      ncycle++;

      if (TRACE){
	printf("%s -I- %3.3d acceptable cycle(s) found thus far\n",
	       thisprog,ncycle);
      }
    }
    else if (TRACE) {
      /* output some info about the PHAs lost if this cycle won't be returned*/
      printf("%s -I- PHA words lost: xnpha=%d tnpha=%d mnpha=%d\n",
	     thisprog,xnphatot, tnphatot, mnphatot);
    }

  } /* loop to handle incomplete and/or (partially) bad cycles */

  return (SMSSUCCESS);
}
