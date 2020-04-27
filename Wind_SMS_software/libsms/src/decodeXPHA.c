/** \file decodeXPHA.c
    \brief Decode SWICS PHA word into detailed structure
*/
/**
   Description: Breaks double XPHA word into component parts
   and returns tpha structure.  (where x/X stands for SWICS)
   See DPU manual, pages 294-5.
*/
/*
   Author: Jim Raines, 2Dec99

   Method: 
   The 4 byte PHA word is first assembled then taken apart into various 
   components as described on p293-4 in the DPU manual.

   Requires: 

   See libsmsInit for further documentation.

   This file is controlled by the Concurent Version System (CVS):
   $Id: decodeXPHA.c,v 1.4 2005/11/29 19:22:11 jfeeman Exp $

   Modification History: (keep this last section)

   Moved E/q lookup table into smscxdvs2eq for consistency with STICS.
   J. Raines, 16Feb00.

*/

#include "libsms.h"

XPHA decodeXPHA(int nedb, int npha){ /* nedb ==> edb # ; npha ==> PHA word # */
  /* decode STICS PHA word */
  int i,j;  /* short range loop counters */
  char thisprog[] = "decodeXPHA"; /* name of this program */

  BYTE PhaWord[4];
  double dxpha; /* double SWICS PHA word */

  XPHA sxpha; /* xpha structure */

  /* names of SWICS detectors */
  char *szSSD[] = { "none", "SSD1", "SSD2", "SSD3"}; 

  /* constants and variables for PHA calc., defined in calc. code below */
  double Ed; /* digital energy; energy calc. */ 
  double EOC; /* energy conversion const. */
  double EADC; /* energy conversion const.

  /*****************************/
  /* collect bytes of PHA word */
  /*****************************/
  /*++ Need to add two of these here - one for 'cycle.edb' and one for 'cycle.edb_r_h' ++*/
  for (j = 0; j < 4; j++) {
    if(isHigh == 1)
      PhaWord[j] = cycle.edb_r_h[nedb].xpha[4*npha + 3 - j]; /*++ ??? ++*/
    else if(isHigh == 0)
      PhaWord[j] = cycle.edb[nedb].xpha[4*npha + 3 - j];
  }

  dxpha = dBuildWord(PhaWord,0,3);
 
  /******************************************************/
  /* pull apart word in order  of diagram in DPU manual */
  /******************************************************/
  sxpha.sector = getBits(dxpha,24,27);
  sxpha.energy = getBits(dxpha,16,23); /* compressed?*/
  sxpha.range = getBits(dxpha,12,15);
  sxpha.ssdId = getBits(dxpha,10,11);
  sxpha.tof = getBits(dxpha,0,9);

  strcpy(sxpha.ssdName, szSSD[sxpha.ssdId]);

  /***********************************************/
  /* calculate some quantities in physical units */
  /***********************************************/

  /* predicted energy / charge (E/Q) */
  sxpha.eoq = smsgxeqtab(smsgxdvs(nedb));

  /* energy -- DPU manual p303 */

  EOC = 0; /* temp. hardcoded --FIX: get from HDB (somewhere in bytes 207-210*/
  EADC = 0.5; /* temp. hardcoded; same as above */

  sxpha.energykev = (Ed - EOC)/EADC;

  /* NOT CURRENTLY USED */
  /* mass */
  sxpha.mass = 0;

  /* mass/charge */
  sxpha.moq = 0;

  return(sxpha);
}
