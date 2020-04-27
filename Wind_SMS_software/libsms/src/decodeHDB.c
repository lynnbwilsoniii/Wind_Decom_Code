/** \file decodeHDB.c
    \brief Decode HDB into detailed structure
*/
/**
   Description: intelligently copies HDB array into HDB structure
*/
/*
   Author: Jim Raines, 17Nov99

   See libsmsInit for further documentation.

   This file is controlled by the Concurent Version System (CVS):
   $Id: decodeHDB.c,v 1.5 2005/11/29 19:22:11 jfeeman Exp $

*/
#include "libsms.h"

HDB decodeHDB(BYTE abHDB[]){
  int i,j;
  int iinit; /* (temporary) initial value for loop counter */
  int imax; /* (temporary) maximum for loop counter */
  
  char thisprog[] = "decodeHDB"; /* name of this program */

  HDB hdb; /* local hdb structure to be returned */

  BYTE *pabHDB; /* pointer to abHDB */  
  BYTE *phdbr; /* pointer to r (raw) array in hdb structure */

  BYTE abTmp[3]; /* temp. byte array for build words in reverse byte order */


  /*********************************************/
  /* copy abHDB into raw part of hdb structure */
  /*********************************************/
  pabHDB = &abHDB[0];
  phdbr = &hdb.r[0]; /* point at beginning of r array */

  for (i=0; i < 280; i++){
    *phdbr++ = *(pabHDB++);
   //printf("abHDB[%3.3d]=%3.3d r[%3.3d]=%3.3d\n",i,abHDB[i],i,cycle.hdb.r[i]);
  }

  /**************/
  /* Decode HDB */
  /**************/

  for (i=0; i < 3; i++) { /* copy bytes into abTmp in reverse order */
    abTmp[i] = hdb.r[5-i]; 
  }

  hdb.SciRecCount = lBuildWord(abTmp,0,2);
  hdb.NumHDB = getBits(hdb.r[5],0,5); /* number of HDB in set of 64 
					 for commutated values */

  if (DEBUG > 2) {
    printf("%s -D- cycle.hdb.SciRecCount=%3.3d\n",thisprog,hdb.SciRecCount);
  }

  /* STICS stepping voltages */
  /* measured +DPPS high voltage for prev. step = SciRecCount (number of HDB)*/
  hdb.tmpDPPShv = (hdb.r[36] << 4) + getBits(hdb.r[37],4,7);

  /* measured -DPPS high voltage for prev. step = SciRecCount (number of HDB)*/
  hdb.tmnDPPShv = (getBits(hdb.r[37],0,3) << 4) + hdb.r[38];

  /* compression groups */
  hdb.tCompGrp1 = getBits(hdb.r[189],0,0); /* STICS compression group 1 */
  hdb.tCompGrp2 = getBits(hdb.r[189],1,1); /* STICS compression group 2 */
  hdb.xCompGrp1 = getBits(hdb.r[189],2,2); /* SWICS compression group 1 */
  hdb.xCompGrp2 = getBits(hdb.r[189],3,3); /* SWICS compression group 2 */
  hdb.mCompGrp1 = getBits(hdb.r[189],4,4); /* MASS  compression group 1 */
  hdb.mCompGrp2 = getBits(hdb.r[189],5,5); /* MASS  compression group 2 */

  return(hdb);
}
