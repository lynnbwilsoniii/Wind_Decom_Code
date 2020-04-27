/** \file readRecord.c
    \brief Reads a data record from the open file, performs some basic
   checks on the read and gets DRH decoded
*/
/*
   Author: Jim Raines, 3Dec99

   Method: 

   Requires: (called functions and globals read/written)

   See libsmsInit for further documentation.

   This file is controlled by the Concurent Version System (CVS):
   $Id: readRecord.c,v 1.6 2005/11/29 19:22:11 jfeeman Exp $

   Modification History: (keep last)

   Made abBuffer local. And passed abDRH to getDRH instead of abBuffer.  This
   fixed an anomalous problem of not finding an EDB at MF[000] in record with
   time 00:09:12.  (I don't know why this fixed it!)
   J. Raines, 26Jan00

*/

#include "libsms.h"

int readRecord(BYTE MinorFrame[250][42], int *curMinorFrame){
  /* should be called to read a record only when curMinorFrame is 249 */
  /* NOTE: MinorFrame needs to be passed *out* ; is above right? */

  int i,j;  /* short range loop counters */
  int imax; /* short-range loop max. */
  char thisprog[] = "readRecord"; /* name of this program */

  unsigned char abBuffer[MAXDATA]; /* buffer for holding char read until we 
				    decide what to do with them */

  unsigned char DataRecord[MAXDATA]; /* data record bytes 
					(data record header is stripped off) */
  BYTE abDRH[300]; /* data record header */

  int errors = 0; /* accumulated non-fatal errors */

  int nchars; /* number of characters in recent read of data from server */

  if (*curMinorFrame != 249) {
    if (DEBUG > 2){
      printf("%s -W- warning -- current minor frame != 249",thisprog);
      printf(" -- part of H/EDB may be lost\n");
    }
    errors++;
  }

  /************************/
  /* reads from open file */
  /************************/
  nchars = fread(abBuffer,1, flr.iBytesPerRec,pInfile);
  *curMinorFrame = 0;

  if (DEBUG > 2) printf("%s -D- %d bytes read\n",thisprog,nchars);

  if (nchars <= 0) {
    /* TODO: put in a real test for EOF */
    if (DEBUG > 2) printf("%s -D- no characters read; assuming EOF\n",thisprog);
    return(SMSEOF);
  }

  /********************************/
  /* Copy out data into subarrays */
  /********************************/

  /* NOTE: The copying of subarrays into other arrays helps keep 
   the counting of bytes straight and doesn't waste enough memory
   to worry about.      */

  /* get some DRH stuff */
  for (i = 0; i < 300; i++){
    abDRH[i] = abBuffer[i];
  } /* copy into array first? */
  getDRH(abDRH);
	
  /* copy out data record without Data Record Header (300 bytes) */
  imax = sizeof(abBuffer) - 300; /* imax only temp. */ 
  for ( i=0; i<= imax; i++)	  
    DataRecord[i] = abBuffer[i+300];

  /* copy out fixed CHK data */
  for (i=0; i < 250; i++) /* 250 mf/record according to DFCD p3-39 */
    for ( j=0; j < 2; j++ ) 
      cycle.CHK[i][j] = DataRecord[i*42 + j];
  
  /* extract 250 minor frames */
  /* skips bytes 0 & 1 because they are HK data */
  for (i=0; i < 250; i++) /* 250 and 42 are according to DFCD p3-39 */
    for ( j=2; j < 42; j++ ) 
      MinorFrame[i][j-2] = DataRecord[i*42 + j];

  return (SMSSUCCESS);
}
