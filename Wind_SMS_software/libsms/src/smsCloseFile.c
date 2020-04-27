/** \file smsCloseFile.c
    \brief Closes the open data file using the global file descriptor
*/
/*
   Author: Jim Raines, 2Feb00

   Method: 

   Requires: (called functions and globals read/written)

   See libsmsInit for further documentation.

   This file is controlled by the Concurent Version System (CVS):
   $Id: smsCloseFile.c,v 1.4 2005/11/29 19:22:11 jfeeman Exp $

   Modification History: (keep this last section)

*/

#include "libsms.h"

int smsCloseFile(){
  int i,j;  /* short range loop counters */
  char thisprog[] = "smsCloseFile"; /* name of this program */
  int retval; /* return value */

  if ((retval = fclose(pInfile)) != 0 ) {
    if (retval == EOF) 
      printf("%s -W- warning -- fclose encountered a write error\n",thisprog);
    else
      printf("%s -W- warning -- fclose returned non-zero value (%d)\n",
	     thisprog, retval);
  }
  else
    retval == SMSSUCCESS;

  return(retval);
}
