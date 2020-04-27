/*
   Author: Jim Raines, Sep99

   See libsmsInit for further documentation.

   This file is controlled by the Concurent Version System (CVS):
   $Id: smsPrintFLR.c,v 1.4 2005/12/05 18:00:32 jfeeman Exp $

*/

#include <stdio.h>
#include "libsms.h"

void smsPrintFLR(unsigned char *abFLR) {

  int i, j;  /* loop counters */

  printf("Spacecraft ID: %d\n",
  lBuildWord(abFLR, 0, 3));

  return;
}
