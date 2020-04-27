/* oversize.c -- tests outcome when a variable is incremented beyond it
   storage size
   Jim Raines, 20Jan00

*/

#include <stdio.h>
#include "../libsms.h"

#define TWOPOW16 65535
#define TWOPOW24 16777216
#define TWOPOW32 4294967296

main(int argc, char *argv[]){
  int j,k,jmax;

  int i = TWOPOW24 - 5;
  unsigned short int si = TWOPOW16 - 5;
  unsigned long int li = TWOPOW24 - 5;

  i = 65530; /* 65535 should be upperlimit */

  jmax = (int) argv[1];

  for (j=0; j < jmax; j++)
    printf("j=%d si=%d i=%d li=%d\n",j,si++,i++,li++);

  return;
}
