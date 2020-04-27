/* setbits.c -- test setting and clearing bits
   Jim Raines, 15Mar00
*/

#include "libsms.h"

main(int argc, char *argv[]){
  int i,j;
  unsigned int byte,lobit,hibit;

  char thisprog[] = "setbits";
  
  printf("%s: set a bit in byte -- hit C-c to exit\n",thisprog);
  
  while (1) {
    printf("byte=");
    scanf("%d", &byte);
    printf("bit-to-set=");
    scanf("%d", &lobit);

    byte = byte | (int)(pow(2,lobit));

    printf("byte=%d\n",byte);

  }

  return;
}
