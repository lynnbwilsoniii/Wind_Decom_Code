/* getbits.c -- user interface wrapper for testing getBits function
   Jim Raines, 29Nov99
*/

#include <stdio.h>
unsigned getBits(unsigned, int, int);

main(int argc, char *argv[]){
  int i,j;
  unsigned int byte,lobit,hibit;

  char thisprog[] = "getbits";
  
  printf("%s: get range of bits from byte -- hit C-c to exit\n",thisprog);
  
  printf("rightshift %d\n",176 >> 4);
  printf("leftshift %d\n",~0 << 8);

  while (1) {
    printf("byte=");
    scanf("%d", &byte);
    printf("lobit=");
    scanf("%d", &lobit);
    printf("hibit=");
    scanf("%d", &hibit);

    printf("byte=%d bits[%d,%d]=%d\n",byte,lobit,hibit,
	   getBits(byte,lobit,hibit));
  }

  return;
}
