/* test-main -- a stub main for testing libsms subroutines
   Jim Raines, 7Oct99
*/

#include <stdio.h>
#include <math.h>

long smsDecompress(unsigned char, int );

#define NNUM 11

main (int argc, char *argv[]){

  int i,j;
  long lResult;
  int iMantissa, iExponent;

  unsigned short int num[NNUM] = { 0, 1, 31, 32, 33, 63, 64, 127, 128, 
				   191, 255 };

  iMantissa = 15;
  iExponent = 11;
  i = 2;
  j = iExponent -1;
  lResult = (16+iMantissa)*pow(2,iExponent - 1);
  printf("pow test: %d**%d= (float)%f (dec)%d lResult=%d\n",i,j,pow(i,j),
	 (unsigned long)pow(i,j), lResult);

  for (i=0; i < NNUM ; i++) {
    printf("i=%d c %3.3d dcA %7.7d dcC %7.7d\n",
	   i,num[i],smsDecompress(num[i],0),smsDecompress(num[i],1));
  }
  return(0);
}
