/* test-main2 -- written to test changing a var. in and out of a function
   Jim Raines, 15Nov99
*/

#include <stdio.h>

int somefunc(int *);

main (int argc, char *argv[]){
  int i;
  int curMinorFrame = 0;

  for (i=0; i < 250; i++) somefunc(&curMinorFrame);

  printf("\n");
  return(0);
}

int somefunc(int *curMinorFrame){
  *curMinorFrame++;
  printf("%d ",*curMinorFrame);
}
