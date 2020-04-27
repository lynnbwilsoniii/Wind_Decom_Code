/* array.c -- test array assignment where element number comes from a function
   in same assignment line as value of that array element

   Jim Raines, 24Jan00
*/

#include <stdio.h>
#include "../libsms.h"

int func(int i, int *ip);

main(int argc, char *argv[]){
  int i,j,k;
  int a[11];

  for (i = 0; i < 10; i++){
    a[k] = func(i, &k); /*note: both value of a[k] and k are produced by func*/
    printf("a[%d] = %d\n",k,a[k]);
  }

  return;
}

int func(int i, int *ip){
  *ip = i;

  return(i*i);
}
