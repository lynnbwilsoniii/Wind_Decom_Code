/* struct-test.c -- tests copying an array into a structure
   Jim Raines, 16Nov99

*/

#include "libsms.h"

int main(int argc, char *argv[]){
  int i,j;

  BYTE abEDB[800];  /* entire EDB  */
  EDB tmpedb; /* temporary EDB to use for union before EDB is put in place */
  EDB *ptmpedb; /* pointer to tmpedb */

  abEDB[0] = 0x14;
  abEDB[1] = 0x6f;

  tmpedb.h[0] = 0x14;
  //  tmpedb.h[1] = 0x6f;
  tmpedb.h[1] = 111;

  ptmpedb = &tmpedb;

  for (i=0; i < 2; i++){
    printf("%d\n",ptmpedb->h[i]);
    ptmpedb++;
  }
}
