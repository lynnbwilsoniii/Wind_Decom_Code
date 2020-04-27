/* struct-test2.c -- tests copying an array into a structure
   Jim Raines, 16Nov99

*/

#include "libsms.h"

int main(int argc, char *argv[]){
  int i,j;

  BYTE abEDB[800];  /* entire EDB  */
  BYTE *pabEDB; /* pointer to abEDB */

  struct data {
    BYTE header[11];
    BYTE xcore[62];
    BYTE xpha[324];
  } datas;

  union U {
    struct data data1;
    BYTE xa[sizeof(datas)]; /* size adds up to 397 */
    //BYTE xa[397];
  } u;

  union U *pu;
  BYTE *pxa;

  abEDB[0] = 20; /* 20 is 0x14 (XDB identifier) */
  abEDB[1] = 112; /* 111 is 0x6f (XDB identifier) */
  abEDB[2] = 104; /* 104 is standard EDB value at byte2 */
  abEDB[11] = 99; /* junk */

  pu = &u;
  pxa = &u.xa[0];
  pabEDB = &abEDB[0];
  
  /* copy into union via array */
  for (i=0; i < 15; i++){
    *pxa = *pabEDB;
    printf("*(pabEDB)=%3.3d (*pxa)=%3.3d\n", *(pabEDB++),*pxa++);
  }

  /* print union members via structure */
  for (i=0; i < 11; i++){
    printf("u.xa[%d]=%3.3d, u.data1.header[%d]=%3.3d \n", i, u.xa[i],
	   i,u.data1.header[i]);
  }

  for (i=11; i < 15; i++){
    printf("u.xa[%d]=%3.3d, u.data1.xcore[%d]=%3.3d \n", i, u.xa[i],
	   i-11,u.data1.xcore[i-11]);
  }


  /*
  for (i=0; i < 3; i++){
    (*pu->xa) = *pabEDB;
    printf("*(pabEDB)=%3.3d (*pu->xa)=%3.3d ", *(pabEDB++),(*pu->xa)++);
    printf("u.xa[%d]=%3.3d, u.data1.header[%d]=%3.3d \n", i, u.xa[i],
	   i,u.data1.header[i]);
  }
  */
}
