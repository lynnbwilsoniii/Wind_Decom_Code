/* d-t.c -- date -- time test
   Jim Raines, 29Feb00
*/
#include <stdio.h>
//#define LVER LIBSMSVERSION

main(){
  char thisprog[] = "d-t";
  char version[12];

  FILE *pVerFile;

  /* open version file */
  pVerFile = fopen("version","r");

  if (pVerFile == NULL) {
    sprintf(version, "(unavail)");
  }
  else{
    fscanf(pVerFile, "%s",&version);
  }

  printf("%s: This is version %s built on %s %s.\n",thisprog,
	 LIBSMSVERSION,__DATE__,__TIME__);
}
