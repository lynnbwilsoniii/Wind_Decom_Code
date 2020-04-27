/** \file smsOpenFile.c
    \brief Open WIND/SMS data file, copies out FLR and make decisions 
   about data file.  Also initializes some variables, like debug and trace
   stuff.
*/
/*
   Author: Jim Raines, 25Oct99

   See libsmsInit for further documentation.

   This file is controlled by the Concurent Version System (CVS):
   $Id: smsOpenFile.c,v 1.6 2005/11/29 19:22:11 jfeeman Exp $

   Modification History

   Changed test for repos. of file pointer to >= SMSFATAL in case other 
   errors were detected by getFLR.  Also, made abBuffer local.
   J. Raines, 26Jan00

   Moved init. of gDbgLvl and gTrace into libsmsInit and make this routine
   call that one on first run.
   J. Raines, 16Feb00
*/

#include "libsms.h"

int smsOpenFile(char Infile[]){
  int i,j; /* disposable loop counters */

  char thisprog[] = "smsOpenFile";

  unsigned char abBuffer[MAXDATA]; /* buffer for holding char read until we 
				    decide what to do with them */

  unsigned char abFLR[2792]; /* File Label Record, DFCD p3-2 */
  int nchars; /* number of characters read from file last */

  int result; /* generic var. for storing function return codes */
  int errors = 0; /* accumulated non-fatal errors */

  static int fFirstTime = TRUE; /* T when this is first run of libsms */

  /* initialize some global variables -- once per libsms invocation */
  if (fFirstTime) {
    libsmsInit();
    fFirstTime = FALSE;
  }

  /* initialize some global variables that must be re-init. for every file */
  ncycle = 0;  /* number of acceptable cycles */
  
  /* open file */
  if ((pInfile = fopen( Infile, "rb")) == NULL) {
    fprintf(stderr, "%s -E- Error opening file %s\n", thisprog, Infile);
    return(SMSFATAL);
  }
  else if (TRACE) printf("%s -I- %s opened successfully\n",thisprog,Infile);

  /* copy out FLR */
  nchars = fread(abBuffer,1, 10800,pInfile);

  /*printf("%s: file position is %d\n",thisprog,ftell(pInfile));*/

  for (i = 0; i < 2792; i++) /* FLR is 2792 bytes long */
    abFLR[i] = abBuffer[i];

  /* fill external structure flr */
  errors = getFLR(abFLR);

  /* test result here */
  if (errors >= SMSFATAL) {
    /* The number of bytes per record must be wrong; fix file pointer by
       placing it where it would have been from an fread of the right number*/ 
    fseek( pInfile, flr.iBytesPerRec, SEEK_SET); /* correct. */
    printf("%s -I- file pointer re-positioned to %d.\n",
	   thisprog, flr.iBytesPerRec);
    errors = 0;
  }

  return(errors);
}
