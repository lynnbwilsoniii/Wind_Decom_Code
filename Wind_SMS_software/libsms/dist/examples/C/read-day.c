/* File: read-day.c

   Description: Read a whole day of WIND/STICS data to test libsms handling
   of end of file conditions

   Author: Jim Raines, 14Jan00
*/

#include "libsms.h"

main(int argc, char *argv[]){
  int i,j;   /* short range loop counters */

  /* default input data file -- right side used if not date on command line */
  char infile[] = "/home/jraines/wind/dat/wi_lz_sms_19990618_v01.dat";

  char thisprog[] = "read-day";

  int result;

  int nedb; /* number of edb in cycle */
  int ncycle = 0; /* number of cycle found in day */
  int cyctime; /* cycle time in msec */

  switch (argc){ 
  case 2:
    sprintf( infile, "/home/winddata/LV1/wi_lz_sms_%s_v01.dat",argv[1]);
    //printf("%s: reading from %s\n", thisprog,infile);
    break;
  default:
    printf("%s: reading from %s\n", thisprog,infile);
    break; /* just for good measure */
  }

  /* open file */
  result = smsOpenFile(infile);

  /* set debug levels */
  smssetdbg(0);
  smssettrace(FALSE);

  while (((result = smsReadCycle()) == SMSSUCCESS)){
    ncycle++;

    printf("%s: Cycle[%3.3d] time is %s\n",thisprog,ncycle,
	   smsgtimes()); /* get time string for cycle */

    //printf("%s: year=%d doy=%d\n",thisprog,smsgyear(),smsgdoy());
    
    cyctime = smsgss1970(); /* get time of cycle */
    printf("%s: Cycle[%3.3d] is %d s since 1970\n",thisprog,ncycle,
	   cyctime);

    printf("%s: ss1970=%f\n",thisprog,smsgss1970());
    //smsDecommHDB();

  }

  return (SMSSUCCESS);
}

