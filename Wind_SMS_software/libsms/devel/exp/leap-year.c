/* leap-year.c -- is input year a leap year?
   Jim Raines, 31Jan00
*/

#include <stdio.h>
#include "../libsms.h"

main(int argc, char *argv[]){
  int i,j,k;
  long ss1970; /* seconds since 1970 */

  int pyear; /* prev. year for leap checking */
  int ipyear; /* integer version of pyear */
  float fpyear; /* floating point version of pyear */
  int doy, year, sec;
  int nleapdays = 6; /* number of leap days; 72-92 leap years initially */

  int fLeap = FALSE;

  printf("Check to see if this year is a leap year (ctrl-c to exit)...\n");

  while (1) {
    printf("year=");
    scanf("%d", &year);

    /* Leap years: a year is leap if it a) is div. by 4 and not a century or
       b) a century but div. by 400 */
    if ((year % 100) == 0) { /* year is a century */
      if ((year % 400) == 0) fLeap = TRUE; /* is divisible by 400 */
    }
    else { /* year is not a century */
      if ((year % 4) == 0) fLeap = TRUE; /* is divisible by 4 */
    }

    if (fLeap) printf("%d is a leap year\n",year);

    fLeap = FALSE;
  }
    
  return;
}
