/** \file fTemperature.c
    \brief Converts digital thermister temperature to temp. in C
*/
/*
   Author: Peter Bedini (from chinook program)

   Method:
   This are (presumably) linear fits to pre-flight calibration curves of 
   the thermisters (thermocouples) voltage output versus temperature.  There 
   may be some digital <-> analog stuff in there as well.

   Requires: read(-->) written(<--)
   ldexp (C standard math library)

   Arguments: read(-->) written(<--)
   --> bValue     digital temperature from TM
   --> curve      choice of linear curve to use


   See libsmsInit for further documentation.

   This file is controlled by the Concurent Version System (CVS):
   $Id: fTemperature.c,v 1.3 2005/12/05 18:00:32 jfeeman Exp $

   Major Modification History: (keep this last section)
     Modified for use in libsms from fTemp in chinook.c.  J. Raines, 22Jan2001
*/

#include "libsms.h"

/** Convert digital thermister temperature to temp. in C */
float fTemperature( unsigned char bValue, int curve ) {
  char thisprog[] = "fTemperature";
  
  double dValue ; 
  float rTemp ;  /* temperature in C (returned) */

  dValue = (double)bValue ;

  /* choose calibration curve / polynomial fit */
  switch( curve ) {

  case 0:
    rTemp = 6.558508E2 - 1.802378E1 * dValue + 2.133615E-1 * pow(dValue,2) - 
      1.295112E-3 * pow(dValue,3) + 3.927925E-6 * pow(dValue,4) - 
      4.754717E-9 * pow(dValue,5) ;
    break ;

  case 1:
    rTemp = 1.437601E2 - 3.224279 * dValue + 4.848285E-2 * pow(dValue, 2) - 
      4.101237E-4 * pow(dValue, 3) + 1.682762E-6 * pow(dValue, 4) - 
      2.660079E-9 * pow(dValue, 5) ;
    break ;

  case 2:
    rTemp = 7.886257E1 - 1.848392 * dValue + 2.143749E-2 * pow(dValue, 2) -
      1.483171E-4 * pow(dValue, 3) + 5.149126E-7 * pow(dValue, 4) - 
      7.140936E-10 * pow(dValue, 5) ;
    break ;
  }

  //printf("%s: pow(dValue, 3)=%f\n",thisprog, pow(dValue,3));

  //printf("%s: bValue=%d dValue=%f rTemp=%g\n",thisprog,bValue,dValue,
  // rTemp);
  return rTemp ;

}
