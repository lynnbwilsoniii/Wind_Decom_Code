/** \file dBuildWord.c
    \brief Convert byte array to a double
*/
/**
   Description:  Returns a double out of a range of bytes
   (lowbyte to highbyte) from a byte array (data).
*/
/*     
   Author:  Jim Raines, Sep99

   See libsmsInit for further documentation.

   This file is controlled by the Concurent Version System (CVS):
   $Id: dBuildWord.c,v 1.3 2005/11/29 19:22:11 jfeeman Exp $

*/


#include <stdio.h>
#include <math.h>

double dBuildWord(unsigned char data[],long int lowbyte,long int highbyte){
  int i,j;
  double dword = 0, temp; /* word being built */
  int n = 0; /* power */

  for ( i = lowbyte; i <= highbyte; i++){
    temp = pow(2,8*(n++))*data[i]; 
    dword = dword + temp; 
  }

  return dword;
}
