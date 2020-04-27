/** \file lBuildWord.c
    \brief Returns a long int out of a range of bytes
   (lowbyte to highbyte) from a byte array (data).
*/
/*
   Author:  Jim Raines, Sep99

   See libsmsInit for further documentation.

   This file is controlled by the Concurent Version System (CVS):
   $Id: lBuildWord.c,v 1.3 2005/11/29 19:22:11 jfeeman Exp $

*/


#include <stdio.h>
#include <math.h>

unsigned long lBuildWord(unsigned char *data,long int lowbyte,long int highbyte){
  int i,j;
  unsigned long lword = 0, temp; /* word being built */
  int n = 0; /* power */

  for ( i = lowbyte; i <= highbyte; i++){
    temp = ((unsigned long)(pow(2,8*(n++))))*data[i]; 
    lword = lword + temp; 
  }

  return lword;
}
