/** \file smsDecompress.c
    \brief Expand 1 byte to long int according to WIND scheme
*/
#include <math.h>

long smsDecompress(unsigned char bCompNum, int iIsCodeC) {

  /* author: Jim Raines
     created: 7Oct99
     based on: wind_decompress by John Paquette, 12Oct94

     I/O:
     input: bCompNum is compressed number
            iIsCodeC is true if compression scheme is C, else A
     output: returns decompressed number from lDecompNum

     dependencies:
     none.

     includes:
     stdio.h, math.h
  */
  /**
     method: 

     Compressed WIND values have been logarithmically coded so that big
     numbers, up to 7864320, can be encoded into 1 byte (i.e. a short int).
     Of course, this results in loss of precision, which is where the 
     logarithmic nature comes in.  This allows small values to be represented
     with a reasonable precision, while still allowing very large values.
     
     For example, numbers 0-32 are represented as 0-32, which means no 
     precision is lost.  On the other hand, numbers 8192-16384  are 
     represented by 160-176, where, obviously, the precision is much less.
     Thus, the precision scales with the size of the value, which is the most
     acceptable situation.
  */
  /*     
   See libsmsInit for further documentation.

   This file is controlled by the Concurent Version System (CVS):
   $Id: smsDecompress.c,v 1.3 2005/11/29 19:22:11 jfeeman Exp $

  */

  int i,j;  /* short range loop counters */

  long lDecompNum;  /* decompressed number */
  long iMantissa, /* mantissa of number */
    iExponent;             /* exponent of number */

  if (iIsCodeC) { /* use code C decompression */
    /* figure out how many bits are in each mantissa and exponent and
       decompress accordingly */
    if ( (bCompNum >> 6) < 3) { /* true if bit 6 and/or 7 is 0 */

      /* 4 bit mantissa and 4 bit exponent */

      iMantissa = bCompNum & 15; /* bits 0-3 */
      iExponent = bCompNum >> 4; /* bits 4-7 */

      /* DECOMPRESS
	 Example: For iMantissa=15 and iExponent=11, we get 31,744 */

      if (iExponent == 0) lDecompNum = iMantissa;
      else if ( iExponent >= 1 && iExponent <= 11 ) {
	lDecompNum = (16 + iMantissa)*pow(2,iExponent - 1);
      }
    }
    else {

      /* 3 bit mantissa and 5 bit exponent */

      iMantissa = bCompNum & 7; /* bits 0-2 */
      iExponent = bCompNum >> 3; /* bits 3-7 */

      /* DECOMPRESS
	 Example: For iMantissa=7 and iExponent=31, we get 7,864,320 */

      lDecompNum = (8 + iMantissa)*pow(2,iExponent - 12);
    }
  }
  else { /* use code A decompression */

      /* 4 bit mantissa and 4 bit exponent */

      iMantissa = bCompNum & 15; /* bits 0-3 */
      iExponent = bCompNum >> 4; /* bits 4-7 */

      /* DECOMPRESS
	 Example: For iMantissa=15 and iExponent=15, we get 507,904 */

      if (iExponent == 0) lDecompNum = iMantissa;
      else if ( iExponent >= 1 && iExponent <= 15 ) {
	lDecompNum = (16 + iMantissa)*pow(2,iExponent - 1);
      }
  }

  return lDecompNum;
}
