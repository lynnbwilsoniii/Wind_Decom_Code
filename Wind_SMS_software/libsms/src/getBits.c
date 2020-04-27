/** \file getBits.c
    \brief get a range of bits from a byte 
*/
/*
   Jim Raines, 29Nov99

   method: 
   Right shift over to lobit then mask off beyond hibit.
   Based on Kernighan and Ritchie (2nd Ed.), p49.

   e.g. getBits(176,3,5)
      bit manip.   comments
      10110000     176
           >>3     shift over to lobit, bit 3
      --------
      00010110
     &00000111     mask off above hibit, bit 5 (see mask gen. below)
      --------
      00000110     6

      Note: mask generation
      11111111     ~0 (ones complement of 00000000)
           <<3     shift by number of bits to get
      --------
      11111000     0's are filled into places emptied by shift
     ~             take one's complement, i.e., switch all bits
      --------
      00000111

   Usage Note:  lo/hibit are counting from RIGHT!  Therefore, right-most 
   bit is bit 0 while left-most bit is bit 7.

   See libsmsInit for further documentation.

   This file is controlled by the Concurent Version System (CVS):
   $Id: getBits.c,v 1.6 2005/12/05 18:00:32 jfeeman Exp $
*/

/** Based on Kernighan and Ritchie (2nd Ed.), p49. (See code for more documentation on method) */
unsigned getBits(unsigned byte, int lobit, int hibit) {
  int i,j;
  int error = 0; /* place for accumulating errors */
  char thisprog[] = "getBits";

  int n; /* number of bits to get */

  /* check arguments */
  if (hibit < lobit) error = error | 1;
  if (lobit < 0) error = error | 2;
  if (error){
    printf("%s -W- warning -- error in arguments (%d);",thisprog,error);
    printf(" return value unpredictable.\n");
  }
  
  /* calc. number of bits to get */
  n = hibit - lobit + 1; 
			 
  /* do calc. and return */
  return (byte >> (lobit)) & ~(~0 << n);
}
