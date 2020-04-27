/* bitfield-test.c -- tests using a bit field to avoid bit-shifting
   Jim Raines, 16Nov99

*/

#include <stdio.h>
#define BYTE unsigned char

int main(int argc, char *argv[]){
  int i,j;

  union{
    BYTE bb2;
    struct bfByte2 {
      unsigned int numsf : 5; /* bytes 0-4: number of subframes */
      unsigned int dbid : 1;  /* byte 5: 0 for EDB ; 1 for HDB */
      unsigned int sfif : 1;  /* byte 6: 0 ==> 37 bytes per subframe; 1 ==> 40 */
      unsigned int highbitrate : 1;/* byte 7: 1 ==>high bit rate; 0 ==> low bit rate*/
    } sb2;
  } ub2;

  ub2.bb2 = 104; /* binary 01101000: numsf=8; dbid=1; sfif=1; hbr=1 */
  printf("byte2: bits0-4=%d bit5=%d bit6=%d bit7=%d\n",
	 ub2.sb2.numsf, ub2.sb2.dbid, ub2.sb2.sfif, ub2.sb2.highbitrate);
}
