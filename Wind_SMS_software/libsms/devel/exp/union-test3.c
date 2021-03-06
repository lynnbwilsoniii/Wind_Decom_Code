/* union-test3.c -- test copying data into H/EDB's with union 
   Jim Raines, 16Nov99

   Result: The union ub2 works fine but writing to edb2.abEDB doesn't 
   seem to read out right from edb2.h.d.X.
   I think it is an alignment problem in that the union byte2 isn't really
   only one byte long.
*/

#include <stdio.h>
#define BYTE unsigned char

int main(int argc, char *argv[]){
  int i,j;

  struct edbheader {
    BYTE id0;  /* first identifier of E/HDB */
    BYTE id1;  /* second identifier of E/HDB */

    union{
      BYTE r; /* raw */
      struct bfByte2 {
	unsigned int numsf : 5; /* bytes 0-4: number of subframes */
	unsigned int dbid : 1;  /* byte 5: 0 for EDB ; 1 for HDB */
	unsigned int sfif : 1;  /* byte 6: 0 ==> 37 bytes per subframe; 1 ==> 40 */
	unsigned int highbitrate : 1;/* byte 7: 1 ==>high bit rate; 0 ==> low bit rate*/
      } d; /* decoded */
    } byte2;

    BYTE filler[797];
  };

  union{
    BYTE abEDB[800];
    struct edbheader h;
  } edb2;

  edb2.abEDB[0] = 0x14;
  edb2.abEDB[1] = 0x6f;
  edb2.abEDB[2] = 104; /* numsf=8, dbid=1,sfif=1,hbr=0*/

  for(i=0; i < 3 ; i++) printf("%d ",edb2.abEDB[i]);
  printf("\n");

  printf("byte0 %d byte1 %d byte2 %d %d %d %d\n", edb2.h.id0,edb2.h.id1,
	 edb2.h.byte2.d.numsf,edb2.h.byte2.d.dbid,edb2.h.byte2.d.sfif,
	 edb2.h.byte2.d.highbitrate);

  /* accessing through nearest union */
  edb2.h.byte2.r = 104; /* numsf=8, dbid=1,sfif=1,hbr=0*/

  printf("byte0 %d byte1 %d byte2 %d %d %d %d\n", edb2.h.id0,edb2.h.id1,
	 edb2.h.byte2.d.numsf,edb2.h.byte2.d.dbid,edb2.h.byte2.d.sfif,
	 edb2.h.byte2.d.highbitrate);


}
