/* union-test.c -- test copying data into H/EDB's with union 
   Jim Raines, 16Nov99

   Result: The union works fine but the bit field part doesn't seem to.
   I think it is an alignment problem.
*/

#include <stdio.h>
#define BYTE unsigned char

int main(int argc, char *argv[]){
  int i,j;

  union{
    struct{
      BYTE abEDB[800];
    } a;
    struct edbheader {
      BYTE id0;  /* first identifier of E/HDB */
      BYTE id1;  /* second identifier of E/HDB */

      struct byte2 {
	unsigned int numsf : 5; /* bytes 0-4: number of subframes */
	unsigned int dbid : 1;  /* byte 5: 0 for EDB ; 1 for HDB */
	unsigned int sfif : 1;  /* byte 6: 0 ==> 37 bytes per subframe; 1 ==> 40 */
	unsigned int highbitrate : 1;/* byte 7: 1 ==>high bit rate; 0 ==> low bit rate*/
      } byte2;
      BYTE filler[797];
    } h;
  } edb;

  edb.a.abEDB[0] = 0x14;
  edb.a.abEDB[1] = 0x6f;
  edb.a.abEDB[2] = 104; /* numsf=8, dbid=1,sfif=1,hbr=0*/

  for(i=0; i < 3 ; i++) printf("%d ",edb.a.abEDB[i]);
  printf("\n");

  printf("byte0 %d byte1 %d byte2 %d %d %d %d\n", edb.h.id0,edb.h.id1,
	 edb.h.byte2.numsf,edb.h.byte2.dbid,edb.h.byte2.sfif,
	 edb.h.byte2.highbitrate);

}
