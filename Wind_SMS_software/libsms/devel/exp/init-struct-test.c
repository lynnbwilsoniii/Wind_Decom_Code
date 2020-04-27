/* init-struct-test.c -- test of initializing a structure
   Jim Raines, 30Nov99
*/

#include <stdio.h>

main(int argc, char *argv[]){
  int i,j;

  struct edb_header_info {
    char *members[28];
    int nmembers;
  } info = {
    "numsf", "fIsEDB", "sfbytes", "fHighBitrate","fMPresent","fRamCheckStat",
    "msn", "revcount", "nxpha","ntpha","hk", "nmpha", "fValidCom", 
    "fComSemErr","fComSynErr", "fMCovPowOn","fXHeatPowOn", "fXCovPowOn", 
    "fTHeatPowOn", "fTCovPowOn","fMHPSstpflg", "fMSPMCPstpflg", 
    "fMSTMCPstpflg", "fXPAPSstpflg","fXSPMCPstpflg", "fXSTMCPstpflg", 
    "fTMCP3456stpflg","fTMCP012stpflg",28
  };

  for (i = 0; i < info.nmembers; i++) {
    printf("%s\n",info.members[i]);
  }
  return;
}
