

#include <iostream>
#include <iomanip>
#include <fstream>
#include <string>
#include <vector>
#include <math.h>
#include <time.h>
#include <stdlib.h>
extern "C" {
#include "libsms.h"
}

using namespace std;

int main() {

  /*************************************************************************************
  ------------DECLARE SYSTEM TYPE VARIABLES NECESSARY FOR THE FUNCTION------------------
  *************************************************************************************/
  
  string progName = "wtLV2_mass_moq";
  
  /*************************************************************************************
  ------------OPEN OUTPUT FILE AND WRITE HEADER LINES-----------------------------------
  *************************************************************************************/
  
  string outputFileName;
  outputFileName = "wtLV2_mass_moq.dat";
  ofstream outFile;
  outFile.open(outputFileName.c_str());
  
  /*************************************************************************************
  ------------DECLARE SPACECRAFT VARIABLES FOR PRINTING---------------------------------
  *************************************************************************************/  

  int moqmassMat[59][127];
  
  for (int i=0;i<59;i++){
    for (int j=0;j<127;j++){
      moqmassMat[i][j] = 0;
    }
  }
 
 /*****************************************************************************************
 ---------------------------READ IN THE FILE LIST------------------------------------------
 *****************************************************************************************/
 ifstream inData;
 string ion;
 int numFiles, swFrameBol, numPoints;
 
 vector<string> FILENAME;
 
 inData.open("dateFile.dat");
 inData >> numFiles;
 inData >> ion;
 inData >> numPoints;
 inData >> swFrameBol;
 
 do {
   string FName;
   inData >> FName;
   
   FILENAME.push_back(FName);
 } while(!inData.eof());
 inData.close();
 
 /*****************************************************************************************
 ---------------------------LOOP THROUGH AND READ FILES------------------------------------
 *****************************************************************************************/
 
 for (int fc=0; fc<numFiles; fc++) {
   
  cout << "Opening and Processing " << FILENAME[fc] << endl;
  smsOpenFile(const_cast<char*>(FILENAME[fc].c_str())); 
    
  while(smsReadCycle()==0){  
    int nedb, npha;
    for (nedb=0; nedb<60;nedb++){
      for (npha=0; npha < smsgtnpha(nedb); npha++){
	int nm = 0;
	int nq = 0;
	double mass = 0.0;
	double moq = 0.0;
	
	//determine the nm and nq bins
	//nm = smsgtnm(nedb,npha);
	//nq = smsgtnq(nedb,npha);
	//outFile << nm << "     " << nq << endl;
	mass = smsgtmass(nedb,npha);
	moq  = smsgtmoq(nedb,npha);
	outFile << mass << "     " << moq << endl;
	//moqmassMat[nm][nq]++;
	
      }//loop through PHAs
    } // loop through edbs
  } // time selection structure
} // loop through cycles
	
//Get ready to run the next file
smsCloseFile();
  
/*print to file
for (int i=0; i<59; i++){
  for (int j=0; j<127; j++){
    outFile << moqmassMat[i][j] << "     ";
  }
  outFile << endl;
}*/

outFile.close();
  
return 0;
}
