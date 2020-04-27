
//header files to include
#include <sys/stat.h>
#include <iostream>
#include <iomanip>
#include <fstream>
#include <time.h>
#include <stdlib.h>
#include <wtdcLV2_Lite.h>

extern "C" {
#include "libsms.h"
}

using namespace std;

//global constants

//declare function prototypes for functions which will be used in the processing
moments calcMoments(double[][16][3], double[][16][3], double[], double, double);
vector<momentsBySector> calcMomentsBySector(double[][16][3], double[][16][3], double[], double, double);
double calcVelSWE(vector<SWERecord>, double, double);

int main() {
  
  /*************************************************************************************
  ------------DECLARE SYSTEM TYPE VARIABLES NECESSARY FOR THE FUNCTION------------------
  *************************************************************************************/
  
  ifstream inData;
  string ion;
  string progName = "wtdcLV2_Lite";
  string SWEFilename, outDir;
  
  int numFiles, dirBol, selBol, datOutput, momFile;
  double numBins, errThresh, velThresh, effThresh, wCutOff;
  
  vector<string> FILENAMES;
  cout << endl << "******************************************************************" << endl;
  cout << "*******    - " << progName << " - Beginning to process Wind/STICS Data-    *******" << endl;
  cout << endl << "******************************************************************" << endl;
  
  /*************************************************************************************
  ------------READ IN LIST OF FILES TO PROCESS------------------------------------------
  *************************************************************************************/ 
  
  inData.open("dateFile.dat");
  inData >> numFiles;
  inData >> ion;
  inData >> numBins;
  inData >> errThresh;
  inData >> velThresh;
  inData >> effThresh;
  inData >> dirBol;
  inData >> selBol;
  inData >> wCutOff;
  inData >> SWEFilename;
  inData >> datOutput;
  inData >> momFile;
  inData >> outDir;
  
  //process the case that the user wants to output to the working directory
  if (outDir == "WORKINGDIRECTORY") {
  	outDir = "";
  }
  
  do {
    string FName;
    inData >> FName;
    
    FILENAMES.push_back(FName);
  } while(!inData.eof());
  inData.close();
  
  /*************************************************************************************
  ------------OPEN OUTPUT FILE AND WRITE HEADER LINES-----------------------------------
  *************************************************************************************/
  
  string outputFileName;
  string fileStart, fileStop;
  string outDataLabel[3] = {"Counts","DF","dJ"};
  string outDirLabel[3] = {"3D","Sectored","1D"};
  string selLabel[2] = {"TOF","M-MOQ"};
  fileStart = FILENAMES[0].substr(31,8);
  fileStop = FILENAMES[numFiles-1].substr(31,8);
  
  struct stat st;
  if (stat(outDir.c_str(), &st) != 0) {
  	string systemCommand = "mkdir "+outDir;
  	system(systemCommand.c_str());
  }

  outputFileName = outDir+progName+"_SCFrame_"+ion+"_"+outDataLabel[datOutput]+"_"+outDirLabel[dirBol]+"_"+selLabel[selBol]+"_"+fileStart+"-"+fileStop+".dat";
  ofstream outFile;
  outFile.open(outputFileName.c_str());
 
  time_t rawtime;
  time(&rawtime);
  
  outFile << "# Distribution function from Wind Level II - Lite Version" << endl;
  outFile << "# Created: " << ctime(&rawtime);
  outFile << "#" << endl;
  outFile << "#   year";
  outFile << setw(13) << "doy";
  outFile << setw(13) << "sector";
  outFile << setw(13) << "telescope";
  outFile << setw(12) << "eoq";
  outFile << setw(9) << "ion";
  outFile << setw(14) << outDataLabel[datOutput];
  outFile << setw(14) << outDataLabel[datOutput]+"_error";
  outFile << setw(14) << "delT" << endl;

  /*************************************************************************************
  ------------OPEN MOMENT FILE (IF DESIRED) AND WRITE HEADER LINES----------------------
  *************************************************************************************/ 
  ofstream outMomFile;
  
  if (momFile == 1) {
  	string outputMomentFileName;
  	outputMomentFileName = outDir+progName+"_nvt_"+ion+"_"+selLabel[selBol]+"_"+fileStart+"-"+fileStop+".dat";
	outMomFile.open(outputMomentFileName.c_str());

	outMomFile << "# NVT Values Integrated over all directions from Wind Level II - Lite Version" << endl;
	outMomFile << "# Created: " << ctime(&rawtime);
	outMomFile << "#" << endl;
	outMomFile << "#   year";
	outMomFile << setw(13) << "doy";
	outMomFile << setw(9)  << "ion";
	outMomFile << setw(13) << "n [1/cm^-3]";
	outMomFile << setw(13) << "n_err";
	outMomFile << setw(13) << "v [km/s]";
	outMomFile << setw(13) << "v_err";
	outMomFile << setw(13) << "v_th [km/s]";
	outMomFile << setw(13) << "v_th_err";
	outMomFile << setw(13) << "delT [s]" << endl;
  }
  
  /*************************************************************************************
  ------------OPEN SECTORED MOMENT FILE (IF DESIRED) AND WRITE HEADER LINES-------------
  *************************************************************************************/
  
  ofstream outMomSecFile;
  
  if (momFile == 1) {
  	string outputMomentSectorFileName;
  	outputMomentSectorFileName = outDir+progName+"_Sectored_Moments_"+ion+"_"+selLabel[selBol]+"_"+fileStart+"-"+fileStop+".dat";
  	
	outMomSecFile.open(outputMomentSectorFileName.c_str());

	outMomSecFile << "# Sectored Moments from Wind Level II - Lite Version" << endl;
	outMomSecFile << "# Created: " << ctime(&rawtime);
	outMomSecFile << "#" << endl;
	outMomSecFile << "#   year";
	outMomSecFile << setw(13) << "doy";
	outMomSecFile << setw(9)  << "ion";
	outMomSecFile << setw(13) << "Sector";
	outMomSecFile << setw(13) << "F_0";
	outMomSecFile << setw(13) << "F_0_err";
	outMomSecFile << setw(13) << "F_1";
	outMomSecFile << setw(13) << "F_1_err";
	outMomSecFile << setw(13) << "F_2";
	outMomSecFile << setw(13) << "F_2_err";
	outMomSecFile << setw(13) << "delT [s]" << endl;
  }

  	/*************************************************************************************
	------------OPEN UP AND READ SWE FILE-------------------------------------------------
	*************************************************************************************/

	vector<SWERecord> sweData;
  
	cout << "*******    - " << progName << " - Reading in SWE Data -    *******" << endl << endl;
	
	ifstream inDataSWE;
	
	inDataSWE.open(SWEFilename.c_str());
  
	do {
		SWERecord SWEEntry;
		double temp[9];

		inDataSWE >> SWEEntry.year >> SWEEntry.day >> SWEEntry.fracday >> temp[0] >> temp[1] >>
	  		temp[2] >> temp[3] >> temp[4] >> temp[5] >> SWEEntry.Vx >> SWEEntry.Vy >> SWEEntry.Vz >>
	  		temp[6] >> temp[7] >> temp[8];
  
		sweData.push_back(SWEEntry);
	} while (!inDataSWE.eof());
  
  	inDataSWE.close();
  	
  /*************************************************************************************
  ------------DECLARE SPACECRAFT VARIABLES FOR PHASE SPACE CALCULATION------------------
  *************************************************************************************/  

  double eoq_SC[32];
  		  
  double mass = 0.0;
  double charge = 0.0;
  int brRange = 0;
  
  for (int i=0;i<32;i++){
    eoq_SC[i] = 6.190722*pow(1.1225857,(double(i)));
  }
  
  double geomF = 0.029;
  double delEfac = 0.019;
  double delT = 3.0/16.0;
  double DC0 = 2.143;
  double DCOther = 1.068;
  /*************************************************************************************
  ------------POPULATE TOF RANGE VECTORS AS WELL AS PUT EFFICIENCY FILE IN MEMORY-------
  *************************************************************************************/
  
  double TOFLow[32];
  double TOFHigh[32];
  double massLow = 0.0;
  double massHigh = -999.99;
  double moqLow = -999.99;
  double moqHigh = -999.99;
  string effFile;
  string TOFFile;
  
  cout << "*******    " << progName << " - Setting TOF flight ranges    *******" << endl;
  
  if (ion == "H+") {
  	  //effFile = "/home/jagruesb/Research/WIND/wind_STICS_Verification/20110419_STICS_ACE_Efficiencies_Compare/H_Effic.dat";
      //effFile = "/shrg1/wind/utility/tables/H_Effic.dat";
      effFile = "../efficiencyFiles/H_Effic.dat";
      mass = 1.0079;
      charge = 1.0;
      brRange = 2;
      
      if (selBol == 0) {
      	cout << "Using TOF ranges for protons (Triples)."<<endl;
      	TOFFile = "/shrg1/wind/utility/tables/tof_ranges_H+.dat";
      	//TOFFile = "tof_ranges_H+.dat";
      } else if (selBol == 1) {
      	cout << "Using M-MOQ bins for protons (Triples)."<< endl;
      	massLow = 0.5;	//for triples only
      	massHigh = 2.13;
      	moqLow = 0.90;
      	moqHigh = 1.26;
      }
  } else if (ion == "H+_D") {
      //effFile = "/home/jagruesb/Research/WIND/wind_STICS_Verification/20110419_STICS_ACE_Efficiencies_Compare/H_Effic.dat";
      //effFile = "/shrg1/wind/utility/tables/H_Effic.dat";
      effFile = "../efficiencyFiles/H_Effic.dat";
      mass = 1.0079;
      charge = 1.0;
      brRange = 2;
      if (selBol == 0) {            
      	cout << "Using TOF ranges for protons (Doubles)."<<endl;
      	TOFFile = "/shrg1/wind/utility/tables/tof_ranges_H+.dat";
      	massLow = 0.0;
      	//TOFFile = "tof_ranges_H+.dat";
      } else if (selBol == 1) {
      	cout << "Using M-MOQ bins for protons (Doubles)." << endl;
		massLow = 0.0;  //for doubles
      	massHigh = 2.13;
      	moqLow = 0.90;
      	moqHigh = 1.26;
      }
  } else if (ion == "He+") {
      //effFile = "/home/jagruesb/Research/WIND/wind_STICS_Verification/20110419_STICS_ACE_Efficiencies_Compare/He_Effic.dat";
      //effFile = "/shrg1/wind/utility/tables/He_Effic.dat";
      effFile = "../efficiencyFiles/He_Effic.dat";
      mass = 4.0026;
      charge = 1.0;
      brRange = 2;
      if (selBol == 0) {      
      	cout << "Using TOF ranges for He+." << endl;
      	TOFFile = "/shrg1/wind/utility/tables/tof_ranges_He+.dat";
	  } else if (selBol == 1) {
	  	cout << "Using M-MOQ bins for He+." << endl;
		massHigh = 6.30;
		massLow = 2.79;
		moqLow = 3.55;
		moqHigh = 4.67;
	  }
  } else if (ion == "He2+") {
      //effFile = "/shrg1/wind/utility/tables/He_Effic.dat";
      //effFile = "/home/jagruesb/Research/WIND/wind_STICS_Verification/20110419_STICS_ACE_Efficiencies_Compare/He_Effic.dat";
      effFile = "../efficiencyFiles/He_Effic.dat";
      mass = 4.0026;
      charge = 2;
      brRange = 2;
      if (selBol == 0) {  
      	cout << "Using TOF ranges for alphas." << endl;
      	TOFFile = "/shrg1/wind/utility/tables/tof_ranges_He2+.dat";
      } else if (selBol == 1) {
		cout << "Using M-MOQ binning for alphas." << endl;
    	massHigh = 6.30;
      	massLow = 2.79;
      	moqLow = 1.61;
      	moqHigh = 2.32;
      }
  } else if (ion == "C+") {
      //effFile = "/shrg1/wind/utility/tables/C_N_O_Ne_Effic.dat";
      //effFile = "/home/jagruesb/Research/WIND/wind_STICS_Verification/20110419_STICS_ACE_Efficiencies_Compare/C_Effic.dat";
      effFile = "../efficiencyFiles/C_Effic.dat";
      mass = 12.011;
      charge = 1;
      brRange = 1;
      if (selBol == 0) {
      	cout << "Using TOF ranges for C+." << endl;
      	TOFFile = "/shrg1/wind/utility/tables/tof_ranges_C+.dat";
      } else if (selBol == 1) {
      	cout << "No current M-MOQ Bins defined for C+. Resulting in no data." << endl;
      }
  } else if (ion == "C2+") {
      //effFile = "/shrg1/wind/utility/tables/C_N_O_Ne_Effic.dat";
      //effFile = "/home/jagruesb/Research/WIND/wind_STICS_Verification/20110419_STICS_ACE_Efficiencies_Compare/C_Effic.dat";
      effFile = "../efficiencyFiles/C_Effic.dat";
      mass = 12.011;
      charge = 2;
      brRange = 0;
      if (selBol == 0) {
      	cout << "Using TOF ranges for C2+." << endl;
      	TOFFile = "/shrg1/wind/utility/tables/tof_ranges_C2+.dat";
	  } else if (selBol == 1) {
      	cout << "No current M-MOQ Bins defined for C2+. Resulting in no data." << endl;
	  }
   } else if (ion == "C4+") {
      //effFile = "/shrg1/wind/utility/tables/C_N_O_Ne_Effic.dat";
      //effFile = "/home/jagruesb/Research/WIND/wind_STICS_Verification/20110419_STICS_ACE_Efficiencies_Compare/C_Effic.dat";
      effFile = "../efficiencyFiles/C_Effic.dat";
      mass = 12.011;
      charge = 4;
      brRange = 0;
      if (selBol == 0) {
       	cout << "No current TOF bands defined for C4+." << endl;
      } else if (selBol == 1) {
      	cout << "Using M-MOQ matrix box for C4+" << endl;
      	massLow = 9.04;
      	massHigh = 12.99;
      	moqLow = 2.87;
      	moqHigh = 3.24; 
      }    
  } else if (ion == "C5+") {		
      //effFile = "/shrg1/wind/utility/tables/C_N_O_Ne_Effic.dat";
      //effFile = "/home/jagruesb/Research/WIND/wind_STICS_Verification/20110419_STICS_ACE_Efficiencies_Compare/C_Effic.dat";
      effFile = "../efficiencyFiles/C_Effic.dat";
      mass = 12.011;
      charge = 5;
      brRange = 0;
      if (selBol == 0) {
      	cout << "No current TOF bands defined for C5+." << endl;
      } else if (selBol == 1) {
      	cout << "Using M-MOQ matrix box for C5+" << endl;
      	massLow = 9.04;
      	massHigh = 12.99;
      	moqLow = 2.25;
      	moqHigh = 2.54;
      }
  } else if (ion == "C6+") {
      //effFile = "/shrg1/wind/utility/tables/C_N_O_Ne_Effic.dat";
      //effFile = "/home/jagruesb/Research/WIND/wind_STICS_Verification/20110419_STICS_ACE_Efficiencies_Compare/C_Effic.dat";
      effFile = "../efficiencyFiles/C_Effic.dat";
      mass = 12.011;
      charge = 6;
      brRange = 0;
      if (selBol == 0) {
      	cout << "No current TOF bands defined for C6+." << endl;
      } else if (selBol == 1) { 
	    cout << "Using M-MOQ matrix box for C6+" << endl;
    	massLow = 9.04;
      	massHigh = 12.99;
      	moqLow = 1.87;
      	moqHigh = 2.18;
      }
  } else if (ion == "O+") {
  	  //effFile = "/shrg1/wind/utility/tables/C_N_O_Ne_Effic.dat";
      //effFile = "/home/jagruesb/Research/WIND/wind_STICS_Verification/20110419_STICS_ACE_Efficiencies_Compare/O_Effic.dat";
      effFile = "../efficiencyFiles/O_Effic.dat";
      mass = 15.999;      
      charge = 1;
      brRange = 1;
      if (selBol == 0) {
   	 	cout << "Using TOF ranges for O+." << endl;
		TOFFile = "/shrg1/wind/utility/tables/tof_ranges_O+.dat"; 
		TOFFile = "/shrg1/wind/utility/tables/tof_ranges_O+.dat" ;	 	
	  } else if (selBol == 1) {
	  	cout << "Using M-MOQ bins for O+." << endl;
	  	massLow = 4.8;
	  	massHigh = 79.3;
	  	moqLow = 14.89;
	  	moqHigh = 20.20;
	  } 
  } else if (ion == "O6+") {
      //effFile = "/shrg1/wind/utility/tables/C_N_O_Ne_Effic.dat";
      //effFile = "/home/jagruesb/Research/WIND/wind_STICS_Verification/20110419_STICS_ACE_Efficiencies_Compare/O_Effic.dat";
      effFile = "../efficiencyFiles/O_Effic.dat";
      mass = 15.999;
      charge = 6;
      brRange = 0; 
      if (selBol == 0) {
      	cout << "Using TOF ranges for O6+." << endl;
      	TOFFile = "/shrg1/wind/utility/tables/tof_ranges_O6+.dat";
	  } else if (selBol == 1) {
	  	cout << "Using M-MOQ Binning for O6+." << endl;
	  	massLow = 10.83;
      	massHigh = 20.41;
      	moqLow = 2.54;
      	moqHigh = 2.87;
	  }
  } else if (ion == "O7+") {
  	  //effFile = "/shrg1/wind/utility/tables/C_N_O_Ne_Effic.dat";
      //effFile = "/home/jagruesb/Research/WIND/wind_STICS_Verification/20110419_STICS_ACE_Efficiencies_Compare/O_Effic.dat";
	  effFile = "../efficiencyFiles/O_Effic.dat";
	  mass = 15.999;
      charge = 7;
      brRange = 0;
      if (selBol == 0) {
      	cout << "No current TOF bands defined for O7+." << endl;
      } else if (selBol ==1) {
        cout << "Using M-MOQ matrix box for O7+." << endl;    
      	massLow = 14.21;
      	massHigh = 20.41;
      	moqLow = 2.11;
      	moqHigh = 2.46;
      }
  } else if (ion == "Ne+") {
  	  //effFile = "/shrg1/wind/utility/tables/C_N_O_Ne_Effic.dat";
      effFile = "../efficiencyFiles/C_N_O_Ne_Effic.dat";
      mass = 20.180;
      charge = 1;
      brRange = 1;
      if (selBol == 0) {
   	 	cout << "Using TOF ranges for Ne+." << endl;
   	 	TOFFile = "/shrg1/wind/utility/tables/tof_ranges_Ne+.dat";
	  } else if (selBol == 1) {
	  	cout << "No current M-MOQ bins defined for Ne+." << endl;
	  }
  } else if (ion == "Fe8+") {
      //effFile = "/shrg1/wind/utility/tables/Fe_Effic.dat";
      //effFile = "/home/jagruesb/Research/WIND/wind_STICS_Verification/20110419_STICS_ACE_Efficiencies_Compare/Fe_Effic.dat";
      effFile = "../efficiencyFiles/Fe_Effic.dat";
      mass = 55.845;
      charge = 8;
      brRange = 0;
	  if (selBol == 0) {  	
	  	cout << "No current TOF bands for Fe8+." << endl;
	  } else if (selBol == 1) {  
      	cout << "Using M-MOQ matrix box for Fe8+" << endl;
      	massLow = 38.45;
      	massHigh = 79.28;
      	moqLow = 6.53;
      	moqHigh = 7.38;
      }
  } else if (ion == "Fe9+") {
      //effFile = "/shrg1/wind/utility/tables/Fe_Effic.dat";
      //effFile = "/home/jagruesb/Research/WIND/wind_STICS_Verification/20110419_STICS_ACE_Efficiencies_Compare/Fe_Effic.dat";
      effFile = "../efficiencyFiles/Fe_Effic.dat";
      mass = 55.845;
      charge = 9;
      brRange = 0;
	  if (selBol == 0) {
	  	cout << "No current TOF bands defined for Fe9+." << endl;
	  } else if (selBol == 1) {  
      	cout << "Using M-MOQ matrix box for Fe9+" << endl;
      	massLow = 38.45;
      	massHigh = 79.28;
      	moqLow = 5.78;
      	moqHigh = 6.53;
      }
  } else if (ion == "Fe10+") {
      //effFile = "/shrg1/wind/utility/tables/Fe_Effic.dat";
      //effFile = "/home/jagruesb/Research/WIND/wind_STICS_Verification/20110419_STICS_ACE_Efficiencies_Compare/Fe_Effic.dat";
      effFile = "../efficiencyFiles/Fe_Effic.dat";
      mass = 55.845;
      charge = 10;
      brRange = 0;
      if (selBol == 0) {
      	cout << "No current TOF bands defined for Fe10+." << endl;
      } else if (selBol == 1) {  
      	cout << "Using M-MOQ matrix box for Fe10+" << endl;
      	massLow = 38.45;
      	massHigh = 79.28;
      	moqLow = 5.28;
      	moqHigh = 5.78;
      }
  } else if (ion == "Fe11+") {
      //effFile = "/shrg1/wind/utility/tables/Fe_Effic.dat";
      //effFile = "/home/jagruesb/Research/WIND/wind_STICS_Verification/20110419_STICS_ACE_Efficiencies_Compare/Fe_Effic.dat";
      effFile = "../efficiencyFiles/Fe_Effic.dat";
      mass = 55.845;
      charge = 11;
      brRange = 0;
      if (selBol == 0) {
      	cout << "No current TOF bands defined for Fe11+." << endl;
      } else if (selBol == 1) {
      	cout << "Using M-MOQ matrix box for Fe11+" << endl;
      	massLow = 38.45;
      	massHigh = 79.28;
      	moqLow = 4.82;
      	moqHigh = 5.28;
      }
  } else if (ion == "Fe12+") {
      //effFile = "/shrg1/wind/utility/tables/Fe_Effic.dat";
      //effFile = "/home/jagruesb/Research/WIND/wind_STICS_Verification/20110419_STICS_ACE_Efficiencies_Compare/Fe_Effic.dat";
      effFile = "../efficiencyFiles/Fe_Effic.dat";
      mass = 55.845;
      charge = 12;
      brRange = 0;
      if (selBol == 0) {
      	cout << "No current M-MOQ bins defined for Fe12+." << endl;
      } else if (selBol == 1) {      
      	cout << "Using M-MOQ matrix box for Fe12+" << endl;
      	massLow = 38.45;
      	massHigh = 79.28;
      	moqLow = 4.40;
      	moqHigh = 4.82;  
      }
  } else if (ion == "Fe14+") {
      //effFile = "/shrg1/wind/utility/tables/Fe_Effic.dat";
      //effFile = "/home/jagruesb/Research/WIND/wind_STICS_Verification/20110419_STICS_ACE_Efficiencies_Compare/Fe_Effic.dat";
      effFile = "../efficiencyFiles/Fe_Effic.dat";
      mass = 55.845;
      charge = 14;
      brRange = 0;  
  	  if (selBol == 0) {
  	  	cout << "No current M-MOQ bins defined for Fe14+." << endl;
  	  } else if (selBol == 1) {
      	cout << "Using M-MOQ matrix box for Fe14+" << endl;
      	massLow = 38.45;
      	massHigh = 79.28;
      	moqLow = 3.77;
      	moqHigh = 4.14;
      }
  } else if (ion == "Fe16+") {
      //effFile = "/shrg1/wind/utility/tables/Fe_Effic.dat";
      //effFile = "/home/jagruesb/Research/WIND/wind_STICS_Verification/20110419_STICS_ACE_Efficiencies_Compare/Fe_Effic.dat";
      effFile = "../efficiencyFiles/Fe_Effic.dat";
      mass = 55.845;
      charge = 16;
      brRange = 0;
      if (selBol == 0) {
      	cout << "No current M-MOQ bins for Fe16+." << endl;
      } else if (selBol == 1) {  
      	cout << "Using M-MOQ matrix box for Fe16+" << endl;
      	massLow = 38.45;
      	massHigh = 79.28;
      	moqLow = 3.34;
      	moqHigh = 3.55;
      }
  }

  for (int i = 0; i<32; i++) {
  	TOFLow[i] = 0.0;
  	TOFHigh[i] = 0.0;
  }
  
  ifstream inDataTOF;
  
  inDataTOF.open(TOFFile.c_str());
  
  for (int i = 0; i<32; i++) {
  	if (selBol == 0) {
    	inDataTOF >> TOFLow[i] >> TOFHigh[i];
    } else if (selBol == 1) {
    	TOFLow[i] = -999.99;
    	TOFHigh[i] = -999.99;
    }
  }
  inDataTOF.close();

  /*************************************************************************************
  ------------CALCULATE THE PARTICLE VELOCITY FOR EACH EOQ STEP-------------------------
  *************************************************************************************/
  double vel_part[32];

  for (int i=0;i<32; i++){
  	vel_part[i] = 437.9*sqrt(eoq_SC[i]*(charge/mass));
  }
     

  /*************************************************************************************
  ------------READ IN EFFICIENCY FILE AND PUT INTO VECTORS------------------------------
  *************************************************************************************/
  vector<effRecord> effTable;
  
  cout << "*******    " << progName << " - Setting Efficiencies    *******" << endl;
  
  ifstream inDataEff;
  inDataEff.open(effFile.c_str());
   
  do {
    effRecord effValue;
    inDataEff >> effValue.eoqEff >> effValue.startEff >> effValue.stopEff >> effValue.tripleEff;
    
    effTable.push_back(effValue);
    
  } while (!inDataEff.eof());
  
  inDataEff.close();
  
  /*************************************************************************************
  ------------BEGIN LOOPING THROUGH FILES AND DOING TRANSFORMATION----------------------
  *************************************************************************************/
  int lnpha[60][3][16][3]; //number of phas [edbs][m-moq range][sector][telescope]
  double brw[60][16][3];   //basic rate weighting [edb][sector][telescope]
  double finalFormOutputData[32][16][3]; //final output array [eoq][sector][telescope]
  double countError[32][16][3]; //final count error [eoq][sector][telescope]
  double prevfrac = 0.0;
  double fracdoy;
  
  //initialize the distribution function
  for (int i=0;i<32;i++){
  	for (int j=0;j<16;j++){
  		for (int k=0;k<3;k++){
     		finalFormOutputData[i][j][k] = 0.0;
      		countError[i][j][k] = 0.0;
      	}
  	}
  }
  
  for (int fc=0; fc< numFiles; fc++){
    //Open file in dateFile.dat
    cout << "Opening and Processing " << FILENAMES[fc] << endl;
    smsOpenFile(const_cast<char*>(FILENAMES[fc].c_str())); 
    cout << outFile << endl;
    while(smsReadCycle()==0){  
      
     //initialize variables for cycle
     double timeCyc, hour, min, sec;
     int doy;
     for(int edb = 0; edb < 60; ++edb){
     	for(int j = 0; j < 16; ++j){
        	for(int k = 0; k < 3; k++) {
	 			brw[edb][j][k]=0;	 
	 			for(int i = 0; i < 3; ++i){
	  				lnpha[edb][i][j][k] = 0;
	  			}
			}
       	}	
     }
     
     //read in time information for cycle
     timeCyc = smsgtime();
     hour = floor(timeCyc/3600000.0);
     min  = floor((timeCyc-(hour*3600000.0))/60000.0);
     sec  = floor((timeCyc-(hour*3600000.0)-(min*60000.0))/1000.0);
     
     //calculate doy and fracdoy for this cycle
     doy = smsgdoy();
     fracdoy = (hour/24.0)+(min/(24.0*60.0))+(sec/(24.0*60.0*60.0));
     if (prevfrac == 0.0) prevfrac = doy+fracdoy;
     
     //calculate the basic rate weighting for the cycle for each edb, sector, and telescope
     //first determine the total number of PHAs in each range and sector 
     for (int edb = 0; edb<60; edb++) {
       for (int j=0;j<smsgtnpha(edb); j++) {
	  	long sec=smsgtsector(edb,j);
	  	long rng=smsgtrange(edb,j);
	  	long sanode = smsgtstart(edb,j);
	  	long tel=(sanode-1)>>1;
	  	if (tel <= 2) {
	  		lnpha[edb][rng][sec][tel]++;
	  	}
       } //loop through the phas
     } //loop through the edbs

     //now calculate the basic rate weighting for each edb and each sector
     for (int edb=0; edb<60; edb++) {
       for (int sec=0;sec<16;sec++) {
       	for (int tel=0;tel<3;tel++){
	 		int element=(sec>>1);
	 		int num1 = element*2;
	 		int num2 = element*2+1;
	 		double sumPHA = (double)lnpha[edb][brRange][num1][tel]+(double)lnpha[edb][brRange][num2][tel];
	 		if (sumPHA > 0.0){
	  			if (brRange==0) brw[edb][sec][tel] = smsgtbr0(edb,element)/sumPHA;
	  			if (brRange==1) brw[edb][sec][tel] = smsgtbr1(edb,element)/sumPHA;
	  			if (brRange==2) brw[edb][sec][tel] = smsgtbr2(edb,element)/sumPHA;
	  		} //if there are PHAs in the range
	 		if (brw[edb][sec][tel] < 1.) brw[edb][sec][tel] = 1.0;
	 	} //loop through the telescopes
       } //loop throught the sectors
     } //loop through the edbs

     //accumulate the phas and the basic rate weighted phas
     for (int i=0; i<60; i++) {
       for (int j=0; j<smsgtnpha(i); ++j){
	 	//determine if the PHA falls into the correct mass/moq box
	 	//also dictate what sector the PHA is falling into
	 	int dvsStepPHA = smsgtdvs(i);
	 	if (((selBol == 0) && (smsgttofd(i,j) >= TOFLow[dvsStepPHA]) && (smsgttofd(i,j) <= TOFHigh[dvsStepPHA])) || 
	 	((selBol == 1) && (smsgtmass(i,j) <= massHigh) && (smsgtmass(i,j) >= massLow) && (smsgtmoq(i,j) <= moqHigh) && (smsgtmoq(i,j) >= moqLow))) {
	   		//compute the velocity of the PHA
	   		double eoqPHA = smsgteoq(i,j);
	   
			//determine the telescope number
			long sAnode = smsgtstart(i,j);
			long telNum = (sAnode-1)>>1;
			
	   		//now compute the efficiencies for this edb
	   		double n1 = 0.0;
	   		double n2a = 0.0;
	   		double n2b = 0.0;
	   		int lengthEff = effTable.size();
	   		if (eoqPHA <= effTable[0].eoqEff) {
	    	//if the eoq falls outside of the efficiency table's bounds then use the starting efficiency
	    		n1 = effTable[0].startEff;
	    		n2a = effTable[0].stopEff;
	    		n2b = effTable[0].tripleEff;
	   		} else if (eoqPHA > effTable[lengthEff-1].eoqEff) {
	    		//if the eoq falls outside of the efficiency table's bounds then use the starting efficiency
	    		n1 = effTable[lengthEff-1].startEff;
	    		n2a = effTable[lengthEff-1].stopEff;
	    		n2b = effTable[lengthEff-1].tripleEff;
	   		} else {
	    		//use linear interpolation for eoq in the efficiency tables' bounds
	    		int found = 0;
	    		int IndCount = 0;
	    		int effInd;
	    		double x0, x1, y0, y1;
	    		//find the efficiency in the table
	    		do {
	      			if (effTable[IndCount].eoqEff >= eoqPHA) {
						found = 1;
						effInd = IndCount - 1;
	      			} else {
						IndCount++;
	      			}
	    		} while(found == 0);
	  
	    		//interpolate the start efficiency
	    		x0 = effTable[effInd].eoqEff;
	    		x1 = effTable[effInd+1].eoqEff;
	    		y0 = effTable[effInd].startEff;
	    		y1 = effTable[effInd+1].startEff;
	    		n1 = y0 + (eoqPHA - x0)*((y1-y0)/(x1-x0));
	    		//interpolate the stop efficiency
	    		y0 = effTable[effInd].stopEff;
	    		y1 = effTable[effInd+1].stopEff; 
	    		n2a = y0 + (eoqPHA- x0)*((y1-y0)/(x1-x0));
	    
	    		//interpolate the triple efficiency
	    		y0 = effTable[effInd].tripleEff;
	    		y1 = effTable[effInd+1].tripleEff;
	    		n2b = y0 + (eoqPHA- x0)*((y1-y0)/(x1-x0));
	  		 }
	   
	   		//selection to determine if the mass zero is included (use double efficiency then)
	   		double StopEff = 0.0;
	   		if (massLow == 0.0) {
	     		StopEff = n2b;
	   		} else {
	     		StopEff = n2a;
	   		}
	   
	   		//selection to determine deadtime correction factor based on sector
	   		double DC = 0.0;
	   		if (smsgtsector(i,j) > 0) {
	     		DC = DCOther;
	   		} else {
	     		DC = DC0;
	   		}
	   
	   		//selection to determine the box efficiency to use, Chotoo has a DVS dependent one for hydrogen
	   		double boxEff = 0.0;
	   		if (ion == "H+"){
	    		if (smsgtdvs(i) < 15) {
	     			boxEff = 0.90;
	    		} else {
	     			boxEff = 0.99;
	    		}
	   		} else {
	     		boxEff = 0.98;
	   		}
	   
	   		//compute the distribution function
	   		double df_denom = geomF*delEfac*eoqPHA*charge*n1*StopEff*delT*boxEff;
	   		double df_num = 0.0;
	   		if (telNum <= 2) {
	   			df_num   = brw[i][smsgtsector(i,j)][telNum]*DC;
	   		}
	   		double dJ = df_num/df_denom;
	   		//when using triples the efficiencies are not defined for low energies
	   		//so set to 0.0, instead of having a density of infinity
	   		if (df_denom == 0.0) {
	     		dJ = 0.0;
	   		}
	   
	   		double errInc = 1.0;
	   		//set to zero if efficiency is less than a cutoff value
	   		if (((n1*StopEff) <= effThresh) && (mass != 1.0079)) {
	     		dJ = 0.0;
	     		errInc = 0.0;
	   		}
	   		
	   		//Put the appropriate type of output in the array to be output to the file
	   		if (telNum <= 2) {
				switch (datOutput)
				{
					case 0:
						//output in counts only
						finalFormOutputData[smsgtdvs(i)][smsgtsector(i,j)][telNum] += 1.0;
						break;
					case 1:
						//output the distribution function
						//below only works for triple coincidence
						if (smsgtmass(i,j) > 0.0 && smsgtessd(i,j) > 0.0) {
							finalFormOutputData[smsgtdvs(i)][smsgtsector(i,j)][telNum] += 1.076*(pow(smsgtmass(i,j),2)/(2*smsgtessd(i,j)))*dJ;
						} else {
						//below works for either double or triple coincidence add a selection in here!
							finalFormOutputData[smsgtdvs(i)][smsgtsector(i,j)][telNum] += 1.076*(pow(mass,2)/(2*eoqPHA*charge))*dJ;	   		
						}
						break;
					case 2:
						//output he differential flux
						finalFormOutputData[smsgtdvs(i)][smsgtsector(i,j)][telNum] += dJ;
						break;
				}
				//Now increment the error array
				countError[smsgtdvs(i)][smsgtsector(i,j)][telNum] += errInc;
			} //selection to make sure that only PHAs with valid telescope numbers are considered
	 		}//PHA selection structure
       	}//loop through the phas in the current edb
     } //loop through the edbs
     
     //now print to file if enough energy bins are populated
     double doyFracNow = smsgdoy()+fracdoy;
     double cyc_accum = (doyFracNow-prevfrac)*(24.0*60.0*60.0);
     
     //process the error checking to see if we will output to the file
     double goodBins = 0.0;
     //determine bin range based on the velocity threshhold
	 int found = 0;
	 int velIndCheck = 0;
	 do {
		if (vel_part[velIndCheck] >= velThresh) {
			found = 1;
		} else if (velIndCheck > 32) {
			found = 1;
			velIndCheck = 31;
		} else {
			velIndCheck += 1;
		}
	 } while(found == 0); 
	 //Need to compress the output data based on desired compression in order to test adaptive cadence
	 //First, full 3-d
	 if (dirBol == 0) {
	 	for (int i=velIndCheck; i<32; i++) {
     		for (int j=0; j<16; j++) {
     			for (int k=0; k<3; k++) {
     				if (countError[i][j][k] > 0.0) {
						double errorStep = (pow(countError[i][j][k],0.5)/countError[i][j][k])*(finalFormOutputData[i][j][k]/cyc_accum);
						double errCheck = errorStep/(finalFormOutputData[i][j][k]/cyc_accum);
						if (errCheck < errThresh) {
							goodBins += 1;
						}
					}
				}
	 		}
	 	}
	 //Next, if we want the 2-d only separated by sector
	 } else if (dirBol == 1) {
	 	for (int i=velIndCheck; i<32; i++) {
	 		for (int j=0; j<16; j++) {
     			double dfCompress = 0.0;
     			double errCompress = 0.0;
     			for (int k=0; k<3; k++) {
     				dfCompress += finalFormOutputData[i][j][k];
     				errCompress += countError[i][j][k];
     			}
     			if (errCompress > 0.0) {
					double errorStep = (pow(errCompress,0.5)/errCompress)*(dfCompress/(cyc_accum*3.0));
					double errCheck = errorStep/(dfCompress/(3.0*cyc_accum));
					if (errCheck < errThresh) {
						goodBins += 1;
					}
				}
			}
	 	}
	 //Finally, if we want to compress both sector and telescope
	 } else if (dirBol == 2) {
     	for (int i=velIndCheck; i<32; i++) {
     		double dfCompress = 0.0;
     		double errCompress = 0.0;
     		for (int j=0; j<16; j++) {
     			for (int k=0; k<3; k++) {
     				dfCompress += finalFormOutputData[i][j][k];
     				errCompress += countError[i][j][k];
     			}
     		}
     		if (errCompress > 0.0) {
				double errorStep = (pow(errCompress,0.5)/errCompress)*(dfCompress/(cyc_accum*16.0*3.0));
				double errCheck = errorStep/(dfCompress/(16.0*3.0*cyc_accum));
				if (errCheck < errThresh) {
					goodBins += 1;
				}
			}
	 	}
	 } 
     
     //Print the data line to the file if we satisfy the number of bins
     
     if (goodBins >= numBins) {
     	//create time variables
     	double yearNow = smsgyear();
     	prevfrac = doyFracNow;
     	
     	//if user wants one, create the moment and nvt file
     	if (momFile == 1) {
			
			//calculate the current solar wind velocity
			double velSW = calcVelSWE(sweData, doyFracNow, cyc_accum);
			double velCutOff = wCutOff*velSW;
			
			//calculate the moments from the obtained distribution function
			moments moments3D;
			moments3D = calcMoments(finalFormOutputData, countError, vel_part, cyc_accum, velCutOff);

			//Now print the moments to file
			outMomFile << setw(8) << setprecision(2) << fixed << yearNow;
			outMomFile << setw(13) << setprecision(4) << fixed << doyFracNow;
			outMomFile << setw(9) << ion;
			outMomFile << setw(13) << setprecision(4) << scientific << moments3D.n;
			outMomFile << setw(13) << setprecision(4) << scientific << moments3D.n_err;
			outMomFile << setw(13) << setprecision(2) << fixed << moments3D.v_0;
			outMomFile << setw(13) << setprecision(4) << fixed << moments3D.v_0_err;
			outMomFile << setw(13) << setprecision(2) << fixed << moments3D.v_th;
			outMomFile << setw(13) << setprecision(4) << fixed << moments3D.v_th_err;
			outMomFile << setw(13) << setprecision(2) << scientific << cyc_accum << endl;	
			
			//Now work on the sectored moment file	
			//calculate the moments from the obtained distribution function
			vector<momentsBySector> momentsSec;
			momentsSec = calcMomentsBySector(finalFormOutputData, countError, vel_part, cyc_accum, velCutOff);

			//Now print the moments to file
			for (int indSec = 0; indSec < 16; indSec++) {
				outMomSecFile << setw(8) << setprecision(2) << fixed << yearNow;
				outMomSecFile << setw(13) << setprecision(4) << fixed << doyFracNow;
				outMomSecFile << setw(9) << ion;
				outMomSecFile << setw(13) << setprecision(2) << indSec;
				outMomSecFile << setw(13) << setprecision(4) << scientific << momentsSec[indSec].f0;
				outMomSecFile << setw(13) << setprecision(4) << scientific << momentsSec[indSec].f0_err;
				outMomSecFile << setw(13) << setprecision(4) << scientific << momentsSec[indSec].f1;
				outMomSecFile << setw(13) << setprecision(4) << scientific << momentsSec[indSec].f1_err;
				outMomSecFile << setw(13) << setprecision(4) << scientific << momentsSec[indSec].f2;
				outMomSecFile << setw(13) << setprecision(4) << scientific << momentsSec[indSec].f2_err;
				outMomSecFile << setw(13) << setprecision(2) << scientific << cyc_accum << endl;
			}
		}	

     	if (dirBol == 0) {
			for (int i=0; i<32; i++){
				for (int j=0; j<16; j++){
					for (int k=0; k<3; k++){
						outFile << setw(8) <<  setprecision(2) << fixed << yearNow;
						outFile << setw(13) << setprecision(4) << fixed << doyFracNow;
						outFile << setw(13) << j;
						outFile << setw(13) << k;
						outFile << setw(12) << eoq_SC[i];
						outFile << setw(9) << ion;
						double delTNorm = cyc_accum;
						if (datOutput == 0) delTNorm = 1.0;
						outFile << setw(14) << setprecision(2) << scientific << (finalFormOutputData[i][j][k])/delTNorm;
						if (countError[i][j][k] > 0.0) {
							outFile << setw(14) << setprecision(2) << scientific << (pow(countError[i][j][k],0.5)/countError[i][j][k])*(finalFormOutputData[i][j][k]/delTNorm);
						} else {
							outFile << setw(14) << setprecision(2) << scientific << 0.0;
						}
						outFile << setw(14) << setprecision(2) << scientific << cyc_accum;
						outFile << endl;
						finalFormOutputData[i][j][k] = 0.0;
						countError[i][j][k] = 0.0;
					} //loop through the telescopes in the output file for the 3d version
				} //loop through the sectors in the output file for the 3d version
			} //loop through DVS steps in the output file for the 3d version
		} else if (dirBol == 1) { // selection for the dir Boolean
			for (int i=0; i<32; i++){
				for (int j=0; j<16; j++){
					//compress each telescope angle
					double DFPrint = 0.0;
					double countErrorPrint = 0.0;
					for (int k=0; k<3; k++){
						DFPrint += finalFormOutputData[i][j][k];
						countErrorPrint += countError[i][j][k];
						finalFormOutputData[i][j][k] = 0.0;
						countError[i][j][k] = 0.0;
					}
					//now print the file
					outFile << setw(8) <<  setprecision(2) << fixed << yearNow;
					outFile << setw(13) << setprecision(4) << fixed << doyFracNow;
					outFile << setw(13) << j;
					outFile << setw(13) << "4"; 
					outFile << setw(12) << eoq_SC[i];
					outFile << setw(9) << ion;
					double delTNorm = 3.0*cyc_accum;
					if (datOutput == 0) delTNorm = 1.0;
					outFile << setw(14) << setprecision(2) << scientific << (DFPrint)/(delTNorm);
					if (countErrorPrint > 0.0) {
						outFile << setw(14) << setprecision(2) << scientific << (pow(countErrorPrint,0.5)/countErrorPrint)*(DFPrint/(delTNorm));
					} else {
						outFile << setw(14) << setprecision(2) << scientific << 0.0;
					}
					outFile << setw(14) << setprecision(2) << scientific << cyc_accum;
					outFile << endl;
				} //loop through the sectors in the output file for the 2d version
			} //loop through DVS steps in the output file for the 2d version	
		} else if (dirBol == 2) { // selection for the dir Boolean
			for (int i=0; i<32; i++){
				//compress each sector angle
				double DFPrint = 0.0;
				double countErrorPrint = 0.0;
				for (int j=0; j<16; j++){
					for (int k=0; k<3; k++){
						DFPrint += finalFormOutputData[i][j][k];
						countErrorPrint += countError[i][j][k];
						finalFormOutputData[i][j][k] = 0.0;
						countError[i][j][k] = 0.0;
					}
				}
				//now print the file
				outFile << setw(8) <<  setprecision(2) << fixed << yearNow;
				outFile << setw(13) << setprecision(4) << fixed << doyFracNow;
				outFile << setw(13) << "17";
				outFile << setw(13) << "4"; 
				outFile << setw(12) << eoq_SC[i];
				outFile << setw(9) << ion;
				double delTNorm = 16.0*3.0*cyc_accum;
				if (datOutput == 0) delTNorm = 1.0;
				outFile << setw(14) << setprecision(2) << scientific << (DFPrint)/(delTNorm);
				if (countErrorPrint > 0.0) {
					outFile << setw(14) << setprecision(2) << scientific << (pow(countErrorPrint,0.5)/countErrorPrint)*(DFPrint/(delTNorm));
				} else {
					outFile << setw(14) << setprecision(2) << scientific << 0.0;
				}
				outFile << setw(14) << setprecision(2) << scientific << cyc_accum;
				outFile << endl;
			} //loop through DVS steps in the output file for the 1d version
		} //selection for the dir Boolean
	} //selection for the accumulation time
		
		
		
    } //loop through cycles in the open file
  	//close current wind file
  	smsCloseFile();
  } //loop files

  //close the output file and return to command prompt
  outFile.close();
  
  return 0;
} //end of program


/*********************************************************************

	Function to calculate the average solar wind density
	
	input:  swe - structure holding the swe data
			doy - current doy interested in
			cyc_accum - the length which the distfunc data has been accumulated for
	
	output: vel - average solar wind velocity output
	
*********************************************************************/

double calcVelSWE(vector<SWERecord> swe, double doy, double cyc_accum) {
	
		//declare any necessary variables
		double vel = 0.0;
		double numPoints = 0.0;
		
		//determine the end doyFr
		double doyEnd = doy + cyc_accum/SEC_IN_DAY;
		
		//now loop through the SWE file and average the velocity
		for (unsigned int i=0; i<swe.size(); i++) {
			//if the doy is within the bounds then add in the velocity
			double sweTime = swe[i].day+swe[i].fracday;
			if (sweTime >= doy && sweTime < doyEnd) {
				vel += sqrt(pow(swe[i].Vx,2)+pow(swe[i].Vy,2)+pow(swe[i].Vz,2));
				numPoints += 1.0;
			}
		}
		
		//finally, normalize the velocity based on the number of points including in the addition
		vel /= numPoints;
		
		//return the velocity
		return vel;
}
