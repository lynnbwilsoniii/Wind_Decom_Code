/****************************************************************

filename: wtdcLV2_CSD_Supra.cpp

purpose: Function which will read in Wind/STICS distribution function
		 files and calculates the charge state distribution for a 
		 given atomic element
		 
****************************************************************/

//header files to include
#include "wtdcLV2_CSD_Supra.h"

//name the namespace
using namespace std;

//declaring function prototypes
vector<string> getFileNames(string, string, string, int);
string intToString(int);
vector<distFuncRecord> readFile(string);
vector<doyRecord> uniqDoy(vector<distFuncRecord>);
double calcVelSWE(vector<SWERecord>, double, double);
vector<double> calcDens(vector<distFuncRecord>, double, double, double) ;
string cleanWSString(string);
double calcVelParticle(string, double);
double deltaV(string,double);

int main() {

	/*************************************************************************************
	------------DECLARE SYSTEM TYPE VARIABLES NECESSARY FOR THE FUNCTION------------------
	*************************************************************************************/

	//string containing the filenames
	string SWEFilename, atom, timePeriod, dataDir;
	int atomNum;
	
	//file IO support
	ifstream inData;
	ofstream outData;
	
	//structures to hold the distribution function files
	vector< vector<distFuncRecord> > ionsDF;
	vector< unsigned int> ionsProcessed;
	
	//Extra Variables we use
	string progName = "wtdcLV2_CSD_Supra";
	
	//density calculation cutoff definition
	double wCutOff = 0.0;
	
	/*************************************************************************************
	------------OPEN UP AND READ DRIVER FILE----------------------------------------------
	*************************************************************************************/
	inData.open("driver.dat");
	inData >> dataDir;
	inData >> atom;
	inData >> atomNum;
	inData >> wCutOff;
	inData >> timePeriod;
	inData >> SWEFilename;
	
	inData.close();
	/*************************************************************************************
	------------OPEN UP AND READ EACH DISTFUNC FILE---------------------------------------
	*************************************************************************************/

	//first step is to get the list of filenames which will be opened up
	vector<string> dfFilenames = getFileNames(atom, timePeriod, dataDir, atomNum);
	unsigned int numChargeStates = dfFilenames.size();
	
	cout << endl;
	cout << "*******    - " << progName << " - Reading in the Distribution Function Files -    *******"<< endl;
	cout << endl;
	
	int fullNumber = numChargeStates;
	
	for (int i = 0; i<fullNumber; i++) {
		//temp structure to hold the current charge state file
		vector<distFuncRecord> ionDFFile;
		
		//read in each charge state file
		cout << "*******    - " << progName << " -  Charge State " << (i+1) <<" file    *******" << endl << endl;
		ionDFFile = readFile(dfFilenames[i]);
		
		//put into the vector holding all the atoms files if the file was opened
		if (ionDFFile[0].year > 0.0) {
			ionsDF.push_back(ionDFFile);
			ionsProcessed.push_back(i+1);
		}
	}

	/*************************************************************************************
	------------OPEN UP AND READ SWE FILE-------------------------------------------------
	*************************************************************************************/

	vector<SWERecord> SWEData;
  
	cout << "*******    - " << progName << " - Reading in SWE Data -    *******" << endl << endl;

	inData.open(SWEFilename.c_str());
  
	do {
		SWERecord SWEEntry;
		double temp[9];

		inData >> SWEEntry.year >> SWEEntry.day >> SWEEntry.fracday >> temp[0] >> temp[1] >>
	  		temp[2] >> temp[3] >> temp[4] >> temp[5] >> SWEEntry.Vx >> SWEEntry.Vy >> SWEEntry.Vz >>
	  		temp[6] >> temp[7] >> temp[8];
  
		SWEData.push_back(SWEEntry);
	} while (!inData.eof());
	
	/*************************************************************************************
	------------CREATE THE OUTPUT FILE AND WRITE HEADERS----------------------------------
	*************************************************************************************/	
	
	//open the file
	string outFileName = progName+"_"+atom+"_"+timePeriod+"_Supra.dat";
	outData.open(outFileName.c_str());
	
	//Now write the header
	time_t rawtime;
  	time(&rawtime);
  
  	outData << "# Charge state distribution created from Wind Level II distribution functions - Lite Version" << endl;
  	outData << "# Created: " << ctime(&rawtime);
  	outData << "#" << endl;
  	outData << "#   year";
  	outData << setw(13) << "doy";
  	for (unsigned int i = 0; i < numChargeStates; i++) {
  		outData << setw(13) << "n_"+atom+intToString(i+1)+"+";
  		outData << setw(13) << "n_"+atom+intToString(i+1)+"+_err";
  	}
  	outData << endl;
  	
  	  	
	/*************************************************************************************
	------------CALCULATE THE RATIO AND OUTPUT TO FILE------------------------------------
	*************************************************************************************/
	
	cout << "*******    - " << progName << " - beginning ratio calculation -    *******" << endl;

	// determine the unique doys that we will be processing
	vector<doyRecord> doys;
	doys = uniqDoy(ionsDF[0]);
	
	//Determine the densities for both the top ion and the total density in the denominator
	for (unsigned int indDoy=0; indDoy< doys.size(); indDoy++) {
	
		//First, determine the solar wind velocity from the SWE file
		double velSW = calcVelSWE(SWEData, doys[indDoy].doy, doys[indDoy].cyc_accum);
		
		//calculate the denominator of the ratio, the total density
		double total_n = 0.0;
		double total_n_err = 0.0;
		vector<double> n_ion (2,0.0);
		
		//add in the first ion information as we know the time index for this
		n_ion = calcDens(ionsDF[0], doys[indDoy].doy, wCutOff, velSW);
		total_n += n_ion[0];
		total_n_err += pow(n_ion[1],2.0);
		
		//now loop through the total charge states adding up the contributions from each
		for (unsigned int i=1; i< ionsProcessed.size(); i++) {

			//add the contribution from this ion to the total density
			n_ion = calcDens(ionsDF[i], doys[indDoy].doy, wCutOff, velSW);
			total_n += n_ion[0];
			total_n_err += pow(n_ion[1],2.0);
			
		} // loop to through all the ions adding into the total results
				
		//take square root of error
		total_n_err = sqrt(total_n_err);
		
		//begin writing info to the Output to file
	  	outData << setw(8) << doys[indDoy].year;
		outData << setw(13) <<  setprecision(4) << doys[indDoy].doy;
		
		// now loop through all the ions and calculate the ratio and the error in the ratio if able
		for (unsigned int i=0; i< numChargeStates; i++) {
			
			//check to see if the current ion has been processed and what it's index is 
			bool looping = true;
			bool processed = false;
			unsigned int chargeInd = 0;
			while (looping) {
				if (ionsProcessed[chargeInd] == (i+1)) {
					looping = false;
					processed = true;
				} else if (chargeInd < ionsProcessed.size()) {
					chargeInd++;
				} else {
					looping = false;
				} 
			}
			
			if (processed) {
				//finally calculate the 
				double ratio = 0.0;
				double ratio_err = 0.0;
			
				//calculate the density for this ion
				n_ion = calcDens(ionsDF[i], doys[indDoy].doy, wCutOff, velSW);
				
				//now check if we can calculate the charge state ratio
				if (total_n > 0.0 && n_ion[0] > 0.0) {
					//able to calculate the ratio
					ratio = n_ion[0] / total_n;
					ratio_err = ratio*sqrt( pow((n_ion[1]/n_ion[0]),2.0) +
										pow((total_n_err/total_n),2.0));
				} else {
					//not able to calculate the ratio
					ratio = -sqrt(-1.0);
					ratio_err = ratio;
				}
				
				//write the relative density for this ion to the file
				outData << setw(13) << setprecision(5) << ratio;
				outData << setw(13) << setprecision(5) << ratio_err;
			} else {
				outData << setw(13) << setprecision(5) << -sqrt(-1.0);
				outData << setw(13) << setprecision(5) << -sqrt(-1.0);
			} // check to see if ion was processed
		} //loop through all the charge states	
		outData << endl;
	
	} // loop through the day of years
	
	//close file and program
	cout << endl;
	cout << "*******    - " << progName << " - Closing Output File, Program Ending -    *******" << endl << endl;
	outData.close();
}
