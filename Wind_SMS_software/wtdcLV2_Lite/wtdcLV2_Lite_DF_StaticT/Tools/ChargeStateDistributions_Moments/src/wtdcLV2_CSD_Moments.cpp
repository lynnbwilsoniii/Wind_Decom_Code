
/****************************************************************

filename: wtdcLV2_CSD_Moments.cpp

purpose: Function which will read in two Wind/STICS moment files and 
		 calculates the charge state distribution for a given atomic
		 element
		 
****************************************************************/

//header files to include
#include "wtdcLV2_CSD.h"

//name the namespace
using namespace std;

//declaring the function prototypes
double checkStringNan(string);
vector<string> getFileNames(string, string, string, int);
vector<momentFile> readFile(string);
string intToString(int);
double cleanDouble(double);

int main() {

	/*************************************************************************************
	------------DECLARE SYSTEM TYPE VARIABLES NECESSARY FOR THE FUNCTION------------------
	*************************************************************************************/
	//variable to hold contents of driver file which controls input files
	string dataDir, atom, timePeriod;
	int atomNum; 
	
	//structure to hold the moment files
	vector< vector<momentFile> > ionDistributionMoments;
	vector<unsigned int> ionsProcessed;
	
	//other variables that the program uses
	ofstream outData;
	ifstream inData;
	string progName = "wtdcLV2_CSD";
	
	/*************************************************************************************
	------------OPEN UP AND READ DRIVER FILE----------------------------------------------
	*************************************************************************************/
	inData.open("driver.dat");
	inData >> dataDir;
	inData >> atom;
	inData >> atomNum;
	inData >> timePeriod;
	inData.close();

	//Need to get a list of the filenames
	vector<string> fileNames = getFileNames(atom, timePeriod, dataDir, atomNum);
	unsigned int numChargeStates = fileNames.size();
	
	/*************************************************************************************
	------------OPEN UP AND READ EACH MOMENT FUNCTION FILE--------------------------------
	*************************************************************************************/

	cout << "-------------------Reading in the Moment Files-----------------"<< endl;
	
	int fullNumber = numChargeStates;
	
	for (int i = 0; i<fullNumber; i++) {
		//temp structure to hold the current charge state file
		vector<momentFile> ionMomentFile;
		
		//read in each charge state file
		cout << progName << " -  Charge State " << (i+1) <<" file" << endl;
		ionMomentFile = readFile(fileNames[i]);
		
		//put into the vector holding all the atoms files
		if (ionMomentFile[0].year > 0.0) {
			ionDistributionMoments.push_back(ionMomentFile);
			ionsProcessed.push_back(i+1);
		}
	}
	
	/*************************************************************************************
	------------CREATE THE OUTPUT FILE AND WRITE HEADERS----------------------------------
	*************************************************************************************/
	
	//open the file
	string outFileName = progName+"_"+atom+"_"+timePeriod+".dat";
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
 	
	//loop through the top ion structure and calculate the ratio for each value
	for (unsigned int indDoy = 0; indDoy < ionDistributionMoments[0].size(); indDoy++) {
			
		//begin by calculating the total density for the time period	
		//declare variables to be used in this calculation
		double total_n = 0.0;
		double total_n_err = 0.0;
		
		//add in the first ion information as we know the time index for this
		total_n += cleanDouble(ionDistributionMoments[0][indDoy].n);
		total_n_err += pow(cleanDouble(ionDistributionMoments[0][indDoy].n_err),2.0);
		
		//now loop through the total charge states adding up the contributions from each
		for (unsigned int i=1; i< ionsProcessed.size(); i++) {
			int ionInd = 0;
			bool foundBottom = false;
		
			//find the index in the ion structure which has the same doy as the current doy of the first ion
			while (!foundBottom) {
				if (ionDistributionMoments[0][indDoy].doy == ionDistributionMoments[i][ionInd].doy) {
					foundBottom = true;
				} else {
					ionInd++;
				} //loop to find bottom index
			}
			
			//Finally, add the contribution from this ion to the total density
			total_n += cleanDouble(ionDistributionMoments[i][ionInd].n);
			total_n_err += pow(cleanDouble(ionDistributionMoments[i][ionInd].n_err),2.0);
			
		} // loop to through all the ions adding into the total results
		
		//take square root of error
		total_n_err = sqrt(total_n_err);
		
		//begin writing info to the Output to file
	  	outData << setw(8) << ionDistributionMoments[0][indDoy].year;
		outData << setw(13) <<  setprecision(4) << ionDistributionMoments[0][indDoy].doy;
		
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
			
				if (total_n > 0.0) {
					//able to calculate the ratio
					ratio = ionDistributionMoments[chargeInd][indDoy].n / total_n;
					ratio_err = ratio*sqrt( pow((ionDistributionMoments[chargeInd][indDoy].n_err/ionDistributionMoments[chargeInd][indDoy].n),2.0) +
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
		
	} // for loop going through the top ion structure

	//close file and program
	outData.close();

} //end main function
