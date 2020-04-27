/****************************************************************

filename: wtdcLV2_IonicRatio_Suprathermal.cpp

purpose: Function which will read in two Wind/STICS distfunc files and 
		 calculates the ionic ratio for each time step, from only the 
		 suprathermal contribution.
		 
****************************************************************/

//header files to include
#include "wtdcLV2_Ion_Supra.h"

//name the namespace
using namespace std;

//declaring the function prototypes
vector<distFuncRecord> readFile(string);
double calcVelParticle(string, double);
double deltaV(string,double);
vector<doyRecord> uniqDoy(vector<distFuncRecord>);
string cleanWSString(string);
vector<double> calcDens(vector<distFuncRecord>, double, double, double);
double calcVelSWE(vector<SWERecord>, double, double);

int main () {

	/*************************************************************************************
	------------DECLARE SYSTEM TYPE VARIABLES NECESSARY FOR THE FUNCTION------------------
	*************************************************************************************/
	
	//string containing the filenames
	string ionTopFileName, ionBottomFileName, SWEFilename, fileTag;
	
	//vector structures to hold the contents of these two files
	vector<distFuncRecord> ionTop;
	vector<distFuncRecord> ionBot;

	//output data variables
	ofstream outData;
	ifstream inData;
	
	//other program dependent variables
	string progName = "wtdcLV2_IonicRatio_Suprathermal";

	//density calculation stuff
	double wVelCutOff = 0.0;
	
	/*************************************************************************************
	------------OPEN UP AND READ DRIVER FILE----------------------------------------------
	*************************************************************************************/
	inData.open("driver.dat");
	inData >> ionTopFileName;
	inData >> ionBottomFileName;
	inData >> wVelCutOff;
	inData >> SWEFilename;
	inData >> fileTag;
	inData.close();
	
	/*************************************************************************************
	------------OPEN UP AND READ EACH DISTFUNC FILE---------------------------------------
	*************************************************************************************/
	
	cout << "******** -Reading in the Distribution Function Files-  **************"<< endl;
	
	ionTop = readFile(ionTopFileName);
	ionBot = readFile(ionBottomFileName);
	
	//check that both files are 3-d distfunc files
	if (ionTop[0].sector == 17 || ionTop[0].telescope == 4 || 
		ionBot[0].sector == 17 || ionBot[0].telescope == 4) {
		cout << "Please only enter 3-dimensional distribution function files" << endl;
		exit(0);
	}

	/*************************************************************************************
	------------OPEN UP AND READ SWE FILE-------------------------------------------------
	*************************************************************************************/

	vector<SWERecord> SWEData;
  
	cout << "*******    " << progName << " - Reading in SWE Data    *******" << endl;

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
	string outFileName = progName+"_"+fileTag+".dat";
	outData.open(outFileName.c_str());
	
	//Now write the header
	time_t rawtime;
  	time(&rawtime);
  
  	outData << "# Ionic Ratios created from Wind Level II distribution functions - Lite Version" << endl;
  	outData << "# Created: " << ctime(&rawtime);
  	outData << "#" << endl;
  	outData << "#   year";
  	outData << setw(13) << "doy";
  	outData << setw(13) << "Ion Top";
  	outData << setw(13) << "Ion Bottom";
  	outData << setw(13) << "Ionic Ratio";
  	outData << setw(13) << "Ratio_error" << endl;

	/*************************************************************************************
	------------CALCULATE THE RATIO AND OUTPUT TO FILE------------------------------------
	*************************************************************************************/

	cout << "***************** " << progName << " - beginning ratio calculation - ***********" << endl;

	// determine the unique doys that we will be processing
	vector<doyRecord> doys;
	doys = uniqDoy(ionTop);
	
	//Determine the densities for both the upper and lower ion for each doy
	for (unsigned int i=0; i< doys.size(); i++) {
		//First, determine the solar wind velocity from the SWE file
		double velSW = calcVelSWE(SWEData, doys[i].doy, doys[i].cyc_accum);
		
		//Now, calculate the density for each ion
		vector<double> nTop (2,0);
		vector<double> nBot (2,0);
		
		nTop = calcDens(ionTop, doys[i].doy, wVelCutOff, velSW);
		nBot = calcDens(ionBot, doys[i].doy, wVelCutOff, velSW);
		
		//Calculate the ratio and the error in the ratio
		double ratio = 0.0;
		double ratio_err = 0.0;
		
		if (nBot[0] > 0.0) {
			//able to calculate ratio
			ratio = nTop[0]/nBot[0];
			ratio_err = ratio*sqrt( pow((nTop[1]/nTop[0]),2.0) + pow((nBot[1]/nBot[0]),2.0));
		} else {
			//ratio will blow up since bottom density is 0
			ratio = -sqrt(-1.0);
			ratio_err = ratio;
		}
		
		//Output to file
	  	outData << setw(8) << doys[i].year;
		outData << setw(13) <<  setprecision(4) << doys[i].doy;
		outData << setw(13) << ionTop[0].ion;
		outData << setw(13) << ionBot[0].ion;
		outData << setw(13) << setprecision(5) << ratio;
		outData << setw(13) << setprecision(5) << ratio_err << endl;
	} // loop through the unique doys
	
	//close output file and end function
	outData.close();
} //End of Main Function
