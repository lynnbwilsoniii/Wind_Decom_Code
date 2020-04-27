/****************************************************************

filename: wtdcLV2_IonicRatio.cpp

purpose: Function which will read in two Wind/STICS moment files and 
		 calculates the ionic ratio for each time step
		 
****************************************************************/

//header files to include
#include "wtdcLV2_Ion.h"

//name the namespace
using namespace std;

//declaring the function prototypes
double checkStringNan(string);
vector<momentFile> readFile(string);

int main () {

	/*************************************************************************************
	------------DECLARE SYSTEM TYPE VARIABLES NECESSARY FOR THE FUNCTION------------------
	*************************************************************************************/
	//variable to hold contents of driver file which controls input files
	string momentFile1, momentFile2, fileTag;
	
	//structures to hold the moment files
	vector<momentFile> ionTop;
	vector<momentFile> ionBottom;

	//other variables that the program uses
	ofstream outData;
	ifstream inData;
	string progName = "wtdcLV2_IonicRatio";

	/*************************************************************************************
	------------OPEN UP AND READ DRIVER FILE----------------------------------------------
	*************************************************************************************/
	inData.open("driver.dat");
	inData >> momentFile1;
	inData >> momentFile2;
	inData >> fileTag;
	inData.close();
	
	/*************************************************************************************
	------------OPEN UP AND READ EACH MOMENT FUNCTION FILE--------------------------------
	*************************************************************************************/

	cout << "-------------------Reading in the Moment Files-----------------"<< endl;
	cout << progName << " - beginning with the first file" << endl;
	
	ionTop = readFile(momentFile1);
	
	cout << progName << " - Now reading the second file" << endl;
	ionBottom = readFile(momentFile2);
	
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
  	
	//loop through the top ion structure and calculate the ratio for each value
	for (unsigned int i = 0; i < ionTop.size(); i++) {
		
		//declare variables to be used in this calculation
		double ratio = 0.0;
		double ratio_err = 0.0;
		int bottomInd = 0;
		bool foundBottom = false;
		
		//find the index in the bottom ion structure which has the same doy as the current
		//top ion
		while (!foundBottom) {
			if (ionTop[i].doy == ionBottom[bottomInd].doy) {
				foundBottom = true;
			} else {
				bottomInd++;
			}
		} // loop to find bottom index
		
		// calculate the ratio and the error in the ratio if able
		if (ionBottom[bottomInd].n > 0.0) {
			//able to calculate the ratio
			ratio = ionTop[i].n / ionBottom[bottomInd].n;
			ratio_err = ratio*sqrt( pow((ionTop[i].n_err/ionTop[i].n),2.0) +
									pow((ionBottom[bottomInd].n_err/ionBottom[bottomInd].n),2.0));
		} else {
			//not able to calculate the ratio
			ratio = -sqrt(-1.0);
			ratio_err = ratio;
		}
		
		//Output to file
	  	outData << setw(8) << ionTop[i].year;
		outData << setw(13) <<  setprecision(4) << ionTop[i].doy;
		outData << setw(13) << ionTop[i].ion;
		outData << setw(13) << ionBottom[bottomInd].ion;
		outData << setw(13) << setprecision(5) << ratio;
		outData << setw(13) << setprecision(5) << ratio_err << endl;;	
		
	} // for loop going through the top ion structure
	
	//close the output file now that we are done
	outData.close();
	
} // main function
