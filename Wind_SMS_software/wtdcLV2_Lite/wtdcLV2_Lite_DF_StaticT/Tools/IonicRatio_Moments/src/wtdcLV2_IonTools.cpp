//supplemental functions which can be used by the various programs in the
//tools

//include declarations
#include <wtdcLV2_Ion.h>

//name the namespace
using namespace std;

/**********************************************************************

	Function to check the string input from a datafile and if it is a 
	nan, return the double representation of a nan. If not, return the 
	double value.
	
	input: input - string that is read from the data file
	
	output: outputValue - the double representation of the value in the file
	
*********************************************************************/

double checkStringNan(string input) {
	
	//declare any variables
	double outputValue = 0.0;
	
	//check to see if the input was a nan
	if (input == "nan") {
		outputValue = sqrt(-1.0);
	} else {
		outputValue = atof(input.c_str());
	}
	
	//return the double value
	return outputValue;
	
}

/*********************************************************************

	Function to read in the contents of the nvt file and populate the
	appropriate structure
	
	input: fileName - string name of the moment file
	
	output: momentFile - structure which holds all the values from the file
	
*********************************************************************/

vector<momentFile> readFile(string fileName) {

	//declare variables which we will need
	ifstream inData;
	vector<momentFile> momentFileData;
	
	//open file
	inData.open(fileName.c_str());
	
	//read the first four header lines
	string line;
	for (int i=0;i<4;i++) getline(inData, line);

	//now read in the file
	while(getline(inData,line)) {
	
		//declare the entry structure
		momentFile momentEntry;

		//now read in and place data into the structure		
		momentEntry.year = atof(line.substr(0,8).c_str());
		momentEntry.doy = atof(line.substr(8,13).c_str());
		momentEntry.ion = line.substr(21,9).c_str();
		momentEntry.n = checkStringNan(line.substr(30,13));
		momentEntry.n_err = checkStringNan(line.substr(43,13));
		momentEntry.v = checkStringNan(line.substr(56,13));
		momentEntry.v_err = checkStringNan(line.substr(69,13));
		momentEntry.v_th = checkStringNan(line.substr(82,13));
		momentEntry.v_th_err = checkStringNan(line.substr(95,13));
		momentEntry.cyc_accum = atof(line.substr(108,13).c_str());
		
		//add the entry to the vector
		momentFileData.push_back(momentEntry);
	}
	
	//close file
	inData.close();
	
	//return the structure
	return momentFileData;
}
