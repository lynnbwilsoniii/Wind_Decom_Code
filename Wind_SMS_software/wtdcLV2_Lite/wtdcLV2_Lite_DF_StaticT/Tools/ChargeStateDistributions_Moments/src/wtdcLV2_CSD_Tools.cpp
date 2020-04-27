//supplemental functions which can be used by the various programs in the
//tools

//include declarations
#include <wtdcLV2_CSD.h>

//name the namespace
using namespace std;

/*********************************************************************

	Function to change an integer into a string
	
	input: integer - input integer
	
	output: outString - output string
	
*********************************************************************/

string intToString(int integer) {
	
	ostringstream sin;
	sin << integer;
	string outString = sin.str();

	return outString;
}

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

/**********************************************************************

	Function to check if a value is a nan, if so return 0. We use this so
	that all values don't become a nan if accumulated through
	
	input: input - double that is provided to look at 
	
	output: outputValue - the double that has potentially been cleaned of nan
	
*********************************************************************/

double cleanDouble(double input) {
	
	//declare any variables
	double outputValue = input;
	
	//check to see if the input was a nan
	if (input != input) { //this is only true if input is a NAN, according to IEEE standard properties of Nan
		outputValue = 0.0;
	}

	//return the double value
	return outputValue;
	
}


/*********************************************************************

	Function to build a list of filenames for the charge state distribution
	calculation based on a particular atom
	
	input: atom - string holding the element
		   timePeriod - string holding the time range, in the same format of the 
		   				distribution function file
		   dataDir - string with the name of the directory holding the data files
	
	output: fileNames - vector holding all of the filenames
	
*********************************************************************/

vector<string> getFileNames(string atom, string timePeriod, string dataDir, int atomicNumber) {

	//declare the variables we need
	string fileBase = "wtdcLV2_Lite_nvt_";
	
	//based on the element we generate a list of files
	vector<string> fileNames;
	
	if (atom == "H") {
		fileNames.push_back(dataDir+fileBase+atom+"+_"+timePeriod+".dat");
	} else if (atom != "H") {
		for (int i=0; i<atomicNumber; i++) {
			if (i == 0) {
				fileNames.push_back(dataDir+fileBase+atom+"+_"+timePeriod+".dat");
			} else {
				fileNames.push_back(dataDir+fileBase+atom+intToString(i+1)+"+_"+timePeriod+".dat");
			}
		}
	} 
	//return the filelist
	return fileNames;

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
	
	if (inData.fail()) {
		//file couldn't be open let user know
		cout << fileName << " - Couldn't be open" << endl;
		
		//now put a single empty structure into the vector
		momentFile momentEntry;
		momentFileData.push_back(momentEntry);
	} else {
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
	}
	
	//close file
	inData.close();
	
	//return the structure
	return momentFileData;
}

