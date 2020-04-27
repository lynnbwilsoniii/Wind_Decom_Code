//supplemental functions which can be used by the various programs in the
//tools

//include declarations
#include <wtdcLV2_Ion_Supra.h>

//name the namespace
using namespace std;

/*********************************************************************

	Function to read in the contents of the distfunc file and populate the
	appropriate structure
	
	input: fileName - string name of the moment file
	
	output: dfFile - structure which holds all the values from the file
	
*********************************************************************/

vector<distFuncRecord> readFile(string fileName) {

	//declare variables which we will need
	ifstream inData;
	vector<distFuncRecord> df;
	
	//open file
	inData.open(fileName.c_str());
	
	//read the first four header lines
	string line;
	for (int i=0;i<4;i++) getline(inData, line);

	//now read in the file
	while(getline(inData,line)) {
	
		//declare the entry structure
		distFuncRecord dfEntry;

		//now read in and place data into the structure		
		dfEntry.year = atof(line.substr(0,8).c_str());
		dfEntry.doy = atof(line.substr(8,13).c_str());
		dfEntry.sector = atof(line.substr(21,13).c_str());
		dfEntry.telescope = atof(line.substr(34,13).c_str());
		dfEntry.eoq = atof(line.substr(47,12).c_str());
		dfEntry.ion = line.substr(59,9).c_str();
		dfEntry.df = atof(line.substr(68,14).c_str());
		dfEntry.df_error = atof(line.substr(82,14).c_str());
		dfEntry.cyc_accum = atof(line.substr(96,14).c_str());
		
		//add the entry to the vector
		df.push_back(dfEntry);
	}
	
	//close file
	inData.close();
	
	//return the structure
	return df;
}

/*********************************************************************

	Function to return the particle velocity for each energy step from 
	STICS given the name of ion
	
	input: ion - string name of the ion
	
	output: velPart - vector of velocities for each eoq step
	
*********************************************************************/

double calcVelParticle(string ion, double eoqSC) {
	
  	//determine the mass and charge, based on the ion name
  	double mass = 0.0;
  	double charge = 0.0;
  	
  	if (ion == "H+") {
		mass = 1.0079;
		charge = 1.0;
	} else if (ion == "H+_D") {
		mass = 1.0079;
		charge = 1.0;
	} else if (ion == "He+") {
		mass = 4.0026;
		charge = 1.0;
	} else if (ion == "He2+") {
		mass = 4.0026;
		charge = 2.0;
	} else if (ion == "C+") {
		mass = 12.011;
		charge = 1.0;
	} else if (ion == "C2+") {
		mass = 12.011;
		charge = 2.0;
	} else if (ion == "C4+") {
		mass = 12.011;
		charge = 4.0;
	} else if (ion == "C5+") {		
		mass = 12.011;
		charge = 5.0;
	} else if (ion == "C6+") {
		mass = 12.011;
		charge = 6.0;
	} else if (ion == "O+") {
		mass = 15.999;      
		charge = 1.0;
	} else if (ion == "O6+") {
		mass = 15.999;
		charge = 6.0;
	} else if (ion == "O7+") {
		mass = 15.999;
		charge = 7.0;
	} else if (ion == "Ne+") {
		mass = 20.180;
		charge = 1.0;
	} else if (ion == "Fe8+") {
		mass = 55.845;
		charge = 8.0;
	} else if (ion == "Fe9+") {
		mass = 55.845;
		charge = 9.0;
	} else if (ion == "Fe10+") {
		mass = 55.845;
		charge = 10.0;
	} else if (ion == "Fe11+") {
		mass = 55.845;
		charge = 11.0;
	} else if (ion == "Fe12+") {
		mass = 55.845;
		charge = 12.0;
	} else if (ion == "Fe14+") {
		mass = 55.845;
		charge = 14.0;
	} else if (ion == "Fe16+") {
		mass = 55.845;
		charge = 16.0;
	}
	
	//finally calculate the velocity and append it to the velocity vector
	double velPart;
	velPart = 437.74*sqrt(eoqSC*(charge/mass));
	
	//return to calling function
	return velPart;
	
}

/*********************************************************************

	Function to calculate the deltaV span in the velocity bin
	
	input: eoq - current df entries eoq
	
	output: delV - the span between this velocity bin and the next highest
	
*********************************************************************/

double deltaV(string ion, double eoq) {

	//declare an array holding all the eoqs from STICS
	double eoqSC [] = {6.1907, 6.9496, 7.8015, 8.7579, 9.8315, 11.0367, 12.3896, 
					13.9084, 15.6134, 17.5274, 19.6760, 22.0880, 24.7956, 27.8352, 
					31.2474, 35.0779, 39.3780, 44.2052, 49.6241, 55.7073, 62.5362,
					70.2022, 78.8080, 88.4688, 99.3138, 111.4882, 125.1551, 140.4973,
					157.7203, 177.0545, 198.7588, 223.1238};
					
	//Determine the next highest eoq
	int found = 0;
	int index = 0;
	double eoqNext = 0.0;
	while (found == 0) {
		//check that index is within bounds of options
		if (index > 31) {
			found = 1; //to exit the loop
			//just assume that deltaV[last-1] = deltaV[last]
			eoqNext = eoqSC[31];
			eoq = eoqSC[30];
		} else {
			//check to see if eoq is higher than current one
			if (eoqSC[index] > eoq) {
				found = 1;
				eoqNext = eoqSC[index];
			} else {
				index++;
			}
		}
	}
	
	//Finally, determine the change in velocity
	double delV = calcVelParticle(ion, eoqNext) - calcVelParticle(ion, eoq);
	
	//return and end
	return delV;	
}
  	
/*********************************************************************

	Function to remove the leading whitespace from a string
	
	input: stringWhiteSpace - string with white space in front of it
	
	output: stringClean - string with the leading white space removed
	
*********************************************************************/
  	
string cleanWSString(string stringWhiteSpace) {

	//find the index of the first non white space character
	size_t found = stringWhiteSpace.find_first_not_of(" ");
	
	//now determine the length of the string
	int length = (stringWhiteSpace.length()-found)+1;
	
	//create the substring containing only the ion name
	string stringClean = stringWhiteSpace.substr(found, length);
	
	return stringClean;

}  

/*********************************************************************

	Function to determine the unique doyfr in a Wind STICS distfunc file
	
	input: dfFile - structure which holds all the values from the file
	
	output: doys - vector of the unique doyfr in the structure
	
*********************************************************************/

vector<doyRecord> uniqDoy(vector<distFuncRecord> dfFile) {

	//begin by constructing the vector to hold the unique doys
	vector<doyRecord> doys;
	
	//put the very first doy into the array because that is unique, as we have no doys yet
	doyRecord doyTemp;
	doyTemp.year = dfFile[0].year;
	doyTemp.doy  = dfFile[0].doy;
	doyTemp.cyc_accum = dfFile[0].cyc_accum;
	doys.push_back(doyTemp);
	
	int currentDoyVecInd = 0; //index of the most recent unique doy added. Since 
							  //the dffile is wrote in a way that increases with time 
							  //we only ever need to test the most recent unique time added
	
	//now loop through the entire structure and grab just the unique ones
	for (unsigned int i = 1; i< dfFile.size(); i++) {
		if (dfFile[i].doy > doys[currentDoyVecInd].doy) {
			doyTemp.year = dfFile[i].year;
			doyTemp.doy  = dfFile[i].doy;
			doyTemp.cyc_accum = dfFile[i].cyc_accum;
			doys.push_back(doyTemp);
			
			currentDoyVecInd++;
		} //add new doy if it is unique
	} // loop through all doys
	
	//return the vector and exit the subfunction
	return doys;
}

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

/*********************************************************************

	Function to calculate the density of the plasma above a particular 
	velocity cutoff
	
	method: loop through the distribution function file, and if the entry falls on the correct 
			day and has a particle velocity above a set threshhold, then add this contribution 
			to the density

	
	input:  dfFile - structure which holds all the values from the file
			velPart - velocity vector for the particular ion
			doyCurrent - the DOY that is being used
			cutOff - the particular velocity cutoff for the suprathermal particles (in W)
			SW Velocity - the current bulk velocity from the SWE file
	
	output: n - Vector containing density and error of the suprathermal component of the plasma
	
*********************************************************************/

vector<double> calcDens(vector<distFuncRecord> dfFile, double doy, double cutOff, double SWVel) {
	
	//declare any variables we will need
	double solid_angle[3] = {0.537057, 0.892396, 0.537057}; //solid angle from the integration over the viewing angle
	for (int i=0; i<3; i++) solid_angle[i] *= (PI/8.0);
	double n_step = 0.0;
	double n_step_err = 0.0;
	
	//now loop through the distfunc file
	for (unsigned int i=0; i<dfFile.size(); i++) {
		//determine velocity of entry and convert to W-space
		if (SWVel > 0.0) {
			double velSPart = calcVelParticle(cleanWSString(dfFile[i].ion), dfFile[i].eoq)/SWVel;
			//check to see if the particular entry in the file occurs on the correct date and has an acceptable velocity
			if (dfFile[i].doy == doy && velSPart >= cutOff) {
				int tele = (int) dfFile[i].telescope;
				n_step += solid_angle[tele]*dfFile[i].df*
						  pow((velSPart*SWVel),2.0)*deltaV(cleanWSString(dfFile[i].ion),dfFile[i].eoq);
				n_step_err += pow(solid_angle[tele]*dfFile[i].df*
						  pow((velSPart*SWVel),2.0)*deltaV(cleanWSString(dfFile[i].ion),dfFile[i].eoq),2.0);
			}
		} else {
			//non real solar wind velocity
			n_step = 0.0;
			n_step_err = 0.0;
		}
	}
	
	//now normalize the error and prepare a vector to return in reasonable units
	n_step_err = sqrt(n_step_err);
	vector<double> n (2,0);
	n[0] = n_step/1.0e15;
	n[1] = n_step_err/1.0e15;
	
	//return the vector
	return n;
}
