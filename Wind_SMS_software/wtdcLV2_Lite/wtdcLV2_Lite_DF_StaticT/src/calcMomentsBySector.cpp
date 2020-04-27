
/*----------------------------------------------------------------------------------------
Name: calcMomentsBySector

Purpose: Calculates and returns the 3-d moments to the STICS processor broken into different sectors

Inputs: df - the ions distribution function
		df_err - array holding the error in df
		vel - the particle velocity
		cyc_accum - holds the accumulation time for the current time period
		velCutOff - the suprathermal cutoff velocity
		
Outputs: momentsBySector - 3d Moments structure that holds all the moments and 
					 derived physical parameters
----------------------------------------------------------------------------------------*/
#include <iostream>
#include "wtdcLV2_Lite.h"

using namespace std;

vector<momentsBySector> calcMomentsBySector(const double _df[][16][3], const double _df_err[][16][3], double vel_part[], double cyc_accum, double velCutOff) {
	
	//create the output vector
	vector<momentsBySector> returnMoments;
	vector<double> inside (3, 0.0);
	vector< vector<double> > inside2 (16, inside);
	vector< vector< vector<double> > > df (32, inside2);
	vector< vector< vector<double> > > df_err (32, inside2);

	for(int i=0; i< 32; i++) {
		for(int j=0; j < 16; j++) {
			for(int k=0; k < 3; k++) {
				df[i][j][k] = _df[i][j][k];
				df_err[i][j][k] = _df_err[i][j][k];
			}
		}
	}
	
	//calculate the delta v array which will be used throughout
	double deltaV[32];
	for (int i=0; i<31; i++)
	{
		deltaV[i] = vel_part[i+1]-vel_part[i];
	}
	//need to handle the final step
	deltaV[31] = deltaV[30];
	
	//store the integrated solid angle element for each telescope, sector viewing
	double solid_angle[3] = {0.537057, 0.892396, 0.537057};
	for (int i=0; i<3; i++) solid_angle[i] *= (PI/8.0);
	
	//quickly normalize the distribution function by the accumulation time as this hasn't happened yet
	//also turn the errors from percentage error to an actual error as this hasn't happened yet either
	//Finally, here we will set the distribution function to 0 if it is below the velocity cutoff
	//Easiest to do this here, as it will then filter down throughout the moment calculations
	for (int i=0; i<32; i++) {
		for (int j=0; j<16; j++) {
			for (int k=0; k<3; k++) {
				df[i][j][k] /= cyc_accum;
				if (df_err[i][j][k] > 0.0) {
					df_err[i][j][k] = (sqrt(df_err[i][j][k])/df_err[i][j][k])*df[i][j][k];
				} else {
					df_err[i][j][k] = 0.0;
				}
					
				//if the particle doesn't have enough speed, set the df to 0
				if (vel_part[i] < velCutOff) {
					df[i][j][k] = 0.0;
					df_err[i][j][k] = 0.0;
				}
			}
		}
	}
	
	//loop through the sectors calculating the moment in each sector
	for (int j=0; j< 16; j++) {
		// make a structure which is the current moment record
		momentsBySector momentsSector;
		momentsSector.sector = j;
		
		//************calculate the 0th moment********************
		// Adapted from ACE/SWICS from distfunc.cc, but taking into consideration the viewing angle of STICS
		// F0 = n
		// units: 1/km^3
		for (int i=0; i<32; i++) {
			for (int k=0; k<3; k++) {
				momentsSector.f0 += solid_angle[k]*df[i][j][k]*pow(vel_part[i],2)*deltaV[i];
			}
		}
		
		//if the integrated moment was going to be 0.0, turn it into a Nan as this is 
		//our general fill value
		if (momentsSector.f0 == 0.0) {
			momentsSector.f0 = -sqrt(-1.0);
		}
	
		//************calculate the error in the 0th moment********
		for (int i=0; i<32; i++) {
			for (int k=0; k<3; k++) {
				momentsSector.f0_err += pow((solid_angle[k]*pow(vel_part[i],2)*deltaV[i]*df_err[i][j][k]),2.0);
			}
		}
		momentsSector.f0_err = sqrt(momentsSector.f0_err);
		
		//if the integrated moment was going to be 0.0, turn it into a Nan as this is 
		//our general fill value
		if (momentsSector.f0_err == 0.0) {
			momentsSector.f0_err = -sqrt(-1.0);
		}
		
		//need the 1st moment in order to get the velocity
		//declare these variables
		double v = 0.0;
		
		//************calculate the 1st moment*********************
		// Just like ACE/SWICS from distfunc.cc
		// F1 = n*v0
		// units: 1/s*km^2
		for (int i=0; i<32; i++) {
			for (int k=0; k<3; k++) {
				momentsSector.f1 += solid_angle[k]*df[i][j][k]*pow(vel_part[i],3)*deltaV[i];
			}
		}
		
		//************calculate the error in the 1st moment********
		for (int i=0; i<32; i++) {
			for (int k=0; k<3; k++) {
				momentsSector.f1_err += pow((solid_angle[k]*df_err[i][j][k]*pow(vel_part[i],3)*deltaV[i]),2.0);
			}
		}
		momentsSector.f1_err = sqrt(momentsSector.f1_err);
		
		//*************calculate the mean local velocity************
		// Easy from above
		// v0 = n*V0/n = F1/F0
		// units: km/s
		if (momentsSector.f0 > 0.0) {
			v = momentsSector.f1/momentsSector.f0; //not used in F2 moment now, (Pat edit)
			
		} else {
			//to get a nan we just use some nonsensical math operation
			v = -sqrt(-1);
		}

		//*************calculate the 2nd moment*********************
		// Just like ACE/SWICS from distfunc.cc
		// F2 = n vth^2
		// units: 1/s*km^2
		for (int i=0; i<32; i++) {
			for (int k=0; k<3; k++) {
				momentsSector.f2 += solid_angle[k]*df[i][j][k]*pow((vel_part[i]-0.0),2.0)*pow(vel_part[i],2)*deltaV[i];
				//Pat Edit: Changed this to calculation of average energy, instead of thermal energy
				//i.e. replaced "v" with 0.0 in moment.
			}
		}
	
		//************calculate the error in the 2nd moment********
		for (int i=0; i<32; i++) {
			for (int k=0; k<3; k++) {
				momentsSector.f2_err += pow(solid_angle[k]*df_err[i][j][k]*pow((vel_part[i]-0.0),2.0)*pow(vel_part[i],2)*deltaV[i],2.0);
				//Pat Edit: Just as above, replaced "v" with 0.0 in moment.
			}
		}
		momentsSector.f2_err = sqrt(momentsSector.f2_err);
	
		//now put this new recorde in the vector
		returnMoments.push_back(momentsSector);
	}

	return returnMoments;	
}
