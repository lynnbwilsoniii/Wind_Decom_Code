/*----------------------------------------------------------------------------------------
Name: calcMoments.cpp

Purpose: Calculates and returns the 3-d moments to the STICS processor

Inputs: df - the ions distribution function
		df_err - array holding the measurement error from the df
		vel - the particle velocity
		cyc_accum - the amount of time the current measurement was accumulated for
		velCutOff - the suprathermal cutoff velocity
		
Outputs: moments - 3d Moments structure that holds all the moments and 
					 derived physical parameters
----------------------------------------------------------------------------------------*/
#include <iostream>
#include "wtdcLV2_Lite.h"

using std::vector;

moments calcMoments(const double _df[][16][3], const double _df_err[][16][3], double vel_part[], double cyc_accum, double velCutOff) {
	// making it pass by value instead of reference
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

	//begin by initializing all the variables	
	double f0        = 0.0;
	double f0_err    = 0.0;
	double f1        = 0.0;
	double f1_err    = 0.0;
	double f2        = 0.0;
	double f2_err    = 0.0;
	double n         = 0.0;
	double v         = 0.0;
	double v_th      = 0.0;
	double n_err     = 0.0;
	double v_err     = 0.0;
	double v_th_err  = 0.0;
	
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
				//normalize the distribution function
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
	
	//************calculate the 0th moment********************
	// Adapted from ACE/SWICS from distfunc.cc, but taking into consideration the viewing angle of STICS
	// F0 = n
	// units: 1/km^3
	for (int i=0; i<32; i++) {
		for (int j=0; j<16; j++) {
			for (int k=0; k<3; k++) {
				f0 += solid_angle[k]*df[i][j][k]*pow(vel_part[i],2)*deltaV[i];
			}
		}
	}
	
	//************calculate the error in the 0th moment********
	for (int i=0; i<32; i++) {
		for (int j=0; j<16; j++) {
			for (int k=0; k<3; k++) {
				f0_err += pow((solid_angle[k]*pow(vel_part[i],2)*deltaV[i]*df_err[i][j][k]),2.0);
			}
		}
	}
	f0_err = sqrt(f0_err);

	//************calculate the 1st moment*********************
	// Just like ACE/SWICS from distfunc.cc
	// F1 = n*v0
	// units: 1/s*km^2
	for (int i=0; i<32; i++) {
		for (int j=0; j<16; j++) {
			for (int k=0; k<3; k++) {
				f1 += solid_angle[k]*df[i][j][k]*pow(vel_part[i],3)*deltaV[i];
			}
		}
	}
	
	//************calculate the error in the 1st moment********
	for (int i=0; i<32; i++) {
		for (int j=0; j<16; j++) {
			for (int k=0; k<3; k++) {
				f1_err += pow((solid_angle[k]*df_err[i][j][k]*pow(vel_part[i],3)*deltaV[i]),2.0);
			}
		}
	}
	f1_err = sqrt(f1_err);
	
	//*************calculate the mean local velocity************
	// Easy from above
	// v0 = n*V0/n = F1/F0
	// units: km/s
	if (f0 > 0.0) {
		//v = f1/f0;
		v=-sqrt(-1); //Pat edit: changed this so all average speeds are NaN
	} else {
		//to get a nan we just use some nonsensical math operation
		v = -sqrt(-1);
	}
	
	//**************calculate the error in the mean local velocity**
	if (f0 > 0.0 && f1 > 0.0) {
		//v_err = v*sqrt(pow((f1_err/f1),2)+pow((f0_err/f0),2));
		v_err=-sqrt(-1); //Pat edit: changed this so all average speeds are NaN
	} else {
		//fill with a nan if both the first and second moment are junk
		v_err = -sqrt(-1);
	}
	
	//*************calculate the 2nd moment*********************
	// Just like ACE/SWICS from distfunc.cc
	// F2 = n vth^2
	// units: 1/s*km^2
	for (int i=0; i<32; i++) {
		for (int j=0; j<16; j++) {
			for (int k=0; k<3; k++) {
				f2 += solid_angle[k]*df[i][j][k]*pow((vel_part[i]-0.0),2.0)*pow(vel_part[i],2)*deltaV[i];
				//Pat Edit: Changed this to calculation of average energy, instead of thermal energy
				//i.e. replaced "v" with 0.0 in moment.
			}
		}
	}
	
	//************calculate the error in the 2nd moment********
	for (int i=0; i<32; i++) {
		for (int j=0; j<16; j++) {
			for (int k=0; k<3; k++) {
				f2_err += pow(solid_angle[k]*df_err[i][j][k]*pow((vel_part[i]-0.0),2.0)*pow(vel_part[i],2)*deltaV[i],2.0);
				//Pat Edit: Just as above, replaced "v" with 0.0 in moment.
			}
		}
	}
	f2_err = sqrt(f2_err);
	
	//*************calculate the thermal velocity***************
	// Again, easy from the moments
	// vth = sqrt(F2/F0)
	// units: km/s
	if (f0 > 0.0) {
		v_th = sqrt(f2/f0);
	} else {
		//to get a nan we just use some nonsensical math operation
		v_th = -sqrt(-1);
	}
	
	//**************calculate the error in thermal velocity*******
	if (f0 > 0.0 && f2 > 0.0) {
		v_th_err = (1.0/2.0)*v_th*sqrt(pow((f2_err/f2),2)+pow((f0_err/f0),2)); 
		//added the (1.0/2.0), Jacob forgot this in his error propagation of vth=sqrt(f2/f0)
	} else {
		//fill with a nan if both the first and second moment are junk
		v_th_err = -sqrt(-1);
	}	
	
	//**************Finally, calculate the density**************
	// Easy from moments, distfunc.cc uses the duty cycle, for now I'm leaving it out
	// n = f0
	// units: 1/km^3 and convert to 1/cm^3 with factor of 1.0e-15
	if (f0 > 0.0) {
		n = f0/1.0e15;
	} else {
		//to get a nan we just use some nonsensical math operation
		n = -sqrt(-1);
	}

	//***************And the error in this density*****************
	if (f0 > 0.0) {
		n_err = f0_err/1.0e15;
	} else {
		//fill with nan if there is no density
		n_err = -sqrt(-1);
	}
	
	//**************Populate the structure to return to the main function
	moments returnMoments;
	returnMoments.f0   = f0;
	returnMoments.f1   = f1;
	returnMoments.f2   = f2;
	returnMoments.n    = n;
	returnMoments.v_0  = v;
	returnMoments.v_th = v_th;
	returnMoments.n_err = n_err;
	returnMoments.v_0_err = v_err;
	returnMoments.v_th_err = v_th_err;

	return returnMoments;	
}
