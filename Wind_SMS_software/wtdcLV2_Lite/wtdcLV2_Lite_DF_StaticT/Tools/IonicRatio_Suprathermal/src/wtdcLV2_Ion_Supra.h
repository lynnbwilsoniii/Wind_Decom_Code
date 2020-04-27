
//Header file for all the ionic output algorithms from the distfunc files

//*********************Header Guard
#ifndef WTDCLV2_ION_SUPRA_H
#define WTDCLV2_ION_SUPRA_H

//*********************Include any dependencies throughout all the pieces
#include <string>
#include <iostream>
#include <math.h>
#include <vector>
#include <fstream>
#include <sstream>
#include <stdlib.h>
#include <iomanip>
#include <time.h>
#include <cstddef>

//********************define structures which we will be using

//structure for the SWE file
struct SWERecord {
  double year;
  double day;
  double fracday;
  double Vx;
  double Vy;
  double Vz;
};

//structure for the distfunc file
struct distFuncRecord {
	double year;
	double doy;
	double sector;
	double telescope;
	double eoq;
	std::string ion;
	double df;
	double df_error;
	double cyc_accum;
};

//structure for the doys, this way we include the cyc_accum for each unique doy
struct doyRecord {
	double year;
	double doy;
	double cyc_accum;
};

//********************declare any global constants and variables that we will use
const double PI = 4.0*atan(1.0);
const double SEC_IN_DAY = 60.0*60.0*24.0;

#endif
