
//Header file for all the ionic output algorithms from the moment files

//*********************Header Guard
#ifndef WTDCLV2_CSD_H
#define WTDCLV2_CSD_H

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


//********************define structures which we will be using

struct momentFile {
	double year;
	double doy;
	std::string ion;
	double n;
	double n_err;
	double v;
	double v_err;
	double v_th;
	double v_th_err;
	double cyc_accum;
	
	momentFile()
	{
		year = 0.0;
		doy = 0.0;
		ion = " ";
		n = 0.0;
		n_err = 0.0;
		v = 0.0;
		v_th = 0.0;
		v_th_err = 0.0;
		cyc_accum = 0.0;
	}
};

//********************declare any global constants and variables that we will use
const double PI = 4.0*atan(1.0);
const double SEC_IN_DAY = 60.0*60.0*24.0;

#endif
