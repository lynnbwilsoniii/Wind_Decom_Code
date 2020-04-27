
//Header file for all the ionic output algorithms from the moment files

//*********************Header Guard
#ifndef WTDCLV2_ION_H
#define WTDCLV2_ION_H

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
};

//********************declare any global constants and variables that we will use
const double PI = 4.0*atan(1.0);


#endif
